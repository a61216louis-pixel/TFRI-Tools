library(shiny)
library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(emmeans)
library(rcompanion)
library(FSA)   # dunnTest()：Kruskal-Wallis/Scheirer-Ray-Hare 之後的無母數事後比較
library(DT)
# 注意：multcompView 不用 library() 掛載，程式中一律用 multcompView::multcompLetters() 明確呼叫，
# 避免與 rcompanion 內部已匯入的 multcompView 命名空間互相衝突（會導致卸載錯誤）。

options(shiny.maxRequestSize = 500 * 1024^2)

# ============================================================
# 共用函式
# ============================================================

TAX_COLS <- c("OTU_ID", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

# 從樣本欄位名稱解析 Treatment / Rep / Subsample / Block
# 命名規則: (CtP|Ct|NaP|Na) R <repnum> <A/B> _ ...
parse_sample_metadata <- function(sample_cols) {
  pat <- "^(CtP|Ct|NaP|Na)R([0-9]+)([A-Za-z])(?:_|$)"
  m <- regexec(pat, sample_cols, perl = TRUE)
  parts <- regmatches(sample_cols, m)

  ok <- lengths(parts) == 4
  validate(need(all(ok),
                 paste0("以下樣本欄位名稱無法辨識 Treatment/Rep/Subsample，請確認命名規則 (CtP/Ct/NaP/Na + R + 數字 + A/B)：\n",
                        paste(sample_cols[!ok], collapse = ", "))))

  treatment <- vapply(parts, `[`, character(1), 2)
  repnum    <- vapply(parts, `[`, character(1), 3)
  subsample <- vapply(parts, `[`, character(1), 4)

  meta <- data.frame(
    SampleID  = sample_cols,
    Treatment = factor(treatment, levels = c("Ct", "CtP", "Na", "NaP")),
    RepNum    = as.integer(repnum),
    Subsample = toupper(subsample),
    stringsAsFactors = FALSE
  )
  meta$Salt    <- factor(ifelse(meta$Treatment %in% c("Na", "NaP"), "Na", "Ct"), levels = c("Ct", "Na"))
  meta$Microbe <- factor(ifelse(meta$Treatment %in% c("CtP", "NaP"), "P", "NoP"), levels = c("NoP", "P"))
  meta$Block   <- factor(ifelse(meta$RepNum %in% c(1, 2), "Block1", "Block2"), levels = c("Block1", "Block2"))
  meta$RepID   <- paste0(meta$Treatment, "_R", meta$RepNum)  # 同一個 Rep(Tray) 的唯一識別，A/B 為其假重複
  meta
}

# 依規則過濾 ASV：移除葉綠體/粒線體、豐富度(總讀序數)、普遍率(出現樣本數)
filter_asv <- function(tax_df, counts_mat, opts) {
  keep <- rep(TRUE, nrow(tax_df))

  if (opts$rm_chloroplast) {
    keep <- keep & !(tolower(coalesce(tax_df$Order, "")) == "chloroplast")
  }
  if (opts$rm_mito) {
    keep <- keep & !(tolower(coalesce(tax_df$Family, "")) == "mitochondria")
  }

  total_count <- rowSums(counts_mat)
  prevalence  <- rowSums(counts_mat > 0)

  keep <- keep & (total_count >= opts$min_total_count) & (prevalence >= opts$min_prevalence)

  list(
    tax = tax_df[keep, , drop = FALSE],
    counts = counts_mat[keep, , drop = FALSE],
    n_before = nrow(tax_df),
    n_after = sum(keep),
    reads_before = sum(counts_mat),
    reads_after = sum(counts_mat[keep, , drop = FALSE])
  )
}

# 計算 Alpha 多樣性指數 (輸入: samples-as-rows 的計數矩陣)
compute_alpha_indices <- function(comm) {
  obs     <- specnumber(comm)
  shannon <- diversity(comm, index = "shannon")
  simpson <- diversity(comm, index = "simpson")
  pielou  <- ifelse(obs > 1, shannon / log(obs), NA)
  chao1   <- tryCatch(estimateR(comm)["S.chao1", ], error = function(e) rep(NA, nrow(comm)))
  data.frame(
    SampleID = rownames(comm),
    Observed = obs,
    Shannon  = shannon,
    Simpson  = simpson,
    Pielou   = pielou,
    Chao1    = as.numeric(chao1),
    row.names = NULL
  )
}

# LMM: value ~ Salt*Microbe + Block(固定或隨機)，回傳 ANOVA table 與模型
# block_effect: "random" -> (1|Block) 隨機效應 (適合 Block 層級數多的情況)
#               "fixed"  -> + Block  固定效應 (Block 層級數少，如本設計只有2個，建議用這個，避免 singular fit)
run_lmm <- function(df, block_effect = "fixed") {
  if (block_effect == "random") {
    m <- tryCatch(
      lmerTest::lmer(value ~ Salt * Microbe + (1 | Block), data = df),
      error = function(e) e, warning = function(w) w
    )
  } else {
    m <- tryCatch(
      lm(value ~ Salt * Microbe + Block, data = df),
      error = function(e) e
    )
  }
  if (inherits(m, "error") || inherits(m, "warning")) {
    return(list(model = NULL, anova = NULL, error = conditionMessage(m)))
  }
  av <- tryCatch({
    a <- as.data.frame(anova(m))
    a$Term <- rownames(a)
    a
  }, error = function(e) NULL)
  list(model = m, anova = av, error = NULL)
}

# emmeans Tukey 事後檢定 + compact letter display（依 Treatment 四組）
# 改用 multcompView::multcompLetters 直接從成對 P 值組字母標示，避開 multcomp::cld() 對 emmGrid 的相容性問題
run_posthoc <- function(df, block_effect = "fixed") {
  df$Group <- interaction(df$Salt, df$Microbe, sep = "_", drop = TRUE)

  m <- tryCatch({
    if (block_effect == "random") {
      lmerTest::lmer(value ~ Group + (1 | Block), data = df)
    } else {
      lm(value ~ Group + Block, data = df)
    }
  }, error = function(e) e, warning = function(w) w)

  if (inherits(m, "error") || inherits(m, "warning")) {
    return(list(pairwise = NULL, cld = NULL, error = conditionMessage(m)))
  }

  res <- tryCatch({
    em <- emmeans(m, ~ Group)
    pw <- as.data.frame(pairs(em, adjust = "tukey"))

    pvals <- pw$p.value
    names(pvals) <- gsub("\\s+", "", pw$contrast)  # "Ct_NoP - Ct_P" -> "Ct_NoP-Ct_P"

    letters_res <- multcompView::multcompLetters(pvals)$Letters

    emm_df <- as.data.frame(em)
    emm_df$Group <- as.character(emm_df$Group)
    emm_df$.group <- as.character(letters_res[emm_df$Group])

    list(pairwise = pw, cld = emm_df, error = NULL)
  }, error = function(e) list(pairwise = NULL, cld = NULL, error = conditionMessage(e)))

  res
}

# 無母數 Scheirer-Ray-Hare 二因子檢定 (Salt x Microbe)
run_srh <- function(df) {
  tryCatch({
    res <- rcompanion::scheirerRayHare(value ~ Salt * Microbe, data = df, verbose = FALSE)
    as.data.frame(res)
  }, error = function(e) NULL)
}

# Dunn's test：SRH/Kruskal-Wallis 之後的無母數事後比較 (四組 Salt_Microbe)，
# 用 Benjamini-Hochberg 校正多重比較，並用 multcompLetters 產生字母標示
run_dunn <- function(df) {
  df$Group <- interaction(df$Salt, df$Microbe, sep = "_", drop = TRUE)

  res <- tryCatch({
    dt <- FSA::dunnTest(value ~ Group, data = df, method = "bh")
    pw <- dt$res  # 欄位: Comparison, Z, P.unadj, P.adj

    pvals <- pw$P.adj
    names(pvals) <- gsub("\\s+", "", pw$Comparison)  # "Ct_NoP - Ct_P" -> "Ct_NoP-Ct_P"

    letters_res <- multcompView::multcompLetters(pvals)$Letters

    summ <- df %>%
      dplyr::group_by(Group) %>%
      dplyr::summarise(median = median(value, na.rm = TRUE), n = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(Group = as.character(Group))
    summ$.group <- as.character(letters_res[summ$Group])

    list(pairwise = pw, cld = summ, error = NULL)
  }, error = function(e) list(pairwise = NULL, cld = NULL, error = conditionMessage(e)))

  res
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  titlePanel("苦楝根系微生物相多樣性分析平台（RCBD LMM）"),
  sidebarLayout(
    sidebarPanel(
      fileInput("asv_csv", "上傳 ASV 豐度表 (asv_table_final.csv)", accept = ".csv"),
      helpText("需含 OTU_ID, Kingdom, Phylum, Class, Order, Family, Genus, Species + 各樣本讀序數欄位。"),
      hr(),
      h4("① 過濾條件"),
      checkboxInput("rm_chloroplast", "移除葉綠體 (Order = Chloroplast)", TRUE),
      checkboxInput("rm_mito", "移除粒線體 (Family = Mitochondria)", TRUE),
      numericInput("min_total_count", "最小總讀序數 (豐富度篩選)：", 10, min = 0),
      numericInput("min_prevalence", "最小出現樣本數 (普遍率篩選)：", 2, min = 1),
      hr(),
      h4("② 標準化 / 稀疏化 (Rarefaction)"),
      radioButtons("rarefy_mode", "稀疏化深度：",
                   choices = c("自動 (取最小樣本深度)" = "auto", "手動指定" = "manual")),
      conditionalPanel("input.rarefy_mode == 'manual'",
                        numericInput("rarefy_depth_manual", "指定深度 (reads)：", 10000, min = 100)),
      numericInput("rarefy_seed", "亂數種子 (seed)：", 1234),
      hr(),
      h4("③ 統計分析設定"),
      checkboxInput("aggregate_rep", "統計檢定 (LMM/SRH/PERMANOVA) 先將 A/B 子樣本平均為 Rep 層級 (n=12，建議勾選，避免假重複)", TRUE),
      checkboxInput("beta_show_all_points", "Beta 排序圖 (PCoA/NMDS) 顯示所有子樣本點 (n=24)，而非只顯示 Rep 平均點", TRUE),
      radioButtons("block_effect", "LMM 中 Block 的處理方式：",
                   choices = c("固定效應 +Block（建議：Block 層級數少，如本設計僅2組）" = "fixed",
                               "隨機效應 (1|Block)（適合 Block 層級數多，如 ≥5 組）" = "random"),
                   selected = "fixed"),
      radioButtons("beta_metric", "Beta 多樣性距離：",
                   choices = c("Bray-Curtis" = "bray", "Jaccard" = "jaccard")),
      numericInput("permanova_perm", "PERMANOVA 排列次數：", 999, min = 99),
      radioButtons("permanova_by", "PERMANOVA 平方和拆解方式：",
                   choices = c("Terms（建議：本設計為平衡正交設計，會列出所有主效果+交互作用）" = "terms",
                               "Margin（注意：有交互作用時只列出交互作用，主效果會被省略）" = "margin"),
                   selected = "terms"),
      helpText("提醒：adonis2 的 by=\"margin\" 在模型含交互作用時，只檢定交互作用本身，",
               "不會列出 Salt/Microbe 主效果（vegan 的既定行為，非錯誤）。",
               "本設計為平衡 2×2 因子設計，用 by=\"terms\" 即可得到與邊際檢定一致、且完整的主效果+交互作用結果。"),
      hr(),
      actionButton("run_analysis", "④ 執行分析", class = "btn-primary", width = "100%")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("過濾摘要",
                 br(),
                 tableOutput("filter_summary"),
                 downloadButton("dl_filter_summary", "下載過濾摘要 (.csv)"),
                 hr(),
                 h4("稀疏化曲線 (Rarefaction Curve)"),
                 plotOutput("rarecurve_plot", height = "450px")
        ),
        tabPanel("Alpha 多樣性",
                 br(),
                 h4("多樣性指數表"),
                 DTOutput("alpha_table"),
                 downloadButton("dl_alpha_table", "下載 Alpha 多樣性表 (.csv)"),
                 hr(),
                 h4("盒鬚圖 (依處理組)"),
                 plotOutput("alpha_boxplot", height = "600px"),
                 downloadButton("dl_alpha_box", "下載盒鬚圖 (.png)"),
                 hr(),
                 h4("交互作用圖 (Salt × Microbe)"),
                 plotOutput("alpha_interaction", height = "500px"),
                 downloadButton("dl_alpha_inter", "下載交互作用圖 (.png)"),
                 hr(),
                 h4("RCBD LMM 結果 (value ~ Salt * Microbe + (1|Block))"),
                 tableOutput("lmm_anova"),
                 downloadButton("dl_lmm_anova", "下載 LMM ANOVA 表 (.csv)"),
                 h4("Tukey 事後比較 (Salt × Microbe，四組)"),
                 tableOutput("posthoc_table"),
                 downloadButton("dl_posthoc", "下載 Tukey 事後比較表 (.csv)"),
                 uiOutput("posthoc_error_msg"),
                 hr(),
                 h4("無母數檢定 Scheirer-Ray-Hare (穩健性檢驗)"),
                 tableOutput("srh_table"),
                 downloadButton("dl_srh", "下載無母數檢定表 (.csv)"),
                 hr(),
                 h4("無母數事後比較 Dunn's Test (Salt × Microbe，四組，BH 校正)"),
                 helpText("SRH/Kruskal-Wallis 顯著之後的無母數事後比較，與 Tukey (母數) 各自獨立，",
                          "不依賴常態性假設；建議只解讀 SRH 本身顯著的指數對應的 Dunn's Test 結果。"),
                 tableOutput("dunn_table"),
                 downloadButton("dl_dunn", "下載 Dunn's Test 表 (.csv)"),
                 uiOutput("dunn_error_msg")
        ),
        tabPanel("Beta 多樣性",
                 br(),
                 uiOutput("beta_n_caption"),
                 h4("PCoA 排序圖"),
                 plotOutput("pcoa_plot", height = "550px"),
                 downloadButton("dl_pcoa", "下載 PCoA 圖 (.png)"),
                 hr(),
                 h4("NMDS 排序圖"),
                 plotOutput("nmds_plot", height = "550px"),
                 downloadButton("dl_nmds", "下載 NMDS 圖 (.png)"),
                 hr(),
                 h4("PERMANOVA (adonis2)"),
                 helpText("為避免 A/B 子樣本假重複膨脹顯著性，統計檢定固定使用「統計檢定設定」中指定的樣本層級（見上方 n）。"),
                 tableOutput("permanova_table"),
                 downloadButton("dl_permanova", "下載 PERMANOVA 表 (.csv)"),
                 uiOutput("permanova_by_caption"),
                 h4("組間離散度同質性檢定 (betadisper + permutest)"),
                 tableOutput("betadisper_table"),
                 downloadButton("dl_betadisper", "下載 betadisper 表 (.csv)")
        ),
        tabPanel("物種組成",
                 br(),
                 fluidRow(
                   column(4,
                          selectInput("tax_level", "分類階層：",
                                      choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                      selected = "Phylum")
                   ),
                   column(4,
                          numericInput("comp_top_n", "顯示前 N 類（其餘歸為「其他」）：", value = 10, min = 3, max = 30)
                   ),
                   column(4,
                          radioButtons("comp_group_level", "堆疊圖 X 軸：",
                                       choices = c("處理組 (Treatment，取平均)" = "treatment",
                                                   "個別樣本" = "sample"),
                                       selected = "treatment")
                   )
                 ),
                 h4("相對豐度堆疊長條圖"),
                 plotOutput("composition_plot", height = "550px"),
                 downloadButton("dl_composition", "下載組成圖 (.png)"),
                 hr(),
                 h4("相對豐度表格（依處理組平均，各分類單元 %）"),
                 DTOutput("composition_table"),
                 downloadButton("dl_composition_table", "下載組成表格 (.csv)")
        )
      )
    )
  )
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  raw <- reactiveValues(tax = NULL, counts = NULL, meta = NULL)
  filt <- reactiveValues(tax = NULL, counts = NULL, summary = NULL)
  rare <- reactiveValues(comm = NULL, depth = NULL)
  alpha <- reactiveValues(table = NULL, stat_df = NULL, lmm = NULL, posthoc = NULL, posthoc_errors = NULL,
                          srh = NULL, dunn = NULL, dunn_errors = NULL)
  beta <- reactiveValues(dist = NULL, pcoa = NULL, nmds = NULL, permanova = NULL, disper = NULL,
                          comm_rel = NULL, meta_used = NULL, meta_plot = NULL, n_plot = NULL, n_stats = NULL)

  observeEvent(input$run_analysis, {
    req(input$asv_csv)

    result <- tryCatch({
      withProgress(message = "執行分析中...", value = 0, {

        # ---- 1. 讀取資料 ----
        incProgress(0.05, detail = "讀取資料...")
        df <- read.csv(input$asv_csv$datapath, check.names = FALSE, stringsAsFactors = FALSE)
        validate(need(all(TAX_COLS %in% colnames(df)),
                       paste0("檔案缺少必要欄位：", paste(setdiff(TAX_COLS, colnames(df)), collapse = ", "))))

        sample_cols <- setdiff(colnames(df), TAX_COLS)
        validate(need(length(sample_cols) >= 2, "至少需要 2 個樣本欄位才能進行多樣性分析。"))

        tax_df <- df[, TAX_COLS, drop = FALSE]
        counts_mat <- as.matrix(df[, sample_cols, drop = FALSE])
        rownames(counts_mat) <- df$OTU_ID
        storage.mode(counts_mat) <- "numeric"

        meta <- parse_sample_metadata(sample_cols)
        raw$tax <- tax_df; raw$counts <- counts_mat; raw$meta <- meta

        # ---- 2. 過濾 ----
        incProgress(0.15, detail = "過濾葉綠體/粒線體/豐富度/普遍率...")
        opts <- list(rm_chloroplast = input$rm_chloroplast, rm_mito = input$rm_mito,
                     min_total_count = input$min_total_count, min_prevalence = input$min_prevalence)
        f <- filter_asv(tax_df, counts_mat, opts)
        validate(need(nrow(f$counts) >= 2, "過濾後剩餘 ASV 數量過少（<2），請放寬過濾條件。"))
        filt$tax <- f$tax; filt$counts <- f$counts
        filt$summary <- data.frame(
          項目 = c("ASV 數 (過濾前)", "ASV 數 (過濾後)", "保留比例",
                  "總讀序數 (過濾前)", "總讀序數 (過濾後)", "讀序保留比例"),
          數值 = c(f$n_before, f$n_after, sprintf("%.1f%%", 100 * f$n_after / f$n_before),
                  f$reads_before, f$reads_after, sprintf("%.1f%%", 100 * f$reads_after / f$reads_before))
        )

        # ---- 3. 稀疏化 (Rarefaction) ----
        incProgress(0.3, detail = "稀疏化中...")
        comm_full <- t(f$counts)  # samples as rows
        depth <- if (input$rarefy_mode == "auto") min(rowSums(comm_full)) else input$rarefy_depth_manual
        validate(need(depth > 0, "稀疏化深度必須大於 0，請確認樣本讀序數。"))
        low_samples <- rownames(comm_full)[rowSums(comm_full) < depth]
        if (length(low_samples) > 0) {
          showNotification(paste0("以下樣本讀序數低於稀疏化深度，將被排除：", paste(low_samples, collapse = ", ")),
                            type = "warning", duration = 10)
        }
        comm_full <- comm_full[rowSums(comm_full) >= depth, , drop = FALSE]
        validate(need(nrow(comm_full) >= 4, "通過稀疏化深度門檻的樣本數過少（<4），請調降深度或檢查資料。"))

        set.seed(input$rarefy_seed)
        comm_rare <- rrarefy(comm_full, depth)
        rare$comm <- comm_rare; rare$depth <- depth
        meta_used <- meta[match(rownames(comm_rare), meta$SampleID), ]

        # ---- 4. Alpha 多樣性 ----
        incProgress(0.5, detail = "計算 Alpha 多樣性指數...")
        a_tab <- compute_alpha_indices(comm_rare)
        a_tab <- left_join(a_tab, meta_used, by = "SampleID")
        alpha$table <- a_tab

        idx_cols <- c("Observed", "Shannon", "Simpson", "Pielou", "Chao1")
        long_alpha <- a_tab %>%
          dplyr::select(SampleID, all_of(idx_cols), Treatment, RepNum, Block, Salt, Microbe) %>%
          pivot_longer(all_of(idx_cols), names_to = "Index", values_to = "value")

        if (isTRUE(input$aggregate_rep)) {
          stat_df <- long_alpha %>%
            dplyr::group_by(Index, Treatment, RepNum, Block, Salt, Microbe) %>%
            dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
        } else {
          stat_df <- long_alpha %>% dplyr::select(Index, Treatment, RepNum, Block, Salt, Microbe, value)
        }
        alpha$stat_df <- stat_df

        # ---- 5. LMM + 事後檢定 + 無母數檢定 (逐指數) ----
        incProgress(0.65, detail = "執行 LMM 與事後檢定...")
        lmm_list <- list(); posthoc_list <- list(); srh_list <- list(); posthoc_errors <- list()
        dunn_list <- list(); dunn_errors <- list()
        for (idx in idx_cols) {
          sub <- stat_df %>% dplyr::filter(Index == idx)
          lm_res <- run_lmm(sub, block_effect = input$block_effect)
          lmm_list[[idx]] <- lm_res$anova

          ph <- run_posthoc(sub, block_effect = input$block_effect)
          posthoc_list[[idx]] <- ph$cld
          if (!is.null(ph$error)) posthoc_errors[[idx]] <- ph$error

          srh_list[[idx]] <- run_srh(sub)

          dn <- run_dunn(sub)
          dunn_list[[idx]] <- dn$cld
          if (!is.null(dn$error)) dunn_errors[[idx]] <- dn$error
        }
        alpha$lmm <- lmm_list
        alpha$posthoc <- posthoc_list
        alpha$posthoc_errors <- posthoc_errors
        alpha$srh <- srh_list
        alpha$dunn <- dunn_list
        alpha$dunn_errors <- dunn_errors

        # ---- 6. Beta 多樣性 ----
        incProgress(0.8, detail = "計算 Beta 多樣性...")
        comm_rel_full <- comm_rare / rowSums(comm_rare)   # 全部子樣本 (n=24)，用於排序圖
        meta_full <- meta_used

        # 統計檢定用矩陣：依 aggregate_rep 決定是否先平均為 Rep 層級 (n=12)，避免 A/B 子樣本假重複膨脹 PERMANOVA 的顯著性
        if (isTRUE(input$aggregate_rep)) {
          rel_df <- as.data.frame(comm_rel_full) %>%
            dplyr::mutate(SampleID = rownames(comm_rel_full)) %>%
            left_join(meta_full, by = "SampleID") %>%
            dplyr::mutate(RepID = paste0(Treatment, "_R", RepNum))
          agg_rel <- rel_df %>%
            dplyr::group_by(RepID, Treatment, RepNum, Block, Salt, Microbe) %>%
            dplyr::summarise(across(where(is.numeric), mean), .groups = "drop")
          comm_for_stats <- as.matrix(agg_rel %>% dplyr::select(-RepID, -Treatment, -RepNum, -Block, -Salt, -Microbe))
          rownames(comm_for_stats) <- agg_rel$RepID
          meta_for_stats <- agg_rel %>% dplyr::select(RepID, Treatment, RepNum, Block, Salt, Microbe) %>%
            dplyr::rename(SampleID = RepID)
        } else {
          comm_for_stats <- comm_rel_full
          meta_for_stats <- meta_full
        }
        beta$comm_rel <- comm_for_stats
        beta$meta_used <- meta_for_stats

        d_stats <- vegdist(comm_for_stats, method = input$beta_metric)
        beta$dist <- d_stats

        # 排序圖用矩陣：可獨立於統計檢定，選擇呈現全部子樣本點 (n=24) 或跟統計一致的 Rep 平均點
        if (isTRUE(input$beta_show_all_points)) {
          comm_for_plot <- comm_rel_full
          meta_for_plot <- meta_full
        } else {
          comm_for_plot <- comm_for_stats
          meta_for_plot <- meta_for_stats
        }
        beta$meta_plot <- meta_for_plot
        beta$n_plot <- nrow(comm_for_plot)
        beta$n_stats <- nrow(comm_for_stats)

        d_plot <- vegdist(comm_for_plot, method = input$beta_metric)

        pcoa <- cmdscale(d_plot, k = 2, eig = TRUE)
        beta$pcoa <- pcoa

        nmds <- tryCatch(metaMDS(d_plot, k = 2, trymax = 100, trace = FALSE), error = function(e) NULL)
        beta$nmds <- nmds

        perm_formula <- as.formula("d_stats ~ Salt * Microbe")
        permanova <- tryCatch(
          adonis2(perm_formula, data = meta_for_stats, permutations = input$permanova_perm,
                  by = input$permanova_by,
                  strata = if (nlevels(meta_for_stats$Block) > 1) meta_for_stats$Block else NULL),
          error = function(e) NULL
        )
        beta$permanova <- permanova

        disper <- tryCatch({
          bd <- betadisper(d_stats, meta_for_stats$Treatment)
          pt <- permutest(bd, permutations = input$permanova_perm)
          as.data.frame(pt$tab)
        }, error = function(e) NULL)
        beta$disper <- disper

        incProgress(1, detail = "完成！")
      })
      NULL
    }, error = function(e) e)

    if (!is.null(result)) {
      showNotification(paste("分析失敗：", conditionMessage(result)), type = "error", duration = NULL)
    }
  })

  # ---- 過濾摘要 ----
  output$filter_summary <- renderTable({ req(filt$summary); filt$summary })
  output$dl_filter_summary <- downloadHandler(
    filename = "filter_summary.csv",
    content = function(file) { req(filt$summary); readr::write_excel_csv(filt$summary, file) }
  )

  output$rarecurve_plot <- renderPlot({
    req(raw$counts)
    comm <- t(raw$counts)
    rarecurve(comm, step = 200, label = FALSE, col = "steelblue")
  })

  # ---- Alpha 多樣性 ----
  output$alpha_table <- renderDT({
    req(alpha$table)
    datatable(alpha$table %>% dplyr::mutate(across(where(is.numeric), ~round(., 3))), options = list(pageLength = 10))
  })

  output$dl_alpha_table <- downloadHandler(
    filename = "alpha_diversity_table.csv",
    content = function(file) readr::write_excel_csv(alpha$table, file)
  )

  # 把 Tukey 事後比較的字母標示 (依 Salt_Microbe 的 Group) 對應回 Treatment (Ct/CtP/Na/NaP)，
  # 並算出每個 Index/Treatment 該把字母標在圖上哪個高度（略高於該組箱型圖/點的最大值）
  build_posthoc_label_df <- function() {
    req(alpha$posthoc, alpha$stat_df)
    treat_map <- alpha$stat_df %>%
      dplyr::distinct(Treatment, Salt, Microbe) %>%
      dplyr::mutate(Group = paste0(Salt, "_", Microbe))

    letters_df <- dplyr::bind_rows(lapply(names(alpha$posthoc), function(idx) {
      p <- alpha$posthoc[[idx]]
      if (is.null(p)) return(NULL)
      p %>% dplyr::select(Group, .group) %>% dplyr::mutate(Index = idx)
    }))
    req(nrow(letters_df) > 0)
    letters_df <- letters_df %>% left_join(treat_map, by = "Group")

    pos_df <- alpha$stat_df %>%
      dplyr::group_by(Index) %>%
      dplyr::mutate(rng = max(value, na.rm = TRUE) - min(value, na.rm = TRUE)) %>%
      dplyr::group_by(Index, Treatment) %>%
      dplyr::summarise(y = max(value, na.rm = TRUE) + 0.08 * dplyr::first(rng), .groups = "drop")

    letters_df %>% left_join(pos_df, by = c("Index", "Treatment"))
  }

  make_alpha_boxplot <- function() {
    req(alpha$stat_df)
    p <- ggplot(alpha$stat_df, aes(x = Treatment, y = value, fill = Treatment)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      geom_jitter(width = 0.15, size = 1.8, alpha = 0.6) +
      facet_wrap(~ Index, scales = "free_y") +
      scale_fill_manual(values = c(Ct = "#8DA0CB", CtP = "#66C2A5", Na = "#FC8D62", NaP = "#E78AC3")) +
      theme_bw(base_size = 13) +
      labs(x = "處理組", y = "指數數值", title = "Alpha 多樣性指數盒鬚圖（字母標示：Tukey 事後比較，相同字母代表無顯著差異）") +
      theme(legend.position = "none")

    label_df <- tryCatch(build_posthoc_label_df(), error = function(e) NULL)
    if (!is.null(label_df) && nrow(label_df) > 0) {
      p <- p + geom_text(data = label_df, aes(x = Treatment, y = y, label = .group),
                          inherit.aes = FALSE, fontface = "bold", size = 4.2)
    }
    p
  }
  output$alpha_boxplot <- renderPlot({ make_alpha_boxplot() })
  output$dl_alpha_box <- downloadHandler(
    filename = "alpha_diversity_boxplot.png",
    content = function(file) ggsave(file, make_alpha_boxplot(), width = 10, height = 7, dpi = 300)
  )

  make_interaction_plot <- function() {
    req(alpha$stat_df)
    summ <- alpha$stat_df %>%
      dplyr::group_by(Index, Salt, Microbe) %>%
      dplyr::summarise(mean_val = mean(value, na.rm = TRUE),
                se = sd(value, na.rm = TRUE) / sqrt(n()), .groups = "drop")
    ggplot(summ, aes(x = Salt, y = mean_val, color = Microbe, group = Microbe)) +
      geom_point(size = 3, position = position_dodge(0.15)) +
      geom_line(position = position_dodge(0.15)) +
      geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se), width = 0.1,
                    position = position_dodge(0.15)) +
      facet_wrap(~ Index, scales = "free_y") +
      scale_color_manual(values = c(NoP = "grey40", P = "#1B9E77")) +
      theme_bw(base_size = 13) +
      labs(x = "鹽逆境 (Salt)", y = "平均值 ± SE", title = "Salt × Microbe 交互作用圖")
  }
  output$alpha_interaction <- renderPlot({ make_interaction_plot() })
  output$dl_alpha_inter <- downloadHandler(
    filename = "alpha_interaction_plot.png",
    content = function(file) ggsave(file, make_interaction_plot(), width = 10, height = 7, dpi = 300)
  )

  lmm_anova_df <- reactive({
    req(alpha$lmm)
    dplyr::bind_rows(lapply(names(alpha$lmm), function(idx) {
      a <- alpha$lmm[[idx]]
      if (is.null(a)) return(NULL)
      a$Index <- idx; a$Term <- rownames(a)
      a
    })) %>% dplyr::select(Index, Term, dplyr::everything())
  })
  output$lmm_anova <- renderTable({ lmm_anova_df() }, rownames = FALSE)
  output$dl_lmm_anova <- downloadHandler(
    filename = "alpha_LMM_ANOVA.csv",
    content = function(file) readr::write_excel_csv(lmm_anova_df(), file)
  )

  posthoc_df <- reactive({
    req(alpha$posthoc)
    tab <- dplyr::bind_rows(lapply(names(alpha$posthoc), function(idx) {
      p <- alpha$posthoc[[idx]]
      if (is.null(p)) return(NULL)
      p$Index <- idx
      p
    }))
    req(nrow(tab) > 0)
    tab %>% dplyr::select(Index, dplyr::everything()) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 4)))
  })
  output$posthoc_table <- renderTable({ posthoc_df() })
  output$dl_posthoc <- downloadHandler(
    filename = "alpha_Tukey_posthoc.csv",
    content = function(file) readr::write_excel_csv(posthoc_df(), file)
  )

  output$posthoc_error_msg <- renderUI({
    req(alpha$posthoc_errors)
    if (length(alpha$posthoc_errors) == 0) return(NULL)
    msgs <- lapply(names(alpha$posthoc_errors), function(idx) {
      tags$li(paste0(idx, "：", alpha$posthoc_errors[[idx]]))
    })
    div(style = "color:#b06000;",
        tags$strong("⚠ 以下指數的事後比較未能完成："),
        tags$ul(msgs))
  })

  srh_df <- reactive({
    req(alpha$srh)
    dplyr::bind_rows(lapply(names(alpha$srh), function(idx) {
      s <- alpha$srh[[idx]]
      if (is.null(s)) return(NULL)
      s$Index <- idx; s$Term <- rownames(s)
      s
    })) %>% dplyr::select(Index, Term, dplyr::everything())
  })
  output$srh_table <- renderTable({ srh_df() }, rownames = FALSE)
  output$dl_srh <- downloadHandler(
    filename = "alpha_ScheirerRayHare.csv",
    content = function(file) readr::write_excel_csv(srh_df(), file)
  )

  dunn_df <- reactive({
    req(alpha$dunn)
    tab <- dplyr::bind_rows(lapply(names(alpha$dunn), function(idx) {
      d <- alpha$dunn[[idx]]
      if (is.null(d)) return(NULL)
      d$Index <- idx
      d
    }))
    req(nrow(tab) > 0)
    tab %>% dplyr::select(Index, dplyr::everything()) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 4)))
  })
  output$dunn_table <- renderTable({ dunn_df() }, rownames = FALSE)
  output$dl_dunn <- downloadHandler(
    filename = "alpha_DunnTest.csv",
    content = function(file) readr::write_excel_csv(dunn_df(), file)
  )

  output$dunn_error_msg <- renderUI({
    req(alpha$dunn_errors)
    if (length(alpha$dunn_errors) == 0) return(NULL)
    msgs <- lapply(names(alpha$dunn_errors), function(idx) {
      tags$li(paste0(idx, "：", alpha$dunn_errors[[idx]]))
    })
    div(style = "color:#b06000;",
        tags$strong("⚠ 以下指數的 Dunn's Test 未能完成："),
        tags$ul(msgs))
  })

  # ---- Beta 多樣性 ----
  output$beta_n_caption <- renderUI({
    req(beta$n_plot, beta$n_stats)
    div(style = "color:#555; margin-bottom:10px;",
        sprintf("排序圖樣本數：n = %d ｜ PERMANOVA/betadisper 統計檢定樣本數：n = %d",
                beta$n_plot, beta$n_stats))
  })

  make_pcoa_plot <- function() {
    req(beta$pcoa)
    pcoa <- beta$pcoa
    var_exp <- round(100 * pcoa$eig[1:2] / sum(pcoa$eig[pcoa$eig > 0]), 1)
    pts <- as.data.frame(pcoa$points); colnames(pts) <- c("PCoA1", "PCoA2")
    pts$SampleID <- rownames(pts)
    pts <- left_join(pts, beta$meta_plot, by = "SampleID")
    ggplot(pts, aes(PCoA1, PCoA2, color = Treatment, shape = Block)) +
      geom_point(size = 4, alpha = 0.85) +
      stat_ellipse(aes(group = Treatment), level = 0.68, linetype = 2) +
      scale_color_manual(values = c(Ct = "#8DA0CB", CtP = "#66C2A5", Na = "#FC8D62", NaP = "#E78AC3")) +
      theme_bw(base_size = 13) +
      labs(x = paste0("PCoA1 (", var_exp[1], "%)"), y = paste0("PCoA2 (", var_exp[2], "%)"),
           title = paste0("PCoA (", toupper(input$beta_metric), "), n = ", beta$n_plot))
  }
  output$pcoa_plot <- renderPlot({ make_pcoa_plot() })
  output$dl_pcoa <- downloadHandler(
    filename = "beta_PCoA.png",
    content = function(file) ggsave(file, make_pcoa_plot(), width = 8, height = 6.5, dpi = 300)
  )

  make_nmds_plot <- function() {
    req(beta$nmds)
    pts <- as.data.frame(scores(beta$nmds, display = "sites"))
    pts$SampleID <- rownames(pts)
    pts <- left_join(pts, beta$meta_plot, by = "SampleID")
    ggplot(pts, aes(NMDS1, NMDS2, color = Treatment, shape = Block)) +
      geom_point(size = 4, alpha = 0.85) +
      stat_ellipse(aes(group = Treatment), level = 0.68, linetype = 2) +
      scale_color_manual(values = c(Ct = "#8DA0CB", CtP = "#66C2A5", Na = "#FC8D62", NaP = "#E78AC3")) +
      theme_bw(base_size = 13) +
      labs(title = paste0("NMDS (", toupper(input$beta_metric), "), stress = ", round(beta$nmds$stress, 3),
                           ", n = ", beta$n_plot))
  }
  output$nmds_plot <- renderPlot({ make_nmds_plot() })
  output$dl_nmds <- downloadHandler(
    filename = "beta_NMDS.png",
    content = function(file) ggsave(file, make_nmds_plot(), width = 8, height = 6.5, dpi = 300)
  )

  permanova_df <- reactive({
    req(beta$permanova)
    df <- as.data.frame(beta$permanova)
    df$Term <- rownames(df)
    df %>% dplyr::select(Term, dplyr::everything())
  })
  output$permanova_table <- renderTable({ permanova_df() }, rownames = FALSE)
  output$dl_permanova <- downloadHandler(
    filename = "beta_PERMANOVA.csv",
    content = function(file) readr::write_excel_csv(permanova_df(), file)
  )

  output$permanova_by_caption <- renderUI({
    req(input$permanova_by)
    lbl <- if (input$permanova_by == "margin") "Margin（邊際效果）" else "Terms（循序型 Type I）"
    div(style = "color:#555; font-size:90%;", paste0("平方和拆解方式：", lbl))
  })

  betadisper_df <- reactive({
    req(beta$disper)
    df <- beta$disper
    df$Term <- rownames(df)
    df %>% dplyr::select(Term, dplyr::everything())
  })
  output$betadisper_table <- renderTable({ betadisper_df() }, rownames = FALSE)
  output$dl_betadisper <- downloadHandler(
    filename = "beta_betadisper.csv",
    content = function(file) readr::write_excel_csv(betadisper_df(), file)
  )

  # ---- 物種組成 ----

  # 計算指定分類階層的相對豐度長表 (個別樣本層級)，並將非前 N 大的類群歸為「其他」
  composition_long <- reactive({
    req(filt$tax, rare$comm, raw$meta, input$tax_level, input$comp_top_n)
    comm <- rare$comm
    tax_df <- filt$tax

    # Species 階層：組合「屬 + 種小名」為完整學名，避免只顯示種小名 (例如 ananatis) 造成混淆
    if (input$tax_level == "Species") {
      sp <- tax_df$Species
      ge <- tax_df$Genus
      level_col <- ifelse(
        is.na(sp) | sp == "",
        NA_character_,
        ifelse(is.na(ge) | ge == "", paste("Unclassified", sp), paste(ge, sp))
      )
    } else {
      level_col <- tax_df[[input$tax_level]]
    }
    level_col[is.na(level_col) | level_col == ""] <- "Unclassified"
    names(level_col) <- tax_df$OTU_ID

    rel <- comm / rowSums(comm)
    long <- as.data.frame(rel) %>%
      dplyr::mutate(SampleID = rownames(rel)) %>%
      pivot_longer(-SampleID, names_to = "OTU_ID", values_to = "rel_abund") %>%
      dplyr::mutate(Taxon = level_col[OTU_ID]) %>%
      dplyr::group_by(SampleID, Taxon) %>%
      dplyr::summarise(rel_abund = sum(rel_abund), .groups = "drop")

    top_n <- max(1, round(input$comp_top_n))
    top_taxa <- long %>% dplyr::group_by(Taxon) %>% dplyr::summarise(tot = sum(rel_abund)) %>%
      dplyr::arrange(dplyr::desc(tot)) %>% dplyr::slice_head(n = top_n) %>% dplyr::pull(Taxon)

    long$Taxon2 <- ifelse(long$Taxon %in% top_taxa, long$Taxon, "Other")
    # 讓「其他」固定排在最後（畫圖/表格順序一致）
    taxon_order <- c(top_taxa, "Other")
    long$Taxon2 <- factor(long$Taxon2, levels = taxon_order)

    # 關鍵修正：同一樣本內被歸為同一 Taxon2（尤其是「其他」）的多個原始分類單元，
    # 必須先在樣本內加總成單一數值，之後跨樣本取平均時分母/分子才會一致；
    # 否則直接對(樣本×分類單元)逐列取平均，會把「其他」的總豐度稀釋掉，
    # 導致堆疊長條圖加總小於 100%。
    long <- long %>%
      dplyr::group_by(SampleID, Taxon2) %>%
      dplyr::summarise(rel_abund = sum(rel_abund), .groups = "drop") %>%
      left_join(raw$meta, by = "SampleID")

    long
  })

  make_composition_plot <- function() {
    long <- composition_long() %>% dplyr::mutate(rel_abund = rel_abund * 100)

    if (input$comp_group_level == "treatment") {
      plot_df <- long %>% dplyr::group_by(Treatment, Taxon2) %>%
        dplyr::summarise(rel_abund = mean(rel_abund), .groups = "drop")
      p <- ggplot(plot_df, aes(x = Treatment, y = rel_abund, fill = Taxon2)) +
        labs(x = "處理組", title = paste0(input$tax_level, " 層級相對豐度組成（處理組平均，前",
                                        max(1, round(input$comp_top_n)), "大 + 其他）"))
    } else {
      plot_df <- long
      p <- ggplot(plot_df, aes(x = SampleID, y = rel_abund, fill = Taxon2)) +
        labs(x = "樣本", title = paste0(input$tax_level, " 層級相對豐度組成（個別樣本，前",
                                        max(1, round(input$comp_top_n)), "大 + 其他）"))
    }

    p + geom_bar(stat = "identity", position = "stack") +
      theme_bw(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
      labs(y = "平均相對豐度 (%)", fill = input$tax_level)
  }
  output$composition_plot <- renderPlot({ make_composition_plot() })
  output$dl_composition <- downloadHandler(
    filename = function() paste0(input$tax_level, "_composition.png"),
    content = function(file) ggsave(file, make_composition_plot(), width = 10, height = 6.5, dpi = 300)
  )

  # ---- 物種組成表格（依處理組平均，寬表：列=分類單元，欄=處理組，數值=平均相對豐度 %） ----
  composition_table_df <- reactive({
    long <- composition_long()
    wide <- long %>%
      dplyr::group_by(Treatment, Taxon2) %>%
      dplyr::summarise(rel_abund = mean(rel_abund), .groups = "drop") %>%
      dplyr::mutate(rel_abund = round(100 * rel_abund, 2)) %>%
      tidyr::pivot_wider(names_from = Treatment, values_from = rel_abund, values_fill = 0)

    num_cols <- setdiff(names(wide), "Taxon2")
    wide <- wide %>%
      dplyr::mutate(平均 = rowMeans(dplyr::across(dplyr::all_of(num_cols)))) %>%
      dplyr::arrange(dplyr::desc(平均)) %>%
      dplyr::rename(!!input$tax_level := Taxon2)
    wide
  })

  output$composition_table <- renderDT({
    req(composition_table_df())
    datatable(composition_table_df(), options = list(pageLength = 15), rownames = FALSE)
  })

  output$dl_composition_table <- downloadHandler(
    filename = function() paste0(input$tax_level, "_composition_table.csv"),
    content = function(file) readr::write_excel_csv(composition_table_df(), file)
  )
}

shinyApp(ui, server)
