library(shiny)
library(tidyverse)
library(car)
library(rstatix)
library(multcompView)
library(ggpubr)
library(agricolae)
library(multcomp)
library(corrplot)
library(vegan)
library(pairwiseAdonis)
library(mice)
library(lme4)
library(lmerTest)
library(emmeans)
library(effectsize)   # ★ 新增：η² 計算
library(patchwork)    # ★ 新增：多圖聯圖排版
library(MuMIn)        # ★ 新增：R²m / R²c (Nakagawa & Schielzeth 2013)
library(nlme)         # ★ 縱貫分析：AR(1) 自相關結構
library(ggrepel)      # ★ 縱貫圖表：自動避開重疊文字標注
library(showtext)     # ★ 縱貫圖表：PNG 中文字型支援
if (!requireNamespace("zip", quietly=TRUE)) {
  tryCatch(install.packages("zip", quiet=TRUE), error=function(e) NULL)
}
library(knitr)        # ★ 縱貫分析：renderUI 內 HTML 表格渲染
library(kableExtra)   # ★ 縱貫分析：HTML 表格樣式
library(plotly)       # ★ 三因子互動 3D 圖
library(sortable)     # ★ 拖曳排序（處理水準順序）


# ══════════════════════════════════════════════════════════════════════════════
# ★ 參數顯示名稱與單位對照表
# 格式：raw 欄位名 = list(name = "顯示名稱", unit = "單位")
# 圖表標題使用 name；Y 軸使用 "name (unit)"
# ══════════════════════════════════════════════════════════════════════════════
# ── 使用者自定義標籤對照表 ────────────────────────────────────────────────────
# 格式：「欄位名」= list(name = "中文名稱", name_en = "English Name", unit = "單位")
# 範例：
#   "Height_cm" = list(name = "株高", name_en = "Plant Height", unit = "cm"),
#   "SOD_activity" = list(name = "SOD 活性", name_en = "SOD Activity", unit = "U/mg"),
#
# 若欄位名不在此表中，get_label() 將直接使用欄位名本身作為標籤。
# ─────────────────────────────────────────────────────────────────────────────
label_map <- list(
  # 請在此處填入你的欄位對照（可留空，系統將自動使用欄位名）
)


# ── 取得顯示名稱與 Y 軸標籤（支援中/英文切換）──────────────────────────────
# lang: "zh"（中文）或 "en"（英文），預設讀取 input$label_lang（由 Shiny reactive 傳入）
get_label <- function(raw_name, lang = "zh") {
  if (raw_name %in% names(label_map)) {
    entry    <- label_map[[raw_name]]
    disp     <- if (lang == "en") entry$name_en else entry$name
    unit_str <- entry$unit
    # 英文模式：單位中文字詞對應替換
    if (lang == "en") {
      unit_str <- gsub("個 mm⁻²", "stomata mm⁻²", unit_str)  # 氣孔密度
      unit_str <- gsub("^個$",    "count",          unit_str)  # 純計數（根尖等）
    }
    ylab <- if (nchar(unit_str) > 0) {
      paste0(disp, " (", unit_str, ")")
    } else {
      disp
    }
    list(title = disp, ylab = ylab)
  } else {
    # fallback：自動解析 "名稱_單位" 格式，支援語言切換
    # 先查使用者自訂翻譯（user_label_en，在 server 外全域）
    user_en <- if (exists("user_label_en") && raw_name %in% names(user_label_en))
                 user_label_en[[raw_name]] else NULL

    parts <- strsplit(raw_name, "_")[[1]]

    if (length(parts) >= 2) {
      unit_candidate <- parts[length(parts)]
      name_parts     <- parts[-length(parts)]
      is_unit <- (nchar(unit_candidate) <= 12 &&
                  grepl("^[A-Za-z0-9%²³µμ°./\\-\\+]+$", unit_candidate)) ||
                 unit_candidate %in% c("cm","cm2","cm3","mm","g","kg","mg",
                                       "ml","L","SPAD","nm","um","ppm","ppb",
                                       "MPa","kPa","Pa","mol","mmol","umol",
                                       "mgL","ugL","nmol","percent","mm2","mm3")
      if (is_unit) {
        zh_name  <- paste(name_parts, collapse = " ")
        zh_name  <- trimws(gsub("\\.{2,}", " ", gsub("\\.", " ", zh_name)))
        en_name  <- if (!is.null(user_en)) user_en
                    else paste(name_parts, collapse = " ")
        disp     <- if (lang == "en") en_name else zh_name
        ylab     <- paste0(disp, " (", unit_candidate, ")")
        list(title = disp, ylab = ylab)
      } else {
        zh_name <- trimws(gsub("_", " ", gsub("\\.{2,}", " ", gsub("\\.", " ", raw_name))))
        en_name <- if (!is.null(user_en)) user_en else zh_name
        disp    <- if (lang == "en") en_name else zh_name
        list(title = disp, ylab = disp)
      }
    } else {
      zh_name <- trimws(gsub("_", " ", gsub("\\.{2,}", " ", gsub("\\.", " ", raw_name))))
      en_name <- if (!is.null(user_en)) user_en else zh_name
      disp    <- if (lang == "en") en_name else zh_name
      list(title = disp, ylab = disp)
    }
  }
}

# 使用者自訂英文標籤（從 Shiny server 更新）
user_label_en <- list()

# ── CSV 輸出輔助（UTF-8 BOM，確保 Excel 正確顯示中文）────────────────────
write_csv_bom <- function(df, file, row.names = FALSE) {
  # 先寫 UTF-8 BOM（EF BB BF），讓 Excel 正確識別中文編碼
  con_bin <- file(file, open = "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con_bin)
  close(con_bin)
  # 再以 append 模式寫入 CSV 內容
  con_txt <- file(file, open = "a", encoding = "UTF-8")
  write.csv(df, con_txt, row.names = row.names)
  close(con_txt)
}

# --- 核心運算函數 ---
get_cld_custom <- function(df, param, method, post_hoc, use_block = FALSE, block_type = "random") {
  sub_df <- df %>% filter(!is.na(.data[[param]]))
  if (!("處理" %in% names(sub_df))) {
    fc_cld <- na.omit(c(col_factorA_r(), col_factorB_r()))
    fc_cld <- fc_cld[fc_cld %in% names(sub_df)]
    if (length(fc_cld) >= 2)
      sub_df$處理 <- do.call(paste, c(lapply(fc_cld, function(x) sub_df[[x]]), sep="_"))
    else if (length(fc_cld) == 1)
      sub_df$處理 <- sub_df[[fc_cld[1]]]
    else
      return(NULL)
  }
  sub_df$處理 <- as.factor(sub_df$處理)
  if (length(levels(sub_df$處理)) < 2) return(NULL)

  has_block <- "區集" %in% colnames(sub_df) && length(unique(sub_df$區集)) > 1

  if (method == "ANOVA") {
    old_opts <- options(contrasts = c("contr.sum", "contr.poly"))
    on.exit(options(old_opts))

    # CLD 一律用「處理」欄（已合併所有因子水準組合）作為整體因子
    if (has_block && block_type == "fixed") sub_df$區集 <- as.factor(sub_df$區集)

    if (has_block && block_type == "fixed") {
      model_aov_for_cld <- tryCatch(
        aov(as.formula(paste0("`", param, "` ~ 處理 + 區集")), data = sub_df),
        error = function(e) NULL)
    } else {
      model_aov_for_cld <- tryCatch(
        aov(as.formula(paste0("`", param, "` ~ 處理")), data = sub_df),
        error = function(e) NULL)
    }
    if (is.null(model_aov_for_cld)) return(NULL)

    # 確認殘差 df 足夠
    if (df.residual(model_aov_for_cld) <= 0) return(NULL)

    res <- tryCatch({
      if (post_hoc == "Tukey") {
        agricolae::HSD.test(model_aov_for_cld, "處理", group = TRUE)
      } else {
        agricolae::LSD.test(model_aov_for_cld, "處理", p.adj = "none", group = TRUE)
      }
    }, error = function(e) NULL, warning = function(w) NULL)

    if (is.null(res)) return(NULL)

    cld_vec <- as.character(res$groups$groups)
    names(cld_vec) <- rownames(res$groups)
    return(cld_vec)

  } else if (method == "LMM") {
    tryCatch({
      if (has_block) {
        sub_df$區集 <- as.factor(sub_df$區集)
        sub_df$重複 <- as.factor(sub_df$重複)
        # LMM 區集永遠作隨機效應
        formula_lmm <- if (has_block) {
          as.formula(paste0("`", param, "` ~ 處理 + (1|區集/重複)"))
        } else {
          as.formula(paste0("`", param, "` ~ 處理 + (1|重複)"))
        }
      } else {
        formula_lmm <- as.formula(paste0("`", param, "` ~ 處理 + (1|重複)"))
      }
      model_lmm <- lmer(formula_lmm, data = sub_df)
      emm <- emmeans(model_lmm, ~ 處理)
      cld_adjust <- if (!is.null(post_hoc) && post_hoc == "LSD") "none" else "tukey"
      cld_res <- multcomp::cld(emm, Letters = letters, adjust = cld_adjust)
      cld_vec <- trimws(as.character(cld_res$.group))
      names(cld_vec) <- as.character(cld_res$處理)
      return(cld_vec)
    }, error = function(e) return(NULL))

  } else {
    dunn_res <- sub_df %>% dunn_test(as.formula(paste0("`", param, "` ~ 處理")), p.adjust.method = "bonferroni")
    p_vals <- dunn_res$p.adj; names(p_vals) <- paste0(dunn_res$group1, "-", dunn_res$group2)
    return(multcompLetters(p_vals)$Letters)
  }
}

p_to_star <- function(p) {
  if (is.na(p) || !is.numeric(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("ns")
}

# ── 三層數據視覺化 helper ──
# 輸入：raw_df（個株資料）、param、treat_levels、layer_mode、j_alpha、show_rep_label、treat_colors
add_data_layers <- function(p, raw_df_in, param, treat_levels, layer_mode,
                             j_alpha, show_rep_label, treat_colors, text_size=12,
                             darken=0) {
  # darken：0 = 原色，正值加深（透過 colorspace::darken 實作）
  ind_colors <- treat_colors
  if (darken > 0 && requireNamespace("colorspace", quietly=TRUE)) {
    ind_colors <- colorspace::darken(treat_colors, amount=darken)
  }
  if (is.null(raw_df_in) || !(param %in% names(raw_df_in))) return(p)

  df_ind <- raw_df_in %>%
    rename(Value = !!param) %>%
    filter(!is.na(Value)) %>%
    mutate(處理 = factor(處理, levels = treat_levels))

  df_rep <- df_ind %>%
    group_by(處理, 重複) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(Value)) %>%
    mutate(重複_label = as.character(重複))

  # Layer 1：個株原始值（半透明小點，顏色對應處理）
  if (layer_mode %in% c("l1","l12")) {
    p <- p + geom_jitter(
      data        = df_ind,
      aes(x = 處理, y = Value, colour = 處理),
      width       = 0.15, alpha = j_alpha, size = 1.5,
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    scale_colour_manual(values = ind_colors, guide = "none")
  }

  # Layer 2：Rep 均值（較大實心點 + 可選標注）
  if (layer_mode == "l12") {
    rep_colors <- if (darken > 0 && requireNamespace("colorspace", quietly=TRUE)) {
      colorspace::darken(treat_colors, amount = min(darken + 0.2, 0.6))
    } else if (requireNamespace("colorspace", quietly=TRUE)) {
      colorspace::darken(treat_colors, amount = 0.2)
    } else {
      treat_colors
    }
    p <- p + geom_point(
      data        = df_rep,
      aes(x = 處理, y = Value, colour = 處理),
      size        = 3.0, alpha = 0.85, shape = 16,
      position    = position_jitter(width = 0.05, seed = 42),
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    scale_colour_manual(values = rep_colors, guide = "none")
    if (show_rep_label) {
      p <- p + ggrepel::geom_text_repel(
        data        = df_rep,
        aes(x = 處理, y = Value, label = 重複_label, colour = 處理),
        size        = text_size / 5.5,
        position    = position_jitter(width = 0.05, seed = 42),
        direction   = "y",
        segment.size= 0.3, segment.alpha = 0.5,
        min.segment.length = 0.2, box.padding = 0.3,
        inherit.aes = FALSE, show.legend = FALSE
      )
    }
  }
  p
}

# ── 公式建立 helper：單因子 / 複因子自動切換 ──
make_formula_str <- function(sub_df, has_block, block_type, method="LMM",
                             fa_col=NULL, fb_col=NULL, fc_col=NULL) {
  # fa_col/fb_col：由呼叫端傳入實際欄位名（col_factorA_r()/col_factorB_r()）
  # 若未傳入則從 sub_df 猜測（向後相容）
  all_cols <- names(sub_df)

  # fa_col 必須由呼叫端提供（col_factorA_r()），不在此猜測
  if (is.null(fa_col) || !isTRUE(fa_col %in% all_cols)) {
    fa_col <- NA_character_
    fb_col <- NA_character_
  }

  fa_c <- if (!is.na(fa_col) && fa_col %in% all_cols) fa_col else NA_character_
  fb_c <- if (!is.null(fb_col) && !is.na(fb_col) && fb_col %in% all_cols &&
               fb_col != fa_c) fb_col else NA_character_

  # 檢查因子欄是否有效
  fc_c <- if (!is.null(fc_col) && !is.na(fc_col) && fc_col %in% all_cols &&
              fc_col != fa_c && fc_col != fb_c) fc_col else NA_character_

  is_fact <- !is.na(fa_c) && !is.na(fb_c) &&
             length(unique(sub_df[[fb_c]])) > 1

  # 計算有幾個因子
  n_factors <- length(Filter(Negate(is.na), c(fa_c, fb_c, fc_c)))
  # 4因子以上才觸發降級（改為只排除 E, F 以上）
  has_extra_factors <- n_factors < 4 && any(paste0("Factor", LETTERS[5:6]) %in% all_cols)

  # 公式建構：支援 1/2/3 因子
  fac_formula <- if (n_factors >= 3 && !has_extra_factors) {
    # 3因子：A * B * C（包含所有交互項）
    paste0("`", fa_c, "` * `", fb_c, "` * `", fc_c, "`")
  } else if (is_fact && !has_extra_factors) {
    # 2因子：A * B
    paste0("`", fa_c, "` * `", fb_c, "`")
  } else if (!is.na(fa_c)) {
    # 1因子：只有 A
    paste0("`", fa_c, "`")
  } else if ("處理" %in% all_cols) {
    "處理"
  } else {
    paste0("`", all_cols[1], "`")
  }

  if (method == "ANOVA_str") {
    if (is_fact) {
      if (has_block && block_type == "fixed")
        paste0("Value ~ ", fac_formula, " + 區集")
      else paste0("Value ~ ", fac_formula)
    } else {
      if (has_block && block_type == "fixed") "Value ~ 處理 + 區集"
      else                                    "Value ~ 處理"
    }
  } else { # LMM str
    if (is_fact) {
      if (has_block) paste0("Value ~ ", fac_formula, " + (1|區集/重複)")
      else           paste0("Value ~ ", fac_formula, " + (1|重複)")
    } else {
      if (has_block) "Value ~ 處理 + (1|區集/重複)"
      else           "Value ~ 處理 + (1|重複)"
    }
  }
}

# Cohen's d 大小標籤（Cohen 1988：0.2/0.5/0.8）
cohen_d_label <- function(d) {
  if (is.na(d) || !is.numeric(d)) return("—")
  ad <- abs(d)
  if (ad >= 0.8)  return(if (d > 0) "large (+)"  else "large (−)")
  if (ad >= 0.5)  return(if (d > 0) "medium (+)" else "medium (−)")
  if (ad >= 0.2)  return(if (d > 0) "small (+)"  else "small (−)")
  return("negligible")
}

# ★ η² 計算函數（動態支援 1/2/3 因子）
compute_eta2 <- function(temp_df, method, design_type, block_type = "random",
                         fa_col = NULL, fb_col = NULL, fc_col = NULL) {
  # 建立空白回傳值（動態因子名稱）
  fa_nm <- if (!is.null(fa_col) && !is.na(fa_col)) fa_col else "A"
  fb_nm <- if (!is.null(fb_col) && !is.na(fb_col)) fb_col else NA
  fc_nm <- if (!is.null(fc_col) && !is.na(fc_col)) fc_col else NA
  n_fac <- sum(!is.na(c(fa_nm, fb_nm, fc_nm)))

  # 建立 NA 預設值（命名符合動態因子名）
  make_na_result <- function() {
    res <- list()
    res[[paste0("eta2_", 1)]] <- NA
    if (n_fac >= 2) res[[paste0("eta2_", 2)]] <- NA
    if (n_fac >= 3) res[[paste0("eta2_", 3)]] <- NA
    if (n_fac >= 2) res[["eta2_inter_AB"]] <- NA
    if (n_fac >= 3) {
      res[["eta2_inter_AC"]]  <- NA
      res[["eta2_inter_BC"]]  <- NA
      res[["eta2_inter_ABC"]] <- NA
    }
    res
  }

  # 從 eta_squared 結果中動態提取各效應
  extract_eta2_dynamic <- function(eta_df, col) {
    params <- eta_df$Parameter
    excl_pat <- "^\\(Intercept\\)$|^區集$|^Block|^重複$|^Rep$"
    valid    <- !grepl(excl_pat, params)
    main_idx <- which(valid & !grepl(":", params))
    inter_idx <- which(valid & grepl(":", params))

    res <- list()
    # 主效應
    if (length(main_idx) >= 1) res[["eta2_1"]] <- round(eta_df[[col]][main_idx[1]], 3)
    if (length(main_idx) >= 2) res[["eta2_2"]] <- round(eta_df[[col]][main_idx[2]], 3)
    if (length(main_idx) >= 3) res[["eta2_3"]] <- round(eta_df[[col]][main_idx[3]], 3)

    # 交互效應（依冒號數量區分二向/三向）
    if (length(inter_idx) >= 1) {
      two_way  <- inter_idx[lengths(regmatches(params[inter_idx], gregexpr(":", params[inter_idx]))) == 1]
      three_way <- inter_idx[lengths(regmatches(params[inter_idx], gregexpr(":", params[inter_idx]))) == 2]

      if (length(two_way) >= 1) res[["eta2_inter_AB"]] <- round(eta_df[[col]][two_way[1]], 3)
      if (length(two_way) >= 2) res[["eta2_inter_AC"]] <- round(eta_df[[col]][two_way[2]], 3)
      if (length(two_way) >= 3) res[["eta2_inter_BC"]] <- round(eta_df[[col]][two_way[3]], 3)
      if (length(three_way) >= 1) res[["eta2_inter_ABC"]] <- round(eta_df[[col]][three_way[1]], 3)
    }
    res
  }

  tryCatch({
    has_block <- "區集" %in% colnames(temp_df) && length(unique(temp_df$區集)) > 1
    if (method == "ANOVA") {
      formula_str <- make_formula_str(temp_df, has_block, block_type, "ANOVA_str",
                                      fa_col, fb_col, fc_col)
      model_lm <- tryCatch(lm(as.formula(formula_str), data=temp_df), error=function(e) NULL)
      if (is.null(model_lm) || df.residual(model_lm) <= 0) return(make_na_result())
      eta <- tryCatch(effectsize::eta_squared(model_lm, partial=FALSE), error=function(e) NULL)
      if (is.null(eta)) return(make_na_result())
      return(extract_eta2_dynamic(as.data.frame(eta), "Eta2"))
    } else if (method == "LMM") {
      f_str <- make_formula_str(temp_df, has_block, "random", "LMM",
                                fa_col, fb_col, fc_col)
      model_lmm <- tryCatch(
        lmer(as.formula(f_str), data=temp_df, control=lmerControl(optimizer="bobyqa")),
        error = function(e) NULL)
      if (is.null(model_lmm)) return(make_na_result())
      eta <- tryCatch(effectsize::eta_squared(model_lmm, partial=TRUE), error=function(e) NULL)
      if (is.null(eta)) return(make_na_result())
      return(extract_eta2_dynamic(as.data.frame(eta), "Eta2_partial"))
    }
    return(make_na_result())
  }, error = function(e) make_na_result())
}

# R²m / R²c / ΔR²（僅 LMM 有意義；ANOVA/Kruskal 回傳 NA）
compute_r2 <- function(temp_df, method,
                        fa_col = NULL, fb_col = NULL, fc_col = NULL) {
  if (method != "LMM") return(list(r2m = NA, r2c = NA, delta_r2 = NA))
  tryCatch({
    has_blk_r2 <- "區集" %in% colnames(temp_df) && length(unique(temp_df$區集)) > 1
    f_str <- make_formula_str(temp_df, has_blk_r2, "random", "LMM",
                              fa_col, fb_col, fc_col)
    m <- lmer(as.formula(f_str), data = temp_df,
              control = lmerControl(optimizer = "bobyqa"))
    r2 <- MuMIn::r.squaredGLMM(m)
    r2m <- round(r2[1, "R2m"], 3)
    r2c <- round(r2[1, "R2c"], 3)
    list(r2m = r2m, r2c = r2c, delta_r2 = round(r2c - r2m, 3))
  }, error = function(e) list(r2m = NA, r2c = NA, delta_r2 = NA))
}

# 縱貫分析用：data.frame → 帶樣式的 HTML 表格（供 renderUI 內使用）
df_to_html <- function(df, caption = NULL, rownames = TRUE) {
  df_fmt <- df %>% mutate(across(where(is.numeric), ~{
    ifelse(abs(.x) < 0.0001 & .x != 0,
           formatC(.x, format = "e", digits = 2),
           as.character(round(.x, 4)))
  }))
  tbl <- knitr::kable(df_fmt, format = "html", caption = caption,
                       escape = FALSE, align = "l",
                       row.names = rownames) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped","hover","condensed"),
      full_width = FALSE, font_size = 13)
  HTML(as.character(tbl))
}

# ★ 新增：依 n 門檻分層相關矩陣函數
# 回傳每個 param 在 use_mean_by_rep=TRUE 下 per-rep 均值後的非缺失 n
get_param_n <- function(df_rep_mean, params) {
  sapply(params, function(p) sum(!is.na(df_rep_mean[[p]])))
}

# --- UI 介面 ---
ui <- fluidPage(
  titlePanel("統計分析系統 (含 LMM / η² / R²m·R²c / 分層相關)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "1. 上傳 CSV", accept = ".csv"),
      tags$details(
        tags$summary(style="font-size:11px; color:#4A6FA5; cursor:pointer;",
          "▶ 欄位命名建議（點擊展開）"),
        div(style="font-size:11px; color:#6c757d; padding:4px 0 2px 6px;",
          "結構欄：Treatment / Rep / Block（選填）",
          tags$br(),
          "參數欄：", tags$code("名稱_單位"),
          "（如 ", tags$code("Height_cm"), "、",
          tags$code("EC_dS.m-1"), "、", tags$code("SOD_U.mg-1"), "）",
          tags$br(),
          tags$span(style="color:#C0392B;",
            "⚠ 單位不可含空格，複合單位用點 . 連接"),
          tags$br(),
          downloadButton("demo_csv_dl", "下載範例 CSV",
                         class="btn-xs btn-default",
                         style="margin-top:4px; font-size:11px; padding:2px 8px;")
        )
      ),
      # ★ 欄位確認區（上傳後顯示）
      conditionalPanel(
        condition = "output.file_uploaded == true",
        div(style="background:#f0f4f8; border-left:4px solid #4A90D9; padding:8px 10px; margin-bottom:8px;",
          h5("欄位指定", style="margin-top:0; color:#1B4332;"),
          uiOutput("col_mapping_ui"),
          hr(),
          h6("複因子設計（選填）：", style="color:#555;"),
          helpText(style="font-size:11px;color:#888;",
            "若為單因子設計請留空，系統自動以「處理」欄進行分析。"),
          uiOutput("col_factor_ui")
        )
      ),
      # ★ 英文標籤設定（可折疊）
      tags$details(
        tags$summary(
          style = "cursor:pointer; font-weight:bold; color:#4A6FA5; margin-bottom:4px;",
          "▶ 英文標籤設定（選填，供切換 English 標籤語言使用）"
        ),
        div(style="padding:6px 0;",
          fluidRow(
            column(6,
              fileInput("en_label_file", "上傳對照表 CSV：",
                        accept=".csv", width="100%",
                        placeholder="param, label_en")
            ),
            column(6,
              br(),
              downloadButton("en_label_template_dl", "下載範例 CSV",
                             class="btn-sm btn-default")
            )
          ),
          helpText(style="font-size:11px;color:#888;margin-top:0;",
                   "CSV 格式：兩欄（param, label_en），",
                   "param 為資料欄位名，label_en 為英文顯示名稱。"),
          uiOutput("en_label_ui")
        ),
        br()
      ),
      # ★ 可折疊參數勾選區（HTML details/summary）
      tags$details(
        tags$summary(
          style = "cursor:pointer; font-weight:bold; color:#1B4332; margin-bottom:4px;",
          "▶ 選擇分析參數（點擊展開/收合）"
        ),
        fluidRow(
          column(6, actionButton("param_select_all",   "全選", class = "btn-sm btn-default", style = "margin:4px 0;")),
          column(6, actionButton("param_deselect_all", "全消", class = "btn-sm btn-default", style = "margin:4px 0;"))
        ),
        uiOutput("select_param_multi")
      ),
      hr(),
      radioButtons("design_type", "2. 實驗設計:",
                   choices = c("一般分析 (CRD)" = "CRD", "非均衡區集設計 (RCBD)" = "RCBD")),
      # ★ RCBD 說明：R1+R2 → Block1，R3 → Block2（已自動指派）
      conditionalPanel(
        condition = "input.design_type == 'RCBD'",
        radioButtons("block_effect_type", "區集效應類型：",
                     choices = c("固定效應 (Fixed Effect)"  = "fixed",
                                 "隨機效應 (Random Effect)" = "random"),
                     selected = "random", inline = FALSE),
        helpText("區集欄位請在上方「欄位指定」中指定，或留空（系統將以重複欄代替）。",
                 br(), "固定效應：ANOVA 含 + 區集 項；隨機效應：LMM 含 (1|區集/重複) 項。")
      ),
      uiOutput("select_reps"),
      checkboxInput("use_mean_by_rep", "先按重複(Rep)取均值分析", value = FALSE),
      uiOutput("rep_mean_warning_ui"),
      helpText("提示：不勾選均值分析時，系統將自動處理多株重複數據變異 (LMM) 或使用 MICE 插補 (PCA)。"),
      hr(),
      radioButtons("test_method", "3. 檢定方法:", choices = c("ANOVA", "Kruskal", "LMM")),
      conditionalPanel(condition = "input.test_method == 'ANOVA'",
                       selectInput("post_hoc_anova", "事後比較:", choices = c("LSD", "Tukey"))),
      hr(),
      # ★ 資料轉換（套用至 Value，在分布檢定、LMM、相關矩陣前執行）
      h5("資料轉換（可選）"),
      helpText(style = "font-size:12px; color:#6c757d;",
               "轉換套用於所有分析（分布檢定、LMM、相關矩陣）。",
               "建議：鮮重/乾重偏態 → log(x+1)；比例資料 → arcsin(√x)。"),
      # 全域預設轉換
      selectInput("global_transform", "全域預設轉換：",
                  choices = c("無轉換"        = "none",
                              "log(x+1)"     = "log1p",
                              "√x（開根號）"  = "sqrt",
                              "arcsin(√x)"   = "arcsin"),
                  selected = "none"),
      # 個別參數覆蓋（可折疊）
      tags$details(
        tags$summary(style = "cursor:pointer; font-size:12px; color:#1B4332;",
                     "▶ 個別參數轉換覆蓋（展開設定）"),
        uiOutput("param_transform_ui")
      ),
      hr(),
      h4("4. 計畫性對比 (3階段選取)"),
      uiOutput("contrast_target_ui"),
      uiOutput("contrast_level_ui"),
      hr(),
      h4("下載與圖表設置"),
      sliderInput("plot_text_size", "文字大小控制", min = 8, max = 24, value = 14),
      sliderInput("plot_dpi", "下載解析度 (DPI)", min = 72, max = 600, value = 300),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("1. 統計摘要 (含η² / R²)",
                 uiOutput("current_model_banner_ui"),
                 fluidRow(
                   column(6, downloadButton("download_stats",    "下載統計摘要 CSV（含η² / R²）")),
                   column(6, downloadButton("download_pairwise", "下載 Pairwise P 值 CSV"))
                 ),
                 hr(),
                 tabsetPanel(
                   tabPanel("主效應摘要", tableOutput("full_stats_table")),
                   tabPanel("Pairwise 成對檢驗",
                            br(),
                            fluidRow(
                              column(4,
                                uiOutput("pw_filter_contrast_ui")
                              ),
                              column(4,
                                radioButtons("pw_method_type", "成對檢驗方法：",
                                             choices = c("Tukey / Dunn (emmeans)"    = "tukey",
                                                         "Independent t-test (組間Welch)" = "ttest"),
                                             selected = "tukey", inline = FALSE)
                              ),
                              column(4,
                                checkboxInput("pw_sig_only", "僅顯示 P < 0.10", value = FALSE)
                              )
                            ),
                            helpText(style="font-size:11px;color:#6c757d;",
                              "參數範圍與左側『選擇分析參數』勾選同步。",
                              "Independent t-test：以各組 Rep 均值為獨立觀測（n=3/組），Welch 校正。",
                              "Cohen's d：pooled SD；large ≥0.8、medium ≥0.5、small ≥0.2。"),
                            tableOutput("pairwise_table")
                   )
                 )
        ),
        tabPanel("2. 數值標籤",
                 downloadButton("download_means", "下載數值標籤 CSV"),
                 hr(),
                 tableOutput("full_means_table")
        ),
        tabPanel("3. 分布檢定",
                 downloadButton("download_dist", "下載分布檢定 CSV"),
                 hr(),
                 uiOutput("dist_tab_header"),
                 tableOutput("full_dist_table")
        ),
        tabPanel("4. 相關性分析",
                 fluidRow(
                   column(6, downloadButton("download_corr_csv",  "下載相關矩陣 CSV")),
                   column(6, downloadButton("download_corr_plot", "下載相關性圖 PNG"))
                 ),
                 hr(),
                 # ★ 相關分析設定（移入 Tab）
                 fluidRow(
                   column(4,
                     radioButtons("corr_method", "相關分析方法：",
                                  choices = c("Pearson（常態數據）" = "pearson",
                                              "Spearman（等級/非參數）" = "spearman"),
                                  selected = "spearman")
                   ),
                   column(8,
                     # ★ n 分層控制：僅在「不取均值」時顯示
                     conditionalPanel(
                       condition = "input.use_mean_by_rep == false",
                       fluidRow(
                         column(6,
                           radioButtons("corr_n_mode", "分析範圍（原始值模式）：",
                                        choices = c("全部參數（混合 n）"    = "all",
                                                    "僅 n ≥ 門檻"          = "high_n",
                                                    "僅 n < 門檻（小樣本）" = "low_n"),
                                        selected = "all")
                         ),
                         column(6,
                           numericInput("corr_n_threshold", "n 門檻值（每組植株數）：",
                                        value = 6, min = 2, max = 50, step = 1),
                           helpText("n≥6：每株/批次指標；n<6：破壞性少量取樣。")
                         )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.use_mean_by_rep == true",
                       helpText(
                         style = "color:#6c757d; font-style:italic;",
                         "取均值模式下所有參數 n=3（Rep），n 分層無意義，自動使用「全部參數」。"
                       )
                     )
                   )
                 ),
                 hr(),
                 h4("相關性熱圖"),
                 verbatimTextOutput("corr_n_info"),
                 plotOutput("corr_plot", height = "600px"),
                 tableOutput("corr_detail_table")
        ),
        tabPanel("5. 多變量分析 (PCA/ADONIS)",
                 fluidRow(
                   column(6, downloadButton("download_pca_plot",   "下載 PCA 圖 PNG")),
                   column(6, downloadButton("download_adonis_csv", "下載 ADONIS 檢定表 CSV"))
                 ),
                 hr(),
                 # ★ 多變量分析 n 分層控制（不取均值時才顯示）
                 conditionalPanel(
                   condition = "input.use_mean_by_rep == false",
                   fluidRow(
                     column(4,
                       radioButtons("mv_n_mode", "分析參數範圍（原始值模式）：",
                                    choices = c("全部參數（混合 n）"   = "all",
                                                "僅 n ≥ 門檻"         = "high_n",
                                                "僅 n < 門檻（小樣本）" = "low_n"),
                                    selected = "all")
                     ),
                     column(4,
                       numericInput("mv_n_threshold", "n 門檻值（每組植株數）：",
                                    value = 6, min = 2, max = 50, step = 1)
                     ),
                     column(4,
                       helpText(
                         "門檻以每組有效植株數計算。",
                         "n≥6：每株/批次測量；n<6：破壞性少量取樣（根系形態 n=3）。",
                         "氣孔開度 n=15 可設門檻 15 單獨分群。"
                       )
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "input.use_mean_by_rep == true",
                   helpText(
                     style = "color:#6c757d; font-style:italic;",
                     "取均值模式下自動使用全部參數進行多變量分析。"
                   )
                 ),
                 verbatimTextOutput("mv_n_info"),
                 hr(),
                 h4("A. PCA 降維分析"),
                 plotOutput("pca_plot", height = "500px"),
                 hr(),
                 h4("B. 各參數對主成分之變異貢獻度 (%)"),
                 tableOutput("pca_contribution_table"),
                 hr(),
                 h4("C. ADONIS (PERMANOVA) 多變量檢定"),
                 verbatimTextOutput("adonis_res"),
                 h4("D. Pairwise ADONIS (兩兩事後檢定)"),
                 tableOutput("pairwise_adonis_table")
        ),
        tabPanel("單一參數預覽",
                 fluidRow(
                   # ── 左側設定欄 ──
                   column(4,
                     h4("圖表設定"),
                     downloadButton("download_plot", "下載目前圖表 PNG"),
                     hr(),
                     radioButtons("label_lang_single", "圖表標籤語言：",
                                  choices  = c("中文" = "zh", "English" = "en"),
                                  selected = "zh", inline = TRUE),
                     hr(),
                     uiOutput("select_single_preview"),
                     radioButtons("single_plot_type", "圖表類型：",
                                  choices = c("長條圖（均值±SE）" = "bar",
                                              "盒鬚圖"           = "box",
                                              "琴形圖（Violin）"  = "violin"),
                                  selected = "bar", inline = TRUE),
                     hr(),
                     h5("數據層次設定"),
                     radioButtons("layer_mode_single", NULL,
                       choices = c(
                         "不顯示原始點"                   = "none",
                         "Layer 1：個株原始值"            = "l1",
                         "Layer 1+2：個株 + Rep 均值"    = "l12"
                       ),
                       selected = "none"),
                     conditionalPanel(
                       condition = "input.layer_mode_single != 'none'",
                       sliderInput("jitter_alpha_single", "個株點透明度：",
                                   min = 0.05, max = 1.0, value = 0.30, step = 0.05),
                       sliderInput("jitter_darken_single", "個株點深淺（0=原色，↑加深）：",
                                   min = 0, max = 0.6, value = 0.1, step = 0.05),
                       checkboxInput("show_rep_label_single",
                                     "標注 Rep 編號（R1/R2/R3）", value = TRUE)
                     ),
                     hr(),
                     div(style = "background:#e8f5e9; border-left:4px solid #2D6A4F; padding:6px 10px; font-size:12px;",
                       strong("提示："),
                       "統計結果依左側『檢定方法』設定自動更新。",
                       br(),
                       "RCBD 區集效應類型依左側選擇套用。"
                     )
                   ),
                   # ── 右側圖表＋統計結果欄 ──
                   column(8,
                     plotOutput("single_preview_plot", height = "400px"),
                     hr(),
                     # ── 當前分析模型顯示 ──
                     uiOutput("current_model_ui"),
                     hr(),
                     uiOutput("preview_anova_title"),
                     verbatimTextOutput("preview_anova_res"),
                     h4("B. 事後檢定 (Post-hoc)"),
                     tableOutput("preview_posthoc_table"),
                     hr(),
                     h4("C. 效果量 (η² / R²m / R²c)"),
                     tableOutput("preview_eta2_table"),
                     hr(),
                     h4("D. 條件式計畫性對比 (Simple Main Effects)"),
                     verbatimTextOutput("preview_contrast_res")
                   )
                 )
        ),
        tabPanel("數據預覽", tableOutput("data_preview")),
        tabPanel("★ 進階圖表",
                 div(style="background:#fff3cd;border-left:4px solid #D4A017;padding:8px 12px;margin-bottom:12px;",
                   icon("info-circle"), strong(" 注意："),
                   "進階圖表一律以「Rep 均值」為繪圖單元，與主分析區取均值勾選無關。"
                 ),
                 # ── A. 點圖疊加 ──
                 h4("A. 點圖疊加（4組，含個別數據點）"),
                 fluidRow(
                   column(4, uiOutput("adv_param_strip_ui")),
                   column(4, radioButtons("adv_lang_strip","標籤語言：",
                                          choices=c("中文"="zh","English"="en"),
                                          selected="zh",inline=TRUE)),
                   column(4, br(), downloadButton("download_strip_plot","下載點圖 PNG"))
                 ),
                 plotOutput("strip_plot", height="420px"),
                 hr(),
                 hr(),
                 # ── B. 成對折線圖 ──
                 h4("B. 成對折線圖"),
                 fluidRow(
                   column(3, uiOutput("adv_param_paired_ui")),
                   column(3, uiOutput("paired_compare_ui")),
                   column(3, radioButtons("adv_lang_paired","標籤語言：",
                                          choices=c("中文"="zh","English"="en"),
                                          selected="zh",inline=TRUE)),
                   column(3, br(), downloadButton("download_paired_plot","下載成對折線圖 PNG"))
                 ),
                 plotOutput("paired_plot", height="420px"),
                 # ── C. 交互作用圖 ──
                 h4("C. 雙因子交互作用圖"),
                 fluidRow(
                   column(3, uiOutput("adv_param_inter_ui")),
                   column(2, br(), checkboxInput("interaction_ribbon","SE 色帶",value=TRUE)),
                   column(3, radioButtons("adv_lang_inter","標籤語言：",
                                          choices=c("中文"="zh","English"="en"),
                                          selected="zh",inline=TRUE)),
                   column(4, br(), downloadButton("download_interaction_plot","下載交互圖 PNG"))
                 ),
                 plotOutput("interaction_plot", height="420px"),
                 hr(),
                 # ── D. 散點圖 ──
                 h4("D. 散點圖 + 迴歸線（雙參數相關）"),
                 fluidRow(
                   column(3,
                     uiOutput("scatter_x_ui"),
                     uiOutput("scatter_y_ui")
                   ),
                   column(3,
                     checkboxInput("scatter_color_group","依處理組著色",value=TRUE),
                     checkboxInput("scatter_show_reg","顯示各組迴歸線",value=FALSE),
                     checkboxInput("scatter_show_overall","顯示整體迴歸線",value=TRUE),
                     checkboxInput("scatter_show_label","標注 Rep 標籤",value=FALSE)
                   ),
                   column(3,
                     radioButtons("scatter_unit","繪圖單元：",
                                  choices=c("Rep 均值（n=3/組）"="rep","個別植株原始值"="raw"),
                                  selected="rep",inline=FALSE),
                     radioButtons("adv_lang_scatter","標籤語言：",
                                  choices=c("中文"="zh","English"="en"),
                                  selected="zh",inline=TRUE)
                   ),
                   column(3, br(), br(), downloadButton("download_scatter","下載散點圖 PNG"))
                 ),
                 plotOutput("scatter_plot", height="450px"),
                 # ── E. 三因子交感圖（僅在選了3個因子時顯示）──
                 conditionalPanel(
                   condition = "output.has_three_factors === 'true'",
                   hr(),
                   h4("E. 三因子交感圖（A × B，按 C 分面）"),
                   fluidRow(
                     column(3, uiOutput("adv_param_3way_ui")),
                     column(2, checkboxInput("threeway_ribbon", "SE 色帶", value = TRUE)),
                     column(2, checkboxInput("threeway_show_pts", "Rep 點", value = TRUE)),
                     column(2, radioButtons("adv_lang_3way", "標籤：",
                                            choices = c("欄名" = "raw", "English" = "en"),
                                            selected = "raw", inline = TRUE)),
                     column(3, br(), downloadButton("download_3way_plot", "下載三因子交感圖 PNG"))
                   ),
                   plotOutput("threeway_plot", height = "500px"),
                   hr(),
                   h4("F. 三因子互動圖（plotly 3D，可旋轉）"),
                   fluidRow(
                     column(4, uiOutput("adv_param_3d_ui")),
                     column(4, uiOutput("threeway_3d_zfactor_ui")),
                     column(4, br(), helpText("拖曳可旋轉；滾輪縮放。"))
                   ),
                   plotly::plotlyOutput("threeway_3d_plot", height = "520px")
                 )
        ),
        tabPanel("★ 縱貫分析",
                 fluidRow(
                   # ── 左側設定欄 ──
                   column(3,
                     h4("資料載入"),
                     fileInput("long_file", "上傳縱貫資料 CSV",
                               accept = ".csv"),
                     helpText(style="font-size:11px;color:#6c757d;",
                       "必要欄位：Treatment, Rep, Plant_ID, Week 及分析參數。",
                       "可選欄位：Block, Phase（若無系統自動補齊預設值）。"),
                     hr(),
                     h4("下載"),
                     fluidRow(
                       column(12,
                         downloadButton("long_download_plots",   "縱貫圖 PNG (zip)"),
                         br(), br(),
                         downloadButton("long_download_emmeans", "emmeans CSV"),
                         br(), br(),
                         downloadButton("long_download_anova",   "ANOVA CSV")
                       )
                     ),
                     hr(),
                     h4("分析設定"),
                     uiOutput("long_params_ui"),
                     hr(),
                     radioButtons("long_week_type", "Week 自變數型態：",
                       choices  = c("類別 (factor)"   = "factor",
                                    "連續 (numeric)"  = "numeric"),
                       selected = "factor", inline = TRUE),
                     helpText(style="font-size:11px;color:#6c757d;",
                       "連續：估計各處理每週平均生長速率（斜率）。",
                       "類別：各週獨立比較（Tukey 事後）。"),
                     hr(),
                     radioButtons("long_ar1", "自相關結構：",
                       choices  = c("無 (lmerTest)"                = "none",
                                    "AR(1) — nlme"                 = "ar1",
                                    "CompSymm — nlme"              = "cs",
                                    "自動（依 AIC 各參數獨立選擇）" = "auto"),
                       selected = "auto", inline = FALSE),
                     helpText(style="font-size:11px;color:#6c757d;",
                       "AR(1)：相鄰週次自相關；CompSymm：各週等相關。",
                       "「自動」：每個參數分別比較 lmerTest vs AR(1) AIC，",
                       "自動套用較低 AIC 的結構。"),
                     hr(),
                     radioButtons("long_em_mode", "事後比較模式：",
                       choices  = c(
                         "複因子（Salt / Inoculation 主效應 + 交互）" = "factorial",
                         "Treatment 四組 pairwise"    = "treatment"),
                       selected = "factorial", inline = FALSE),
                     helpText(style="font-size:11px;color:#6c757d;",
                       "複因子：分離鹽與接種主效應，統計效力較高。",
                       "Treatment 模式：直接比較所有處理組，各週獨立 Tukey 校正。"),
                     hr(),
                     h5("事後比較重點對："),
                     # 複因子模式：四個 simple effects 選擇顯示
                     conditionalPanel(
                       condition = "input.long_em_mode == 'factorial'",
                       checkboxGroupInput("long_contrast_factorial",
                         label   = NULL,
                         choices = c(
                           "Factor A Effect (lvl 1)" = "salt_none",
                           "Factor A Effect (lvl 2)" = "salt_pgpr",
                           "Factor B Effect (lvl 1)" = "inocu_ctrl",
                           "Factor B Effect (lvl 2)" = "inocu_salt"
                         ),
                         selected = c("salt_none","salt_pgpr",
                                      "inocu_ctrl","inocu_salt")
                       )
                     ),
                     # Treatment 四組模式：直接比較所有處理
                     conditionalPanel(
                       condition = "input.long_em_mode == 'treatment'",
                       uiOutput("long_contrast_pairs_ui")
                     ),
                     hr(),
                     h4("圖表設定"),
                     radioButtons("long_label_lang", "圖表語言：",
                       choices  = c("中文" = "zh", "English" = "en"),
                       selected = "zh", inline = TRUE),
                     sliderInput("long_text_size", "文字大小：",
                                 min = 9, max = 30, value = 20, step = 1),
                     sliderInput("long_plot_dpi", "下載 DPI：",
                                 min = 72, max = 600, value = 300, step = 50),
                     fluidRow(
                       column(6,
                         numericInput("long_plot_width",  "輸出寬 (inch)：",
                                      value=6, min=3, max=15, step=0.5)
                       ),
                       column(6,
                         numericInput("long_plot_height", "輸出高 (inch)：",
                                      value=4.5, min=2, max=12, step=0.5)
                       )
                     ),
                     checkboxInput("long_show_individual",
                                   "顯示個株軌跡（半透明）", value = TRUE),
                     checkboxInput("long_show_ribbon",
                                   "顯示 SE 色帶", value = TRUE),
                   ),
                   # ── 右側主面板 ──
                   column(9,
                     tabsetPanel(id = "long_subtabs",
                       tabPanel("資料總覽",
                         br(),
                         fluidRow(
                           column(6, tableOutput("long_data_summary")),
                           column(6, tableOutput("long_na_summary"))
                         ),
                         hr(),
                         h4("各處理 × 週樣本數"),
                         tableOutput("long_sample_count"),
                         hr(),
                         h4("Baseline 確認（Week 1）"),
                         helpText("處理前各組應無顯著差異（P > 0.05）。"),
                         tableOutput("long_baseline_table"),
                         tableOutput("long_baseline_anova")
                       ),
                       tabPanel("LMM 主分析",
                         br(),
                         helpText(style="font-size:12px;color:#6c757d;",
                           "隨機效應：(1|Block/Plant_ID)（Plant_ID 巢套於 Block）。",
                           "處理期 Week（Week_salt 從 0 起算）。",
                           br(),
                           uiOutput("long_model_note_ui")),
                         uiOutput("long_lmm_output_ui")
                       ),
                       tabPanel("AR(1) 自相關",
                         br(),
                         helpText(style="font-size:12px;color:#6c757d;",
                           "僅對 Fv/Fm 執行（重複測量自相關最明顯）。",
                           "對所有分析參數各自執行 AIC 比較。",
                           "隨機效應：random = (1|Plant_ID_chr)，Block 納入固定效應。",
                           "自相關 grouping = Plant_ID。"),
                         verbatimTextOutput("long_ar1_output"),
                         tableOutput("long_ar1_aic")
                       ),
                       tabPanel("事後比較",
                         br(),
                         helpText(style="font-size:12px;color:#6c757d;",
                           "emmeans Tukey，週類別模型。",
                           "各週獨立比較，已對多重比較校正。"),
                         uiOutput("long_emmeans_output_ui")
                       ),
                       tabPanel("縱貫折線圖",
                         br(),
                         helpText(style="font-size:12px;color:#6c757d;",
                           "X 軸同時顯示移植後週次（Wx）與施鹽後週次（Sx）。",
                           "字母標記來自 emmeans Tukey（週類別模型）。"),
                         uiOutput("long_plots_ui")
                       ),
                       tabPanel("Simple Effect 橫條圖",
                         br(),
                         helpText(style="font-size:12px;color:#6c757d;",
                           "各週 Simple Effect 效應量（estimate，正負方向保留）。",
                           "條形顏色：深色 = 顯著（P<0.05）、淺色 = 趨勢（P<0.1）、灰色 = ns。",
                           "上方資訊框顯示 Salt×Inocu.×Week 及 Salt×Inocu. 兩個交互作用 F/P。"),
                         fluidRow(
                           column(3,
                             h5("選擇參數："),
                             uiOutput("long_forest_params_ui"),
                             hr(),
                             h5("顯示模式："),
                             radioButtons("long_se_display_mode", NULL,
                               choices = c(
                                 "單圖（選擇單一 SE）"    = "single",
                                 "並列：A+B（Factor A effects）" = "ab",
                                 "並列：C+D（Factor B effects）" = "cd"
                               ),
                               selected = "single"),
                             hr(),
                             conditionalPanel(
                               condition = "input.long_se_display_mode == 'single'",
                               h5("選擇 Simple Effect："),
                               radioButtons("long_forest_se", NULL,
                                 choices = c(
                                   "Factor A | Level 1" = "salt_none",
                                   "Factor A | Level 2" = "salt_pgpr",
                                   "Factor B | Level 1" = "inocu_ctrl",
                                   "Factor B | Level 2 ★" = "inocu_salt"
                                 ),
                                 selected = "inocu_salt")
                             ),
                             hr(),
                             checkboxInput("long_forest_show_ns",
                               "顯示 ns 週次", value = TRUE),
                             hr(),
                             downloadButton("long_download_forest",
                                            "下載所有圖 PNG (zip)")
                           ),
                           column(9, uiOutput("long_forest_ui"))
                         )
                       )
                     )
                   )
                 )
        ),
        tabPanel("★ 多參數聯圖",
                 fluidRow(
                   column(4,
                     h4("聯圖參數設定"),
                     downloadButton("download_multipanel", "下載多參數聯圖 PNG"),
                     hr(),
                     # ★ 語言與進階圖表選項（移入此 Tab）
                     radioButtons("label_lang", "圖表標籤語言：",
                                  choices  = c("中文" = "zh", "English" = "en"),
                                  selected = "zh", inline = TRUE),
                     helpText("切換後所有圖表標題與 Y 軸同步更新（含單一參數預覽）。"),
                     hr(),
                     # 圖表類型選擇
                     radioButtons("multipanel_type", "圖表類型：",
                                  choices = c("長條圖（均值±SE）" = "bar",
                                              "盒鬚圖"           = "box",
                                              "琴形圖（Violin）"  = "violin"),
                                  selected = "bar", inline = TRUE),
                     # 欄數自訂
                     sliderInput("multipanel_ncol", "每列圖數（欄數）：",
                                 min = 1, max = 6, value = 3, step = 1),
                     # 單圖尺寸控制
                     numericInput("multipanel_width",  "每圖寬度 (inch)：", value = 4, min = 2, max = 10, step = 0.5),
                     numericInput("multipanel_height", "每圖高度 (inch)：", value = 3.5, min = 2, max = 10, step = 0.5),
                     hr(),
                     hr(),
                     # ★ 數據層次控制（長條圖 / 盒鬚圖 / 琴形圖 通用）
                     h5("數據層次設定"),
                     radioButtons("layer_mode_multi", NULL,
                       choices = c(
                         "不顯示原始點"                   = "none",
                         "Layer 1：個株原始值"            = "l1",
                         "Layer 1+2：個株 + Rep 均值"    = "l12"
                       ),
                       selected = "none"),
                     conditionalPanel(
                       condition = "input.layer_mode_multi != 'none'",
                       sliderInput("jitter_alpha_multi", "個株點透明度：",
                                   min = 0.05, max = 1.0, value = 0.30, step = 0.05),
                       sliderInput("jitter_darken_multi", "個株點深淺（0=原色，↑加深）：",
                                   min = 0, max = 0.6, value = 0.1, step = 0.05),
                       checkboxInput("show_rep_label_multi",
                                     "標注 Rep 編號（R1/R2/R3）", value = TRUE)
                     ),
                     hr(),
                     # 獨立參數勾選區
                     h4("選擇納入聯圖的參數"),
                     actionButton("multipanel_select_all",   "全選", class = "btn-sm btn-default"),
                     actionButton("multipanel_deselect_all", "全消", class = "btn-sm btn-default"),
                     br(), br(),
                     uiOutput("multipanel_param_ui"),

                   ),
                   column(8,
                     h4(textOutput("multipanel_title")),
                     helpText("預覽圖僅供確認版面，下載版本為完整解析度。"),
                     # 動態高度：依列數調整
                     uiOutput("multipanel_plot_ui")
                   )
                 )
        )
      )
    )
  )
)

# --- Server 邏輯 ---
server <- function(input, output, session) {

  # 欄位指定 reactive（使用者選擇或預設）
  # guess_col helper：從欄位清單猜測最符合的欄位名
  guess_col <- function(cols, candidates) {
    m <- intersect(tolower(candidates), tolower(cols))
    if (length(m) > 0) cols[tolower(cols) == m[1]][1] else ""
  }

  col_treat <- reactive({
    if (!is.null(input$col_treat) && input$col_treat != "")
      input$col_treat else "處理"
  })
  col_rep <- reactive({
    if (!is.null(input$col_rep) && input$col_rep != "")
      input$col_rep else "重複"
  })
  col_block <- reactive({
    if (!is.null(input$col_block) && input$col_block != "")
      input$col_block else "區集"
  })

  # 是否為複因子設計（有 2 個以上因子）
  is_factorial_r <- reactive({
    length(factor_cols_r()) >= 2
  })

  # fa_label / fb_label 已移至 factor_cols_ui 後定義

  # ── 預留：未來可擴充至 3 個以上因子 ──
  # extra_factors <- reactive({ list() })  # 額外因子欄位清單（Phase 3 擴充用）

  # 是否有真實區集（區集 ≠ 重複 時才啟用 block）
  has_block_r <- reactive({
    req(raw_df())
    df <- raw_df()
    if (!("區集" %in% names(df))) return(FALSE)
    # 區集欄和重複欄不完全相同 → 有真實 block
    !identical(as.character(df$區集), as.character(df$重複))
  })

  # 處理水準順序（使用者可調整）
  treat_levels_r <- reactive({
    # 優先：使用者在水準排序 UI 勾選的順序
    if (!is.null(input$treat_order) && length(input$treat_order) > 0)
      return(input$treat_order)
    # 次選：raw_df() 合併後的「處理」欄水準
    df <- tryCatch(raw_df(), error=function(e) NULL)
    if (!is.null(df) && "處理" %in% names(df))
      return(as.character(levels(df$處理)))
    # fallback：raw_df_raw() 猜測
    df_raw <- tryCatch(raw_df_raw(), error=function(e) NULL)
    if (is.null(df_raw)) return(character(0))
    fc <- factor_cols_r()
    fc_valid <- Filter(function(c) c %in% names(df_raw), fc)
    if (length(fc_valid) >= 1) {
      if (length(fc_valid) == 1)
        as.character(unique(df_raw[[fc_valid[[1]]]]))
      else
        as.character(unique(
          do.call(paste, c(lapply(fc_valid, function(c) df_raw[[c]]), sep="_"))
        ))
    } else {
      tc <- col_treat()
      if (tc %in% names(df_raw)) as.character(unique(df_raw[[tc]])) else character(0)
    }
  })

  # 原始讀入（不做欄位重命名）
  raw_df_raw <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, na.strings = c("NA", ""))
  })

  output$file_uploaded <- reactive({ !is.null(input$file1) })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)

  # ── 統一取因子欄位名的輔助函數 ──────────────────────────────────────────
  # 所有分析都從這裡取，不在各處用 intersect/硬編碼
  # 直接讀 reactive，不猜測、不 fallback
  # df 參數保留供向後相容，但不用於猜測
  get_fa_col <- function(df = NULL) col_factorA_r()
  get_fb_col <- function(df = NULL) col_factorB_r()
  get_fc_col <- function(df = NULL) col_factorC_r()

  # 取 rep/block 欄位名（只做結構欄查找，不涉及因子）
  get_rep_col <- function(df) {
    intersect(c("重複","Rep","rep","Replicate"), names(df))[1] %||% NULL
  }
  get_block_col <- function(df) {
    intersect(c("區集","Block","block","Blk"), names(df))[1] %||% NULL
  }

  # 取均值分組欄（因子欄 + rep + block）
  get_grp_cols <- function(df) {
    fa <- get_fa_col(); fb <- get_fb_col(); fc <- get_fc_col()
    rc <- get_rep_col(df); bc <- get_block_col(df)
    cols <- na.omit(c(fa, fb, fc, rc, bc))
    unique(cols[cols %in% names(df)])
  }

  # ── 動態 Pairwise / Contrast UI ──

  # contrast_target：Factor A vs Factor B
  output$contrast_target_ui <- renderUI({
    req(raw_df())
    fa <- fa_label(); fb <- fb_label()
    selectInput("contrast_target", "步驟1. 想要比較的因子 (Target):",
                choices = setNames(c(col_factorB_r() %||% "Factor B",
                                     col_factorA_r() %||% "Factor A"),
                                   c(fb, fa)))
  })

  # pw_filter_contrast：動態產生所有兩兩組合
  output$pw_filter_contrast_ui <- renderUI({
    req(raw_df())
    lvls <- treat_levels_r()
    if (length(lvls) < 2) return(NULL)
    pairs <- combn(lvls, 2, simplify=FALSE)
    pair_choices <- setNames(
      sapply(pairs, function(p) paste(p, collapse=" - ")),
      sapply(pairs, function(p) paste(p, collapse=" vs "))
    )
    selectInput("pw_filter_contrast", "篩選比較組合：",
                choices  = c("全部"="all", pair_choices),
                selected = "all")
  })

  # long_contrast_pairs：動態產生縱貫 treatment 模式的兩兩組合
  output$long_contrast_pairs_ui <- renderUI({
    req(long_raw())
    lvls <- levels(long_raw()$Treatment)
    if (length(lvls) < 2) return(NULL)
    pairs <- combn(lvls, 2, simplify=FALSE)
    pair_choices <- setNames(
      sapply(pairs, function(p) paste(p, collapse=" - ")),
      sapply(pairs, function(p) paste(p, collapse=" vs "))
    )
    checkboxGroupInput("long_contrast_pairs", label=NULL,
                       choices=pair_choices,
                       selected=pair_choices[1])
  })

  # Rep 均值警告：每組 n=1 時提示
  output$rep_mean_warning_ui <- renderUI({
    req(raw_df())
    if (!isTRUE(input$use_mean_by_rep)) return(NULL)
    n_per <- raw_df() %>%
      group_by(處理, 重複) %>% summarise(n=n(), .groups="drop") %>%
      pull(n) %>% mean(na.rm=TRUE)
    if (n_per <= 1) {
      div(style="background:#fff3cd; border-left:3px solid #ffc107;
                 padding:5px 8px; font-size:11px; margin-top:2px;",
        "⚠ 每組只有 1 筆（已是 Rep 均值），",
        tags$b("取均值後殘差 df=0，分析將失敗。"),
        "建議取消勾選「先取 Rep 均值」。")
    } else if (n_per < 3) {
      div(style="background:#fff3cd; border-left:3px solid #ffc107;
                 padding:5px 8px; font-size:11px; margin-top:2px;",
        paste0("⚠ 每組平均 ", round(n_per,1), " 筆，"),
        "取均值後統計檢定力可能不足（建議每組 ≥ 3 Rep）。")
    }
  })

  # n_factors 說明 UI
  output$n_factors_warning_ui <- renderUI({
    n <- as.integer(input$n_factors %||% 2)
    if (n > 3)
      div(style="background:#fff3cd; border-left:3px solid #ffc107;
                 padding:4px 8px; font-size:11px; margin-top:4px;",
          tags$b("⚠ 超過3因子："),
          "目前最多支援 3 因子完全交配分析。",
          "請移除多餘因子，或改用 ANOVA 軟體進行高階分析。")
    else if (n == 3)
      div(style="background:#d4edda; border-left:3px solid #28a745;
                 padding:4px 8px; font-size:11px; margin-top:4px;",
          tags$b("✓ 3因子模式："),
          "分析公式為 A × B × C（包含三向交互），",
          "η² 表格將顯示全部 7 個效應項。")
  })

  # 縱貫因子欄位名（reactiveVal，由 long_raw 更新）
  # 縱貫分析的因子預設值（跟著橫斷面選擇）
  long_fa_col_val <- reactive({
    col_factorA_r() %||% "FactorA"
  })
  long_fb_col_val <- reactive({
    col_factorB_r() %||% "FactorB"
  })
  long_fa_col_r   <- reactive({ long_fa_col_val() })
  long_fb_col_r   <- reactive({ long_fb_col_val() })
  long_fa_label   <- reactive({ gsub("_", " ", long_fa_col_val()) })
  long_fb_label   <- reactive({ gsub("_", " ", long_fb_col_val()) })

  # ── 英文標籤：從上傳 CSV 載入 ──
  en_label_from_file <- reactive({
    req(input$en_label_file)
    tryCatch({
      df <- read.csv(input$en_label_file$datapath,
                     stringsAsFactors = FALSE,
                     fileEncoding = "UTF-8-BOM")
      # 支援欄位名大小寫變體
      names(df) <- tolower(trimws(names(df)))
      if (!all(c("param","label_en") %in% names(df))) return(list())
      df <- df[!is.na(df$param) & df$param != "" &
               !is.na(df$label_en) & df$label_en != "", ]
      setNames(as.list(trimws(df$label_en)), trimws(df$param))
    }, error = function(e) list())
  })

  # 英文標籤 UI：預設值從上傳 CSV 填入，可手動覆蓋
  output$en_label_ui <- renderUI({
    req(raw_df())
    params <- setdiff(names(raw_df()[sapply(raw_df(), is.numeric)]),
                      c("流水號"))
    if (length(params) == 0) return(NULL)
    # 已上傳的對照表（若有）
    from_file <- if (!is.null(input$en_label_file))
                   tryCatch(en_label_from_file(), error=function(e) list())
                 else list()
    inputs <- lapply(params, function(p) {
      # 預設值：CSV > 現有手動輸入 > placeholder
      default_val <- if (p %in% names(from_file)) from_file[[p]] else ""
      placeholder <- gsub("_", " ", p)
      fluidRow(
        column(4, tags$small(p, style="color:#666; line-height:2.2;")),
        column(8, textInput(paste0("en_label_", p), NULL,
                            value = default_val,
                            placeholder = placeholder,
                            width = "100%"))
      )
    })
    tagList(
      helpText(style="font-size:11px;color:#888; margin-bottom:4px;",
               "留空則沿用原始欄位名稱（自動解析 _ 分隔的單位）。"),
      do.call(tagList, inputs)
    )
  })

  # 監聽所有英文標籤輸入，更新全域 user_label_en
  observe({
    req(raw_df())
    params <- setdiff(names(raw_df()[sapply(raw_df(), is.numeric)]),
                      c("流水號"))
    # 手動輸入優先，再用 CSV
    from_file <- if (!is.null(input$en_label_file))
                   tryCatch(en_label_from_file(), error=function(e) list())
                 else list()
    new_map <- lapply(params, function(p) {
      val <- input[[paste0("en_label_", p)]]
      if (!is.null(val) && nchar(trimws(val)) > 0)
        trimws(val)
      else if (p %in% names(from_file))
        from_file[[p]]
      else
        NULL
    })
    names(new_map) <- params
    user_label_en <<- Filter(Negate(is.null), new_map)
  })

  # ── 橫斷面範例 CSV 下載 ──
  output$demo_csv_dl <- downloadHandler(
    filename = function() "demo_data_template.csv",
    content  = function(file) {
      # 產生 2×2 複因子、3 Rep、3 株/Rep 的最小範例
      treatments <- expand.grid(
        FactorA = c("Control","Treatment"),
        FactorB = c("None","Inoculated"),
        stringsAsFactors = FALSE
      )
      treatments$Treatment <- paste(treatments$FactorA,
                                    treatments$FactorB, sep="_")
      block_map <- c(R1="Block1", R2="Block1", R3="Block2")
      rows <- do.call(rbind, lapply(seq_len(nrow(treatments)), function(ti) {
        trow <- treatments[ti, ]
        do.call(rbind, lapply(c("R1","R2","R3"), function(rep) {
          do.call(rbind, lapply(1:3, function(plant) {
            data.frame(
              Treatment    = trow$Treatment,
              FactorA      = trow$FactorA,
              FactorB      = trow$FactorB,
              Block        = block_map[rep],
              Rep          = rep,
              Height_cm    = round(rnorm(1, mean=20, sd=3), 1),
              FreshWeight_g= round(rnorm(1, mean=5,  sd=1), 2),
              Chl_SPAD     = round(rnorm(1, mean=40, sd=5), 1),
              EC_dS.m.1    = round(rnorm(1, mean=2,  sd=0.5), 2),
              stringsAsFactors = FALSE
            )
          }))
        }))
      }))
      write_csv_bom(rows, file)
    }
  )

  # 範例 CSV 下載
  output$en_label_template_dl <- downloadHandler(
    filename = function() "en_label_template.csv",
    content  = function(file) {
      # 依目前資料的參數欄位產生範例
      params <- if (!is.null(raw_df())) {
        setdiff(names(raw_df()[sapply(raw_df(), is.numeric)]), c("流水號"))
      } else {
        c("param_1", "param_2", "param_3")
      }
      template <- data.frame(
        param    = params,
        label_en = gsub("_", " ", params),
        stringsAsFactors = FALSE
      )
      write_csv_bom(template, file)
    }
  )

  # 欄位確認 UI
  output$col_mapping_ui <- renderUI({
    req(raw_df_raw())
    df   <- raw_df_raw()
    cols <- names(df)
    # 數值欄位（不作為結構欄位）
    num_cols_all <- names(df)[sapply(df, is.numeric)]
    non_num_cols <- setdiff(cols, num_cols_all)

    all_cols     <- c("（自動偵測）" = "", cols)
    non_num_opts <- c("（留空）" = "", non_num_cols)

    # 猜測預設欄位（含中英文常見命名）
    guess <- function(candidates) {
      m <- intersect(tolower(candidates), tolower(cols))
      if (length(m) > 0) cols[tolower(cols) == m[1]] else ""
    }
    def_treat <- guess(c("處理","treatment","treat","trt"))
    def_rep   <- guess(c("重複","rep","replicate","replication"))
    def_block <- guess(c("區集","block","blk"))

    # 是否找到 Treatment 欄
    found_treat <- (def_treat != "")

    tagList(
      # ── 結構欄位：重複、區集 ──
      fluidRow(
        column(6, selectInput("col_rep",   "重複欄：",
                              choices=all_cols, selected=def_rep)),
        column(6, selectInput("col_block", "區集欄：",
                              choices=all_cols, selected=def_block))
      ),
      # ── 因子欄位（動態，支援任意數量）──
      div(style="margin-top:6px; background:#f8f9fa; border-left:4px solid #4A90D9; padding:8px 10px;",
        tags$b("因子欄位：", style="font-size:13px;"),
        fluidRow(
          column(6,
            numericInput("n_factors", "因子數量：",
                         value=2, min=1, max=6, step=1, width="100%")
          ),
          column(6,
            helpText(style="font-size:11px;color:#888; margin-top:24px;",
              "1=單因子，2=雙因子，3=三因子（完全交配）。",
              tags$br(),
              "4個以上：合併為「處理」欄進行單因子統計。"),
            uiOutput("n_factors_warning_ui")
          )
        ),
        uiOutput("factor_cols_ui")
      ),
      uiOutput("treat_order_ui")
    )
  })

  # 動態因子欄位 UI（依 n_factors 產生）
  output$factor_cols_ui <- renderUI({
    req(raw_df_raw(), input$n_factors)
    df       <- raw_df_raw()
    cols     <- names(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    non_num  <- setdiff(cols, num_cols)
    opt_cols <- c("（留空）" = "", non_num)
    n <- max(1, min(as.integer(input$n_factors), 6))

    # 猜測預設值：排除結構欄（ID/Plant_ID/SampleCode 等）
    struct_excl <- c(
      if (!is.null(input$col_rep)   && input$col_rep   != "") input$col_rep   else "",
      if (!is.null(input$col_block) && input$col_block != "") input$col_block else "",
      "Plant_ID","ID","SampleCode","plant_id","id","sample_id","Sample_ID",
      "Treatment_chr","Phase","phase","Week","week","Treatment","treatment"
    )
    cands   <- setdiff(non_num, struct_excl)
    defaults <- c(cands, rep("", 6))[seq_len(n)]

    # 產生 n 個 selectInput
    factor_labels <- c("A（必填）","B（選填）","C（選填）",
                       "D（選填）","E（選填）","F（選填）")
    inputs <- lapply(seq_len(n), function(i) {
      column(max(2, floor(12/n)),
        selectInput(paste0("col_factor_", i),
                    paste0("因子 ", factor_labels[i], "："),
                    choices  = opt_cols,
                    selected = defaults[i])
      )
    })
    fluidRow(do.call(tagList, inputs))
  })

  # ── Factor 欄位 reactive helpers ──
  # 取得所有使用者選擇的因子欄清單（排除空值）
  factor_cols_r <- reactive({
    n <- max(1, min(as.integer(input$n_factors %||% 2), 6))
    cols <- lapply(seq_len(n), function(i) {
      val <- input[[paste0("col_factor_", i)]]
      if (!is.null(val) && val != "") val else NULL
    })
    Filter(Negate(is.null), cols)
  })

  # 向後相容：col_factorA / col_factorB 對應到 factor_cols_r
  col_factorA_r <- reactive({
    fc <- factor_cols_r()
    if (length(fc) >= 1) fc[[1]] else NULL
  })
  col_factorB_r <- reactive({
    fc <- factor_cols_r()
    if (length(fc) >= 2) fc[[2]] else NULL
  })

  col_factorC_r <- reactive({
    fc <- factor_cols_r()
    if (length(fc) >= 3) fc[[3]] else NULL
  })

  # fa_label / fb_label / fc_label 更新為動態
  fa_label <- reactive({
    lbl <- col_factorA_r() %||% "Salt"
    gsub("_", " ", lbl)
  })
  fb_label <- reactive({
    lbl <- col_factorB_r() %||% "Factor B"
    gsub("_", " ", lbl)
  })
  fc_label <- reactive({
    lbl <- col_factorC_r() %||% "Factor C"
    gsub("_", " ", lbl)
  })

  # 處理水準排序 UI
  output$treat_order_ui <- renderUI({
    req(raw_df_raw())
    df <- raw_df_raw()
    # 取得水準：優先用 col_treat，其次用複因子合併
    fc_list <- factor_cols_r()
    fc_valid <- Filter(function(c) c %in% names(df), fc_list)
    levs <- if (length(fc_valid) >= 1) {
      if (length(fc_valid) == 1) {
        as.character(unique(df[[fc_valid[[1]]]]))
      } else {
        as.character(unique(
          do.call(paste, c(lapply(fc_valid, function(c) df[[c]]), sep="_"))
        ))
      }
    } else if (!is.null(input$col_treat) && input$col_treat != "" &&
               input$col_treat %in% names(df)) {
      as.character(unique(df[[input$col_treat]]))
    } else {
      return(NULL)
    }
    tagList(
      h6("處理水準順序（可拖曳調整）：", style="margin-top:6px;"),
      helpText(style="font-size:11px;color:#888;",
               "拖曳項目調整順序。第一個為對照組，圖表顏色和分析將依此順序排列。"),
      sortable::rank_list(
        text       = NULL,
        labels     = levs,
        input_id   = "treat_order",
        orientation = "vertical",
        options    = sortable::sortable_options(animation = 150)
      )
    )
  })

  raw_df <- reactive({
    req(raw_df_raw())
    df  <- raw_df_raw()
    rc  <- if (!is.null(input$col_rep)   && input$col_rep   != "") input$col_rep   else {
             guess_col(names(df), c("重複","rep","replicate","replication")) }
    bc  <- if (!is.null(input$col_block) && input$col_block != "") input$col_block else ""
    lvs <- if (!is.null(input$treat_order) && length(input$treat_order) > 0)
             input$treat_order else character(0)

    # ── 結構欄位重命名（重複、區集）──
    if (rc != "" && rc != "重複" && rc %in% names(df))
      df <- df %>% rename(重複 = !!rc)
    if (bc != "" && bc != "區集" && bc %in% names(df))
      df <- df %>% rename(區集 = !!bc)
    if ("重複" %in% names(df))
      df$重複 <- as.factor(df$重複)

    # ── 動態因子合併，產生「處理」欄 ──
    fc_list  <- factor_cols_r()
    fc_valid <- Filter(function(c) c %in% names(df), fc_list)

    # ── 若使用者未設定因子欄，從非數值欄自動偵測 ──
    if (length(fc_valid) == 0) {
      excl_auto <- c("Plant_ID","plant_id","Treatment","treatment",
                     "Block","block","Rep","rep","重複","區集",
                     "Phase","phase","ID","SampleCode","Week","week","處理")
      num_cols_auto <- names(df)[sapply(df, is.numeric)]
      cand_auto <- setdiff(names(df), c(excl_auto, num_cols_auto))
      fc_valid  <- cand_auto[seq_len(min(2, length(cand_auto)))]
    }
    n_valid  <- length(fc_valid)

    if (n_valid >= 1) {
      # 合併產生 Treatment（任意數量因子）
      df$處理 <- if (n_valid == 1) {
        as.character(df[[fc_valid[[1]]]])
      } else {
        do.call(paste, c(lapply(fc_valid, function(c) df[[c]]), sep = "_"))
      }
      # 三個以上因子：額外欄位
      if (n_valid >= 3) {
        for (i in 3:n_valid)
          df[[paste0("Factor", LETTERS[i])]] <- as.factor(df[[fc_valid[[i]]]])
      }
    } else {
      # fallback：嘗試猜測處理欄
      tc_guess <- guess_col(names(df), c("處理","treatment","treat","trt"))
      if (tc_guess != "" && tc_guess != "處理" && tc_guess %in% names(df)) {
        df <- df %>% rename(處理 = !!tc_guess)
      }
      # 不建立 Salt/Microbe 副本，直接用 fc_valid 欄位
    }

    # 確保「處理」欄存在且為 factor
    if ("處理" %in% names(df)) {
      # 用 treat_order（若有）或資料本身的水準順序
      ord <- if (!is.null(input$treat_order) && length(input$treat_order) > 0)
               input$treat_order
             else if (length(lvs) > 0)
               lvs
             else
               as.character(unique(df$處理))
      df$處理 <- factor(df$處理, levels = ord)
    }

    # ── 區集欄位處理 ──
    if (!("區集" %in% colnames(df))) {
      df$區集 <- df$重複
    }
    df$區集 <- as.factor(df$區集)

    # (Salt/Microbe 別名已移除，直接使用 fc_valid 欄位名)

    return(df)
  })

  output$contrast_level_ui <- renderUI({
    df_c <- tryCatch(raw_df(), error=function(e) NULL)
    if (is.null(df_c) || nrow(df_c) == 0) return(NULL)
    # 動態取得固定因子的水準
    fa_c <- col_factorA_r(); fb_c <- col_factorB_r()
    target <- if (!is.null(input$contrast_target)) input$contrast_target else fb_c
    fixed_col <- if (!is.null(target) && !is.null(fb_c) &&
                     isTRUE(!is.na(fb_c)) && isTRUE(target == fb_c)) fa_c else fb_c
    if (is.null(fixed_col) || is.na(fixed_col) || !fixed_col %in% names(df_c))
      return(NULL)
    levs <- levels(factor(df_c[[fixed_col]]))
    if (length(levs) == 0) return(NULL)
    selectInput("fixed_level",
                paste0("步驟2. 固定在以下層級 (", fixed_col, "):"),
                choices = levs)
  })


  output$select_param_multi <- renderUI({
    req(raw_df())
    cols <- setdiff(colnames(raw_df() %>% dplyr::select(where(is.numeric))), c("處理", "重複", "區集", "流水號"))
    checkboxGroupInput("selected_params", "選擇分析參數:", choices = cols, selected = cols)
  })

  output$select_reps <- renderUI({
    req(raw_df())
    reps <- sort(unique(as.character(raw_df()$重複)))
    checkboxGroupInput("selected_reps", "納入重複:", choices = reps, selected = reps, inline = TRUE)
  })

  output$select_single_preview <- renderUI({
    req(input$selected_params)
    selectInput("single_param", "預覽參數:", choices = input$selected_params)
  })

  # ★ MICE 插補：自動排除缺失率 > 50% 的欄位，避免對根系/氣孔形態等高缺失欄位插補
  run_mice_impute <- function(df, params) {
    if (!any(is.na(df %>% dplyr::select(all_of(params))))) return(df)
    # 計算各欄缺失率，排除 > 50% 者
    miss_rate  <- sapply(df[params], function(x) mean(is.na(x)))
    params_ok  <- params[miss_rate <= 0.5]
    params_drop <- params[miss_rate > 0.5]
    if (length(params_drop) > 0)
      message("以下欄位缺失率 > 50%，已跳過 MICE 插補：",
              paste(params_drop, collapse = ", "))
    if (length(params_ok) < 1) return(df)
    if (!any(is.na(df %>% dplyr::select(all_of(params_ok))))) return(df)
    to_impute <- df %>% dplyr::select(any_of(c("處理", params_ok)))
    imp_res   <- tryCatch(
      mice(to_impute, m = 1, maxit = 5, method = "pmm", printFlag = FALSE),
      error = function(e) NULL
    )
    if (is.null(imp_res)) return(df)
    df_imputed <- df
    imputed_vals <- complete(imp_res, 1)
    for(p in params_ok) { df_imputed[[p]] <- imputed_vals[[p]] }
    return(df_imputed)
  }

  # ★ 新增：計算取均值後各 param 的實際 n（用於相關矩陣分層）
  rep_mean_df <- reactive({
    req(raw_df(), input$selected_params, input$selected_reps)
    raw_df() %>%
      filter(重複 %in% input$selected_reps) %>%
      group_by(處理, 重複) %>%
      summarise(across(all_of(input$selected_params), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
  })

  # ★ param_n_vec：每組平均有效植株數（原始資料，不取均值）
  # 計算方式：原始有效觀測數 / 處理組數（4）
  # 理由：所有參數取均值後均為 3 Rep，無法區分高/低樣本群；
  #        須以原始每株測量數量作為分層依據
  param_n_vec <- reactive({
    req(raw_df(), input$selected_params, input$selected_reps)
    df_raw  <- raw_df() %>% filter(重複 %in% input$selected_reps)
    n_treat <- length(unique(df_raw$處理))
    n_treat <- max(n_treat, 1)
    sapply(input$selected_params, function(p) {
      n_valid <- sum(!is.na(df_raw[[p]]))
      round(n_valid / n_treat, 1)   # 每組平均有效植株數
    })
  })

  # ★ 修改：corr_engine 加入分層篩選
  corr_engine <- reactive({
    req(raw_df(), input$selected_params, input$corr_method, param_n_vec())
    df_raw <- raw_df() %>% filter(重複 %in% input$selected_reps)

    # 決定要納入相關分析的 params
    # ★ 勾選取均值時，所有參數 n=3，強制使用全部參數
    effective_corr_mode <- if (isTRUE(input$use_mean_by_rep)) "all" else input$corr_n_mode
    n_vec   <- param_n_vec()
    thr     <- input$corr_n_threshold
    params_to_use <- switch(effective_corr_mode,
      "all"    = input$selected_params,
      "high_n" = names(n_vec[n_vec >= thr]),
      "low_n"  = names(n_vec[n_vec <  thr])
    )
    if (length(params_to_use) < 2) return(NULL)

    if (input$use_mean_by_rep) {
      corr_df <- rep_mean_df() %>% dplyr::select(all_of(params_to_use))
      mat_data <- as.matrix(corr_df)
    } else {
      # 不取均值時：直接使用原始值，以 pairwise.complete.obs 處理缺失
      # 不使用 MICE（大量 NA 欄位會導致 mice() 內部 if 條件長度 > 1 報錯）
      to_corr   <- df_raw %>% dplyr::select(all_of(params_to_use))
      # 排除全 NA 欄位（避免 sd = 0 / cor 計算失敗）
      valid_pre <- sapply(to_corr, function(x) sum(!is.na(x)) >= 3)
      to_corr   <- to_corr[, valid_pre, drop = FALSE]
      if (ncol(to_corr) < 2) return(NULL)
      mat_data  <- as.matrix(to_corr)
    }

    valid_cols <- apply(mat_data, 2, sd, na.rm = TRUE) > 0
    mat_data <- mat_data[, valid_cols, drop = FALSE]
    if (ncol(mat_data) < 2) return(NULL)

    method  <- input$corr_method
    cor_res <- cor(mat_data, use = "pairwise.complete.obs", method = method)
    n       <- ncol(mat_data)
    star_mat <- matrix("", n, n)
    colnames(star_mat) <- rownames(star_mat) <- colnames(mat_data)

    for(i in 1:n) {
      for(j in 1:n) {
        if(i != j) {
          p_val <- tryCatch(
            cor.test(mat_data[,i], mat_data[,j], method = method, exact = FALSE)$p.value,
            error = function(e) NA
          )
          star_mat[i,j] <- paste0(round(cor_res[i,j], 2), p_to_star(p_val))
        } else {
          star_mat[i,j] <- "1.00"
        }
      }
    }
    return(list(cor = cor_res, star = star_mat, method = method,
                params_used = colnames(mat_data), n_vec = n_vec[colnames(mat_data)]))
  })

  # ★ 新增：顯示相關矩陣分層資訊
  output$corr_n_info <- renderPrint({
    req(corr_engine())
    res <- corr_engine()
    thr_raw <- input$corr_n_threshold
    unit_label <- if (isTRUE(input$use_mean_by_rep)) "Rep 均值" else "個別植株原始值"
    cat(sprintf(
      "分析模式：%s（n 門檻 = %d Rep/組）\n繪圖單元：%s\n納入參數數：%d\n各參數有效觀測數：\n",
      input$corr_n_mode, thr_raw, unit_label, length(res$params_used)
    ))
    cat("各參數每組平均有效植株數（原始資料）：
")
    print(sort(res$n_vec))
  })

  multivariate_engine <- reactive({
    req(raw_df(), input$selected_params, input$selected_reps)
    df_raw <- raw_df() %>% filter(重複 %in% input$selected_reps)

    # ★ n 分層篩選：勾選取均值時強制使用全部參數
    effective_mv_mode <- if (isTRUE(input$use_mean_by_rep)) "all" else input$mv_n_mode
    n_vec   <- param_n_vec()
    thr_mv  <- input$mv_n_threshold
    mv_params <- switch(effective_mv_mode,
      "all"    = input$selected_params,
      "high_n" = names(n_vec[n_vec >= thr_mv]),
      "low_n"  = names(n_vec[n_vec <  thr_mv])
    )
    mv_params <- intersect(mv_params, input$selected_params)
    if (length(mv_params) < 2) return(NULL)

    if (input$use_mean_by_rep) {
      mv_df <- df_raw %>%
        group_by(across(all_of(intersect(
          unique(na.omit(c("處理", get_fa_col(.), get_fb_col(.), get_rep_col(.)))),
          names(.))))) %>%
        summarise(across(all_of(mv_params), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
      # 高 n 模式幾乎無缺失，可 drop_na；全部/低 n 模式先排除全 NA 欄再補值
      if (input$mv_n_mode == "high_n") {
        mv_df_clean <- mv_df %>% drop_na(all_of(mv_params))
      } else {
        mv_df_clean <- run_mice_impute(mv_df, mv_params)
        mv_df_clean <- mv_df_clean %>% drop_na(any_of(mv_params))
      }
    } else {
      # 不取均值：跳過 MICE，排除缺失率 > 50% 欄位，其餘用 complete.cases
      mv_df <- df_raw %>% dplyr::select(any_of(c("處理","重複")),
                                         all_of(na.omit(c(col_factorA_r(), col_factorB_r()))),
                                         all_of(mv_params))
      miss_rate  <- sapply(mv_df[mv_params], function(x) mean(is.na(x)))
      params_ok  <- mv_params[miss_rate <= 0.5]
      if (length(params_ok) < 2) return(NULL)
      mv_df_clean <- mv_df %>%
        dplyr::select(any_of(c("處理","重複")),
                      all_of(na.omit(c(col_factorA_r(), col_factorB_r()))),
                      all_of(params_ok)) %>%
        filter(complete.cases(.))
      mv_params <- params_ok
    }

    if (nrow(mv_df_clean) < 3) return(NULL)
    mat <- mv_df_clean %>% dplyr::select(all_of(mv_params))
    mat <- mat[, sapply(mat, sd, na.rm=TRUE) > 0, drop = FALSE]
    if (ncol(mat) < 2) return(NULL)

    pca_res <- prcomp(mat, scale. = TRUE)
    loadings_sq <- pca_res$rotation^2
    cp_pc1 <- round(100 * loadings_sq[, "PC1"] / sum(loadings_sq[, "PC1"]), 2)
    cp_pc2 <- round(100 * loadings_sq[, "PC2"] / sum(loadings_sq[, "PC2"]), 2)
    contrib_df <- data.frame(
      參數 = rownames(pca_res$rotation),
      PC1_貢獻度_百分比 = cp_pc1,
      PC2_貢獻度_百分比 = cp_pc2
    ) %>% arrange(desc(PC1_貢獻度_百分比))

    dist_mat <- vegdist(scale(mat), method = "euclidean")
    perms <- if(nrow(mv_df_clean) < 12) 199 else 999
    adonis_res <- tryCatch(
      {
        fa_ad <- col_factorA_r()
        fb_ad <- col_factorB_r()
        fc_ad <- col_factorC_r()
        fac_ad <- na.omit(c(fa_ad, fb_ad, fc_ad))
        fac_ad <- fac_ad[fac_ad %in% names(mv_df_clean)]
        f_ad  <- if (length(fac_ad) >= 2)
          as.formula(paste0("dist_mat ~ ", paste0("`", fac_ad, "`", collapse = " * ")))
        else if (length(fac_ad) == 1)
          as.formula(paste0("dist_mat ~ `", fac_ad[1], "`"))
        else
          stop("no valid factor columns")
        adonis2(f_ad, data = mv_df_clean, permutations = perms)
      },
      error = function(e) NULL
    )
    pw_adonis <- tryCatch(
      pairwise.adonis(dist_mat, mv_df_clean$處理, sim.method = "euclidean", p.adjust.m = "bonferroni"),
      error = function(e) NULL
    )
    return(list(pca = pca_res, adonis = adonis_res, pw = pw_adonis,
                meta = mv_df_clean, mat = mat, contrib = contrib_df))
  })

  all_params_engine <- reactive({
    req(raw_df(), input$selected_reps, input$selected_params,
        col_factorA_r())  # 確保因子 reactive 已就緒
    df_raw <- raw_df() %>% filter(重複 %in% input$selected_reps)
    stats_list <- list(); means_list <- list(); dist_list <- list(); pairwise_list <- list()

    old_opts <- options(contrasts = c("contr.sum", "contr.poly"))
    on.exit(options(old_opts))

    for (p in input$selected_params) {
      # 防護：若取 Rep 均值後每組只剩 1 筆，自動退回個株
      # 用實際存在的處理欄計算每組筆數
      trt_col_n <- if ("處理" %in% names(df_raw)) "處理"
                   else { fa_t <- get_fa_col(df_raw); if(!is.null(fa_t)) fa_t else names(df_raw)[1] }
      trt_col_n <- if (is.na(trt_col_n) || is.null(trt_col_n)) names(df_raw)[1]
                   else trt_col_n
      rep_col_n <- intersect(c("重複","Rep","rep","Replicate"), names(df_raw))[1]
      rep_col_n <- if (is.na(rep_col_n)) names(df_raw)[1] else rep_col_n
      n_per_group <- df_raw %>%
        group_by(across(all_of(c(trt_col_n, rep_col_n)))) %>%
        summarise(n=n(), .groups="drop") %>%
        pull(n) %>% mean(na.rm=TRUE)
      use_mean_eff <- isTRUE(input$use_mean_by_rep) && n_per_group > 1
      temp_df <- if (use_mean_eff) {
        {
          # 取 Rep 均值：只按 Salt/Inoculation 因子欄 + 重複 + 區集 分組
          # 不加「處理」（避免和因子欄重複導致每組只剩 1 筆）
          # 因子欄：使用者選擇的欄位 + 可能的額外因子
          # 直接用使用者選擇的因子欄（不猜測，不硬編碼）
          fa_g <- col_factorA_r()
          fb_g <- col_factorB_r()
          fc_g <- col_factorC_r()
          factor_grp <- unique(na.omit(c(
            if (!is.null(fa_g) && fa_g %in% names(df_raw)) fa_g else NULL,
            if (!is.null(fb_g) && fb_g %in% names(df_raw)) fb_g else NULL,
            if (!is.null(fc_g) && fc_g %in% names(df_raw)) fc_g else NULL
          )))
          if (length(factor_grp) == 0)
            factor_grp <- names(df_raw)[1]  # 最後防線
          # 結構欄：同時支援中文（重複/區集）和英文（Rep/Block）命名
          rep_col   <- intersect(c("重複","Rep","rep","Replicate"), names(df_raw))[1]
          block_col <- intersect(c("區集","Block","block","Blk"),   names(df_raw))[1]
          struct_grp <- na.omit(c(rep_col, block_col))
          grp_cols   <- unique(c(factor_grp, struct_grp))
          {
            tmp <- df_raw %>% group_by(across(all_of(grp_cols))) %>%
              summarise(Value = mean(.data[[p]], na.rm = TRUE), .groups = "drop") %>%
              { if (!"處理" %in% names(.) && "處理" %in% names(df_raw))
                  left_join(., distinct(df_raw[, c(factor_grp, "處理")]),
                            by=factor_grp, relationship = "many-to-many") else . } %>%
              filter(!is.na(Value))
            # 補回 factor_grp 中缺失的因子欄（防止 fc_col_e 被誤判為不存在）
            for (fc_fill in factor_grp) {
              if (!fc_fill %in% names(tmp) && fc_fill %in% names(df_raw)) {
                fc_map <- distinct(df_raw[, intersect(c(factor_grp, fc_fill), names(df_raw))])
                tmp <- left_join(tmp, fc_map, by = intersect(names(tmp), names(fc_map)),
                                 relationship = "many-to-many")
              }
            }
            tmp
          }
        }
      } else {
        df_raw %>% rename(Value = !!p) %>% filter(!is.na(Value))
      }

if(nrow(temp_df) < 3) next

      # ★ 套用資料轉換（分布檢定、LMM 均使用轉換後數值）
      trfm <- get_transform(p)
      if (trfm != "none") {
        temp_df <- temp_df %>%
          mutate(Value = apply_transform(Value, trfm))
        temp_df <- temp_df %>% filter(!is.na(Value) & is.finite(Value))
        if (nrow(temp_df) < 3) next
      }

      # ★ 分布診斷：依檢定方法調整對象
      # block_type 輔助變數（有效區集條件）
      blk_type  <- if (!is.null(input$block_effect_type)) input$block_effect_type else "random"
      blk_col_t <- intersect(c("區集","Block","block","Blk"), names(temp_df))[1]
      has_block <- !is.na(blk_col_t) && length(unique(temp_df[[blk_col_t]])) > 1
      # 若區集欄不叫「區集」，補充命名（供公式使用）
      if (has_block && !is.na(blk_col_t) && blk_col_t != "區集")
        temp_df$區集 <- temp_df[[blk_col_t]]

      # 確保 temp_df 有「處理」欄（若無，用 Salt×Inoculation 合併）
      if (!("處理" %in% names(temp_df))) {
        fc_t <- na.omit(c(get_fa_col(temp_df), get_fb_col(temp_df)))
        if (length(fc_t) >= 2)
          temp_df$處理 <- factor(do.call(paste, c(lapply(fc_t, function(x) temp_df[[x]]), sep="_")))
        else if (length(fc_t) == 1)
          temp_df$處理 <- factor(temp_df[[fc_t[1]]])
      }
      if (!("處理" %in% names(temp_df)) || length(levels(factor(temp_df$處理))) < 2) next

      dist_list[[p]] <- tryCatch({
        if (input$test_method == "LMM") {
          # LMM：診斷模型殘差（區集永遠隨機）
          f_lmm_d <- make_formula_str(temp_df, has_block, "random", "LMM", fa_col_e, fb_col_e, fc_col_e)
          m_diag     <- tryCatch(lmer(as.formula(f_lmm_d), data = temp_df), error = function(e) NULL)
          resid_vals <- if (!is.null(m_diag)) residuals(m_diag) else temp_df$Value
          shapiro_p  <- tryCatch(shapiro.test(resid_vals)$p.value, error = function(e) NA)
          levene_p   <- tryCatch(car::leveneTest(Value ~ 處理, data = temp_df)$`Pr(>F)`[1], error = function(e) NA)
          data.frame(
            Parameter   = p,
            診斷對象    = "LMM 殘差",
            Shapiro_p   = round(shapiro_p, 4),
            Levene_p    = round(levene_p, 4),
            殘差常態    = if (!is.na(shapiro_p) && shapiro_p > 0.05) "Pass" else "Warning",
            變異數齊性  = if (!is.na(levene_p)  && levene_p  > 0.05) "Pass" else "Ref only"
          )
        } else {
          # ANOVA / Kruskal：診斷原始資料
          shapiro_p <- tryCatch(shapiro.test(temp_df$Value)$p.value, error = function(e) NA)
          levene_p  <- tryCatch(car::leveneTest(Value ~ 處理, data = temp_df)$`Pr(>F)`[1], error = function(e) NA)
          data.frame(
            Parameter   = p,
            診斷對象    = "原始資料",
            Shapiro_p   = round(shapiro_p, 4),
            Levene_p    = round(levene_p, 4),
            常態性      = if (!is.na(shapiro_p) && shapiro_p > 0.05) "Pass" else "Warning",
            變異數齊性  = if (!is.na(levene_p)  && levene_p  > 0.05) "Pass" else "Warning"
          )
        }
      }, error = function(e) NULL)

      p_salt <- NA; p_microbe <- NA; p_inter <- NA; p_c <- NA; cont_p <- NA

      # 直接用使用者在 UI 選擇的因子欄名（不透過 get_fa_col 猜測）
      fa_col_e <- col_factorA_r()
      fb_col_e <- col_factorB_r()
      fc_col_e <- col_factorC_r()  # NULL 表示沒選第3因子

      # 安全防護：以 df_raw（原始資料）確認欄名有效，不用 temp_df
      # （temp_df 取均值後可能欄名變少，容易誤判）
      raw_names <- names(df_raw)
      fa_col_e <- if (isTRUE(!is.null(fa_col_e) && fa_col_e %in% raw_names)) fa_col_e else NA_character_
      fb_col_e <- if (isTRUE(!is.null(fb_col_e) && fb_col_e %in% raw_names)) fb_col_e else NA_character_
      fc_col_e <- if (isTRUE(!is.null(fc_col_e) && !is.na(fc_col_e) && fc_col_e %in% raw_names)) fc_col_e else NA_character_

      inter_col_e <- if (!is.na(fb_col_e))
                       paste0(fa_col_e, ":", fb_col_e) else ""

      # 計算因子數與複因子判斷
      n_factors_val <- sum(!is.na(c(fa_col_e, fb_col_e, fc_col_e)))
      has_extra     <- n_factors_val >= 4
      is_fact_e     <- isTRUE(!has_extra &&
                   n_factors_val >= 2 &&
                   !is.na(fa_col_e) && !is.na(fb_col_e) &&
                   length(unique(temp_df[[fb_col_e]])) > 1 &&
                   (is.na(fc_col_e) || fc_col_e %in% names(temp_df)))

      if (input$test_method == "ANOVA") {
        formula_str <- make_formula_str(temp_df, has_block, blk_type, "ANOVA_str", fa_col_e, fb_col_e, fc_col_e)
        cat("[F]", p, "|", formula_str, "| n=", nrow(temp_df),
            "| fa=", fa_col_e, "fb=", fb_col_e,
            "| has_block=", has_block, "blk_type=", blk_type,
            "| is_fact_e=", is_fact_e, "\n")
        model_lm <- tryCatch(lm(as.formula(formula_str), data = temp_df), error = function(e) {
NULL })
        if (!is.null(model_lm) && df.residual(model_lm) > 0) {
          aov_tab <- tryCatch(
            as.data.frame(car::Anova(model_lm, type = "III")),
            error = function(e) NULL)
          if (is.null(aov_tab)) next
          p_col     <- "Pr(>F)"
          row_names <- rownames(aov_tab)
          if (is_fact_e) {
            p_salt    <- if (!is.na(fa_col_e)    && fa_col_e    %in% row_names) aov_tab[fa_col_e,    p_col] else NA
            p_microbe <- if (!is.na(fb_col_e)    && fb_col_e    %in% row_names) aov_tab[fb_col_e,    p_col] else NA
            p_inter   <- if (nchar(inter_col_e)  && inter_col_e %in% row_names) aov_tab[inter_col_e, p_col] else NA
            p_c       <- if (!is.na(fc_col_e)    && fc_col_e    %in% row_names) aov_tab[fc_col_e,    p_col] else NA
            cont_p <- tryCatch({
              fa_e_pw <- col_factorA_r() %||% fa_col_e
              fb_e_pw <- col_factorB_r() %||% fb_col_e
              target_col <- if (!is.null(input$contrast_target) &&
                                 input$contrast_target == fb_e_pw) fb_e_pw else fa_e_pw
              fixed_col  <- if (target_col == fb_e_pw) fa_e_pw else fb_e_pw
              spec    <- as.formula(paste0("~ `", target_col, "` | `", fixed_col, "`"))
              emm     <- emmeans(model_lm, spec)
              cont_tab <- as.data.frame(summary(pairs(emm)))
              cont_tab[cont_tab[[fixed_col]] == input$fixed_level, "p.value"]
            }, error = function(e) NA)
          } else {
            # 單因子：取 fa_col_e 或「處理」
            p_key  <- if (fa_col_e %in% row_names) fa_col_e else "處理"
            p_salt    <- if (p_key %in% row_names) aov_tab[p_key, p_col] else NA
            p_microbe <- NA; p_inter <- NA
            cont_p    <- tryCatch({
              emm <- emmeans(model_lm, ~ 處理)
              min(as.data.frame(summary(pairs(emm)))$p.value, na.rm = TRUE)
            }, error = function(e) NA)
          }
        }
      } else if (input$test_method == "LMM") {
        tryCatch({
          f_lmm_p <- make_formula_str(temp_df, has_block, "random", "LMM", fa_col_e, fb_col_e, fc_col_e)
          m_lmm_p   <- lmer(as.formula(f_lmm_p), data = temp_df,
                             control = lmerControl(optimizer = "bobyqa"))
          aov_lmm   <- as.data.frame(anova(m_lmm_p, type = "3"))
          row_names <- rownames(aov_lmm)
          if (is_fact_e) {
            p_salt    <- if (!is.na(fa_col_e)    && fa_col_e    %in% row_names) aov_lmm[fa_col_e,    "Pr(>F)"] else NA
            p_microbe <- if (!is.na(fb_col_e)    && fb_col_e    %in% row_names) aov_lmm[fb_col_e,    "Pr(>F)"] else NA
            p_inter   <- if (nchar(inter_col_e)  && inter_col_e %in% row_names) aov_lmm[inter_col_e, "Pr(>F)"] else NA
            p_c       <- if (!is.na(fc_col_e)    && fc_col_e    %in% row_names) aov_lmm[fc_col_e,    "Pr(>F)"] else NA
             fa_e2  <- col_factorA_r() %||% fa_col_e
             fb_e2  <- col_factorB_r() %||% fb_col_e
             target_col2 <- if (!is.null(input$contrast_target) && input$contrast_target == fb_e2) fb_e2 else fa_e2
             fixed_col2  <- if (target_col2 == fb_e2) fa_e2 else fb_e2
            spec      <- as.formula(paste0("~ `", target_col2, "` | `", fixed_col2, "`"))
            emm_lmm   <- emmeans(m_lmm_p, spec)
            cont_tab_lmm <- as.data.frame(summary(pairs(emm_lmm)))
            cont_p    <- cont_tab_lmm[cont_tab_lmm[[fixed_col2]] == input$fixed_level, "p.value"]
          } else {
            # 單因子
            p_key2 <- if (fa_col_e %in% row_names) fa_col_e else "處理"
            p_salt    <- if (p_key2 %in% row_names) aov_lmm[p_key2, "Pr(>F)"] else NA
            p_microbe <- NA; p_inter <- NA
            cont_p    <- tryCatch({
              emm_lmm <- emmeans(m_lmm_p, ~ 處理)
              min(as.data.frame(summary(pairs(emm_lmm)))$p.value, na.rm = TRUE)
            }, error = function(e) NA)
          }
        }, error = function(e) NULL)
      } else {
        # Kruskal
        p_salt    <- tryCatch(
          kruskal.test(Value ~ 處理, data = temp_df)$p.value,
          error = function(e) NA)
        p_microbe <- NA; p_inter <- NA; cont_p <- NA
      }

      # ★ η² 計算（ANOVA / LMM 均支援；Kruskal 回傳 NA）
      eta2 <- if (input$test_method %in% c("ANOVA", "LMM")) {
        compute_eta2(temp_df, input$test_method, input$design_type, blk_type,
                     fa_col = fa_col_e, fb_col = fb_col_e, fc_col = fc_col_e)
      } else {
        list(eta2_1 = NA, eta2_2 = NA, eta2_inter_AB = NA)
      }

      # ★ R²m / R²c / ΔR²（僅 LMM）
      r2 <- compute_r2(temp_df, input$test_method, fa_col_e, fb_col_e, fc_col_e)

      # 動態欄位標題
      fA <- fa_label(); fB <- fb_label(); fC <- fc_label()
      has_fc <- !is.null(fc_col_e) && !is.na(fc_col_e)

      # 建構 row_df（依序：A主效應→B主效應→AB交互→C主效應→AC→BC→ABC→R²）
      row_df <- data.frame(
        Parameter      = p,
        P_A            = round(as.numeric(p_salt),    5),
        Sig_A          = p_to_star(as.numeric(p_salt)),
        Eta2_A         = eta2$eta2_1 %||% NA,
        P_B            = round(as.numeric(p_microbe), 5),
        Sig_B          = p_to_star(as.numeric(p_microbe)),
        Eta2_B         = eta2$eta2_2 %||% NA,
        P_Interact     = if(is.na(p_inter)) "-" else as.character(round(as.numeric(p_inter), 5)),
        Sig_Interact   = if(is.na(p_inter)) "-" else p_to_star(as.numeric(p_inter)),
        Eta2_AB        = eta2$eta2_inter_AB %||% NA,
        stringsAsFactors = FALSE
      )

      # 若有第3因子：插入 C 主效應（含 p 值）和所有交互效應欄
      if (has_fc) {
        row_df$P_C        <- round(as.numeric(p_c), 5)
        row_df$Sig_C      <- p_to_star(as.numeric(p_c))
        row_df$Eta2_C     <- eta2$eta2_3 %||% NA
        row_df$Eta2_AC    <- eta2$eta2_inter_AC  %||% NA
        row_df$Eta2_BC    <- eta2$eta2_inter_BC  %||% NA
        row_df$Eta2_ABC   <- eta2$eta2_inter_ABC %||% NA
      }

      # R² 欄
      row_df$R2m        <- r2$r2m
      row_df$R2c        <- r2$r2c
      row_df$Delta_R2   <- r2$delta_r2
      # cont_p 可能是向量，取最小值；NULL/NA/長度0 都回傳 "-"
      cont_p_val <- tryCatch({
        v <- as.numeric(cont_p)
        v <- v[!is.na(v)]
        if (length(v) == 0) NA else min(v)
      }, error = function(e) NA)
      row_df$Contrast_p   <- if(is.na(cont_p_val)) "-" else as.character(round(cont_p_val, 5))
      row_df$Sig_Contrast <- if(is.na(cont_p_val)) "-" else p_to_star(cont_p_val)

      # 套用動態欄位標題
      names(row_df)[names(row_df) == "P_A"]       <- paste0("P_",    fA)
      names(row_df)[names(row_df) == "Sig_A"]     <- paste0("Sig_",  fA)
      names(row_df)[names(row_df) == "Eta2_A"]    <- paste0("Eta2_", fA)
      names(row_df)[names(row_df) == "P_B"]       <- paste0("P_",    fB)
      names(row_df)[names(row_df) == "Sig_B"]     <- paste0("Sig_",  fB)
      names(row_df)[names(row_df) == "Eta2_B"]    <- paste0("Eta2_", fB)
      names(row_df)[names(row_df) == "Eta2_AB"]   <- paste0("Eta2_", fA, ":", fB)
      if (has_fc) {
        names(row_df)[names(row_df) == "P_C"]     <- paste0("P_",    fC)
        names(row_df)[names(row_df) == "Sig_C"]   <- paste0("Sig_",  fC)
        names(row_df)[names(row_df) == "Eta2_C"]  <- paste0("Eta2_", fC)
        names(row_df)[names(row_df) == "Eta2_AC"] <- paste0("Eta2_", fA, ":", fC)
        names(row_df)[names(row_df) == "Eta2_BC"] <- paste0("Eta2_", fB, ":", fC)
        names(row_df)[names(row_df) == "Eta2_ABC"]<- paste0("Eta2_", fA, ":", fB, ":", fC)
      }
      stats_list[[p]] <- row_df


      cld <- get_cld_custom(temp_df, "Value", input$test_method, input$post_hoc_anova, has_block, blk_type)
      if (!is.null(cld)) {
        means_list[[p]] <- temp_df %>% group_by(處理) %>%
          summarise(Mean = mean(Value, na.rm = TRUE), SE = sd(Value, na.rm = TRUE)/sqrt(n()), .groups = "drop") %>%
          mutate(Letter = cld[as.character(處理)], Parameter = p)
      }

      # ★ Pairwise 成對檢驗（Tukey/Dunn 或 Paired t-test）
      pw_method <- if (!is.null(input$pw_method_type)) input$pw_method_type else "tukey"

      pairwise_list[[p]] <- tryCatch({
        if (pw_method == "ttest") {
          # ── Independent (Welch) t-test：以 Rep 均值為獨立觀測（n=3/組）──
          rep_avg <- temp_df %>%
            group_by(across(all_of(intersect(c("處理","重複"), names(temp_df))))) %>%
            summarise(v = mean(Value, na.rm = TRUE), .groups = "drop") %>%
            filter(!is.na(v))
          treats <- levels(factor(rep_avg$處理))
          treat_pairs <- combn(treats, 2, simplify = FALSE)
          pw_rows <- lapply(treat_pairs, function(pair) {
            g1 <- pair[1]; g2 <- pair[2]
            v1 <- rep_avg$v[rep_avg$處理 == g1]
            v2 <- rep_avg$v[rep_avg$處理 == g2]
            if (length(v1) < 2 || length(v2) < 2) return(NULL)
            # Welch independent t-test（var.equal = FALSE）
            tt <- tryCatch(t.test(v1, v2, paired = FALSE, var.equal = FALSE),
                           error = function(e) NULL)
            if (is.null(tt)) return(NULL)
            # Cohen's d（pooled SD）
            n1 <- length(v1); n2 <- length(v2)
            sd_pool <- tryCatch(
              sqrt(((n1-1)*var(v1,na.rm=TRUE) + (n2-1)*var(v2,na.rm=TRUE)) / (n1+n2-2)),
              error = function(e) NA)
            cohens_d <- if (!is.na(sd_pool) && sd_pool > 0) {
              round((mean(v2,na.rm=TRUE) - mean(v1,na.rm=TRUE)) / sd_pool, 3)
            } else NA
            data.frame(
              Parameter     = p,
              comparison    = paste0(g1, " - ", g2),
              mean_g1       = round(mean(v1, na.rm=TRUE), 4),
              mean_g2       = round(mean(v2, na.rm=TRUE), 4),
              mean_diff     = round(mean(v2,na.rm=TRUE) - mean(v1,na.rm=TRUE), 4),
              t.statistic   = round(tt$statistic, 3),
              df            = round(tt$parameter, 2),
              p.value       = round(tt$p.value, 4),
              Sig           = p_to_star(tt$p.value),
              Cohen_d       = cohens_d,
              Cohen_d_label = cohen_d_label(cohens_d),
              n1            = n1,
              n2            = n2,
              stringsAsFactors = FALSE
            )
          })
          pw_rows <- Filter(Negate(is.null), pw_rows)
          if (length(pw_rows) == 0) return(NULL)
          do.call(rbind, pw_rows)

        } else if (input$test_method %in% c("ANOVA", "LMM")) {
          # ── Tukey / emmeans ──
          if (input$test_method == "ANOVA") {
            old_opts2 <- options(contrasts = c("contr.sum", "contr.poly"))
            on.exit(options(old_opts2), add = TRUE)
            f_pw <- if (has_block && blk_type == "fixed") {
              "Value ~ 處理 + 區集"
            } else {
              "Value ~ 處理"
            }
            m_pw <- lm(as.formula(f_pw), data = temp_df)
          } else {
            f_pw <- if (has_block) {
              "Value ~ 處理 + (1|區集/重複)"
            } else {
              "Value ~ 處理 + (1|重複)"
            }
            m_pw <- lmer(as.formula(f_pw), data = temp_df)
          }
          emm_pw   <- emmeans(m_pw, ~ 處理)
          # 對應 post_hoc_anova 設定：Tukey → "tukey"；LSD → "none"（無校正）
          pw_adjust <- if (!is.null(input$post_hoc_anova) && input$post_hoc_anova == "LSD") "none" else "tukey"
          pw_res  <- as.data.frame(summary(pairs(emm_pw, adjust = pw_adjust)))
          pw_res$Parameter <- p
          col_idx <- which(names(pw_res) == "contrast")
          if (length(col_idx) > 0) names(pw_res)[col_idx] <- "comparison"
          keep_cols <- intersect(c("Parameter","comparison","estimate","SE","df","t.ratio","p.value"),
                                 names(pw_res))
          pw_res <- pw_res[, keep_cols, drop = FALSE]
          pw_res$p.value  <- round(pw_res$p.value,  4)
          pw_res$estimate <- round(pw_res$estimate, 4)
          pw_res$t.ratio  <- round(pw_res$t.ratio,  3)
          # Cohen's d = estimate / pooled residual SE（近似）
          treat_levs <- levels(factor(temp_df$處理))
          pw_res$Cohen_d <- sapply(seq_len(nrow(pw_res)), function(i) {
            comp_str <- as.character(pw_res$comparison[i])
            parts    <- trimws(strsplit(comp_str, "-|–")[[1]])
            if (length(parts) < 2) return(NA)
            g1 <- parts[1]; g2 <- parts[2]
            v1 <- temp_df$Value[temp_df$處理 == g1]
            v2 <- temp_df$Value[temp_df$處理 == g2]
            if (length(v1) < 2 || length(v2) < 2) return(NA)
            n1 <- length(v1); n2 <- length(v2)
            sd_pool <- sqrt(((n1-1)*var(v1,na.rm=TRUE) + (n2-1)*var(v2,na.rm=TRUE)) / (n1+n2-2))
            if (is.na(sd_pool) || sd_pool == 0) return(NA)
            round((mean(v2,na.rm=TRUE) - mean(v1,na.rm=TRUE)) / sd_pool, 3)
          })
          pw_res$Cohen_d_label <- sapply(pw_res$Cohen_d, cohen_d_label)
          pw_res$Sig    <- sapply(pw_res$p.value, p_to_star)
          pw_res$Adjust <- pw_adjust   # 記錄校正方式（tukey / none）
          pw_res
        } else {
          # ── Kruskal: Dunn test ──
          d_res <- temp_df %>%
            dunn_test(Value ~ 處理, p.adjust.method = "bonferroni") %>%
            mutate(comparison = paste0(group1, " - ", group2), Parameter = p) %>%
            dplyr::select(Parameter, comparison, statistic, p.adj) %>%
            rename(p.value = p.adj) %>%
            mutate(p.value = round(.data$p.value, 4), Sig = p_to_star(.data$p.value),
                   Cohen_d = NA_real_, Cohen_d_label = "—")
          d_res
        }
      }, error = function(e) NULL)
    }
    return(list(stats    = bind_rows(stats_list),
                means    = bind_rows(means_list),
                dist     = bind_rows(dist_list),
                pairwise = bind_rows(pairwise_list)))
  })

  single_preview_engine <- reactive({
    req(raw_df(), input$single_param, input$selected_reps, col_factorA_r())
    p      <- input$single_param
    fa_sp  <- col_factorA_r(); fb_sp <- col_factorB_r(); fc_sp <- col_factorC_r()
    df_raw <- raw_df() %>% filter(重複 %in% input$selected_reps)

    n_pg_s <- df_raw %>% group_by(處理, 重複) %>%
      summarise(n=n(), .groups="drop") %>% pull(n) %>% mean(na.rm=TRUE)
    use_mean_s <- isTRUE(input$use_mean_by_rep) && n_pg_s > 1

    temp_df <- if (use_mean_s) {
      grp_s        <- get_grp_cols(df_raw)
      factor_grp_s <- na.omit(c(fa_sp, fb_sp, fc_sp))
      factor_grp_s <- factor_grp_s[factor_grp_s %in% names(df_raw)]
      df_raw %>% group_by(across(all_of(grp_s))) %>%
        summarise(Value = mean(.data[[p]], na.rm = TRUE), .groups = "drop") %>%
        { if (!"處理" %in% names(.) && "處理" %in% names(df_raw))
            left_join(., distinct(df_raw[, c(factor_grp_s, "處理")]),
                      by=factor_grp_s, relationship="many-to-many") else . } %>%
        filter(!is.na(Value))
    } else {
      df_raw %>% rename(Value = !!p) %>% filter(!is.na(Value))
    }

    blk_type_s  <- if (!is.null(input$block_effect_type)) input$block_effect_type else "random"
    has_block_s <- input$design_type == "RCBD" && "區集" %in% colnames(temp_df)

    # 對比目標和固定因子（使用 reactive，不硬編碼）
    contrast_target <- input$contrast_target %||% fb_sp %||% fa_sp
    fixed_f <- if (!is.null(contrast_target) && isTRUE(contrast_target == fb_sp)) fa_sp else fb_sp
    fixed_f <- if (is.null(fixed_f)) fa_sp else fixed_f
    fixed_lvl <- input$fixed_level %||% ""

    if (input$test_method == "ANOVA") {
      old_opts <- options(contrasts = c("contr.sum", "contr.poly"))
      on.exit(options(old_opts))
      formula_str <- make_formula_str(temp_df,
                       has_block_s && blk_type_s != "random",
                       blk_type_s, "ANOVA_str", fa_sp, fb_sp, fc_sp)
      model_lm <- tryCatch(lm(as.formula(formula_str), data=temp_df), error=function(e) NULL)
      if (is.null(model_lm) || df.residual(model_lm) <= 0)
        return(list(formula=formula_str, error="殘差自由度不足，模型無法建立"))
      anova_res <- tryCatch(car::Anova(model_lm, type="III"), error=function(e) NULL)
      if (is.null(anova_res))
        return(list(formula=formula_str, error="ANOVA 失敗"))
      blk_label  <- if (has_block_s) paste0("(Block=", blk_type_s, ")") else "(CRD)"
      disp_title <- paste0("A. 變異數分析 (Type III) - ", input$design_type, " ", blk_label)
      # post-hoc 用「處理」欄
      f_post <- if (has_block_s && blk_type_s == "fixed")
        as.formula("Value ~ 處理 + 區集") else as.formula("Value ~ 處理")
      model_aov_post <- tryCatch(aov(f_post, data=temp_df), error=function(e) NULL)
      posthoc_df <- tryCatch({
        if (is.null(model_aov_post)) stop("no model")
        res_post <- if (input$post_hoc_anova == "Tukey")
          agricolae::HSD.test(model_aov_post, "處理")
        else
          agricolae::LSD.test(model_aov_post, "處理")
        res_post$groups %>% rownames_to_column("處理")
      }, error = function(e) data.frame(處理=character(0)))
      eta2 <- compute_eta2(temp_df, "ANOVA", input$design_type, blk_type_s, fa_sp, fb_sp, fc_sp)
      cont_res <- tryCatch({
        req(!is.null(contrast_target), !is.null(fixed_f))
        spec <- as.formula(paste0("~ `", contrast_target, "` | `", fixed_f, "`"))
        summary(pairs(emmeans(model_lm, spec))) %>%
          filter(!!sym(fixed_f) == fixed_lvl)
      }, error = function(e) paste0("對比運算失敗：", e$message))
      return(list(model_summary = anova_res, posthoc = posthoc_df, eta2 = eta2,
                  r2 = list(r2m=NA, r2c=NA, delta_r2=NA),
                  contrast = cont_res, title = disp_title, type = "ANOVA"))

    } else if (input$test_method == "LMM") {
      f_lmm_p   <- make_formula_str(temp_df, has_block_s, "random", "LMM", fa_sp, fb_sp, fc_sp)
      f_lmm_cld <- if (has_block_s) "Value ~ 處理 + (1|區集/重複)" else "Value ~ 處理 + (1|重複)"
      model_lmm_p   <- tryCatch(
        lmer(as.formula(f_lmm_p), data=temp_df, control=lmerControl(optimizer="bobyqa")),
        error=function(e) NULL)
      if (is.null(model_lmm_p))
        return(list(error="LMM 模型建立失敗", title="LMM 失敗", type="LMM",
                    eta2=list(eta2_1=NA,eta2_2=NA,eta2_inter_AB=NA),
                    r2=list(r2m=NA,r2c=NA,delta_r2=NA)))
      anova_res     <- anova(model_lmm_p, type="3")
      model_lmm_cld <- tryCatch(
        lmer(as.formula(f_lmm_cld), data=temp_df, control=lmerControl(optimizer="bobyqa")),
        error=function(e) NULL)
      cld_res <- tryCatch({
        req(!is.null(model_lmm_cld))
        emm_all <- emmeans(model_lmm_cld, ~ 處理)
        adj <- if (isTRUE(input$post_hoc_anova == "LSD")) "none" else "tukey"
        as.data.frame(multcomp::cld(emm_all, Letters=letters, adjust=adj))
      }, error=function(e) data.frame(處理=character(0)))
      eta2    <- compute_eta2(temp_df, "LMM", input$design_type, blk_type_s, fa_sp, fb_sp, fc_sp)
      r2_lmm  <- compute_r2(temp_df, "LMM", fa_sp, fb_sp, fc_sp)
      cont_res <- tryCatch({
        req(!is.null(contrast_target), !is.null(fixed_f), nchar(fixed_lvl) > 0)
        spec <- as.formula(paste0("~ `", contrast_target, "` | `", fixed_f, "`"))
        summary(pairs(emmeans(model_lmm_p, spec))) %>%
          filter(!!sym(fixed_f) == fixed_lvl)
      }, error=function(e) paste0("對比運算失敗：", e$message))
      return(list(model_summary=anova_res, posthoc=cld_res, eta2=eta2, r2=r2_lmm,
                  contrast=cont_res,
                  title="A. 線性混合模型檢定 (LMM) - 因子拆解與簡單主效應分析",
                  type="LMM"))

    } else {
      k_res <- kruskal.test(Value ~ 處理, data=temp_df)
      d_res <- temp_df %>% dunn_test(Value ~ 處理, p.adjust.method="bonferroni")
      return(list(model_summary=k_res, posthoc=d_res,
                  eta2=list(eta2_1=NA, eta2_2=NA, eta2_inter_AB=NA),
                  r2=list(r2m=NA, r2c=NA, delta_r2=NA),
                  contrast="無", title="A. Kruskal-Wallis 檢定", type="Kruskal"))
    }
  })

  output$preview_anova_title <- renderUI({ h4(single_preview_engine()$title) })
  output$preview_anova_res   <- renderPrint({ single_preview_engine()$model_summary })
  output$preview_posthoc_table <- renderTable({ single_preview_engine()$posthoc })

  # ── 當前分析模型顯示 ──────────────────────────────────────────────────────
  make_model_formula_str <- function() {
    fa <- col_factorA_r(); fb <- col_factorB_r(); fc <- col_factorC_r()
    method   <- if (!is.null(input$test_method)) input$test_method else "LMM"
    blk_type <- if (!is.null(input$block_effect_type)) input$block_effect_type else "random"
    has_blk  <- isTRUE(tryCatch({
      df <- raw_df()
      bc <- intersect(c("區集","Block","block","Blk"), names(df))[1]
      !is.na(bc) && length(unique(df[[bc]])) > 1
    }, error = function(e) FALSE))
    
    facs <- na.omit(c(fa, fb, fc))
    if (length(facs) == 0) return("（尚未選擇因子）")
    
    lhs <- "Parameter"
    rhs_fixed <- paste0("`", facs, "`", collapse = " * ")
    
    rhs_random <- if (method == "LMM") {
      if (has_blk && blk_type == "random") " + (1|區集/重複)"
      else if (has_blk && blk_type == "fixed") " + 區集"
      else " + (1|重複)"
    } else ""
    
    paste0(lhs, " ~ ", rhs_fixed, rhs_random)
  }

  model_info_ui <- function(extra_style = "") {
    tryCatch({
      formula_str <- make_model_formula_str()
      method   <- if (!is.null(input$test_method)) input$test_method else "LMM"
      n_fac    <- sum(!is.na(c(col_factorA_r(), col_factorB_r(), col_factorC_r())))
      div(
        style = paste0(
          "background:#e8f4f8; border-left:4px solid #2471a3; ",
          "padding:7px 12px; margin-bottom:8px; font-size:12px; ", extra_style
        ),
        tags$b("當前分析模型："),
        tags$code(formula_str),
        tags$span(
          style = "margin-left:12px; color:#555;",
          paste0("方法：", method, " ｜ ", n_fac, " 因子")
        )
      )
    }, error = function(e) NULL)
  }

  output$current_model_ui <- renderUI({ model_info_ui() })
  output$current_model_banner_ui <- renderUI({ model_info_ui() })

  # ★ η² + R²m/R²c/ΔR² 合併輸出
  output$preview_eta2_table <- renderTable({
    res  <- single_preview_engine()
    eta2 <- res$eta2
    r2   <- res$r2

    eta2_label_fn <- function(v) {
      if (is.na(v)) return("—")
      if (v >= 0.14) return("大 (≥.14)")
      if (v >= 0.06) return("中 (.06–.14)")
      if (v >= 0.01) return("小 (.01–.06)")
      return("極小 (<.01)")
    }
    fmt <- function(x) if (is.na(x)) "—" else sprintf("%.3f", x)

    if (is.null(res) || is.null(res$eta2)) return(NULL)
    eta2_type <- if (isTRUE(res$type == "LMM")) "partial η²" else "η²"

    # ── 上半：因子效應量（partial η²），動態支援 2/3 因子 ──
    fC <- fc_label()
    has_fc_prev <- !is.null(col_factorC_r()) && isTRUE(!is.na(col_factorC_r()))
    
    eta2_rows_idx <- c(
      paste0(fa_label(), "  [", eta2_type, "]"),
      paste0(fb_label(), "  [", eta2_type, "]"),
      paste0(fa_label(), " × ", fb_label(), "  [", eta2_type, "]")
    )
    eta2_rows_val <- c(
      fmt(eta2$eta2_1 %||% NA),
      fmt(eta2$eta2_2 %||% NA),
      fmt(eta2$eta2_inter_AB %||% NA)
    )
    eta2_rows_interp <- c(
      eta2_label_fn(eta2$eta2_1 %||% NA),
      eta2_label_fn(eta2$eta2_2 %||% NA),
      eta2_label_fn(eta2$eta2_inter_AB %||% NA)
    )
    
    if (has_fc_prev) {
      eta2_rows_idx <- c(eta2_rows_idx,
        paste0(fC, "  [", eta2_type, "]"),
        paste0(fa_label(), " × ", fC, "  [", eta2_type, "]"),
        paste0(fb_label(), " × ", fC, "  [", eta2_type, "]"),
        paste0(fa_label(), " × ", fb_label(), " × ", fC, "  [", eta2_type, "]")
      )
      eta2_rows_val <- c(eta2_rows_val,
        fmt(eta2$eta2_3 %||% NA),
        fmt(eta2$eta2_inter_AC %||% NA),
        fmt(eta2$eta2_inter_BC %||% NA),
        fmt(eta2$eta2_inter_ABC %||% NA)
      )
      eta2_rows_interp <- c(eta2_rows_interp,
        eta2_label_fn(eta2$eta2_3 %||% NA),
        eta2_label_fn(eta2$eta2_inter_AC %||% NA),
        eta2_label_fn(eta2$eta2_inter_BC %||% NA),
        eta2_label_fn(eta2$eta2_inter_ABC %||% NA)
      )
    }
    
    eta2_df <- data.frame(
      指標 = eta2_rows_idx,
      值   = eta2_rows_val,
      詮釋 = eta2_rows_interp,
      stringsAsFactors = FALSE
    )

    # ── 下半：模型解釋力（LMM 限定）──
    r2_note <- if (res$type == "LMM") {
      delta_interp <- if (!is.na(r2$delta_r2)) {
        if (r2$delta_r2 >= 0.15) "區集效應大（RCBD必要）"
        else if (r2$delta_r2 >= 0.05) "區集效應中等"
        else "區集效應小"
      } else "—"
      data.frame(
        指標 = c("R²m  [固定效應]",
                 "R²c  [固定+隨機]",
                 "ΔR² = R²c − R²m  [隨機效應貢獻]"),
        值   = c(fmt(r2$r2m), fmt(r2$r2c), fmt(r2$delta_r2)),
        詮釋 = c(paste0(fa_label(), ", ", fb_label(), ", 交互解釋比例"),
                 "含區集/重複隨機效應合計",
                 delta_interp),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        指標 = c("R²m", "R²c", "ΔR²"),
        值   = c("—", "—", "—"),
        詮釋 = c("僅 LMM 適用", "僅 LMM 適用", "僅 LMM 適用"),
        stringsAsFactors = FALSE
      )
    }

    rbind(eta2_df, r2_note)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$preview_contrast_res <- renderPrint({ single_preview_engine()$contrast })

  output$corr_plot <- renderPlot({
    res <- corr_engine(); req(res)
    size_factor <- input$plot_text_size / 14
    par(xpd = TRUE)
    title_str <- paste0("Correlation (", toupper(res$method), ") — ",
                        switch(input$corr_n_mode,
                               "all"    = "全部參數",
                               "high_n" = paste0("n ≥ ", input$corr_n_threshold, " Rep/組"),
                               "low_n"  = paste0("n < ", input$corr_n_threshold, " Rep/組")))
    corrplot(res$cor, method = "color", type = "upper", tl.col = "black", tl.cex = size_factor,
             tl.pos = "lt", cl.cex = size_factor, title = title_str, mar = c(2, 0.5, 4, 3))
    n_c <- ncol(res$cor)
    for(i in 1:(n_c-1)) {
      for(j in (i+1):n_c) {
        text(x = j, y = n_c - i + 1, labels = res$star[i, j],
             cex = size_factor * 0.85, col = "black", adj = 0.5)
      }
    }
  })

  output$corr_detail_table <- renderTable({
    req(corr_engine())
    as.data.frame(corr_engine()$star) %>% rownames_to_column("參數")
  })

  # ★ 分布檢定 Tab 動態標題與說明
  output$dist_tab_header <- renderUI({
    # ★ 先計算 trfm_note，再傳入 switch（不可在 switch 內部定義變數）
    trfm_note <- if (!is.null(input$global_transform) && input$global_transform != "none") {
      paste0("【全域轉換：", input$global_transform, "】")
    } else {
      ""
    }
    desc <- switch(input$test_method,
      "LMM" = tagList(
        h4(paste("分布診斷（LMM 模型殘差）", trfm_note)),
        p(style = "color:#495057; font-size:13px;",
          "Shapiro-Wilk 檢定對象為 LMM 模型殘差（符合 LMM 假設前提）。",
          "Levene 檢定僅供各組變異程度參考，非 LMM 前提條件（標示 Ref only）。")
      ),
      "ANOVA" = tagList(
        h4(paste("分布檢定（原始資料）— ANOVA 前提確認", trfm_note)),
        p(style = "color:#495057; font-size:13px;",
          "Shapiro-Wilk 檢定資料常態性；Levene 檢定變異數齊性。",
          "兩者均通過（Pass）為 ANOVA 使用前提。")
      ),
      "Kruskal" = tagList(
        h4(paste("分布檢定（原始資料）— 非參數檢定依據", trfm_note)),
        p(style = "color:#495057; font-size:13px;",
          "Shapiro-Wilk 可作為選用 Kruskal-Wallis 的佐證依據（資料不符常態時適用非參數檢定）。")
      )
    )
    desc
  })

  output$full_stats_table  <- renderTable({ all_params_engine()$stats })
  output$full_means_table  <- renderTable({ all_params_engine()$means })
  output$full_dist_table   <- renderTable({ all_params_engine()$dist })

  # ★ Pairwise table output（使用基礎 R 索引，完全迴避欄位名稱衝突）
  output$pairwise_table <- renderTable({
    pw_raw <- all_params_engine()$pairwise
    validate(need(!is.null(pw_raw) && nrow(pw_raw) > 0,
                  "尚無 Pairwise 結果，請確認已上傳資料並選取至少一個參數。"))

    # 確認欄位名稱（偵錯用）
    comp_col <- if ("comparison" %in% names(pw_raw)) "comparison" else
                if ("contrast"   %in% names(pw_raw)) "contrast"   else
                names(pw_raw)[2]   # fallback: 第2欄

    # 篩選比較組合（基礎 R，不用 dplyr filter）
    if (!is.null(input$pw_filter_contrast) && input$pw_filter_contrast != "all") {
      sel     <- input$pw_filter_contrast
      parts   <- strsplit(sel, " - ", fixed = TRUE)[[1]]
      rev_sel <- paste(rev(parts), collapse = " - ")
      comp_vec <- as.character(pw_raw[[comp_col]])
      keep     <- grepl(sel, comp_vec, fixed = TRUE) |
                  grepl(rev_sel, comp_vec, fixed = TRUE)
      pw_raw   <- pw_raw[keep, , drop = FALSE]
    }

    # 篩選 P 值（基礎 R）
    if (isTRUE(input$pw_sig_only) && "p.value" %in% names(pw_raw)) {
      pw_raw <- pw_raw[pw_raw[["p.value"]] < 0.10, , drop = FALSE]
    }

    if (nrow(pw_raw) == 0) return(data.frame(Message = "No results match filter"))
    pw_raw
  }, digits = 4)

  # ── 單一參數預覽：長條圖 / 盒鬚圖 統一生成函數 ─────────────────────────
  generate_single_preview_plot <- function() {
    req(all_params_engine(), input$single_param, input$single_plot_type)
    param    <- input$single_param
    means_df <- all_params_engine()$means
    if (is.null(means_df) || nrow(means_df) == 0 || !("Parameter" %in% names(means_df)))
      return(NULL)
    m_data   <- means_df %>%
      filter(Parameter == param) %>%
      mutate(處理 = factor(as.character(處理), levels = treat_levels_r()))

    if (input$single_plot_type == "bar") {
      # ── 長條圖（與原有邏輯一致，加色板）──
      show_j_bar <- isTRUE(input$show_jitter_single)
      j_alpha_bar <- if (!is.null(input$jitter_alpha_single)) input$jitter_alpha_single else 0.50

      p <- ggplot(m_data, aes(x = 處理, y = Mean, fill = 處理)) +
        geom_bar(stat = "identity", color = "black", width = 0.62, alpha = 0.88) +
        geom_errorbar(aes(ymin = Mean, ymax = Mean + SE), width = 0.2, linewidth = 0.7)

      # ★ 三層數據視覺化（Layer 1 + Layer 2）
      layer_mode_s <- if (!is.null(input$layer_mode_single)) input$layer_mode_single else "none"
      show_rep_lbl_s <- isTRUE(input$show_rep_label_single)
      if (layer_mode_s != "none") {
        df_raw_s <- raw_df() %>% filter(重複 %in% input$selected_reps)
        p <- add_data_layers(
          p, df_raw_s, param,
          treat_levels   = treat_levels_r(),
          layer_mode     = layer_mode_s,
          j_alpha        = j_alpha_bar,
          show_rep_label = show_rep_lbl_s,
          treat_colors   = treat_colors_r(),
          text_size      = input$plot_text_size,
          darken         = if (!is.null(input$jitter_darken_single)) input$jitter_darken_single else 0.1
        )
      }

      p <- p +
        geom_text(aes(label = Letter, y = Mean + SE), vjust = -0.55,
                  fontface = "bold", size = input$plot_text_size / 4.5) +
        scale_fill_manual(values = treat_colors_r(), drop = FALSE, na.value = "grey70") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_classic(base_size = input$plot_text_size) +
        theme(legend.position = "none",
              axis.title.x  = element_blank(),
              plot.title    = element_text(face = "bold", hjust = 0.5,
                                           size = input$plot_text_size),
              axis.title.y  = element_text(size = input$plot_text_size * 0.88),
              plot.subtitle = element_text(size = input$plot_text_size * 0.75,
                                           color = "grey40")) +
        labs(title = get_label(param, lang = if (!is.null(input$label_lang_single)) input$label_lang_single else "zh")$title,
             y     = get_label(param, lang = if (!is.null(input$label_lang_single)) input$label_lang_single else "zh")$ylab)

    } else {
      # ── 盒鬚圖 / 琴形圖（比照長條圖，響應 use_mean_by_rep）──
      req(raw_df(), input$selected_reps)
      df_raw <- raw_df() %>% filter(重複 %in% input$selected_reps)

      rc_box <- get_rep_col(df_raw)
      df_box <- if (input$use_mean_by_rep && !is.null(rc_box)) {
        grp_box <- unique(c("處理", rc_box))
        grp_box <- intersect(grp_box, names(df_raw))
        df_raw %>%
          group_by(across(all_of(grp_box))) %>%
          summarise(Value = mean(.data[[param]], na.rm = TRUE), .groups = "drop") %>%
          filter(!is.na(Value)) %>%
          { if ("處理" %in% names(.)) mutate(., 處理 = factor(處理, levels = treat_levels_r())) else . }
      } else {
        df_raw %>%
          rename(Value = !!param) %>%
          filter(!is.na(Value)) %>%
          { if ("處理" %in% names(.)) mutate(., 處理 = factor(處理, levels = treat_levels_r())) else . }
      }

      if (nrow(df_box) < 2) return(NULL)

      n_per_group <- df_box %>% group_by(處理) %>% summarise(n=n()) %>% pull(n) %>% min()
      pt_size   <- if (input$use_mean_by_rep) 2.8 else 1.5
      pt_shape  <- if (input$use_mean_by_rep) 16 else 17
      subtitle_txt <- if (input$use_mean_by_rep) {
        paste0("繪圖單元：Rep 均值；n = ", n_per_group, " rep/組")
      } else {
        paste0("繪圖單元：個別植株原始值；n = ", n_per_group, " 株/組")
      }

      is_violin <- input$single_plot_type == "violin"

      if (is_violin && n_per_group < 10)
        subtitle_txt <- paste0(subtitle_txt, "  ⚠ n < 10，琴形圖核密度估計不穩定")

      if (is_violin) {
        p <- ggplot(df_box, aes(x = 處理, y = Value, fill = 處理)) +
          geom_violin(alpha = 0.55, trim = FALSE, color = "grey40") +
          geom_boxplot(width = 0.14, outlier.shape = NA,
                       fill = "white", alpha = 0.85, linewidth = 0.6, color = "grey30")
      } else {
        box_alpha <- if (n_per_group < 5) 0.32 else 0.62
        p <- ggplot(df_box, aes(x = 處理, y = Value, fill = 處理)) +
          geom_boxplot(outlier.shape = NA, alpha = box_alpha,
                       width = 0.52, linewidth = 0.65, color = "grey30")
      }

      # ★ 三層數據視覺化（盒鬚圖）
      layer_mode_sb <- if (!is.null(input$layer_mode_single)) input$layer_mode_single else "none"
      j_alpha_sb <- if (!is.null(input$jitter_alpha_single)) input$jitter_alpha_single else 0.20
      show_rep_lbl_sb <- isTRUE(input$show_rep_label_single)
      if (layer_mode_sb != "none") {
        p <- add_data_layers(
          p, df_raw, param,
          treat_levels   = treat_levels_r(),
          layer_mode     = layer_mode_sb,
          j_alpha        = j_alpha_sb,
          show_rep_label = show_rep_lbl_sb,
          treat_colors   = treat_colors_r(),
          text_size      = input$plot_text_size,
          darken         = if (!is.null(input$jitter_darken_single)) input$jitter_darken_single else 0.1
        )
      }



      if (nrow(m_data) > 0) {
        y_top   <- max(df_box$Value, na.rm = TRUE)
        y_range <- diff(range(df_box$Value, na.rm = TRUE))
        p <- p + geom_text(data = m_data,
                           aes(x = 處理, y = y_top + y_range * 0.12, label = Letter),
                           fontface = "bold", size = input$plot_text_size / 4.5,
                           inherit.aes = FALSE)
      }

      p <- p +
        scale_fill_manual(values = treat_colors_r(), drop = FALSE, na.value = "grey70") +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.22))) +
        theme_classic(base_size = input$plot_text_size) +
        theme(legend.position = "none",
              axis.title.x  = element_blank(),
              plot.title    = element_text(face = "bold", hjust = 0.5,
                                           size = input$plot_text_size),
              axis.title.y  = element_text(size = input$plot_text_size * 0.88),
              plot.subtitle = element_text(size = input$plot_text_size * 0.75,
                                           color = "grey40")) +
        labs(title    = get_label(param, lang = if (!is.null(input$label_lang_single)) input$label_lang_single else "zh")$title,
             subtitle = subtitle_txt,
             y        = get_label(param, lang = if (!is.null(input$label_lang_single)) input$label_lang_single else "zh")$ylab)
    }
    return(p)
  }

  # 預覽輸出（單一 renderPlot 同時服務長條/盒鬚）
  output$single_preview_plot <- renderPlot({
    p <- generate_single_preview_plot()
    req(p); p
  })

  # 保留 bar_plot 供舊版相容（若有其他地方引用）
  output$bar_plot <- renderPlot({
    req(all_params_engine(), input$single_param)
    means_df3 <- all_params_engine()$means
    if (is.null(means_df3) || nrow(means_df3) == 0 || !("Parameter" %in% names(means_df3))) return(NULL)
    m_data <- means_df3 %>% filter(Parameter == input$single_param)
    ggplot(m_data, aes(處理, Mean, fill = 處理)) +
      geom_bar(stat = "identity", color = "black", width = 0.6) +
      geom_errorbar(aes(ymin = Mean, ymax = Mean + SE), width = 0.2) +
      geom_text(aes(label = Letter, y = Mean + SE), vjust = -0.5, fontface = "bold") +
      scale_fill_manual(values = treat_colors_r(), drop = FALSE, na.value = "grey70") +
      theme_classic(base_size = input$plot_text_size) +
      labs(title = get_label(input$single_param, lang = input$label_lang)$title,
           y     = get_label(input$single_param, lang = input$label_lang)$ylab)
  })

  generate_pca_plot <- function() {
    res <- multivariate_engine(); req(res)
    pca      <- res$pca
    meta     <- res$meta
    pca_data <- as.data.frame(pca$x)
    pca_data$處理 <- meta$處理
    var_exp  <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    loadings <- as.data.frame(pca$rotation); loadings$var <- rownames(loadings)
    mult     <- min(max(abs(pca_data$PC1))/max(abs(loadings$PC1)),
                    max(abs(pca_data$PC2))/max(abs(loadings$PC2))) * 0.7
    group_counts    <- pca_data %>% group_by(處理) %>% summarise(n = n(), .groups = "drop")
    can_draw_ellipse <- all(group_counts$n >= 4)

    p <- ggplot(pca_data, aes(PC1, PC2, color = 處理, fill = 處理))
    if(can_draw_ellipse) { p <- p + stat_ellipse(geom = "polygon", alpha = 0.1, show.legend = FALSE) }
    p <- p + geom_point(size = 4, alpha = 0.8) +
      geom_segment(data = loadings, aes(x=0, y=0, xend=PC1*mult, yend=PC2*mult),
                   arrow = arrow(length = unit(0.2, "cm")), color = "grey40", inherit.aes = FALSE) +
      geom_text(data = loadings, aes(x=PC1*mult*1.1, y=PC2*mult*1.1, label=var),
                color = "black", size = 4, inherit.aes = FALSE) +
      theme_bw(base_size = input$plot_text_size) +
      labs(x = paste0("PC1 (", var_exp[1], "%)"), y = paste0("PC2 (", var_exp[2], "%)"),
           title = if(!can_draw_ellipse) "PCA Biplot (樣本數過少不繪橢圓)" else "PCA Biplot")
    return(p)
  }

  output$mv_n_info <- renderPrint({
    req(raw_df(), input$selected_params, input$selected_reps)
    n_vec  <- param_n_vec()
    thr_mv <- input$mv_n_threshold   # 與每組平均有效 Rep 數比較
    sel <- switch(input$mv_n_mode,
      "all"    = input$selected_params,
      "high_n" = names(n_vec[n_vec >= thr_mv]),
      "low_n"  = names(n_vec[n_vec <  thr_mv])
    )
    unit_label <- if (isTRUE(input$use_mean_by_rep)) "Rep 均值" else "個別觀測（缺失率≤50%）"
    cat(sprintf("分析模式：%s（n 門檻 = %d Rep/組）
繪圖單元：%s
納入參數數：%d
",
                input$mv_n_mode, input$mv_n_threshold, unit_label, length(sel)))
    if (length(sel) > 0) cat("納入參數：", paste(sel, collapse = ", "), "
")
  })

  output$pca_plot              <- renderPlot({ generate_pca_plot() })
  output$pca_contribution_table <- renderTable({ multivariate_engine()$contrib })
  output$adonis_res            <- renderPrint({
    res <- multivariate_engine()$adonis
    if(is.null(res)) "資料不足，無法計算" else res
  })
  output$pairwise_adonis_table <- renderTable({ multivariate_engine()$pw })
  output$data_preview          <- renderTable({ head(raw_df(), 10) })

  # ── 資料轉換函數 ────────────────────────────────────────────────────────────
  # 取得某參數的有效轉換方法（個別覆蓋 > 全域預設）
  get_transform <- function(param) {
    input_id <- paste0("transform_", gsub("[^a-zA-Z0-9]", "_", param))
    indiv <- tryCatch(input[[input_id]], error = function(e) NULL)
    if (!is.null(indiv) && indiv != "global") return(indiv)
    if (!is.null(input$global_transform)) return(input$global_transform)
    return("none")
  }

  # 套用轉換到數值向量
  apply_transform <- function(x, method) {
    switch(method,
      "log1p"  = log(x + 1),
      "sqrt"   = sqrt(pmax(x, 0)),
      "arcsin" = asin(sqrt(pmin(pmax(x / max(x, na.rm = TRUE), 0), 1))),
      x   # none
    )
  }

  # 個別參數轉換 UI
  output$param_transform_ui <- renderUI({
    req(input$selected_params)
    choices_t <- c("跟隨全域" = "global",
                   "無轉換"   = "none",
                   "log(x+1)" = "log1p",
                   "√x"       = "sqrt",
                   "arcsin(√x)" = "arcsin")
    lapply(input$selected_params, function(p) {
      input_id <- paste0("transform_", gsub("[^a-zA-Z0-9]", "_", p))
      fluidRow(
        column(5, p(style = "font-size:11px; margin:4px 0;",
                    get_label(p, lang = "zh")$title)),
        column(7, selectInput(input_id, NULL, choices = choices_t,
                              selected = "global", width = "100%"))
      )
    })
  })

  # ── 進階圖表 A/B/C 獨立參數選擇器 ──────────────────────────────────────
  adv_param_choices <- reactive({
    req(raw_df(), input$selected_params)
    unname(input$selected_params)
  })

  output$adv_param_strip_ui <- renderUI({
    req(adv_param_choices())
    selectInput("adv_param_strip", "製圖參數（A）：",
                choices = adv_param_choices(),
                selected = isolate(input$adv_param_strip) %||%
                           (if ("Number.of.Root.Tips" %in% adv_param_choices())
                              "Number.of.Root.Tips" else adv_param_choices()[1]))
  })

  output$adv_param_paired_ui <- renderUI({
    req(adv_param_choices())
    selectInput("adv_param_paired", "製圖參數（B）：",
                choices = adv_param_choices(),
                selected = isolate(input$adv_param_paired) %||%
                           (if ("Number.of.Root.Tips" %in% adv_param_choices())
                              "Number.of.Root.Tips" else adv_param_choices()[1]))
  })

  output$adv_param_inter_ui <- renderUI({
    req(adv_param_choices())
    selectInput("adv_param_inter", "製圖參數（C）：",
                choices = adv_param_choices(),
                selected = isolate(input$adv_param_inter) %||%
                           (if ("FvFm" %in% adv_param_choices())
                              "FvFm" else adv_param_choices()[1]))
  })

  # ── 散點圖 X/Y 參數選擇器 ───────────────────────────────────────────────
  output$scatter_x_ui <- renderUI({
    req(raw_df())
    num_cols <- unname(setdiff(
      colnames(raw_df() %>% dplyr::select(where(is.numeric))),
      c("流水號", "區集")
    ))
    selectInput("scatter_x", "X 軸參數：", choices = num_cols,
                selected = if ("Number.of.Root.Tips" %in% num_cols) "Number.of.Root.Tips" else num_cols[1])
  })

  output$scatter_y_ui <- renderUI({
    req(raw_df())
    num_cols <- unname(setdiff(
      colnames(raw_df() %>% dplyr::select(where(is.numeric))),
      c("流水號", "區集")
    ))
    selectInput("scatter_y", "Y 軸參數：", choices = num_cols,
                selected = if ("全株鮮重..g." %in% num_cols) "全株鮮重..g." else num_cols[2])
  })

  # ── 散點圖生成函數 ────────────────────────────────────────────────────────
  generate_scatter_plot <- function() {
    req(raw_df(), input$scatter_x, input$scatter_y, input$selected_reps)
    px <- input$scatter_x
    py <- input$scatter_y
    df_raw <- raw_df() %>% filter(重複 %in% input$selected_reps)

    # 依 scatter_unit 決定繪圖單元
    rep_s <- intersect(c("重複","Rep","rep"), names(df_raw))[1]
    trt_s <- if ("處理" %in% names(df_raw)) "處理" else {
      fc_s <- na.omit(c(col_factorA_r(), col_factorB_r()))
      fc_s <- fc_s[fc_s %in% names(df_raw)]
      if (length(fc_s) > 0) fc_s[1] else names(df_raw)[1]
    }
    if (isTRUE(input$scatter_unit == "rep") && !is.na(rep_s)) {
      df_plot <- df_raw %>%
        group_by(across(all_of(intersect(c(trt_s, rep_s), names(.))))) %>%
        summarise(X = mean(.data[[px]], na.rm = TRUE),
                  Y = mean(.data[[py]], na.rm = TRUE),
                  .groups = "drop") %>%
        filter(!is.na(X) & !is.na(Y) & is.finite(X) & is.finite(Y))
    } else {
      df_plot <- df_raw %>%
        rename(X = !!px, Y = !!py) %>%
        filter(!is.na(X) & !is.na(Y) & is.finite(X) & is.finite(Y))
    }
    # 確保有「處理」欄
    if (!"處理" %in% names(df_plot) && "處理" %in% names(df_raw))
      df_plot <- left_join(df_plot, distinct(df_raw[, unique(c(trt_s, "處理"))]), by=trt_s, relationship="many-to-many")
    if ("處理" %in% names(df_plot))
      df_plot <- df_plot %>% mutate(處理 = factor(處理, levels = treat_levels_r()))

    if (nrow(df_plot) < 4) return(NULL)

    # Pearson r（整體）
    r_val  <- tryCatch(
      cor(df_plot$X, df_plot$Y, use = "complete.obs", method = "pearson"),
      error = function(e) NA_real_)
    n_obs  <- nrow(df_plot)
    if (is.na(r_val) || !is.finite(r_val)) {
      r_label <- paste0("r = NA（變異數為零或資料不足，n = ", n_obs, "）")
    } else {
      t_stat <- r_val * sqrt(n_obs - 2) / sqrt(1 - r_val^2)
      p_val  <- 2 * pt(abs(t_stat), df = n_obs - 2, lower.tail = FALSE)
      sig    <- if (isTRUE(p_val < 0.001)) "***" else if (isTRUE(p_val < 0.01)) "**" else
                if (isTRUE(p_val < 0.05)) "*"   else if (isTRUE(p_val < 0.10))  "†"  else "ns"
      r_label <- sprintf("r = %.2f%s (P = %.3f, n = %d)", r_val, sig, p_val, n_obs)
    }

    sc_lang <- if(!is.null(input$adv_lang_scatter)) input$adv_lang_scatter else "zh"
    lbl_x <- get_label(px, lang = sc_lang)
    lbl_y <- get_label(py, lang = sc_lang)

    p <- ggplot(df_plot, aes(x = X, y = Y))

    # 整體迴歸線
    if (isTRUE(input$scatter_show_overall)) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "grey40",
                           fill = "grey80", alpha = 0.25, linewidth = 1,
                           inherit.aes = TRUE)
    }

    # 著色與各組迴歸線
    if (isTRUE(input$scatter_color_group)) {
      p <- p + geom_point(aes(color = 處理, shape = 處理),
                          size = 3.2, alpha = 0.85)
      if (isTRUE(input$scatter_show_reg)) {
        p <- p + geom_smooth(aes(color = 處理, group = 處理),
                             method = "lm", se = FALSE,
                             linewidth = 0.8, linetype = "dashed")
      }
      p <- p + scale_color_manual(values = treat_colors_r(), drop = FALSE, na.value = "grey70") +
               scale_shape_manual(values = setNames(
                 c(16,17,15,18,8,9,10,11)[seq_len(length(treat_levels_r()))],
                 treat_levels_r()))
    } else {
      p <- p + geom_point(size = 3, alpha = 0.8, color = "grey30")
    }

    # Rep 標籤
    if (isTRUE(input$scatter_show_label) && "重複" %in% colnames(df_plot)) {
      p <- p + ggrepel::geom_text_repel(aes(label = paste0(處理,".",重複)),
                                         size = 2.8, max.overlaps = 15)
    }

    p <- p +
      annotate("text", x = Inf, y = -Inf, label = r_label,
               hjust = 1.05, vjust = -0.5,
               size = input$plot_text_size / 4,
               fontface = "italic", color = "grey30") +
      theme_classic(base_size = input$plot_text_size) +
      theme(legend.position = "right") +
      labs(title    = paste0(lbl_x$title, " × ", lbl_y$title),
           subtitle = if (input$scatter_unit == "rep") "Plotting unit: Rep mean" else "Plotting unit: individual plants",
           x        = lbl_x$ylab,
           y        = lbl_y$ylab,
           color    = "Treatment",
           shape    = "Treatment")
    return(p)
  }

  output$scatter_plot <- renderPlot({ generate_scatter_plot() })

  output$download_scatter <- downloadHandler(
    filename = function() {
      paste0(input$scatter_x, "_x_", input$scatter_y, "_scatter_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- generate_scatter_plot(); req(p)
      ggsave(file, plot = p, device = "png", width = 7, height = 6, dpi = input$plot_dpi)
    }
  )

  # ── 圖表輔助：取得單一參數的繪圖用原始資料 ──────────────────────────────
  # ★ 進階圖表專用資料函數：固定按 Rep 取均值，不受 use_mean_by_rep 影響
  # 理由：圖表以「Rep」為實驗單元，不取均值會導致每Rep 9株原始點混入，失去折線意義
  get_plot_raw <- function(param) {
    req(raw_df(), input$selected_reps)
    rep_col_p <- intersect(c("重複","Rep","rep"), names(raw_df()))[1]
    df_raw <- raw_df() %>% filter(.data[[rep_col_p]] %in% input$selected_reps)
    # 分組欄：直接用 col_factorA/B/C_r()（不猜測）+ 重複欄
    fa_p  <- col_factorA_r(); fb_p <- col_factorB_r(); fc_p <- col_factorC_r()
    factor_grp_p <- na.omit(c(
      if (isTRUE(fa_p %in% names(df_raw))) fa_p else NULL,
      if (isTRUE(fb_p %in% names(df_raw))) fb_p else NULL,
      if (isTRUE(!is.null(fc_p) && !is.na(fc_p) && fc_p %in% names(df_raw))) fc_p else NULL
    ))
    rep_grp_p <- intersect(c("重複","Rep","rep"), names(df_raw))[1]
    grp_p <- unique(c(factor_grp_p, rep_grp_p))
    tmp <- df_raw %>%
      group_by(across(all_of(grp_p))) %>%
      summarise(Value = mean(.data[[param]], na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(Value))
    # 補回「處理」欄
    if (!"處理" %in% names(tmp) && "處理" %in% names(df_raw))
      tmp <- left_join(tmp, distinct(df_raw[, unique(c(factor_grp_p, "處理"))]),
                       by = factor_grp_p, relationship = "many-to-many")
    if ("處理" %in% names(tmp))
      tmp <- tmp %>% mutate(處理 = factor(處理, levels = treat_levels_r()))
    tmp
  }

  # ── 色板（4 處理固定色）──────────────────────────────────────────────────
  # treat_colors：依處理水準動態分配顏色
  treat_colors_base <- c("#2D6A4F","#52B788","#C0392B","#E07B54",
                          "#4A90D9","#F4A261","#6A4C93","#80B918")
  treat_colors_r <- reactive({
    lvs <- treat_levels_r()
    if (length(lvs) == 0) {
      setNames(c("#2D6A4F","#52B788","#C0392B","#E07B54",
                 "#4A90D9","#F39C12","#9B59B6","#1ABC9C")[seq_len(min(4,8))],
               c("Group1","Group2","Group3","Group4")[seq_len(4)])
    } else {
      n   <- length(lvs)
      cols <- treat_colors_base[seq_len(min(n, length(treat_colors_base)))]
      if (n > length(treat_colors_base))
        cols <- c(cols, grDevices::rainbow(n - length(treat_colors_base)))
      setNames(cols, lvs)
    }
  })
  # treat_colors：動態依處理水準（make_long_plot 傳入）
  # 這裡保留空向量，實際顏色在 make_long_plot 內動態計算
  treat_colors <- character(0)

  # ── A. 點圖疊加（Strip plot）────────────────────────────────────────────
  generate_strip_plot <- function() {
    p_strip <- if (!is.null(input$adv_param_strip) && nchar(input$adv_param_strip)>0)
                 input$adv_param_strip else if (!is.null(input$single_param)) input$single_param else return(NULL)
    req(p_strip, all_params_engine())
    df_pt  <- get_plot_raw(p_strip)
    means_s <- all_params_engine()$means
    if (is.null(means_s) || nrow(means_s)==0 || !("Parameter" %in% names(means_s))) return(NULL)
    m_data <- means_s %>%
      filter(Parameter == p_strip) %>%
      mutate(處理 = factor(處理, levels = treat_levels_r()))
    if (nrow(df_pt) == 0 || nrow(m_data) == 0) return(NULL)

    p_s   <- p_strip
    y_max <- max(m_data$Mean + m_data$SE, na.rm = TRUE)
    y_pad <- y_max * 0.15

    ggplot(m_data, aes(x = 處理, color = 處理)) +
      # SE errorbar
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                    width = 0.18, linewidth = 0.9) +
      # 均值橫線
      geom_crossbar(aes(y = Mean, ymin = Mean, ymax = Mean),
                    width = 0.38, linewidth = 0.9, fatten = 0) +
      # 個別數據點
      geom_jitter(data = df_pt, aes(y = Value),
                  width = 0.08, size = 3.2, alpha = 0.75, shape = 16) +
      # 顯著字母
      geom_text(aes(y = Mean + SE + y_pad * 0.4, label = Letter),
                fontface = "bold", size = input$plot_text_size / 3.5,
                show.legend = FALSE) +
      scale_color_manual(values = treat_colors_r(), drop = FALSE, na.value = "grey70") +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
      theme_classic(base_size = input$plot_text_size) +
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      labs(title    = get_label(p_strip, lang=if(!is.null(input$adv_lang_strip))input$adv_lang_strip else "zh")$title,
           subtitle = paste0("均值 ± SE；各點 = 1 Rep 均值，n = ",
                              round(nrow(df_pt) / length(treat_levels_r())), " rep/組"),
           y        = get_label(p_strip, lang=if(!is.null(input$adv_lang_strip))input$adv_lang_strip else "zh")$ylab)
  }

  # 成對比較組別 UI（動態）
  output$paired_compare_ui <- renderUI({
    lvs <- treat_levels_r()
    if (length(lvs) < 2) return(helpText("需至少 2 個處理水準"))
    # 產生所有兩兩組合
    pairs <- combn(lvs, 2, simplify = FALSE)
    choices <- setNames(
      sapply(pairs, function(p) paste(p, collapse = "___")),
      sapply(pairs, function(p) paste(p, collapse = " vs "))
    )
    selectInput("paired_compare", "比較組別：",
                choices = choices,
                selected = choices[1])
  })

  output$strip_plot <- renderPlot({ generate_strip_plot() })

  # ── B. 成對折線圖（Paired Line Plot）───────────────────────────────────
  generate_paired_plot <- function() {
    p_pair <- if (!is.null(input$adv_param_paired) && nchar(input$adv_param_paired)>0)
                input$adv_param_paired else if (!is.null(input$single_param)) input$single_param else return(NULL)
    req(p_pair, input$paired_compare)
    # 動態解析選擇的比較組合（用 "___" 分隔）
    grp_pair <- strsplit(input$paired_compare, "___")[[1]]
    if (length(grp_pair) != 2) return(NULL)
    df_pair <- get_plot_raw(p_pair) %>%
      filter(處理 %in% grp_pair) %>%
      mutate(處理 = factor(處理, levels = grp_pair))
    if (nrow(df_pair) < 2) return(NULL)

    # 群組均值
    df_mean <- df_pair %>%
      group_by(處理) %>%
      summarise(Mean = mean(Value, na.rm = TRUE),
                SE   = sd(Value, na.rm = TRUE) / sqrt(n()),
                .groups = "drop")

    # 每條 rep 的折線方向（提升/下降）
    rc_pair <- get_rep_col(df_pair) %||% "重複"
    df_wide <- df_pair %>%
      pivot_wider(names_from = 處理, values_from = Value,
                  id_cols = any_of(rc_pair))
    n_up   <- sum(df_wide[[grp_pair[2]]] > df_wide[[grp_pair[1]]], na.rm = TRUE)
    n_down <- sum(df_wide[[grp_pair[2]]] < df_wide[[grp_pair[1]]], na.rm = TRUE)
    subtitle_txt <- paste0(
      grp_pair[1], " → ", grp_pair[2],
      "：", n_up, " Rep 上升，", n_down, " Rep 下降"
    )

    rc_pair_aes <- get_rep_col(df_pair) %||% "重複"
    ggplot(df_pair, aes(x = 處理, y = Value)) +
      geom_line(aes(group = .data[[rc_pair_aes]], color = .data[[rc_pair_aes]]),
                alpha = 0.55, linewidth = 0.9, linetype = "solid") +
      geom_point(aes(color = .data[[rc_pair_aes]]), size = 3.2, alpha = 0.8) +
      # 均值粗線
      geom_line(data = df_mean, aes(x = 處理, y = Mean, group = 1),
                linewidth = 1.6, color = "black", linetype = "dashed",
                inherit.aes = FALSE) +
      geom_point(data = df_mean, aes(x = 處理, y = Mean),
                 size = 5, shape = 18, color = "black",
                 inherit.aes = FALSE) +
      geom_errorbar(data = df_mean,
                    aes(x = 處理, ymin = Mean - SE, ymax = Mean + SE),
                    width = 0.08, linewidth = 1.1, color = "black",
                    inherit.aes = FALSE) +
      theme_classic(base_size = input$plot_text_size) +
      theme(axis.title.x = element_blank()) +
      labs(title    = get_label(p_pair, lang=if(!is.null(input$adv_lang_paired))input$adv_lang_paired else "zh")$title,
           subtitle = subtitle_txt,
           y        = get_label(p_pair, lang=if(!is.null(input$adv_lang_paired))input$adv_lang_paired else "zh")$ylab,
           color    = rc_pair_aes) +
      scale_y_continuous(expand = expansion(mult = c(0.08, 0.12)))
  }

  output$paired_plot <- renderPlot({ generate_paired_plot() })

  # ── C. 雙因子交互作用圖（Interaction Plot）──────────────────────────────
  generate_interaction_plot <- function() {
    p_inter <- if (!is.null(input$adv_param_inter) && nchar(input$adv_param_inter)>0)
                 input$adv_param_inter else if (!is.null(input$single_param)) input$single_param else return(NULL)
    req(p_inter)
    df_raw_all <- get_plot_raw(p_inter)
    if (nrow(df_raw_all) == 0) return(NULL)

    # 動態取得 Factor A/B 欄位名
    fa_inter <- col_factorA_r()
    fb_inter <- col_factorB_r()
    fA_lbl   <- fa_label()
    fB_lbl   <- fb_label()
    inter_lang <- if (!is.null(input$adv_lang_inter)) input$adv_lang_inter else "zh"

    grp_inter <- na.omit(c(fa_inter, fb_inter))
    df_sum <- df_raw_all %>%
      group_by(across(all_of(intersect(grp_inter, names(.))))) %>%
      summarise(Mean = mean(Value, na.rm = TRUE),
                SE   = sd(Value,  na.rm = TRUE) / sqrt(n()),
                .groups = "drop") %>%
      mutate(across(all_of(intersect(grp_inter, names(.))),
                    ~factor(as.character(.))))

    df_raw_sum <- df_raw_all %>%
      mutate(across(all_of(intersect(grp_inter, names(.))),
                    ~factor(as.character(.))))

    # 動態顏色：依 Factor B 水準數分配
    fb_lvls  <- if (!is.null(fb_inter) && fb_inter %in% names(df_sum))
                  levels(factor(df_sum[[fb_inter]])) else character(0)
    n_mb     <- length(fb_lvls)
    mb_cols  <- treat_colors_base[seq_len(min(n_mb, length(treat_colors_base)))]
    mb_colors <- setNames(mb_cols, fb_lvls)

    subtitle_inter <- if (inter_lang == "en")
      paste0("Mean ± SE; crossing lines indicate ", fA_lbl, " × ", fB_lbl, " interaction")
    else
      paste0("均值 ± SE；交叉線代表 ", fA_lbl, " × ", fB_lbl, " 交互")

    p <- ggplot(df_sum, aes(x = .data[[fa_inter]], y = Mean,
                             color = .data[[fb_inter]], group = .data[[fb_inter]])) +
      { if (isTRUE(input$interaction_ribbon))
          geom_ribbon(aes(ymin = Mean - SE, ymax = Mean + SE, fill = .data[[fb_inter]]),
                      alpha = 0.12, color = NA)
        else list()
      } +
      # 個別 rep 點（淡）
      geom_jitter(data = df_raw_sum, aes(y = Value, color = .data[[fb_inter]]),
                  width = 0.04, size = 2.2, alpha = 0.35, shape = 16) +
      # 均值折線
      geom_line(linewidth = 1.4) +
      geom_point(size = 4.5, shape = 21, fill = "white", stroke = 1.8) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                    width = 0.08, linewidth = 0.9) +
      scale_color_manual(values = mb_colors) +
      scale_fill_manual(values  = mb_colors) +
      theme_classic(base_size = input$plot_text_size) +
      theme(legend.position = "right") +
      labs(title    = get_label(p_inter, lang=inter_lang)$title,
           subtitle = subtitle_inter,
           x        = fA_lbl,
           y        = get_label(p_inter, lang=inter_lang)$ylab,
           color    = fB_lbl,
           fill     = fB_lbl)
    return(p)
  }

  output$interaction_plot <- renderPlot({ generate_interaction_plot() })

  # ── 三因子交感圖控制 ──────────────────────────────────────────────────────

  # 是否選了3個因子（供 conditionalPanel 判斷）
  # has_three_factors：用 renderText 輸出 "true"/"false"
  # conditionalPanel 的 condition 用 output.has_three_factors === "true"
  output$has_three_factors <- renderText({
    tryCatch({
      fc <- col_factorC_r()
      if (is.null(fc) || isTRUE(is.na(fc)) || fc == "") return("false")
      df <- raw_df()
      if (is.null(df) || nrow(df) == 0) return("false")
      if (isTRUE(fc %in% names(df))) "true" else "false"
    }, error = function(e) "false")
  })
  outputOptions(output, "has_three_factors", suspendWhenHidden = FALSE)

  # E. 三因子交感圖參數選擇器
  output$adv_param_3way_ui <- renderUI({
    req(raw_df())
    num_cols <- names(raw_df())[sapply(raw_df(), is.numeric)]
    excl <- c("Rep","Block","Plant_ID","ID","SampleCode","重複","區集")
    params <- setdiff(num_cols, excl)
    def <- if (!is.null(input$adv_param_inter) && input$adv_param_inter %in% params)
             input$adv_param_inter else params[1]
    selectInput("adv_param_3way", "分析參數：", choices = params, selected = def)
  })

  # F. 3D 圖參數選擇器
  output$adv_param_3d_ui <- renderUI({
    req(raw_df())
    num_cols <- names(raw_df())[sapply(raw_df(), is.numeric)]
    excl <- c("Rep","Block","Plant_ID","ID","SampleCode","重複","區集")
    params <- setdiff(num_cols, excl)
    def <- if (!is.null(input$adv_param_3way) && input$adv_param_3way %in% params)
             input$adv_param_3way else params[1]
    selectInput("adv_param_3d", "分析參數：", choices = params, selected = def)
  })

  # F. 3D 圖 Z 軸因子選擇
  output$threeway_3d_zfactor_ui <- renderUI({
    fa <- col_factorA_r(); fb <- col_factorB_r(); fc <- col_factorC_r()
    choices <- stats::setNames(
      c(fa, fb, fc),
      c(fa_label(), fb_label(), fc_label())
    )
    selectInput("threeway_3d_zfactor", "Z 軸因子（分層）：",
                choices = choices, selected = fc)
  })

  # ══════════════════════════════════════════════════════════════════
  # E. 三因子分面交感圖（ggplot2，A×B 折線，C 分面）
  # ══════════════════════════════════════════════════════════════════
  generate_threeway_plot <- function() {
    req(input$adv_param_3way, raw_df())
    fa <- col_factorA_r(); fb <- col_factorB_r(); fc <- col_factorC_r()
    if (is.null(fa) || is.null(fb) || is.null(fc) || is.na(fc) || fc == "") return(NULL)

    df_raw_all <- get_plot_raw(input$adv_param_3way)
    if (is.null(df_raw_all) || nrow(df_raw_all) == 0) return(NULL)

    # 確認欄位存在
    needed <- c(fa, fb, fc)
    if (!all(needed %in% names(df_raw_all))) return(NULL)

    lang <- input$adv_lang_3way %||% "raw"
    get_lbl <- function(col) if (lang == "en") get_label(col, "en")$title else col

    # 計算各 A×B×C 組的均值和 SE
    df_sum <- df_raw_all %>%
      group_by(across(all_of(needed))) %>%
      summarise(Mean = mean(Value, na.rm = TRUE),
                SE   = sd(Value,  na.rm = TRUE) / sqrt(sum(!is.na(Value))),
                .groups = "drop") %>%
      mutate(across(all_of(needed), ~factor(as.character(.))))

    df_pts <- df_raw_all %>%
      mutate(across(all_of(needed), ~factor(as.character(.))))

    # 顏色：依 Factor B 水準
    fb_lvls  <- levels(factor(df_sum[[fb]]))
    n_mb     <- length(fb_lvls)
    mb_cols  <- RColorBrewer::brewer.pal(max(3, n_mb), "Set1")[seq_len(n_mb)]
    mb_colors <- setNames(mb_cols, fb_lvls)

    p <- ggplot(df_sum, aes(x = .data[[fa]], y = Mean,
                             color = .data[[fb]], group = .data[[fb]])) +
      { if (isTRUE(input$threeway_ribbon))
          geom_ribbon(aes(ymin = Mean - SE, ymax = Mean + SE,
                          fill = .data[[fb]]),
                      alpha = 0.12, color = NA)
        else list()
      } +
      { if (isTRUE(input$threeway_show_pts))
          geom_jitter(data = df_pts,
                      aes(y = Value, color = .data[[fb]]),
                      width = 0.05, size = 1.8, alpha = 0.4, shape = 16)
        else list()
      } +
      geom_line(linewidth = 1.3) +
      geom_point(size = 4, shape = 21, fill = "white", stroke = 1.6) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                    width = 0.07, linewidth = 0.8) +
      facet_wrap(vars(.data[[fc]]),
                 labeller = labeller(.default = function(x) paste0(get_lbl(fc), ": ", x)),
                 ncol = length(levels(factor(df_sum[[fc]])))) +
      scale_color_manual(values = mb_colors) +
      scale_fill_manual(values  = mb_colors) +
      theme_classic(base_size = input$plot_text_size %||% 13) +
      theme(legend.position = "right",
            strip.background = element_rect(fill = "#e8f4f8", color = "grey70"),
            strip.text = element_text(face = "bold", size = rel(1.0))) +
      labs(title    = get_label(input$adv_param_3way, lang = if(lang=="en") "en" else "zh")$title,
           subtitle = paste0("均值 ± SE；按 ", get_lbl(fc), " 分面"),
           x        = get_lbl(fa),
           y        = get_label(input$adv_param_3way, lang = if(lang=="en") "en" else "zh")$ylab,
           color    = get_lbl(fb),
           fill     = get_lbl(fb))
    return(p)
  }

  output$threeway_plot <- renderPlot({ generate_threeway_plot() })

  output$download_3way_plot <- downloadHandler(
    filename = function() paste0("3way_interaction_", input$adv_param_3way, ".png"),
    content  = function(file) {
      p <- generate_threeway_plot()
      req(p)
      ggsave(file, plot = p, width = 10, height = 5, dpi = 150)
    }
  )

  # ══════════════════════════════════════════════════════════════════
  # F. 三因子 plotly 互動 3D 圖
  # ══════════════════════════════════════════════════════════════════
  output$threeway_3d_plot <- plotly::renderPlotly({
    req(input$adv_param_3d, raw_df())
    fa <- col_factorA_r(); fb <- col_factorB_r(); fc <- col_factorC_r()
    if (is.null(fa) || is.null(fb) || is.null(fc) || is.na(fc) || fc == "") return(NULL)

    z_fac <- input$threeway_3d_zfactor %||% fc
    xy_facs <- setdiff(c(fa, fb, fc), z_fac)
    if (length(xy_facs) < 2) return(NULL)
    x_fac <- xy_facs[1]; y_fac <- xy_facs[2]

    df_raw_all <- get_plot_raw(input$adv_param_3d)
    if (is.null(df_raw_all) || nrow(df_raw_all) == 0) return(NULL)
    needed <- c(x_fac, y_fac, z_fac)
    if (!all(needed %in% names(df_raw_all))) return(NULL)

    # 計算各組均值
    df_3d <- df_raw_all %>%
      group_by(across(all_of(needed))) %>%
      summarise(Mean = mean(Value, na.rm = TRUE),
                SE   = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value))),
                .groups = "drop") %>%
      mutate(
        x_num = as.numeric(factor(.data[[x_fac]])),
        y_num = as.numeric(factor(.data[[y_fac]])),
        z_num = as.numeric(factor(.data[[z_fac]]))
      )

    # 建立 plotly 3D scatter + error bars
    z_lvls <- levels(factor(df_3d[[z_fac]]))
    pal    <- RColorBrewer::brewer.pal(max(3, length(z_lvls)), "Set1")[seq_along(z_lvls)]

    fig <- plotly::plot_ly()
    for (k in seq_along(z_lvls)) {
      sub <- df_3d %>% filter(.data[[z_fac]] == z_lvls[k])
      fig <- fig %>%
        plotly::add_trace(
          data = sub,
          x = ~x_num, y = ~y_num, z = ~Mean,
          type = "scatter3d", mode = "markers+lines",
          name = paste0(z_fac, ": ", z_lvls[k]),
          marker = list(size = 6, color = pal[k]),
          line   = list(width = 3, color = pal[k]),
          error_z = list(
            type = "data", array = sub$SE,
            visible = TRUE, color = pal[k], width = 4
          )
        )
    }

    x_lvls <- levels(factor(df_3d[[x_fac]]))
    y_lvls <- levels(factor(df_3d[[y_fac]]))

    fig %>%
      plotly::layout(
        title = paste0(input$adv_param_3d, " — 三因子互動圖"),
        scene = list(
          xaxis = list(title = x_fac,
                       tickvals = seq_along(x_lvls),
                       ticktext = x_lvls),
          yaxis = list(title = y_fac,
                       tickvals = seq_along(y_lvls),
                       ticktext = y_lvls),
          zaxis = list(title = input$adv_param_3d)
        ),
        legend = list(title = list(text = z_fac))
      )
  })

  # ── 多參數聯圖 UI ─────────────────────────────────────────────────────────

  # 獨立勾選框：所有數值參數
  output$multipanel_param_ui <- renderUI({
    req(raw_df())
    all_num_cols <- unname(setdiff(
      colnames(raw_df() %>% dplyr::select(where(is.numeric))),
      c("流水號", "區集")
    ))
    checkboxGroupInput("multipanel_params", NULL,
                       choices  = all_num_cols,
                       selected = isolate(input$multipanel_params) %||% unname(all_num_cols[1:6]))
  })

  # 主參數勾選區：全選 / 全消
  observeEvent(input$param_select_all, {
    req(raw_df())
    all_cols <- unname(setdiff(
      colnames(raw_df() %>% dplyr::select(where(is.numeric))),
      c("處理", "重複", "區集", "流水號")
    ))
    updateCheckboxGroupInput(session, "selected_params", selected = all_cols)
  })
  observeEvent(input$param_deselect_all, {
    updateCheckboxGroupInput(session, "selected_params", selected = character(0))
  })

  # 全選 / 全消按鈕
  observeEvent(input$multipanel_select_all, {
    req(raw_df())
    all_num_cols <- unname(setdiff(
      colnames(raw_df() %>% dplyr::select(where(is.numeric))),
      c("流水號", "區集")
    ))
    updateCheckboxGroupInput(session, "multipanel_params", selected = all_num_cols)
  })
  observeEvent(input$multipanel_deselect_all, {
    updateCheckboxGroupInput(session, "multipanel_params", selected = character(0))
  })

  # 聯圖標題（加入繪圖單元說明）
  output$multipanel_title <- renderText({
    type_label <- if(input$multipanel_type == "bar") "長條圖" else "盒鬚圖"
    unit_label <- if(isTRUE(input$use_mean_by_rep)) "Rep 均值" else "個別植株原始值"
    n_p <- length(input$multipanel_params)
    paste0("多參數", type_label, "聯圖（", n_p, " 個參數，",
           input$multipanel_ncol, " 欄 · 繪圖單元：", unit_label, "）")
  })

  # 動態預覽高度（依參數數量與欄數計算列數）
  output$multipanel_plot_ui <- renderUI({
    req(input$multipanel_params, input$multipanel_ncol)
    n_p    <- length(input$multipanel_params)
    n_rows <- ceiling(n_p / input$multipanel_ncol)
    # 預覽高度：每列 220px，最小 300px
    preview_h <- max(300, n_rows * 220)
    plotOutput("multipanel_plot", height = paste0(preview_h, "px"))
  })

  # ── 核心：生成單張圖（長條 or 盒鬚），響應 use_mean_by_rep ───────────────
  make_one_panel <- function(param, plot_type) {
    req(raw_df(), input$selected_reps)
    df_raw <- raw_df() %>% filter(重複 %in% input$selected_reps)

    # ★ 比照主分析與單一參數預覽：依 use_mean_by_rep 決定繪圖單元
    df_plot <- if (input$use_mean_by_rep) {
      df_raw %>%
        group_by(處理, 重複) %>%
        summarise(Value = mean(.data[[param]], na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(Value)) %>%
        mutate(處理 = factor(處理, levels = treat_levels_r()))
    } else {
      df_raw %>%
        rename(Value = !!param) %>%
        filter(!is.na(Value)) %>%
        mutate(處理 = factor(處理, levels = treat_levels_r()))
    }

    if (nrow(df_plot) < 2) return(NULL)

    # 均值 ± SE（長條圖 / 字母標記共用）
    df_sum <- df_plot %>%
      group_by(處理) %>%
      summarise(Mean = mean(Value), SE = sd(Value)/sqrt(n()), .groups = "drop")

    # 從 all_params_engine 取字母標記
    m_data <- tryCatch(
      { mdf <- all_params_engine()$means
        if (!is.null(mdf) && "Parameter" %in% names(mdf)) mdf %>% filter(Parameter == param) else NULL },
      error = function(e) NULL
    )

    if (plot_type == "bar") {
      # ── 長條圖 ──
      p <- ggplot(df_sum, aes(x = 處理, y = Mean, fill = 處理)) +
        geom_bar(stat = "identity", color = "black", width = 0.62, alpha = 0.85) +
        geom_errorbar(aes(ymin = Mean, ymax = Mean + SE), width = 0.2, linewidth = 0.7)

      # 字母標記（只 join Letter 欄，避免 Mean.x/Mean.y 衝突）
      if (!is.null(m_data) && nrow(m_data) > 0) {
        letter_df <- df_sum %>%
          mutate(處理 = factor(處理, levels = treat_levels_r())) %>%
          left_join(
            m_data %>%
              mutate(處理 = factor(處理, levels = treat_levels_r())) %>%
              dplyr::select(處理, Letter),
            by = "處理"
          )
        p <- p + geom_text(data = letter_df,
                           aes(x = 處理, y = Mean + SE, label = Letter),
                           vjust = -0.5, fontface = "bold",
                           size = input$plot_text_size / 4.5,
                           inherit.aes = FALSE)
      }

      # ★ 三層數據視覺化（多參數聯圖長條圖）
      # 永遠用個株原始資料（不受 use_mean_by_rep 影響）
      layer_mode_mb  <- if (!is.null(input$layer_mode_multi)) input$layer_mode_multi else "none"
      j_alpha_bar_m  <- if (!is.null(input$jitter_alpha_multi)) input$jitter_alpha_multi else 0.20
      show_rep_lbl_m <- isTRUE(input$show_rep_label_multi)
      if (layer_mode_mb != "none") {
        df_raw_ind <- raw_df() %>%
          filter(重複 %in% input$selected_reps) %>%
          mutate(處理 = factor(處理, levels = treat_levels_r()))
        p <- add_data_layers(
          p, df_raw_ind, param,
          treat_levels   = treat_levels_r(),
          layer_mode     = layer_mode_mb,
          j_alpha        = j_alpha_bar_m,
          show_rep_label = show_rep_lbl_m,
          treat_colors   = treat_colors_r(),
          text_size      = input$plot_text_size * 0.78,
          darken         = if (!is.null(input$jitter_darken_multi)) input$jitter_darken_multi else 0.1
        )
      }

      p <- p +
        scale_fill_manual(values = treat_colors_r(), drop = FALSE, na.value = "grey70") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_classic(base_size = input$plot_text_size * 0.78) +
        theme(legend.position = "none",
              axis.title.x    = element_blank(),
              plot.title       = element_text(size = input$plot_text_size * 0.85,
                                              face = "bold", hjust = 0.5)) +
        labs(title = get_label(param, lang = input$label_lang)$title,
             y     = get_label(param, lang = input$label_lang)$ylab)

    } else {
      # ── 盒鬚圖 / 琴形圖（繪圖單元同長條圖，響應 show_jitter/alpha）──
      n_per_group <- df_plot %>%
        group_by(處理) %>% summarise(n = n()) %>% pull(n) %>% min()
      pt_size  <- if (input$use_mean_by_rep) 2.2 else 1.5
      pt_shape <- if (input$use_mean_by_rep) 16 else 17

      is_violin <- plot_type == "violin"

      if (is_violin) {
        p <- ggplot(df_plot, aes(x = 處理, y = Value, fill = 處理)) +
          geom_violin(alpha = 0.52, trim = FALSE, color = "grey40") +
          geom_boxplot(width = 0.12, outlier.shape = NA,
                       fill = "white", alpha = 0.85, linewidth = 0.55,
                       color = "grey30")
      } else {
        box_alpha <- if (n_per_group < 5) 0.35 else 0.65
        p <- ggplot(df_plot, aes(x = 處理, y = Value, fill = 處理)) +
          geom_boxplot(outlier.shape = NA, alpha = box_alpha, width = 0.52,
                       linewidth = 0.6, color = "grey30")
      }

      # ★ 三層數據視覺化（多參數聯圖盒鬚圖）
      # 永遠用個株原始資料
      layer_mode_mb2  <- if (!is.null(input$layer_mode_multi)) input$layer_mode_multi else "none"
      j_alpha_m       <- if (!is.null(input$jitter_alpha_multi)) input$jitter_alpha_multi else 0.20
      show_rep_lbl_m2 <- isTRUE(input$show_rep_label_multi)
      if (layer_mode_mb2 != "none") {
        df_raw_ind2 <- raw_df() %>%
          filter(重複 %in% input$selected_reps) %>%
          mutate(處理 = factor(處理, levels = treat_levels_r()))
        p <- add_data_layers(
          p, df_raw_ind2, param,
          treat_levels   = treat_levels_r(),
          layer_mode     = layer_mode_mb2,
          j_alpha        = j_alpha_m,
          show_rep_label = show_rep_lbl_m2,
          treat_colors   = treat_colors_r(),
          text_size      = input$plot_text_size * 0.78,
          darken         = if (!is.null(input$jitter_darken_multi)) input$jitter_darken_multi else 0.1
        )
      }

      # n<5 / violin n<10 標注


      # 字母標記
      if (!is.null(m_data) && nrow(m_data) > 0) {
        y_top   <- max(df_plot$Value, na.rm = TRUE)
        y_range <- diff(range(df_plot$Value, na.rm = TRUE))
        m_data3 <- m_data %>%
          mutate(處理 = factor(處理, levels = treat_levels_r()))
        p <- p + geom_text(data = m_data3,
                           aes(x = 處理, y = y_top + y_range * 0.1, label = Letter),
                           fontface = "bold",
                           size = input$plot_text_size / 4.5,
                           inherit.aes = FALSE)
      }
      p <- p +
        scale_fill_manual(values = treat_colors_r(), drop = FALSE, na.value = "grey70") +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.22))) +
        theme_classic(base_size = input$plot_text_size * 0.78) +
        theme(legend.position = "none",
              axis.title.x    = element_blank(),
              plot.title       = element_text(size = input$plot_text_size * 0.85,
                                              face = "bold", hjust = 0.5),
              axis.title.y     = element_text(size = input$plot_text_size * 0.72)) +
        labs(title = get_label(param, lang = input$label_lang)$title,
             y     = get_label(param, lang = input$label_lang)$ylab)
    }
    return(p)
  }

  # ── 組合所有圖為 patchwork ────────────────────────────────────────────────
  generate_multipanel <- function() {
    req(input$multipanel_params, length(input$multipanel_params) >= 1)
    req(all_params_engine())

    plot_list <- lapply(input$multipanel_params, function(p) {
      tryCatch(make_one_panel(p, input$multipanel_type), error = function(e) NULL)
    })
    plot_list <- Filter(Negate(is.null), plot_list)
    if (length(plot_list) == 0) return(NULL)

    wrap_plots(plot_list, ncol = input$multipanel_ncol) +
      plot_annotation(
        title    = paste0("多參數", if(input$multipanel_type=="bar") "長條" else "盒鬚", "圖聯圖"),
        subtitle = paste0(length(plot_list), " 個參數 · ",
                          input$multipanel_ncol, " 欄 · ",
                          format(Sys.Date(), "%Y-%m-%d")),
        theme    = theme(plot.title    = element_text(size = input$plot_text_size,
                                                       face = "bold"),
                         plot.subtitle = element_text(size = input$plot_text_size * 0.75,
                                                       color = "grey40"))
      )
  }

  output$multipanel_plot <- renderPlot({
    p <- generate_multipanel()
    req(p)
    p
  })

  # --- Downloads ---
  output$download_stats <- downloadHandler(
    filename = function() paste0("Stats_Summary_Eta2_R2_", Sys.Date(), ".csv"),
    content  = function(file) write_csv_bom(all_params_engine()$stats, file)
  )
  output$download_pairwise <- downloadHandler(
    filename = function() paste0("Pairwise_Tukey_", Sys.Date(), ".csv"),
    content  = function(file) {
      pw <- all_params_engine()$pairwise
      req(pw)
      write_csv_bom(pw, file)
    }
  )
  output$download_means <- downloadHandler(
    filename = function() "Means_Label.csv",
    content  = function(file) write_csv_bom(all_params_engine()$means, file)
  )
  output$download_dist <- downloadHandler(
    filename = "Dist_Test.csv",
    content  = function(file) write_csv_bom(all_params_engine()$dist, file)
  )
  output$download_corr_csv <- downloadHandler(
    filename = function() paste0("Corr_Matrix_", toupper(input$corr_method), "_", Sys.Date(), ".csv"),
    content  = function(file) write_csv_bom(corr_engine()$star, file, row.names = TRUE)
  )
  output$download_corr_plot <- downloadHandler(
    filename = function() paste0("CorrPlot_", toupper(input$corr_method), "_", input$corr_n_mode, "_", Sys.Date(), ".png"),
    content  = function(file) {
      res <- corr_engine(); req(res)
      down_factor <- input$plot_text_size / 10
      png(file, width = 10, height = 10, units = "in", res = input$plot_dpi)
      par(xpd = TRUE)
      title_str <- paste0("Correlation (", toupper(res$method), ") — ",
                          switch(input$corr_n_mode,
                                 "all"    = "全部",
                                 "high_n" = paste0("n≥", input$corr_n_threshold),
                                 "low_n"  = paste0("n<",  input$corr_n_threshold)))
      corrplot(res$cor, method = "color", type = "upper", tl.col = "black",
               tl.cex = down_factor, tl.pos = "lt", cl.cex = down_factor,
               title = title_str, mar = c(3, 1, 5, 4))
      n_c <- ncol(res$cor)
      for(i in 1:(n_c-1)) {
        for(j in (i+1):n_c) {
          text(x = j, y = n_c - i + 1, labels = res$star[i, j],
               cex = down_factor * 0.8, col = "black", adj = 0.5)
        }
      }
      dev.off()
    }
  )
  output$download_plot <- downloadHandler(
    filename = function() {
      type_tag <- if (input$single_plot_type == "bar") "bar" else "box"
      paste0(input$single_param, "_", type_tag, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- generate_single_preview_plot(); req(p)
      ggsave(file, plot = p, device = "png", width = 8, height = 6, dpi = input$plot_dpi)
    }
  )
  output$download_pca_plot <- downloadHandler(
    filename = function() paste0("PCA_Plot_", Sys.Date(), ".png"),
    content  = function(file) {
      p <- generate_pca_plot()
      ggsave(file, plot = p, device = "png", width = 10, height = 8, dpi = input$plot_dpi)
    }
  )
  output$download_adonis_csv <- downloadHandler(
    filename = function() paste0("Multivariate_ADONIS_", Sys.Date(), ".csv"),
    content  = function(file) {
      res <- multivariate_engine()$adonis
      if(!is.null(res)) write_csv_bom(as.data.frame(res), file, row.names = TRUE) else
        write_csv_bom(data.frame(Error = "Insufficient data for ADONIS"), file)
    }
  )
  output$download_strip_plot <- downloadHandler(
    filename = function() paste0(input$adv_param_strip %||% "strip", "_strip_", Sys.Date(), ".png"),
    content  = function(file) {
      p <- generate_strip_plot(); req(p)
      ggsave(file, plot = p, device = "png", width = 7, height = 5.5, dpi = input$plot_dpi)
    }
  )
  output$download_paired_plot <- downloadHandler(
    filename = function() paste0(input$adv_param_paired %||% "paired", "_paired_", input$paired_compare, "_", Sys.Date(), ".png"),
    content  = function(file) {
      p <- generate_paired_plot(); req(p)
      ggsave(file, plot = p, device = "png", width = 6, height = 5.5, dpi = input$plot_dpi)
    }
  )
  output$download_interaction_plot <- downloadHandler(
    filename = function() paste0(input$adv_param_inter %||% "interaction", "_interaction_", Sys.Date(), ".png"),
    content  = function(file) {
      p <- generate_interaction_plot(); req(p)
      ggsave(file, plot = p, device = "png", width = 8, height = 5.5, dpi = input$plot_dpi)
    }
  )
  output$download_multipanel <- downloadHandler(
    filename = function() {
      type_label <- if(input$multipanel_type == "bar") "bar" else "box"
      paste0("Multipanel_", type_label, "_",
             input$multipanel_ncol, "col_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- generate_multipanel(); req(p)
      n_p    <- length(input$multipanel_params)
      n_rows <- ceiling(n_p / input$multipanel_ncol)
      total_w <- input$multipanel_width  * input$multipanel_ncol
      total_h <- input$multipanel_height * n_rows
      ggsave(file, plot = p, device = "png",
             width = total_w, height = total_h,
             dpi = input$plot_dpi, limitsize = FALSE)
    }
  )

  # ════════════════════════════════════════════════════════════
  # ★ 縱貫分析 Server 區塊
  # ════════════════════════════════════════════════════════════

  # ── 顏色定義 ──
  # 縱貫處理顏色：動態依上傳資料的 Treatment 水準
  long_trt_colors <- reactive({
    df <- tryCatch(long_raw(), error=function(e) NULL)
    if (is.null(df)) return(c())
    lvls <- levels(df$Treatment)
    palette <- c("#4A90D9","#27AE60","#E74C3C","#F39C12",
                 "#9B59B6","#1ABC9C","#E67E22","#2C3E50")
    setNames(palette[seq_along(lvls)], lvls)
  })

  # ── p 值星號（縱貫用，含 "." ）──
  long_p_star <- function(p) {
    if (is.na(p) || !is.numeric(p)) return("—")
    if (p < 0.001) return("***")
    if (p < 0.01)  return("**")
    if (p < 0.05)  return("*")
    if (p < 0.10)  return(".")
    return("ns")
  }

  # ── 1. 讀取資料 ──
  long_raw <- reactive({
    req(input$long_file)
    # 嘗試 UTF-8-BOM（相容 Excel 存檔）
    df <- tryCatch(
      read.csv(input$long_file$datapath, stringsAsFactors=FALSE,
               fileEncoding="UTF-8-BOM"),
      error = function(e)
        read.csv(input$long_file$datapath, stringsAsFactors=FALSE)
    )
    cols <- names(df)

    # ── 複因子展開格式偵測 ──
    # 優先：橫斷面因子欄設定；次選：自動從資料偵測非數值、非結構欄
    excl_struct <- c("Plant_ID","plant_id","Treatment","treatment","Block","block",
                     "Rep","rep","Phase","phase","週","週次","重複","區集",
                     "Week","week","Salt","Inoculation","Treatment_chr")
    non_num_free <- setdiff(cols[!sapply(df, is.numeric)], excl_struct)

    fa_col_x <- col_factorA_r()
    fb_col_x <- col_factorB_r()

    # 若橫斷面未設定，從資料欄位自動偵測
    if (is.null(fa_col_x) || !fa_col_x %in% cols) {
      fa_col_x <- if (length(non_num_free) >= 1) non_num_free[1] else NULL
    }
    if (is.null(fb_col_x) || !fb_col_x %in% cols) {
      fb_col_x <- if (length(non_num_free) >= 2) non_num_free[2] else NULL
    }

    # 實際使用的欄位名（供 reactiveVal 記錄）
    fa_col <- fa_col_x
    fb_col <- fb_col_x

    if (!is.null(fa_col) && fa_col %in% cols) {
      # 複因子展開格式
      if (!is.null(fb_col) && fb_col %in% cols) {
        df$Treatment   <- paste(df[[fa_col]], df[[fb_col]], sep="_")
        df$Salt        <- as.factor(df[[fa_col]])
        df$Inoculation <- as.factor(df[[fb_col]])
      } else {
        df$Treatment   <- as.character(df[[fa_col]])
        df$Salt        <- as.factor(df[[fa_col]])
        df$Inoculation <- factor("All")
        fb_col <- NULL
      }
    } else if ("Treatment" %in% cols) {
      # Treatment 欄已存在：嘗試拆解
      if (!("Salt" %in% cols)) {
        parts  <- strsplit(as.character(df$Treatment), "_")
        df$Salt        <- as.factor(sapply(parts, `[`, 1))
        df$Inoculation <- as.factor(sapply(parts,
                            function(x) if(length(x)>=2) x[2] else "All"))
      } else {
        df$Salt        <- as.factor(df$Salt)
        df$Inoculation <- if ("Inoculation" %in% cols)
                            as.factor(df$Inoculation) else factor("All")
      }
      fa_col <- "Salt"; fb_col <- "Inoculation"
    } else {
      # fallback
      excl2 <- c("Plant_ID","Block","Rep","Phase","週","週次","重複","區集","Week","week")
      cands  <- setdiff(cols[!sapply(df, is.numeric)], excl2)
      if (length(cands) > 0) {
        df$Treatment   <- as.character(df[[cands[1]]])
        df$Salt        <- as.factor(df[[cands[1]]])
        df$Inoculation <- factor("All")
        fa_col <- cands[1]; fb_col <- NULL
      }
    }

    # Treatment levels
    lvs <- if (!is.null(input$treat_order) && length(input$treat_order) > 0)
             input$treat_order
           else as.character(unique(df$Treatment))
    df$Treatment <- factor(df$Treatment, levels = lvs)

    # Rep / 重複
    rc <- if ("Rep" %in% cols) "Rep" else if ("重複" %in% cols) "重複" else NULL
    df$Rep <- if (!is.null(rc)) as.factor(df[[rc]]) else factor("R1")

    # Plant_ID
    if (!("Plant_ID" %in% cols))
      df$Plant_ID <- paste(df$Treatment, df$Rep, seq_len(nrow(df)), sep="_")
    df$Plant_ID <- as.factor(df$Plant_ID)

    # Block
    if      ("Block" %in% cols)     df$Block <- as.factor(df$Block)
    else if ("區集"  %in% cols)     df$Block <- as.factor(df$區集)
    else                             df$Block <- df$Rep

    # Phase
    if ("Phase" %in% cols) {
      df$Phase <- factor(df$Phase,
                         levels=c("Baseline","Salt treatment","Treatment"))
    } else {
      df$Phase <- factor("Treatment")
    }

    # Week 衍生
    df$Week_factor      <- factor(df$Week)
    if (!("Week_transplant" %in% cols)) df$Week_transplant <- df$Week
    if (!("Week_salt"       %in% cols))
      df$Week_salt <- df$Week - min(df$Week, na.rm=TRUE)
    df$Week_salt_factor <- factor(df$Week_salt)
    df$Week_numeric     <- as.numeric(as.character(df$Week))
    df$Treatment_chr    <- as.character(df$Treatment)

    # 記錄實際使用的因子欄位名（透過 reactiveVal 讓外部可讀取）
    long_fa_col_val(if (!is.null(fa_col) && fa_col != "") fa_col else "Salt")
    long_fb_col_val(if (!is.null(fb_col) && fb_col != "") fb_col else "Inoculation")

    df
  })


  # 施鹽期資料（Week 2-9）
  long_main <- reactive({
    req(long_raw())
    df <- long_raw()
    # 若有 Phase 欄且含 "Salt treatment"，只取該 Phase；否則用全部資料
    if ("Phase" %in% names(df) && "Salt treatment" %in% df$Phase)
      df %>% filter(Phase == "Salt treatment")
    else
      df
  })

  # 週均值：動態依 long_params 計算
  long_weekly_summary <- reactive({
    req(long_raw())
    df  <- long_raw()
    # 取所有數值參數欄位（排除結構欄）
    excl <- c("Week","Week_salt","Week_transplant","Week_numeric",
              "Week_factor","Week_salt_factor","Block","Rep","Plant_ID",
              "Salt","Inoculation","Treatment","Phase","Treatment_chr")
    params <- setdiff(names(df)[sapply(df, is.numeric)], excl)

    grp_cols <- intersect(c("Treatment","Week","Week_transplant",
                             "Week_salt","Phase"), names(df))
    df %>%
      group_by(across(all_of(grp_cols))) %>%
      summarise(
        across(all_of(params),
               list(mean = ~mean(.x, na.rm=TRUE),
                    se   = ~sd(.x, na.rm=TRUE)/sqrt(sum(!is.na(.x)))),
               .names = "{.col}__{.fn}"),
        .groups = "drop"
      )
  })

  # ── 2. 資料總覽 ──
  # long_forest_params UI（Simple Effect 圖用）
  output$long_forest_params_ui <- renderUI({
    req(long_raw())
    df   <- long_raw()
    excl <- c("Week","Week_salt","Week_transplant","Week_numeric",
              "Week_factor","Week_salt_factor","Block","Rep","Plant_ID",
              "Salt","Inoculation","Treatment","Phase","Treatment_chr")
    params <- setdiff(names(df)[sapply(df, is.numeric)], excl)
    if (length(params) == 0) return(helpText("無數值參數"))
    checkboxGroupInput("long_forest_params", NULL,
                       choices  = setNames(params, params),
                       selected = params[1:min(3, length(params))],
                       inline   = FALSE)
  })

  # 縱貫模型說明文字（動態）
  output$long_model_note_ui <- renderUI({
    fa <- tryCatch(long_fa_label(), error=function(e) fa_label())
    fb <- tryCatch(long_fb_label(), error=function(e) fb_label())
    helpText(style="font-size:11px;color:#555;",
      paste0("因子：", fa, " × ", fb, " × Week。"),
      paste0("模型公式：outcome ~ ",
             fa, " × ", fb, " × Week_salt_factor + (1|Block/Plant_ID)"))
  })

  # 縱貫分析參數選擇 UI（依上傳資料動態產生）
  output$long_params_ui <- renderUI({
    req(long_raw())
    df   <- long_raw()
    excl <- c("Week","Week_salt","Week_transplant","Week_numeric",
              "Week_factor","Week_salt_factor","Block","Rep","Plant_ID",
              "Salt","Inoculation","Treatment","Phase","Treatment_chr")
    params <- setdiff(names(df)[sapply(df, is.numeric)], excl)
    if (length(params) == 0) return(helpText("無數值參數"))
    checkboxGroupInput("long_params", "分析參數：",
                       choices  = setNames(params, params),
                       selected = params,
                       inline   = FALSE)
  })

  output$long_data_summary <- renderTable({
    req(long_raw())
    df <- long_raw()
    data.frame(
      項目 = c("總列數","植株數","處理數","週次數","Block數"),
      值   = c(nrow(df),
               length(unique(df$Plant_ID)),
               length(unique(df$Treatment)),
               length(unique(df$Week)),
               length(unique(df$Block)))
    )
  }, striped=TRUE, hover=TRUE)

  output$long_na_summary <- renderTable({
    req(long_raw())
    df <- long_raw()
    params <- c("Height_cm","Height_net","FvFm","Leaf_count")
    params_avail <- intersect(params, names(df))
    data.frame(
      參數     = params_avail,
      有效筆數 = sapply(params_avail, function(p) sum(!is.na(df[[p]]))),
      缺失筆數 = sapply(params_avail, function(p) sum(is.na(df[[p]])))
    )
  }, striped=TRUE, hover=TRUE)

  output$long_sample_count <- renderTable({
    req(long_raw())
    long_raw() %>%
      count(Treatment, Week) %>%
      tidyr::pivot_wider(names_from=Week, values_from=n,
                         names_prefix="W") %>%
      as.data.frame()
  }, striped=TRUE, hover=TRUE)

  # ── 3. Baseline 確認 ──
  output$long_baseline_table <- renderTable({
    req(long_raw())
    df <- long_raw() %>% filter(Phase == "Baseline")
    params <- intersect(c("Height_cm","FvFm","Leaf_count"), names(df))
    df %>%
      group_by(Treatment) %>%
      summarise(
        n         = n(),
        across(all_of(params),
               list(mean = ~round(mean(.x, na.rm=TRUE), 3),
                    SE   = ~round(sd(.x, na.rm=TRUE)/sqrt(sum(!is.na(.x))), 4)),
               .names = "{.col}_{.fn}"),
        .groups = "drop"
      )
  }, striped=TRUE, hover=TRUE, caption="Week 1 各組均值 ± SE（前測基準）")

  output$long_baseline_anova <- renderTable({
    req(long_raw())
    df <- long_raw() %>% filter(Phase == "Baseline")
    params <- intersect(c("Height_cm","FvFm","Leaf_count"), names(df))
    rows <- lapply(params, function(p) {
      sub <- df %>% filter(!is.na(.data[[p]]))
      tryCatch({
        m   <- lmer(as.formula(paste0(p,
                    " ~ Salt * Inoculation + (1|Block)")),
                    data=sub, control=lmerControl(optimizer="bobyqa"))
        aov <- anova(m)
        factors <- c("Salt","Inoculation","Salt:Inoculation")
        factors <- intersect(factors, rownames(aov))
        rows <- lapply(factors, function(f) {
          data.frame(
            參數   = p, 因子 = f,
            F值    = round(aov[f,"F value"], 3),
            df_num = aov[f,"NumDF"],
            df_den = round(aov[f,"DenDF"], 1),
            P值    = round(aov[f,"Pr(>F)"], 4),
            顯著性 = long_p_star(aov[f,"Pr(>F)"]),
            結論   = if(aov[f,"Pr(>F)"] > 0.05) "✓ ns" else "⚠ sig"
          )
        })
        do.call(rbind, rows)
      }, error = function(e) data.frame(參數=p, 因子="error", F值=NA,
                                        df_num=NA, df_den=NA, P值=NA,
                                        顯著性="err", 結論=conditionMessage(e)))
    })
    do.call(rbind, rows)
  }, striped=TRUE, hover=TRUE, caption="Baseline LMM ANOVA（Treatment 應 ns）")

  # ── 4. LMM 主分析 ──
  long_lmm_results <- reactive({
    req(long_main(), input$long_params)
    df      <- long_main()
    params  <- input$long_params
    wk_type <- input$long_week_type

    lapply(setNames(params, params), function(p) {
      sub <- df %>% filter(!is.na(.data[[p]]))
      tryCatch({
        wk_var   <- if (wk_type == "factor") "Week_salt_factor" else "Week_salt"
        # 動態因子欄位名
        fa_col_e <- long_fa_col_r()
        fb_col_e <- long_fb_col_r()
        if (!fa_col_e %in% names(sub)) fa_col_e <- "Salt"
        if (!fb_col_e %in% names(sub)) fb_col_e <- "Inoculation"

        # ★ 複因子：動態公式
        f_str <- paste0("`", p, "` ~ `", fa_col_e, "` * `", fb_col_e, "` * ",
                        wk_var, " + (1|Block/Plant_ID)")
        m <- lmer(as.formula(f_str), data=sub, REML=TRUE,
                  control=lmerControl(optimizer="bobyqa"))
        aov_res <- as.data.frame(anova(m))
        # 把 rownames（模型欄位名）替換為使用者顯示名，設為「因子」欄
        rn <- rownames(aov_res)
        rn <- gsub(fa_col_e,           long_fa_label(), rn, fixed=TRUE)
        rn <- gsub(fb_col_e,           long_fb_label(), rn, fixed=TRUE)
        rn <- gsub("Week_salt_factor", "Week",     rn, fixed=TRUE)
        rn <- gsub("Week_transplant",  "Week",     rn, fixed=TRUE)
        rn <- gsub("Week_salt",        "Week",     rn, fixed=TRUE)
        aov_res$因子 <- rn
        rownames(aov_res) <- rn   # 同步更新 rownames
        aov_res$P顯著 <- sapply(aov_res[["Pr(>F)"]], long_p_star)
        aov_res <- aov_res %>%
          dplyr::select(因子, NumDF, DenDF,
                        `F value`, `Pr(>F)`, P顯著) %>%
          mutate(across(where(is.numeric), ~round(.x, 4)))

        # partial η²
        eta2 <- tryCatch({
          e <- effectsize::eta_squared(m, partial=TRUE)
          as.data.frame(e)
        }, error=function(e) NULL)

        # R²m / R²c
        r2 <- tryCatch({
          r <- MuMIn::r.squaredGLMM(m)
          list(r2m=round(r[1,"R2m"],3), r2c=round(r[1,"R2c"],3))
        }, error=function(e) list(r2m=NA, r2c=NA))

        list(model=m, anova=aov_res, eta2=eta2, r2=r2, param=p)
      }, error=function(e) list(model=NULL, anova=NULL, eta2=NULL,
                                 r2=list(r2m=NA,r2c=NA), param=p,
                                 error=conditionMessage(e)))
    })
  })

  output$long_lmm_output_ui <- renderUI({
    req(long_lmm_results())
    res   <- long_lmm_results()
    # 動態參數標籤：用 get_label 取得中英文顯示名稱
    params_avail <- if (!is.null(input$long_params) && length(input$long_params) > 0)
                      input$long_params
                    else names(res)
    param_labels <- setNames(
      sapply(params_avail, function(p) get_label(p, lang="zh")$title),
      params_avail
    )
    panels <- lapply(names(res), function(p) {
      lbl <- get_label(p, lang="zh")$title
      r   <- res[[p]]

      # ANOVA 表
      anova_block <- if (!is.null(r$anova)) {
        # 格式化 P 值為科學記號
        aov_fmt <- r$anova
        aov_fmt[["Pr(>F)"]] <- ifelse(
          !is.na(aov_fmt[["Pr(>F)"]]) & aov_fmt[["Pr(>F)"]] < 0.0001,
          formatC(aov_fmt[["Pr(>F)"]], format = "e", digits = 2),
          as.character(round(aov_fmt[["Pr(>F)"]], 4))
        )
        tagList(
          h5("A. 固定效應 ANOVA（Type III）"),
          df_to_html(aov_fmt, rownames=FALSE)
        )
      } else {
        tags$p(paste0("⚠ 模型失敗：", r$error), style = "color:red;")
      }

      # R² 表
      r2_block <- if (!is.null(r$r2)) {
        r2v <- r$r2
        r2_df <- data.frame(
          指標 = c("R²m（固定效應）", "R²c（固定+隨機）",
                   "ΔR² = R²c−R²m（隨機效應貢獻）"),
          值   = c(ifelse(is.na(r2v$r2m), "—", sprintf("%.3f", r2v$r2m)),
                   ifelse(is.na(r2v$r2c), "—", sprintf("%.3f", r2v$r2c)),
                   ifelse(is.na(r2v$r2m) || is.na(r2v$r2c), "—",
                          sprintf("%.3f", r2v$r2c - r2v$r2m))),
          stringsAsFactors = FALSE
        )
        tagList(
          h5("B. 模型解釋力"),
          df_to_html(r2_df)
        )
      }

      tagList(h4(paste0("▌ ", lbl)), anova_block, r2_block, hr())
    })
    do.call(tagList, panels)
  })

  # ── 5. AR(1) 自相關（Fv/Fm）──
  long_ar1_result <- reactive({
    req(long_main(), input$long_params, input$long_ar1)
    if (input$long_ar1 == "none") return(NULL)
    df_all <- long_main() %>%
      mutate(Plant_ID_chr  = as.character(Plant_ID),
             Week_salt_int = as.integer(Week_salt_factor))

    fa_c <- long_fa_col_r(); fb_c <- long_fb_col_r()
    if (!fa_c %in% names(df_all)) fa_c <- "Salt"
    if (!fb_c %in% names(df_all)) fb_c <- "Inoculation"

    # 對每個參數各自執行 AIC 比較
    res <- lapply(input$long_params, function(p) {
      tryCatch({
        df <- df_all %>% filter(!is.na(.data[[p]]))
        if (nrow(df) < 10) return(list(param=p, error="樣本數不足"))

        f_nlme <- as.formula(paste0(
          "`", p, "` ~ `", fa_c, "` * `", fb_c,
          "` * Week_numeric + Block"))
        cor_struct <- if (input$long_ar1 == "cs")
          nlme::corCompSymm(form = ~Week_numeric | Plant_ID_chr)
        else
          nlme::corAR1(form = ~Week_numeric | Plant_ID_chr)
        m_nlme <- nlme::lme(f_nlme,
          random=~1|Plant_ID_chr, correlation=cor_struct,
          data=df, method="REML")

        f_lmer <- as.formula(paste0(
          "`", p, "` ~ `", fa_c, "` * `", fb_c,
          "` * Week_salt_factor + (1|Block/Plant_ID)"))
        m_lmer <- lmer(f_lmer, data=df, REML=TRUE,
                       control=lmerControl(optimizer="bobyqa"))

        aic_n <- AIC(m_nlme); aic_l <- AIC(m_lmer)
        list(
          param       = p,
          ar1_aic     = round(aic_n, 2),
          lmer_aic    = round(aic_l, 2),
          preferred   = if (aic_n < aic_l) "AR(1)/CS" else "lmerTest",
          phi         = tryCatch(
            coef(m_nlme$modelStruct$corStruct, unconstrained=FALSE),
            error=function(e) NA),
          ar1_summary = summary(m_nlme)$tTable,
          ar1_anova   = anova(m_nlme)
        )
      }, error=function(e) list(param=p, error=conditionMessage(e)))
    })
    setNames(res, input$long_params)
  })


  output$long_ar1_output <- renderPrint({
    req(long_ar1_result())
    res_list <- long_ar1_result()
    for (nm in names(res_list)) {
      r <- res_list[[nm]]
      cat("══════", nm, "══════
")
      if (!is.null(r$error)) {
        cat("⚠ 模型失敗：", r$error, "

"); next
      }
      cat("AR(1)/CS AIC：", r$ar1_aic,
          "  |  lmerTest AIC：", r$lmer_aic,
          "  →  建議：", r$preferred, "
")
      if (!is.na(r$phi))
        cat("自相關係數 phi：", round(r$phi, 4), "
")
      cat("
固定效應摘要：
")
      print(round(r$ar1_summary, 4))
      cat("
")
    }
  })

  output$long_ar1_aic <- renderTable({
    req(long_ar1_result())
    res_list <- long_ar1_result()
    lab <- if (input$long_ar1 == "ar1") "AR(1)" else "CompSymm"
    rows <- lapply(names(res_list), function(nm) {
      r <- res_list[[nm]]
      if (!is.null(r$error)) return(
        data.frame(參數=nm, AR模型=lab, AR_AIC=NA, lmerTest_AIC=NA,
                   建議=paste0("⚠ ", r$error), stringsAsFactors=FALSE))
      data.frame(
        參數       = nm,
        AR模型     = lab,
        AR_AIC     = r$ar1_aic,
        lmerTest_AIC = r$lmer_aic,
        建議       = if (r$ar1_aic < r$lmer_aic) "◀ AR 較佳" else "◀ lmerTest 較佳",
        stringsAsFactors = FALSE
      )
    })
    bind_rows(rows)
  }, striped=TRUE, hover=TRUE, caption="AIC 比較（值越小越好）")

  # ── 6. 事後比較（emmeans，週類別模型）──
  long_emmeans_results <- reactive({
    req(long_main(), input$long_params)
    df     <- long_main()
    params <- input$long_params
    mode   <- if (!is.null(input$long_em_mode)) input$long_em_mode else "factorial"
    # 重點對：依模式取不同 input
    pairs  <- if (mode == "treatment") {
      input$long_contrast_pairs %||% character(0)
    } else {
      character(0)  # 複因子模式重點對由 long_contrast_factorial 控制，不在此篩選
    }

    # Salt/Inoculation → Treatment 對照表（動態建立）
    si_to_trt <- if (!is.null(col_factorA_r()) && col_factorA_r() %in% names(df) &&
                       !is.null(col_factorB_r()) && col_factorB_r() %in% names(df)) {
      salt_lvs_g  <- levels(factor(df[[col_factorA_r()]]))
      inocu_lvs_g <- levels(factor(df[[col_factorB_r()]]))
      combos <- expand.grid(Salt=salt_lvs_g, Ino=inocu_lvs_g, stringsAsFactors=FALSE)
      setNames(paste(combos$Salt, combos$Ino, sep="_"),
               paste(combos$Salt, combos$Ino))
    } else {
      character(0)
    }

    lapply(setNames(params, params), function(p) {
      sub <- df %>% filter(!is.na(.data[[p]]))
      tryCatch({
        if (mode == "treatment") {
          # ── Treatment 四組模式 ──
          m <- lmer(
            as.formula(paste0(p,
                      " ~ Treatment * Week_salt_factor",
                      " + (1|Block/Plant_ID)")),
            data=sub, REML=TRUE,
            control=lmerControl(optimizer="bobyqa"))
          em_trt <- emmeans(m, ~ Treatment | Week_salt_factor, adjust="tukey")
          pw_trt <- as.data.frame(contrast(em_trt, method="pairwise",
                                            simple="Treatment")) %>%
            mutate(Factor="Treatment",
                   comparison=as.character(contrast),
                   Sig=sapply(as.numeric(p.value), long_p_star)) %>%
            dplyr::select(Factor, comparison, Week_salt_factor,
                          estimate, SE, t.ratio, p.value, Sig)
          norm_cmp <- function(s) paste(trimws(strsplit(s,"-")[[1]]), collapse=" - ")
          pw_focus <- pw_trt %>%
            filter(sapply(comparison, function(cmp) {
              cmp_norm <- norm_cmp(as.character(cmp))
              any(sapply(pairs, function(pair) {
                pair_norm <- norm_cmp(pair)
                cmp_norm == pair_norm ||
                cmp_norm == paste(rev(trimws(strsplit(pair_norm,"-")[[1]])),
                                  collapse=" - ")
              }))
            }))
          # CLD：em_trt 有 Treatment 欄，make_long_cld 可直接使用
          list(em=em_trt, pw_all=pw_trt, pw_focus=pw_focus,
               em_salt=NULL, em_inocu=NULL, mode="treatment", param=p,
               si_to_trt=si_to_trt)

        } else {
          # ── 複因子模式（預設）──
          # 動態欄位名稱
          fa_col <- long_fa_col_r()
          fb_col <- long_fb_col_r()
          if (!fa_col %in% names(sub)) fa_col <- "Salt"
          if (!fb_col %in% names(sub)) fb_col <- "Inoculation"

          m <- lmer(
            as.formula(paste0("`", p, "`",
                      " ~ `", fa_col, "` * `", fb_col, "` * Week_salt_factor",
                      " + (1|Block/Plant_ID)")),
            data=sub, REML=TRUE,
            control=lmerControl(optimizer="bobyqa"))

          # ★ Simple effects：動態依實際欄位名計算
          em_formula <- as.formula(paste0(
            "~ `", fa_col, "` * `", fb_col, "` | Week_salt_factor"))
          em_full <- emmeans(m, em_formula, adjust="tukey")

          # 取得實際水準
          salt_lvs  <- levels(factor(sub[[fa_col]]))
          inocu_lvs <- levels(factor(sub[[fb_col]]))

          # Salt simple effects（對每個 Inoculation 水準）
          pw_salt_list <- lapply(inocu_lvs, function(ilv) {
            tryCatch({
              as.data.frame(
                contrast(em_full, method="pairwise", simple=fa_col,
                         by=c(fb_col,"Week_salt_factor"))
              ) %>%
                filter(.data[[fb_col]] == ilv) %>%
                mutate(Factor   = paste0(long_fa_label(), " | ", long_fb_label(), "=", ilv),
                       comparison = as.character(contrast),
                       Sig      = sapply(p.value, long_p_star)) %>%
                dplyr::select(Factor, comparison, Week_salt_factor,
                              estimate, SE, t.ratio, p.value, Sig)
            }, error=function(e) NULL)
          })

          # Inoculation simple effects（對每個 Salt 水準）
          pw_inocu_list <- lapply(salt_lvs, function(slv) {
            tryCatch({
              as.data.frame(
                contrast(em_full, method="pairwise", simple=fb_col,
                         by=c(fa_col,"Week_salt_factor"))
              ) %>%
                filter(.data[[fa_col]] == slv) %>%
                mutate(Factor   = paste0(long_fb_label(), " | ", long_fa_label(), "=", slv),
                       comparison = as.character(contrast),
                       Sig      = sapply(p.value, long_p_star)) %>%
                dplyr::select(Factor, comparison, Week_salt_factor,
                              estimate, SE, t.ratio, p.value, Sig)
            }, error=function(e) NULL)
          })

          pw_all <- bind_rows(
            Filter(Negate(is.null), pw_salt_list),
            Filter(Negate(is.null), pw_inocu_list)
          )

          # em_inter 供 CLD 字母標記用
          em_inter <- em_full

          norm_cmp <- function(s) paste(trimws(strsplit(s,"-")[[1]]), collapse=" - ")
          pw_focus <- pw_all %>%
            filter(sapply(comparison, function(cmp) {
              cmp_norm <- norm_cmp(as.character(cmp))
              any(sapply(pairs, function(pair) {
                pair_norm <- norm_cmp(pair)
                cmp_norm == pair_norm ||
                cmp_norm == paste(rev(trimws(strsplit(pair_norm,"-")[[1]])),
                                  collapse=" - ")
              }))
            }))

          # ★ em_inter（= em_full）回傳，供 CLD 字母標記用
          list(em=em_inter, pw_all=pw_all, pw_focus=pw_focus,
               em_salt=NULL, em_inocu=NULL, mode="factorial", param=p,
               si_to_trt=si_to_trt)
        }
      }, error=function(e) list(em=NULL, pw_all=NULL, pw_focus=NULL,
                                 em_salt=NULL, em_inocu=NULL, mode=mode,
                                 param=p, error=conditionMessage(e)))
    })
  })

  output$long_emmeans_output_ui <- renderUI({
    req(long_emmeans_results())
    res <- long_emmeans_results()
    panels <- lapply(names(res), function(p) {
      lbl <- get_label(p, lang = if(!is.null(input$long_lang)) input$long_lang else "zh")$title
      r    <- res[[p]]
      mode <- if (!is.null(r$mode)) r$mode else "factorial"

      if (is.null(r$pw_all)) {
        return(tagList(
          h4(paste0("▌ ", lbl)),
          tags$p(paste0("⚠ ", r$error), style="color:red;"),
          hr()))
      }

      fmt4 <- function(df) df %>% mutate(across(where(is.numeric), ~round(.x, 4)))

      # 複因子模式：勾選哪個就顯示哪個區塊
      show_blocks <- if (!is.null(input$long_contrast_factorial)) {
        input$long_contrast_factorial
      } else {
        c("salt_none","salt_pgpr","inocu_ctrl","inocu_salt")
      }

      make_block <- function(factor_label, label_str, show_blocks, key) {
        if (!(key %in% show_blocks)) return(NULL)
        sig_rows <- r$pw_all %>% filter(Factor == factor_label, p.value < 0.05)
        tagList(
          h5(label_str),
          if (nrow(sig_rows)>0) df_to_html(fmt4(sig_rows))
          else tags$p("（無顯著週次）", style="color:grey;")
        )
      }

      content_block <- if (mode == "treatment") {
        # ── Treatment 四組模式 ──
        all_sig <- r$pw_all %>% filter(p.value < 0.05)
        focus_block <- if (!is.null(r$pw_focus) && nrow(r$pw_focus) > 0) {
          tagList(
            h5("重點對（依左側勾選）"),
            df_to_html(fmt4(r$pw_focus))
          )
        }
        tagList(
          focus_block,
          h5("全部 pairwise（Tukey，僅顯示 P < 0.05）"),
          if (nrow(all_sig)>0) df_to_html(fmt4(all_sig))
          else tags$p("（無顯著對）", style="color:grey;")
        )
      } else {
        # ── 複因子模式：四個 simple effects ──
        blocks <- list(
          {
            sfm   <- se_factor_map_r()
            keys  <- c("salt_none","salt_pgpr","inocu_ctrl","inocu_salt")
            labels <- c("A","B","C","D")
            lapply(seq_along(keys), function(i) {
              k   <- keys[i]
              lbl <- if (!is.null(sfm[[k]])) sfm[[k]] else k
              make_block(lbl, paste0(labels[i], ". ", lbl),
                         show_blocks, k)
            })
          }
        )
        blocks <- Filter(Negate(is.null), blocks)
        if (length(blocks)==0) tags$p("（未勾選任何區塊）", style="color:grey;")
        else do.call(tagList, blocks)
      }

      tagList(
        h4(paste0("▌ ", lbl)),
        content_block,
        hr()
      )
    })
    do.call(tagList, panels)
  })

    # ── 7. 縱貫折線圖 ──
  # 字母標記產生器
  # si_to_trt：Salt+Inoculation 組合 → Treatment 名稱對照表
  # si_to_trt_map 由縱貫資料動態建立（在 long_raw 載入後）
  si_to_trt_map <- c()  # 預設空，make_long_cld 會自行處理

  make_long_cld <- function(em_obj, param, offset, raw_df, mode = "factorial") {
    tryCatch({
      cld_raw <- multcomp::cld(em_obj, adjust="tukey",
                                Letters=letters, alpha=0.05) %>%
        as.data.frame()

      cld_raw$Week_salt       <- as.integer(as.character(cld_raw$Week_salt_factor))
      cld_raw$Week_transplant <- cld_raw$Week_salt + 4L
      cld_raw$letter          <- trimws(cld_raw$.group)

      if (mode == "treatment") {
        cld_raw$Treatment_chr <- as.character(cld_raw$Treatment)
      } else {
        # 複因子：Salt + Inoculation 反查 Treatment
        # 用 as.character 確保 factor level 不影響比對
        # 動態反查：從 raw_df 建立 Salt+Inoculation → Treatment 對照表
        si_trt_df <- raw_df %>%
          distinct(Salt, Inoculation, Treatment) %>%
          mutate(si_key = paste(as.character(Salt), as.character(Inoculation)))
        si_key <- paste(as.character(cld_raw$Salt),
                        as.character(cld_raw$Inoculation))
        cld_raw$Treatment_chr <- si_trt_df$Treatment[match(si_key, si_trt_df$si_key)]
      }

      result <- cld_raw[, c("Treatment_chr","Week_transplant","letter")]
      result <- result[!is.na(result$Treatment_chr), ]

      # 直接從 raw_df 計算各組週均值±SE（確保 merge key 完全一致）
      wk_j <- raw_df %>%
        filter(!is.na(.data[[param]]),
               Phase == "Salt treatment") %>%
        group_by(Treatment, Week_transplant) %>%
        summarise(
          y_val  = mean(.data[[param]], na.rm=TRUE),
          se_val = sd(.data[[param]],   na.rm=TRUE) /
                   sqrt(sum(!is.na(.data[[param]]))),
          .groups = "drop"
        ) %>%
        mutate(Treatment_chr = as.character(Treatment)) %>%
        dplyr::select(Treatment_chr, Week_transplant, y_val, se_val)

      merged <- merge(result, wk_j,
                      by=c("Treatment_chr","Week_transplant"), all.x=TRUE)
      trt_lvls <- if (!is.null(raw_df$Treatment)) levels(factor(raw_df$Treatment)) else unique(merged$Treatment_chr)
      merged$Treatment <- factor(merged$Treatment_chr, levels = trt_lvls)
      merged$label_y   <- merged$y_val + merged$se_val + offset
      merged[!is.na(merged$y_val), ]
    }, error=function(e) NULL)
  }

  long_cld_data <- reactive({
    req(long_emmeans_results(), long_raw(), input$long_params)
    res    <- long_emmeans_results()
    raw_df <- long_raw()
    mode   <- if (!is.null(input$long_em_mode)) input$long_em_mode else "factorial"
    # 動態計算每個參數的 CLD，offset 預設 5% of 參數範圍
    cld_list <- lapply(input$long_params, function(p) {
      if (is.null(res[[p]]$em)) return(NULL)
      vals   <- raw_df[[p]]
      offset <- if (!is.null(vals)) diff(range(vals, na.rm=TRUE)) * 0.05 else 0.1
      make_long_cld(res[[p]]$em, p, offset, raw_df, mode)
    })
    setNames(cld_list, input$long_params)
  })

  make_long_plot <- function(param, wksum, cld_data, raw_df, show_ind,
                              show_ribbon, text_size, lang) {
     # 動態欄位名稱（long_weekly_summary 用 param__mean / param__se 格式）
     y_col  <- paste0(param, "__mean")
     se_col <- paste0(param, "__se")
     if (!(y_col %in% names(wksum))) return(NULL)
    # 動態 Y 軸標籤和標題（使用 get_label，param 是原始欄名）
    lbl    <- get_label(param, lang=lang)
    if (is.null(lbl) || is.null(lbl$ylab)) {
      # fallback：param 做為標題和 Y 軸標籤
      lbl <- list(title = param, ylab = param)
    }
    if (is.null(lbl) || is.null(lbl$ylab)) {
      # fallback：param 做為標題，無單位
      lbl <- list(title=param, ylab=param)
    }
    y_lab  <- lbl$ylab
    title  <- lbl$title
    sub_wk   <- wksum %>% filter(!is.nan(.data[[y_col]]))
    cld_sub  <- cld_data[[param]]
    # 施鹽起始線：只在有 Phase 欄且含 Baseline 時顯示
    has_baseline <- "Phase" %in% names(sub_wk) &&
                    "Baseline" %in% as.character(sub_wk$Phase) &&
                    "Salt treatment" %in% as.character(sub_wk$Phase)
    salt_start <- if (has_baseline) {
      min(sub_wk$Week_transplant[
            sub_wk$Phase == "Salt treatment"], na.rm=TRUE)
    } else { NULL }

    salt_line <- if (!is.null(salt_start)) list(
      geom_vline(xintercept = salt_start - 0.5,
                 linetype="dashed", colour="#C0392B", linewidth=0.7),
      annotate("text", x=salt_start-0.3, y=Inf,
               label=if(lang=="zh") "施鹽開始" else "Treatment\nstart",
               hjust=0, vjust=1.4, size=text_size/4.5, colour="#C0392B")
    ) else list()
    # X 軸：動態產生週次標籤
    wk_breaks <- sort(unique(sub_wk$Week_transplant))
    has_ws <- "Week_salt" %in% names(sub_wk) &&
              length(unique(sub_wk$Week_salt)) > 1
    wk_labels <- sapply(wk_breaks, function(wx) {
      if (has_ws) {
        ws <- sub_wk$Week_salt[match(wx, sub_wk$Week_transplant)][1]
        paste0("W", wx, "\n(S", ws, ")")
      } else {
        paste0("W", wx)
      }
    })
    x_scale <- scale_x_continuous(breaks = wk_breaks, labels = wk_labels)

    p <- ggplot(sub_wk, aes(x=Week_transplant, y=.data[[y_col]],
                             colour=Treatment, fill=Treatment))

    if (show_ind) {
      p <- p + geom_line(
        data = raw_df %>% filter(!is.na(.data[[param]])),
        aes(y=.data[[param]], group=Plant_ID),
        alpha=0.06, linewidth=0.3, inherit.aes=TRUE)
    }
    if (show_ribbon) {
      p <- p + geom_ribbon(
        aes(ymin=.data[[y_col]]-.data[[se_col]],
            ymax=.data[[y_col]]+.data[[se_col]]),
        alpha=0.15, colour=NA)
    }
    p <- p +
      geom_errorbar(aes(ymin=.data[[y_col]]-.data[[se_col]],
                        ymax=.data[[y_col]]+.data[[se_col]]),
                    width=0.2, linewidth=0.6) +
      geom_line(linewidth=0.8) +
      geom_point(size=1.2)

    if (!is.null(cld_sub) && nrow(cld_sub) > 0) {
      p <- p + geom_text(
        data = cld_sub %>% filter(!is.na(label_y), !is.na(Treatment)),
        aes(x=Week_transplant, y=label_y, label=letter, colour=Treatment),
        size=text_size/4.0, fontface="bold",
        vjust=0, show.legend=FALSE, na.rm=TRUE)
    }

    if (param == "Height_net") {
      p <- p + geom_hline(yintercept=0, linetype="dotted",
                           colour="grey60", linewidth=0.5)
    }
    if (param == "FvFm") {
      p <- p +
        geom_hline(yintercept=0.75, linetype="dotted",
                   colour="grey50", linewidth=0.5) +
        annotate("text", x=3.1, y=0.748,
                 label="Lower normal limit (0.75)",
                 size=text_size/5.5, colour="grey50", hjust=0) +
        coord_cartesian(ylim=c(0.72, 0.83))
    }

    p + salt_line + x_scale +
      scale_colour_manual(values=long_trt_colors()) +
      scale_fill_manual(values=long_trt_colors()) +
      labs(title=title,
           subtitle=if(lang=="zh") "均值 ± SE；字母：emmeans Tukey（各週）"
                   else "Mean ± SE; letters: emmeans Tukey (per week)",
           x=if(lang=="zh") "移植後週次（施鹽後週次）"
             else "Week post-transplant (week after salt treatment)",
           y=y_lab, colour="Treatment", fill="Treatment") +
      theme_classic(base_size=text_size) +
      theme(
        plot.title      = element_text(face="bold", hjust=0.5),
        plot.subtitle   = element_text(size=text_size*0.78, colour="grey40"),
        legend.position = "bottom",
        legend.text     = element_text(size=text_size*0.95),
        legend.title    = element_text(size=text_size*0.95, face="bold"),
        legend.key.size = unit(text_size*0.08, "cm")
      )
  }

  output$long_plots_ui <- renderUI({
    req(input$long_params)
    plot_outputs <- lapply(input$long_params, function(p) {
      lbl <- get_label(p, lang = if(!is.null(input$long_lang)) input$long_lang else "zh")$title
      dl_id   <- paste0("long_dl_", p)
      plot_id <- paste0("long_plot_", p)
      tagList(
        fluidRow(
          column(8, h5(paste0("▌ ", lbl))),
          column(4, style="text-align:right;",
            downloadButton(dl_id,
                           paste0("下載 ", lbl, " PNG"),
                           class="btn-sm btn-default"))
        ),
        plotOutput(plot_id, height="420px"),
        br()
      )
    })
    do.call(tagList, plot_outputs)
  })

  observe({
    req(long_raw(), long_weekly_summary(), long_cld_data(), input$long_params)
    lapply(input$long_params, function(p) {
      local({
        param <- p

        # ── 圖表預覽 ──
        output[[paste0("long_plot_", param)]] <- renderPlot({
          make_long_plot(
            param      = param,
            wksum      = long_weekly_summary(),
            cld_data   = long_cld_data(),
            raw_df     = long_raw(),
            show_ind   = isTRUE(input$long_show_individual),
            show_ribbon= isTRUE(input$long_show_ribbon),
            text_size  = input$long_text_size,
            lang       = input$long_label_lang
          )
        })

        # ── 個別下載（ggsave，與其他區域一致）──
        output[[paste0("long_dl_", param)]] <- downloadHandler(
          filename = function() {
            paste0("long_", param, "_", Sys.Date(), ".png")
          },
          content = function(file) {
            p_plot <- make_long_plot(
              param      = param,
              wksum      = long_weekly_summary(),
              cld_data   = long_cld_data(),
              raw_df     = long_raw(),
              show_ind   = isTRUE(input$long_show_individual),
              show_ribbon= isTRUE(input$long_show_ribbon),
              text_size  = input$long_text_size,
              lang       = input$long_label_lang
            )
            # 固定 96 DPI 輸出，字型大小與螢幕預覽一致
            # 若需要高解析度，請用 zip 批量下載並調高 DPI 滑桿
            ggsave(file, plot = p_plot, device = "png",
                   width = input$long_plot_width,
                   height = input$long_plot_height,
                   dpi = input$long_plot_dpi)
          }
        )
      })
    })
  })

  # ── 8. 下載處理器 ──
  output$long_download_plots <- downloadHandler(
    filename    = function() paste0("longitudinal_plots_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content     = function(file) {
      req(long_raw(), long_weekly_summary(), long_cld_data(), input$long_params)
      tmp_dir   <- tempdir()
      png_files <- c()
      for (p in input$long_params) {
        png_path <- file.path(tmp_dir, paste0("long_", p, "_", Sys.Date(), ".png"))
        p_plot <- make_long_plot(
          param       = p,
          wksum       = long_weekly_summary(),
          cld_data    = long_cld_data(),
          raw_df      = long_raw(),
          show_ind    = isTRUE(input$long_show_individual),
          show_ribbon = isTRUE(input$long_show_ribbon),
          text_size   = input$long_text_size,
          lang        = input$long_label_lang
        )
        ggsave(png_path, plot = p_plot, device = "png",
               width = input$long_plot_width,
               height = input$long_plot_height,
               dpi = input$long_plot_dpi)
        png_files <- c(png_files, png_path)
      }
      # Windows 需要系統有 zip.exe；改用 zip 套件的 zipr() 更可靠
      tryCatch(
        zip::zipr(file, png_files),
        error = function(e) {
          # fallback：用 utils::zip
          wd <- getwd()
          setwd(dirname(png_files[1]))
          utils::zip(file, basename(png_files), flags = "-j")
          setwd(wd)
        }
      )
    }
  )

  output$long_download_emmeans <- downloadHandler(
    filename = function() paste0("longitudinal_emmeans_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(long_emmeans_results())
      res <- long_emmeans_results()
      all_rows <- lapply(names(res), function(p) {
        if (!is.null(res[[p]]$pw_all))
          cbind(Parameter=p, res[[p]]$pw_all)
      })
      df_out <- do.call(rbind, Filter(Negate(is.null), all_rows))
      write_csv_bom(df_out, file)
    }
  )

  output$long_download_anova <- downloadHandler(
    filename = function() paste0("longitudinal_lmm_anova_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(long_lmm_results())
      res <- long_lmm_results()
      all_rows <- lapply(names(res), function(p) {
        if (!is.null(res[[p]]$anova))
          cbind(Parameter=p, res[[p]]$anova)
      })
      df_out <- do.call(rbind, Filter(Negate(is.null), all_rows))
      write_csv_bom(df_out, file)
    }
  )

  # ── Simple Effect 橫條圖 ──

  # SE factor key → Factor label 對照（動態從 emmeans 結果取得）
  se_factor_map_r <- reactive({
    res <- tryCatch(long_emmeans_results(), error=function(e) NULL)
    if (is.null(res)) return(list())
    # 取第一個有結果的參數的 pw_all
    for (nm in names(res)) {
      if (!is.null(res[[nm]]$pw_all) && nrow(res[[nm]]$pw_all) > 0) {
        factors <- unique(res[[nm]]$pw_all$Factor)
        # 依序指定 key
        keys <- c("salt_none","salt_pgpr","inocu_ctrl","inocu_salt")[
                  seq_len(min(4, length(factors)))]
        return(setNames(as.list(factors[seq_along(keys)]), keys))
      }
    }
    list()
  })

  make_se_bar_plot <- function(param, em_res, lmm_res, se_key,
                                show_ns, text_size, lang) {
    r     <- em_res[[param]]
    lmm_r <- lmm_res[[param]]
    if (is.null(r$pw_all)) return(NULL)

    # 動態取得 factor label
    sfm         <- se_factor_map_r()
    factor_label <- if (!is.null(sfm[[se_key]])) sfm[[se_key]] else se_key

    # 動態參數標籤
    param_label_val <- get_label(param, lang=lang)$ylab

    se_display <- sfm  # 直接用實際 Factor 值作顯示名稱

    # 篩選目標 simple effect
    df <- r$pw_all %>%
      filter(Factor == factor_label) %>%
      mutate(
        Week_num  = as.integer(as.character(Week_salt_factor)),
        # 95% CI（先建立，後續取反用）
        ci_lo     = estimate - 1.96 * SE,
        ci_hi     = estimate + 1.96 * SE,
        sig_cat   = case_when(
          p.value < 0.05 ~ "sig",
          p.value < 0.10 ~ "trend",
          TRUE           ~ "ns"
        ),
        sig_label = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          p.value < 0.10  ~ ".",
          TRUE            ~ "ns"
        ),
        direction  = ifelse(estimate >= 0, "positive", "negative"),
        week_label = paste0("S", Week_num)
      ) %>%
      arrange(Week_num)

    if (!show_ns) df <- df %>% filter(sig_cat != "ns")
    if (nrow(df) == 0) return(NULL)

    # ★ 全部取反：統一方向為「第二組 − 第一組」
    # A: Na - Ct（負值 = 鹽害損失）
    # B: NaP - CtP（負值 = 鹽害損失）
    # C: CtP - Ct（正值 = 無鹽時接種效益）
    # D: NaP - Na（正值 = 鹽脅迫下 PGPR 保護效果）
    tmp_lo <- df$ci_lo
    df <- df %>% mutate(estimate = -estimate,
                        ci_lo    = -ci_hi,
                        ci_hi    = -tmp_lo)

    # 顏色：正方向（處理2 > 處理1）深色；負方向反色；ns 灰
    fill_vals <- c(
      "sig_pos"   = "#1a7a4a",  # 深綠（顯著正向）
      "sig_neg"   = "#c0392b",  # 深紅（顯著負向）
      "trend_pos" = "#82c9a0",  # 淺綠（趨勢正向）
      "trend_neg" = "#e8a09a",  # 淺紅（趨勢負向）
      "ns"        = "#cccccc"   # 灰（ns）
    )

    df <- df %>% mutate(
      fill_key = case_when(
        sig_cat == "sig"   & estimate >= 0 ~ "sig_pos",
        sig_cat == "sig"   & estimate  < 0 ~ "sig_neg",
        sig_cat == "trend" & estimate >= 0 ~ "trend_pos",
        sig_cat == "trend" & estimate  < 0 ~ "trend_neg",
        TRUE ~ "ns"
      )
    )

    # ── 上方資訊框：兩個交互作用 F/P ──
    # p 值格式化（簡短顯示）
    fmt_p <- function(p) {
      if (is.na(p) || !is.finite(p)) return("—")
      if (p < 0.001) return("p<0.001")
      if (p < 0.01)  return("p<0.01")
      if (p < 0.05)  return("p<0.05")
      if (p < 0.10)  return("p<0.10")
      sprintf("p=%.3f", p)
    }
    fmt_f <- function(row) sprintf("F=%.2f, %s", row[1,"F value"], fmt_p(row[1,"Pr(>F)"]))

    info_txt <- tryCatch({
      aov_df  <- as.data.frame(lmm_r$anova)
      fac_col <- if ("因子" %in% names(aov_df)) aov_df$因子 else rownames(aov_df)

      # 依 se_key 決定查哪個因子
      is_salt <- se_key %in% c("salt_none", "salt_pgpr")

      # 主效應：Salt 或 Inocu.
      main_pat  <- if (is_salt) paste0("^", long_fa_label(), "$") else paste0("^", long_fb_label(), "$")
      main_row  <- aov_df[grep(main_pat, fac_col, perl=TRUE), ]
      main_lbl  <- if (is_salt) long_fa_label() else long_fb_label()

      # 因子×Week 交互
      inter_pat <- if (is_salt) "^Salt:Week$|Salt.*Week(?!.*Inocu)" else
                                "^Inocu\\.:Week$|Inocu\\..*Week(?!.*Salt)"
      inter_row <- aov_df[grep(inter_pat, fac_col, perl=TRUE), ]
      inter_lbl <- if (is_salt) paste0(long_fa_label(),"×Week") else paste0(long_fb_label(),"×Week")

      f_main  <- if (nrow(main_row)  > 0) fmt_f(main_row)  else "—"
      f_inter <- if (nrow(inter_row) > 0) fmt_f(inter_row) else "—"

      paste0(get_label(param, lang=lang)$ylab, "
",
             main_lbl,  ": ",  f_main,  "
",
             inter_lbl, ": ", f_inter)
    }, error = function(e) get_label(param, lang=lang)$ylab)

    # ── 圖表 ──
    if (nrow(df) == 0 || all(is.na(df$estimate))) return(NULL)
    max_abs <- max(abs(df$estimate), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
    # 星號文字在條形右端外側，需要額外空間
    x_lim   <- c(-max_abs * 1.15, max_abs * 1.55)

    dir_note <- if (se_key %in% c("salt_none","salt_pgpr")) {
      if(lang=="zh") "（負值 = 鹽害；正值 = 鹽促進）"
      else "(neg = salt damage; pos = salt benefit)"
    } else {
      if(lang=="zh") "（正值 = 接種效益；負值 = 接種負效應）"
      else "(pos = inocu. benefit; neg = inocu. cost)"
    }

    p <- ggplot(df, aes(x = estimate,
                        y = factor(week_label,
                                   levels = rev(paste0("S", sort(unique(Week_num))))))) +
      geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.6) +
      geom_col(aes(fill = fill_key), width = 0.65, colour = NA) +
      geom_text(aes(label = sig_label,
                    x     = ifelse(estimate >= 0,
                                   estimate + max_abs * 0.04,
                                   estimate - max_abs * 0.04),
                    hjust = ifelse(estimate >= 0, 0, 1)),
                size = text_size / 2.8, fontface = "bold") +
      scale_fill_manual(values = fill_vals, guide = "none") +
      scale_x_continuous(limits = x_lim,
                         expand = expansion(mult = c(0.02, 0.02))) +
      labs(
        title    = se_display[se_key],
        subtitle = paste0(info_txt, "\n", dir_note),
        x        = paste0("Estimate (", get_label(param, lang=lang)$ylab, ")"),
        y        = if(lang=="zh") "施鹽後週次" else "Week after salt treatment"
      ) +
      theme_classic(base_size = text_size) +
      theme(
        plot.title    = element_text(face = "bold", size = text_size, hjust = 0),
        plot.subtitle = element_text(size = text_size * 0.62,
                                     colour = "grey35", hjust = 0),
        axis.text.y   = element_text(size = text_size * 0.90),
        panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.4)
      )
    if (!inherits(p, "ggplot")) return(NULL)
    p
  }

    # ── 並列 Simple Effect 圖（A+B 或 C+D）──
  make_se_paired_plot <- function(param, em_res, lmm_res, pair,
                                   show_ns, text_size, lang) {
    # pair = "ab"（salt_none + salt_pgpr）或 "cd"（inocu_ctrl + inocu_salt）
    keys <- if (pair == "ab") c("salt_none","salt_pgpr") else c("inocu_ctrl","inocu_salt")

    # 先取得兩個圖的資料，計算共同 x 軸範圍
    get_df <- function(se_key) {
      r <- em_res[[param]]
      if (is.null(r$pw_all)) return(NULL)
      factor_label <- se_factor_map[se_key]
      df <- r$pw_all %>%
        filter(Factor == factor_label) %>%
        mutate(
          Week_num  = as.integer(as.character(Week_salt_factor)),
          ci_lo     = estimate - 1.96 * SE,
          ci_hi     = estimate + 1.96 * SE,
          sig_cat   = case_when(
            p.value < 0.05 ~ "sig", p.value < 0.10 ~ "trend", TRUE ~ "ns"),
          sig_label = case_when(
            p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
            p.value < 0.05 ~ "*",   p.value < 0.10 ~ ".",  TRUE ~ "ns"),
          week_label = paste0("S", Week_num),
          direction  = ifelse(estimate >= 0, "positive", "negative")
        ) %>% arrange(Week_num)
      if (!show_ns) df <- df %>% filter(sig_cat != "ns")
      # 取反（統一方向）
      if (nrow(df) > 0) {
        tmp_lo <- df$ci_lo
        df <- df %>% mutate(estimate = -estimate,
                            ci_lo    = -ci_hi,
                            ci_hi    = -tmp_lo)
      }
      df
    }

    df_a <- get_df(keys[1])
    df_b <- get_df(keys[2])
    if (is.null(df_a) && is.null(df_b)) return(NULL)

    # 共同 x 軸範圍
    all_est <- c(
      if (!is.null(df_a)) df_a$estimate else NULL,
      if (!is.null(df_b)) df_b$estimate else NULL
    )
    max_abs <- max(abs(all_est), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
    x_lim <- c(-max_abs * 1.15, max_abs * 1.85)

    # param_labels 改為動態（各參數用 get_label 取得）
    get_plbl <- function(p) get_label(p, lang=lang)$ylab

    # 標題與 subtitle（三因子 + 兩因子交互）
    fmt_p <- function(p) {
      if (is.na(p)||!is.finite(p)) return("—")
      if (p<0.001) return("p<0.001")
      if (p<0.01)  return("p<0.01")
      if (p<0.05)  return("p<0.05")
      if (p<0.10)  return("p<0.10")
      sprintf("p=%.3f", p)
    }
    fmt_f <- function(row) sprintf("F=%.2f, %s", row[1,"F value"], fmt_p(row[1,"Pr(>F)"]))

    top_subtitle <- tryCatch({
      aov_df  <- as.data.frame(lmm_res[[param]]$anova)
      fac_col <- if ("因子" %in% names(aov_df)) aov_df$因子 else rownames(aov_df)

      is_salt_pair <- (pair == "ab")

      # 依 pair 決定顯示哪些因子
      if (is_salt_pair) {
        # A+B：Salt effects
        # 1. Salt 主效應
        main_row  <- aov_df[grep(paste0("^", long_fa_label(), "$"), fac_col, perl=TRUE), ]
        # 2. Salt×Week
        sw_row    <- aov_df[grep("(?=.*Salt)(?=.*Week)(?!.*Inocu)",
                                 fac_col, perl=TRUE), ]
        # 3. Salt×Inocu.×Week（三因子）
        three_row <- aov_df[grep(paste0("(?=.*",long_fa_label(),")(?=.*",long_fb_label(),")(?=.*Week)"),
                                 fac_col, perl=TRUE), ]
        f_main  <- if (nrow(main_row)>0)  fmt_f(main_row)  else "—"
        f_sw    <- if (nrow(sw_row)>0)    fmt_f(sw_row)    else "—"
        f3      <- if (nrow(three_row)>0) fmt_f(three_row) else "—"
        paste0(get_plbl(param), "
",
               paste0(long_fa_label(),": "), f_main, "  |  ",
               paste0(long_fa_label(),"×Week: "), f_sw, "
",
               paste0(long_fa_label(),"×",long_fb_label(),"×Week: "), f3)
      } else {
        # C+D：Inocu. effects
        # 1. Inocu. 主效應
        main_row  <- aov_df[grepl(paste0("^", long_fb_label(), "$"), fac_col, fixed=FALSE), ]
        # 2. Inocu.×Week
        iw_row    <- aov_df[grep(paste0("(?=.*",long_fb_label(),")(?=.*Week)(?!.*",long_fa_label(),")"),
                                 fac_col, perl=TRUE), ]
        # 3. Salt×Inocu.×Week（三因子）
        three_row <- aov_df[grep(paste0("(?=.*",long_fa_label(),")(?=.*",long_fb_label(),")(?=.*Week)"),
                                 fac_col, perl=TRUE), ]
        f_main  <- if (nrow(main_row)>0)  fmt_f(main_row)  else "—"
        f_iw    <- if (nrow(iw_row)>0)    fmt_f(iw_row)    else "—"
        f3      <- if (nrow(three_row)>0) fmt_f(three_row) else "—"
        paste0(get_plbl(param), "
",
               paste0(long_fb_label(),": "), f_main, "  |  ",
               paste0(long_fb_label(),"×Week: "), f_iw, "
",
               paste0(long_fa_label(),"×",long_fb_label(),"×Week: "), f3)
      }
    }, error=function(e) get_label(param, lang=lang)$ylab)

    fill_vals <- c(
      "sig_pos"="1a7a4a", "sig_neg"="#c0392b",
      "trend_pos"="#82c9a0", "trend_neg"="#e8a09a", "ns"="#cccccc"
    )
    fill_vals <- c(
      sig_pos="#1a7a4a", sig_neg="#c0392b",
      trend_pos="#82c9a0", trend_neg="#e8a09a", ns="#cccccc"
    )

    make_half <- function(df, se_key, show_y_axis = TRUE) {
      if (is.null(df) || nrow(df)==0) return(NULL)
      se_display <- se_factor_map_r()
      df <- df %>% mutate(
        fill_key = case_when(
          sig_cat=="sig"   & estimate>=0 ~ "sig_pos",
          sig_cat=="sig"   & estimate <0 ~ "sig_neg",
          sig_cat=="trend" & estimate>=0 ~ "trend_pos",
          sig_cat=="trend" & estimate <0 ~ "trend_neg",
          TRUE ~ "ns"
        )
      )
      p <- ggplot(df, aes(x=estimate,
                          y=factor(week_label,
                                   levels=rev(paste0("S",sort(unique(Week_num))))))) +
        geom_vline(xintercept=0, colour="grey40", linewidth=0.6) +
        geom_col(aes(fill=fill_key), width=0.65, colour=NA) +
        geom_text(aes(label=sig_label,
                      x=ifelse(estimate>=0, estimate+max_abs*0.04, estimate-max_abs*0.04),
                      hjust=ifelse(estimate>=0, 0, 1)),
                  size=text_size/3.2, fontface="bold") +
        scale_fill_manual(values=fill_vals, guide="none") +
        scale_x_continuous(limits=x_lim, expand=expansion(mult=c(0.02,0.02))) +
        labs(title=se_display[se_key],
             x=paste0("Estimate (", get_plbl(param), ")"),
             y=if(show_y_axis) (if(lang=="zh") "施鹽後週次" else "Week after salt treatment")
               else NULL) +
        theme_classic(base_size=text_size) +
        theme(
          plot.title = element_text(face="bold", size=text_size*0.88, hjust=0),
          axis.title.y = if(show_y_axis) element_text() else element_blank(),
          axis.text.y  = if(show_y_axis) element_text() else element_blank(),
          axis.ticks.y = if(show_y_axis) element_line() else element_blank(),
          panel.grid.major.y = element_line(colour="grey93", linewidth=0.4)
        )
      p
    }

    p_left  <- make_half(df_a, keys[1], show_y_axis=TRUE)
    p_right <- make_half(df_b, keys[2], show_y_axis=FALSE)

    if (is.null(p_left) && is.null(p_right)) return(NULL)
    if (is.null(p_left))  return(p_right)
    if (is.null(p_right)) return(p_left)

    combined <- p_left + p_right +
      patchwork::plot_annotation(
        subtitle = top_subtitle,
        theme = theme(
          plot.subtitle = element_text(size=text_size*0.60, colour="grey35", hjust=0)
        )
      )
    combined
  }

  output$long_forest_ui <- renderUI({
    req(long_emmeans_results(), long_lmm_results(), input$long_forest_params)
    params  <- intersect(input$long_forest_params, names(long_emmeans_results()))
    mode    <- if (!is.null(input$long_se_display_mode)) input$long_se_display_mode else "single"
    se_key  <- if (!is.null(input$long_forest_se)) input$long_forest_se else "inocu_salt"
    plot_outputs <- lapply(params, function(p) {
      lbl <- get_label(p, lang = if(!is.null(input$long_lang)) input$long_lang else "zh")$title
      if (mode == "single") {
        out_id <- paste0("long_se_plot_", p, "_", se_key)
        dl_id  <- paste0("long_se_dl_",   p, "_", se_key)
        tagList(
          fluidRow(
            column(8, h5(paste0("▌ ", lbl))),
            column(4, style="text-align:right;",
              downloadButton(dl_id, paste0("下載 ", lbl, " PNG"),
                             class="btn-sm btn-default"))
          ),
          plotOutput(out_id, height="360px"),
          br()
        )
      } else {
        out_id <- paste0("long_se_paired_", p, "_", mode)
        dl_id  <- paste0("long_se_paired_dl_", p, "_", mode)
        pair_lbl <- if (mode=="ab") "A+B (Factor A effects)" else "C+D (Factor B effects)"
        tagList(
          fluidRow(
            column(8, h5(paste0("▌ ", lbl, "  [", pair_lbl, "]"))),
            column(4, style="text-align:right;",
              downloadButton(dl_id, paste0("下載 ", lbl, " PNG"),
                             class="btn-sm btn-default"))
          ),
          plotOutput(out_id, height="400px"),
          br()
        )
      }
    })
    do.call(tagList, plot_outputs)
  })

  observe({
    req(long_emmeans_results(), long_lmm_results(), input$long_forest_params)
    params  <- intersect(input$long_forest_params, names(long_emmeans_results()))
    mode    <- if (!is.null(input$long_se_display_mode)) input$long_se_display_mode else "single"
    se_key  <- if (!is.null(input$long_forest_se)) input$long_forest_se else "inocu_salt"

    lapply(params, function(p) {
      local({
        param  <- p
        sekey  <- se_key
        dmode  <- mode

        if (dmode == "single") {
          # ── 單圖模式 ──
          out_id <- paste0("long_se_plot_", param, "_", sekey)
          dl_id  <- paste0("long_se_dl_",   param, "_", sekey)

          output[[out_id]] <- renderPlot({
            make_se_bar_plot(
              param=param, em_res=long_emmeans_results(),
              lmm_res=long_lmm_results(), se_key=sekey,
              show_ns=isTRUE(input$long_forest_show_ns),
              text_size=input$long_text_size, lang=input$long_label_lang)
          })
          output[[dl_id]] <- downloadHandler(
            filename=function() paste0("se_bar_",param,"_",sekey,"_",Sys.Date(),".png"),
            content=function(file) {
              p_plot <- make_se_bar_plot(
                param=param, em_res=long_emmeans_results(),
                lmm_res=long_lmm_results(), se_key=sekey,
                show_ns=isTRUE(input$long_forest_show_ns),
                text_size=input$long_text_size, lang=input$long_label_lang)
              ggsave(file, plot=p_plot, device="png",
                     width=input$long_plot_width, height=input$long_plot_height,
                     dpi=input$long_plot_dpi)
            }
          )
        } else {
          # ── 並列模式（A+B 或 C+D）──
          out_id <- paste0("long_se_paired_", param, "_", dmode)
          dl_id  <- paste0("long_se_paired_dl_", param, "_", dmode)

          output[[out_id]] <- renderPlot({
            make_se_paired_plot(
              param=param, em_res=long_emmeans_results(),
              lmm_res=long_lmm_results(), pair=dmode,
              show_ns=isTRUE(input$long_forest_show_ns),
              text_size=input$long_text_size, lang=input$long_label_lang)
          })
          output[[dl_id]] <- downloadHandler(
            filename=function() paste0("se_paired_",param,"_",dmode,"_",Sys.Date(),".png"),
            content=function(file) {
              p_plot <- make_se_paired_plot(
                param=param, em_res=long_emmeans_results(),
                lmm_res=long_lmm_results(), pair=dmode,
                show_ns=isTRUE(input$long_forest_show_ns),
                text_size=input$long_text_size, lang=input$long_label_lang)
              ggsave(file, plot=p_plot, device="png",
                     width=input$long_plot_width*2,
                     height=input$long_plot_height,
                     dpi=input$long_plot_dpi)
            }
          )
        }
      })
    })
  })

  output$long_download_forest <- downloadHandler(
    filename    = function() paste0("se_bar_plots_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content     = function(file) {
      req(long_emmeans_results(), long_lmm_results(), input$long_forest_params)
      params <- intersect(input$long_forest_params, names(long_emmeans_results()))
      dmode  <- if (!is.null(input$long_se_display_mode)) input$long_se_display_mode else "single"
      se_key <- if (!is.null(input$long_forest_se)) input$long_forest_se else "inocu_salt"
      tmp_dir   <- tempdir()
      png_files <- c()

      for (p in params) {
        if (dmode == "single") {
          # 單圖模式
          png_path <- file.path(tmp_dir,
                                paste0("se_bar_", p, "_", se_key, "_",
                                       Sys.Date(), ".png"))
          p_plot <- make_se_bar_plot(
            param=p, em_res=long_emmeans_results(),
            lmm_res=long_lmm_results(), se_key=se_key,
            show_ns=isTRUE(input$long_forest_show_ns),
            text_size=input$long_text_size, lang=input$long_label_lang)
          ggsave(png_path, plot=p_plot, device="png",
                 width=input$long_plot_width, height=input$long_plot_height,
                 dpi=input$long_plot_dpi)
        } else {
          # 並列模式（A+B 或 C+D）
          png_path <- file.path(tmp_dir,
                                paste0("se_paired_", p, "_", dmode, "_",
                                       Sys.Date(), ".png"))
          p_plot <- make_se_paired_plot(
            param=p, em_res=long_emmeans_results(),
            lmm_res=long_lmm_results(), pair=dmode,
            show_ns=isTRUE(input$long_forest_show_ns),
            text_size=input$long_text_size, lang=input$long_label_lang)
          ggsave(png_path, plot=p_plot, device="png",
                 width=input$long_plot_width * 2,
                 height=input$long_plot_height,
                 dpi=input$long_plot_dpi)
        }
        if (!is.null(p_plot)) png_files <- c(png_files, png_path)
      }
      tryCatch(
        zip::zipr(file, png_files),
        error = function(e) {
          wd <- getwd()
          setwd(dirname(png_files[1]))
          utils::zip(file, basename(png_files), flags = "-j")
          setwd(wd)
        }
      )
    }
  )

  # ════════════════════════════════════════════════════════════
  # ★ 縱貫分析 Server 區塊 結束
  # ════════════════════════════════════════════════════════════

} # end server

shinyApp(ui, server)
