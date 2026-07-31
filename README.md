# TFRI-Tools

林業試驗所育林組內部工具集,包含實驗室計算機與行政作業小工具,皆為 Python + Tkinter 桌面 GUI 程式。

## 環境需求

**Python 工具**
- Python 3.9 以上
- 安裝套件:
```bash
pip install pillow numpy
```
(`tkinter` 為 Python 內建,通常不需另外安裝)

**R 統計工具**
- R 4.x 以上,需安裝以下套件:
```r
install.packages(c("shiny","tidyverse","car","rstatix","multcompView",
                    "ggpubr","agricolae","multcomp","corrplot","vegan",
                    "mice","lme4","lmerTest","emmeans","effectsize",
                    "patchwork","MuMIn","nlme","ggrepel","showtext",
                    "knitr","kableExtra","plotly","sortable","zip"))
# pairwiseAdonis 需另從 GitHub 安裝:
# remotes::install_github("pmartinezarbizu/pairwiseAdonis")
```

## 資料夾結構

```
TFRI-Tools/
├── lab-calculators/      實驗室計算工具 (Python)
│   ├── medium_recipe.py
│   ├── qpcr_calculator.py
│   └── buffer_calculator.py
├── office-tools/         行政/文書工具 (Python)
│   ├── TFRI人員爬蟲_GUI.py
│   ├── 三摺桌牌.py
│   ├── 對摺桌牌.py
│   ├── invitation_gui.py
│   └── stamp_gui.py
└── statistics/           統計分析工具 (R Shiny)
    └── Factorial_research_3factor.R
```

## 工具說明

### 實驗室計算工具

| 工具 | 用途 | 執行方式 |
|---|---|---|
| `medium_recipe.py` | 培養基配方換算(LB / YEP / TY),支援自訂體積與額外添加 NaCl 比例 | `python medium_recipe.py` |
| `qpcr_calculator.py` | qPCR 藥品用量與樣本稀釋計算,自動產生 Master Mix 配置清單、樣本稀釋清單、標準品稀釋提醒,可另存文字報告 | `python qpcr_calculator.py` |
| `buffer_calculator.py` | 磷酸鈉鹽緩衝液配製計算(查表法),依目標體積/濃度/pH 換算所需母液體積 | `python buffer_calculator.py` |

### 行政/文書工具

| 工具 | 用途 | 執行方式 |
|---|---|---|
| `TFRI人員爬蟲_GUI.py` | 所內人員資訊查詢/爬取工具(GUI 介面) | `python TFRI人員爬蟲_GUI.py` |
| `三摺桌牌.py` | 三摺式桌牌產生器 | `python 三摺桌牌.py` |
| `對摺桌牌.py` | 對摺式桌牌產生器 | `python 對摺桌牌.py` |
| `invitation_gui.py` | 活動邀請函批次產生與寄送(PDF 產生 + Gmail SMTP 群發) | `python invitation_gui.py` |
| `stamp_gui.py` | 電子印章產生器,輸入姓名可產生長方橫式章與標楷體正方章 | `python stamp_gui.py` |

### 統計分析工具 (R Shiny)

| 工具 | 用途 | 執行方式 |
|---|---|---|
| `Factorial_research_3factor.R` | 三因子田間試驗統計分析平台。支援 RCBD 區集設計,ANOVA / LMM(混合模型)雙軌分析並依殘差診斷自動切換,Tukey / LSD / Dunn 事後檢定與 CLD 分組字母標示;提供 η² 效應量、R²m/R²c、縱貫分析(含 AR(1) 自相關結構)、森林圖(forest plot)、3D 互動圖(plotly),分析結果可批次匯出 PNG/CSV(UTF-8 BOM,Excel 中文顯示正常) | 於 R/RStudio 中開啟後執行 `shiny::runApp("Factorial_research_3factor.R")`,或直接開檔按「Run App」 |

## 注意事項

- `stamp_gui.py` 預設搜尋 Windows 系統字型路徑(標楷體、微軟正黑體),在非 Windows 系統執行時需自行指定字型檔,或於程式啟動時依提示手動選擇字型檔。
- `invitation_gui.py` 若涉及 Gmail SMTP 寄送,需自行設定應用程式密碼,請勿將帳密寫死於程式碼或一併提交至倉庫。
- `TFRI人員爬蟲_GUI.py` 僅供內部行政用途,爬取結果(如輸出的 Excel 檔)請勿提交至倉庫。
- `Factorial_research_3factor.R` 內建 `label_map` 為使用者自訂欄位中英文對照表,預設為空,使用前建議依實際資料欄位名稱填入,否則系統會自動嘗試從欄位名稱解析顯示名稱與單位。
- 統計分析用的原始資料(CSV/Excel 實驗數據)請勿提交至倉庫,避免未發表數據外流;僅上傳分析程式碼本身。

## 授權

僅供林業試驗所育林組內部使用。
