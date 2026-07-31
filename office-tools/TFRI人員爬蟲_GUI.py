"""
農業部林業試驗所 人員資料爬蟲 GUI 版
=====================================
執行：python TFRI人員爬蟲_GUI.py
"""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox
import threading
import requests
from bs4 import BeautifulSoup
import openpyxl
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter
from collections import Counter
from datetime import datetime
import time
import re
import os

# ── 爬蟲設定 ────────────────────────────────────────────
BASE_URL = "https://www.tfri.gov.tw"
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
                  "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "zh-TW,zh;q=0.9,en;q=0.8",
    "Referer": BASE_URL,
}
PAGES = [
    {"類別": "技術單位", "url": f"{BASE_URL}/cp.aspx?n=7712&t=175"},
    {"類別": "研究中心", "url": f"{BASE_URL}/cp.aspx?n=7713&t=188"},
    {"類別": "行政單位", "url": f"{BASE_URL}/cp.aspx?n=7463&t=25"},
]
UNIT_COLORS = {"技術單位": "D6E4F0", "研究中心": "E8F5E9", "行政單位": "FFF3E0"}
UNIT_NAMES = {
    "育林組", "森林經營組", "森林保護組", "森林生態組", "林產利用組", "技術服務組",
    "恆春研究中心", "嘉義研究中心", "蓮華池研究中心", "太麻里研究中心", "六龜研究中心", "福山研究中心",
    "秘書室", "主計室", "人事室", "政風室",
}
LINCHAN_FOREST_BLDG = {"洪昆源", "陳盈如", "吳芯慧", "鄭惠蘭"}
LOC_COLORS = {
    "行政大樓": "FFF9C4", "森林研究大樓": "E8F5E9", "和平大樓": "FCE4EC",
    "技術服務大樓": "E3F2FD", "林產利用大樓": "EDE7F6", "不確定": "F5F5F5",
}

# ── 爬蟲核心函式 ─────────────────────────────────────────
def classify_location(unit, name, ext):
    if not ext:
        return "不確定"
    ext = ext.strip()
    if not ext.isdigit() or len(ext) != 4:
        return "不確定"
    first = ext[0]
    if unit == "林產利用組":
        if name in LINCHAN_FOREST_BLDG:
            return "森林研究大樓"
        if first == "3":
            return "技術服務大樓"
        return "林產利用大樓"
    if first == "1": return "行政大樓"
    elif first == "2": return "森林研究大樓"
    elif first == "3":
        return "和平大樓" if ext[:3] in ("383", "384") else "技術服務大樓"
    return "不確定"

def fetch_page(session, url):
    resp = session.get(url, headers=HEADERS, timeout=20)
    resp.encoding = "utf-8"
    if resp.status_code != 200:
        raise RuntimeError(f"HTTP {resp.status_code}")
    return BeautifulSoup(resp.text, "html.parser")

def parse_phone_from_detail(soup):
    for span in soup.find_all("span"):
        text = span.get_text(strip=True)
        if re.search(r'\d{2}[-\d]+.*ext', text, re.IGNORECASE):
            ext_m = re.search(r'ext\s*\.?\s*(\d+)', text, re.IGNORECASE)
            return text, (ext_m.group(1) if ext_m else "")
    for li in soup.find_all("li"):
        text = li.get_text(strip=True)
        if "電話" in text and ("分機" in text or "ext" in text.lower()):
            ext_m = re.search(r'(?:分機|ext)\s*:?\s*(\d+)', text, re.IGNORECASE)
            return text, (ext_m.group(1) if ext_m else "")
    return "", ""

def parse_detail_url_from_li(li):
    a = li.find("a", href=re.compile(r"News_Content_Table"))
    if a:
        href = a["href"]
        if href.startswith("http"):
            return href
        if not href.startswith("/"):
            href = "/" + href
        return BASE_URL + href
    return ""

def parse_staff(soup, category):
    records = []
    current_unit = ""
    for tag in soup.find_all(["h3", "li"]):
        if tag.name == "h3":
            text = tag.get_text(strip=True)
            if text in UNIT_NAMES:
                current_unit = text
            continue
        if not current_unit:
            continue
        raw = tag.get_text(separator="", strip=True)
        if "姓名" not in raw or "職稱" not in raw or "電子信箱" not in raw:
            continue
        name_m   = re.search(r'姓名(.+?)職稱',    raw)
        title_m  = re.search(r'職稱(.+?)電子信箱', raw)
        email_m  = re.search(r'電子信箱(\S+?)個人資料', raw)
        duties_m = re.search(r'個人資料(.+)$',     raw)
        if not name_m:
            continue
        records.append({
            "類別": category, "單位": current_unit,
            "姓名": name_m.group(1).strip(),
            "職稱": title_m.group(1).strip()  if title_m  else "",
            "Email": email_m.group(1).strip() if email_m  else "",
            "研究領域／業務": duties_m.group(1).strip() if duties_m else "",
            "電話（含分機）": "", "分機": "", "上班處所": "",
            "_detail_url": parse_detail_url_from_li(tag),
        })
    return records

def write_excel(records, output_path):
    wb = openpyxl.Workbook()
    thin  = Side(style="thin",   color="BBBBBB")
    thick = Side(style="medium", color="1F4E79")
    b_data   = Border(left=thin,  right=thin,  top=thin,  bottom=thin)
    b_header = Border(left=thick, right=thick, top=thick, bottom=thick)
    H_FILL  = PatternFill("solid", start_color="1F4E79")
    H_FONT  = Font(name="Arial", bold=True, color="FFFFFF", size=11)
    H_ALIGN = Alignment(horizontal="center", vertical="center", wrap_text=True)
    D_FONT  = Font(name="Arial", size=10)
    C_ALIGN = Alignment(horizontal="center", vertical="top")
    W_ALIGN = Alignment(vertical="top", wrap_text=True)

    ws = wb.active
    ws.title = "人員名單"
    ws.append([f"資料更新時間：{datetime.now().strftime('%Y-%m-%d %H:%M')}　　來源：農業部林業試驗所"])
    ws.merge_cells("A1:I1")
    ws["A1"].font = Font(name="Arial", italic=True, color="888888", size=9)
    ws["A1"].alignment = Alignment(horizontal="right")
    ws.row_dimensions[1].height = 14

    cols   = ["類別", "單位", "姓名", "職稱", "電話（含分機）", "分機", "上班處所", "Email", "研究領域／業務"]
    widths = [10,     18,     8,      18,     22,               8,      14,         28,      55]
    ws.append(cols)
    for ci, (col, w) in enumerate(zip(cols, widths), 1):
        cell = ws.cell(row=2, column=ci)
        cell.value = col; cell.font = H_FONT; cell.fill = H_FILL
        cell.alignment = H_ALIGN; cell.border = b_header
        ws.column_dimensions[get_column_letter(ci)].width = w
    ws.row_dimensions[2].height = 22

    for ri, rec in enumerate(records, 3):
        ws.append([rec["類別"], rec["單位"], rec["姓名"], rec["職稱"],
                   rec["電話（含分機）"], rec["分機"], rec["上班處所"],
                   rec["Email"], rec["研究領域／業務"]])
        unit_fill = PatternFill("solid", start_color=UNIT_COLORS.get(rec["類別"], "FFFFFF"))
        loc_fill  = PatternFill("solid", start_color=LOC_COLORS.get(rec["上班處所"], "FFFFFF"))
        for ci in range(1, 10):
            cell = ws.cell(row=ri, column=ci)
            cell.font = D_FONT; cell.border = b_data
            cell.fill = loc_fill if ci == 7 else unit_fill
            cell.alignment = C_ALIGN if ci <= 7 else W_ALIGN
        ws.row_dimensions[ri].height = 40
    ws.freeze_panes    = "A3"
    ws.auto_filter.ref = f"A2:I{len(records)+2}"

    ws2 = wb.create_sheet("單位統計")
    for c, w in zip("ABCD", [12, 22, 8, 14]):
        ws2.column_dimensions[c].width = w
    ws2.append(["類別", "單位", "人數", "主要上班處所"])
    for ci in range(1, 5):
        cell = ws2.cell(row=1, column=ci)
        cell.font = H_FONT; cell.fill = H_FILL
        cell.alignment = H_ALIGN; cell.border = b_header
    ws2.row_dimensions[1].height = 22
    unit_count = Counter((r["類別"], r["單位"]) for r in records)
    ri = 2
    for (cat, unit), cnt in sorted(unit_count.items()):
        locs = [r["上班處所"] for r in records if r["單位"] == unit]
        main_loc = Counter(locs).most_common(1)[0][0] if locs else ""
        ws2.append([cat, unit, cnt, main_loc])
        fill = PatternFill("solid", start_color=UNIT_COLORS.get(cat, "FFFFFF"))
        for ci in range(1, 5):
            cell = ws2.cell(row=ri, column=ci)
            cell.font = D_FONT; cell.fill = fill; cell.border = b_data
            cell.alignment = Alignment(horizontal="center", vertical="center")
        ri += 1
    ws2.append(["合計", "", f"=SUM(C2:C{ri-1})", ""])
    for ci in range(1, 5):
        cell = ws2.cell(row=ri, column=ci)
        cell.font = Font(name="Arial", bold=True, size=10); cell.border = b_data
        cell.alignment = Alignment(horizontal="center", vertical="center")

    ws3 = wb.create_sheet("處所統計")
    ws3.column_dimensions["A"].width = 18
    ws3.column_dimensions["B"].width = 8
    ws3.append(["上班處所", "人數"])
    for ci in range(1, 3):
        cell = ws3.cell(row=1, column=ci)
        cell.font = H_FONT; cell.fill = H_FILL
        cell.alignment = H_ALIGN; cell.border = b_header
    loc_count = Counter(r["上班處所"] for r in records)
    ri = 2
    for loc, cnt in sorted(loc_count.items()):
        ws3.append([loc, cnt])
        fill = PatternFill("solid", start_color=LOC_COLORS.get(loc, "FFFFFF"))
        for ci in range(1, 3):
            cell = ws3.cell(row=ri, column=ci)
            cell.font = D_FONT; cell.fill = fill; cell.border = b_data
            cell.alignment = Alignment(horizontal="center", vertical="center")
        ri += 1

    wb.save(output_path)


# ── GUI ─────────────────────────────────────────────────
class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("農業部林業試驗所 人員資料爬蟲")
        self.resizable(False, False)
        self.configure(bg="#F0F4F8")
        self._running = False
        self._build_ui()
        self._center_window(620, 580)

    def _center_window(self, w, h):
        sw = self.winfo_screenwidth()
        sh = self.winfo_screenheight()
        self.geometry(f"{w}x{h}+{(sw-w)//2}+{(sh-h)//2}")

    def _build_ui(self):
        # ── 標題列 ──
        header = tk.Frame(self, bg="#1F4E79", height=60)
        header.pack(fill="x")
        header.pack_propagate(False)
        tk.Label(header, text="🌲  農業部林業試驗所 人員資料爬蟲",
                 font=("Microsoft JhengHei", 14, "bold"),
                 bg="#1F4E79", fg="white").pack(expand=True)

        body = tk.Frame(self, bg="#F0F4F8", padx=20, pady=15)
        body.pack(fill="both", expand=True)

        # ── 儲存路徑 ──
        path_frame = tk.LabelFrame(body, text="  儲存設定  ",
                                   font=("Microsoft JhengHei", 10, "bold"),
                                   bg="#F0F4F8", fg="#1F4E79", padx=10, pady=8)
        path_frame.pack(fill="x", pady=(0, 12))

        self.save_path = tk.StringVar(value=os.path.join(
            os.path.expanduser("~"), "Desktop",
            f"TFRI人員名單_{datetime.now().strftime('%Y%m%d')}.xlsx"))

        path_entry = tk.Entry(path_frame, textvariable=self.save_path,
                              font=("Consolas", 9), width=55)
        path_entry.grid(row=0, column=0, sticky="ew", padx=(0, 8))
        tk.Button(path_frame, text="瀏覽…",
                  font=("Microsoft JhengHei", 9),
                  bg="#E3F2FD", relief="flat", cursor="hand2",
                  command=self._browse).grid(row=0, column=1)
        path_frame.columnconfigure(0, weight=1)

        # ── 進度區 ──
        prog_frame = tk.LabelFrame(body, text="  執行進度  ",
                                   font=("Microsoft JhengHei", 10, "bold"),
                                   bg="#F0F4F8", fg="#1F4E79", padx=10, pady=8)
        prog_frame.pack(fill="both", expand=True, pady=(0, 12))

        # 整體進度條
        top_prog = tk.Frame(prog_frame, bg="#F0F4F8")
        top_prog.pack(fill="x", pady=(0, 6))
        self.status_label = tk.Label(top_prog, text="就緒",
                                     font=("Microsoft JhengHei", 10),
                                     bg="#F0F4F8", fg="#333", anchor="w")
        self.status_label.pack(side="left")
        self.pct_label = tk.Label(top_prog, text="",
                                  font=("Microsoft JhengHei", 10, "bold"),
                                  bg="#F0F4F8", fg="#1F4E79", anchor="e")
        self.pct_label.pack(side="right")

        self.progress = ttk.Progressbar(prog_frame, length=560,
                                        mode="determinate", maximum=100)
        self.progress.pack(fill="x", pady=(0, 8))

        # 目前處理的人員
        self.current_label = tk.Label(prog_frame, text="",
                                      font=("Microsoft JhengHei", 9),
                                      bg="#F0F4F8", fg="#555")
        self.current_label.pack(anchor="w")

        # 捲動式 log
        log_frame = tk.Frame(prog_frame, bg="#F0F4F8")
        log_frame.pack(fill="both", expand=True, pady=(6, 0))
        self.log = tk.Text(log_frame, height=12, state="disabled",
                           font=("Consolas", 8), bg="#1A1A2E", fg="#A8D8A8",
                           relief="flat", wrap="none")
        sb_y = ttk.Scrollbar(log_frame, orient="vertical", command=self.log.yview)
        sb_x = ttk.Scrollbar(log_frame, orient="horizontal", command=self.log.xview)
        self.log.configure(yscrollcommand=sb_y.set, xscrollcommand=sb_x.set)
        sb_y.pack(side="right", fill="y")
        sb_x.pack(side="bottom", fill="x")
        self.log.pack(fill="both", expand=True)

        # ── 統計區 ──
        self.stat_frame = tk.LabelFrame(body, text="  統計結果  ",
                                        font=("Microsoft JhengHei", 10, "bold"),
                                        bg="#F0F4F8", fg="#1F4E79", padx=10, pady=6)
        self.stat_frame.pack(fill="x", pady=(0, 12))
        self.stat_label = tk.Label(self.stat_frame, text="—",
                                   font=("Microsoft JhengHei", 9),
                                   bg="#F0F4F8", fg="#555", justify="left")
        self.stat_label.pack(anchor="w")

        # ── 按鈕列 ──
        btn_frame = tk.Frame(body, bg="#F0F4F8")
        btn_frame.pack(fill="x")

        self.run_btn = tk.Button(btn_frame, text="▶  開始抓取",
                                 font=("Microsoft JhengHei", 11, "bold"),
                                 bg="#1F4E79", fg="white", relief="flat",
                                 padx=20, pady=8, cursor="hand2",
                                 command=self._start)
        self.run_btn.pack(side="left", padx=(0, 10))

        self.open_btn = tk.Button(btn_frame, text="📂  開啟檔案",
                                  font=("Microsoft JhengHei", 10),
                                  bg="#E8F5E9", fg="#2E7D32", relief="flat",
                                  padx=16, pady=8, cursor="hand2",
                                  state="disabled", command=self._open_file)
        self.open_btn.pack(side="left")

        tk.Button(btn_frame, text="清除 Log",
                  font=("Microsoft JhengHei", 9),
                  bg="#F5F5F5", fg="#888", relief="flat",
                  padx=10, pady=8, cursor="hand2",
                  command=self._clear_log).pack(side="right")

    def _browse(self):
        path = filedialog.asksaveasfilename(
            defaultextension=".xlsx",
            filetypes=[("Excel 檔案", "*.xlsx")],
            initialfile=f"TFRI人員名單_{datetime.now().strftime('%Y%m%d')}.xlsx",
            title="選擇儲存位置")
        if path:
            self.save_path.set(path)

    def _log(self, msg, color=None):
        self.log.configure(state="normal")
        self.log.insert("end", msg + "\n")
        self.log.see("end")
        self.log.configure(state="disabled")

    def _clear_log(self):
        self.log.configure(state="normal")
        self.log.delete("1.0", "end")
        self.log.configure(state="disabled")

    def _set_status(self, text, pct=None, current=""):
        self.status_label.config(text=text)
        if pct is not None:
            self.progress["value"] = pct
            self.pct_label.config(text=f"{pct:.0f}%" if pct > 0 else "")
        self.current_label.config(text=current)
        self.update_idletasks()

    def _open_file(self):
        path = self.save_path.get()
        if os.path.exists(path):
            os.startfile(path)

    def _start(self):
        if self._running:
            return
        path = self.save_path.get().strip()
        if not path:
            messagebox.showwarning("提示", "請先選擇儲存路徑")
            return
        self._running = True
        self.run_btn.config(state="disabled", text="執行中…", bg="#999")
        self.open_btn.config(state="disabled")
        self.stat_label.config(text="—")
        self._clear_log()
        threading.Thread(target=self._run, args=(path,), daemon=True).start()

    def _run(self, output_path):
        try:
            self._set_status("連線中…", 0)
            self._log("=" * 55)
            self._log(f"  農業部林業試驗所人員資料爬蟲")
            self._log(f"  {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
            self._log("=" * 55)

            session = requests.Session()
            session.get(BASE_URL + "/", headers=HEADERS, timeout=20)
            time.sleep(0.8)

            all_records = []

            # 第一階段：抓組別列表（進度 0–20%）
            for i, page in enumerate(PAGES):
                self._set_status(f"抓取 {page['類別']} 列表…",
                                 pct=i * 7)
                self._log(f"\n▶ {page['類別']}")
                soup = fetch_page(session, page["url"])
                records = parse_staff(soup, page["類別"])
                self._log(f"   → {len(records)} 人")
                all_records.extend(records)
                time.sleep(0.8)

            total = len(all_records)
            self._log(f"\n共 {total} 人，開始抓個人電話…\n")

            # 第二階段：逐人抓電話（進度 20–95%）
            for idx, rec in enumerate(all_records, 1):
                pct = 20 + (idx / total) * 75
                url = rec.pop("_detail_url", "")
                if url:
                    try:
                        soup = fetch_page(session, url)
                        phone_full, ext = parse_phone_from_detail(soup)
                        rec["電話（含分機）"] = phone_full
                        rec["分機"]          = ext
                        rec["上班處所"]      = classify_location(
                                                rec["單位"], rec["姓名"], ext)
                        msg = (f"  [{idx:3d}/{total}] {rec['姓名']}"
                               f" ({rec['單位']}) → 分機:{ext or '無'}"
                               f" → {rec['上班處所']}")
                        self._log(msg)
                    except Exception as e:
                        rec["上班處所"] = "不確定"
                        self._log(f"  [{idx:3d}/{total}] {rec['姓名']} ✗ {e}")
                else:
                    rec["上班處所"] = classify_location(
                                        rec["單位"], rec["姓名"], "")

                self._set_status(
                    f"抓取電話中… ({idx}/{total})", pct=pct,
                    current=f"目前：{rec['姓名']}　{rec['單位']}")
                time.sleep(0.4)

            # 第三階段：輸出 Excel
            self._set_status("寫入 Excel…", pct=96)
            self._log(f"\n輸出中：{output_path}")
            write_excel(all_records, output_path)

            # 統計
            loc_count  = Counter(r["上班處所"] for r in all_records)
            unit_count = Counter(r["單位"]     for r in all_records)
            stat_lines = [f"共 {total} 人\n"]
            stat_lines += [f"  {loc}：{cnt} 人" for loc, cnt in sorted(loc_count.items())]
            self.stat_label.config(text="\n".join(stat_lines))

            self._log("\n── 上班處所統計 ──")
            for loc, cnt in sorted(loc_count.items()):
                self._log(f"  {loc}：{cnt} 人")
            self._log(f"\n✅ 完成！檔案已儲存至：\n   {output_path}")

            self._set_status("✅ 完成！", pct=100, current="")
            self.open_btn.config(state="normal")
            messagebox.showinfo("完成",
                f"成功匯出 {total} 筆人員資料！\n\n{output_path}")

        except Exception as e:
            self._log(f"\n❌ 錯誤：{e}")
            self._set_status("❌ 發生錯誤", current=str(e))
            messagebox.showerror("錯誤", str(e))
        finally:
            self._running = False
            self.run_btn.config(state="normal", text="▶  開始抓取", bg="#1F4E79")


if __name__ == "__main__":
    app = App()
    app.mainloop()
