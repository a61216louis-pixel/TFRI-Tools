#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
A4 對折桌牌產生器 — 視覺化 GUI（升級三列架構：單位 -> 職稱 -> 姓名）
執行：python 桌牌.py
需求：pip install reportlab
"""

import os
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from tkinter import font as tkfont
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import mm
from reportlab.pdfgen import canvas as rl_canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

# ── 字型候選 ─────────────────────────────────────────────────────────────────
FONT_CANDIDATES = [
    ("C:/Windows/Fonts/msjh.ttc",                               "MSJH"),
    ("C:/Windows/Fonts/mingliu.ttc",                            "MingLiu"),
    ("C:/Windows/Fonts/kaiu.ttf",                               "KaiU"),
    ("/usr/share/fonts/opentype/noto/NotoSansCJK-Black.ttc",    "NotoBlack"),
    ("/usr/share/fonts/opentype/noto/NotoSerifCJK-Bold.ttc",    "NotoSerifBold"),
    ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc",  "NotoRegular"),
    ("/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc",            "WQYZenHei"),
]

_registered_font = None

def get_font():
    global _registered_font
    if _registered_font:
        return _registered_font
    for path, name in FONT_CANDIDATES:
        if os.path.exists(path):
            try:
                pdfmetrics.registerFont(TTFont(name, path))
                _registered_font = name
                return name
            except Exception:
                continue
    raise RuntimeError("系統找不到可用的中文字型")

# ── PDF 繪製 ─────────────────────────────────────────────────────────────────
def draw_panel(c, font_name, name, subtitle, dept, w, h, border_style, fixed_fs, is_upside_down):
    """
    繪製單個文字面。
    is_upside_down = True  -> 上半部（倒立面）
    is_upside_down = False -> 下半部（正立面）
    """
    margin = 8 * mm
    c.setFillColorRGB(1, 1, 1)
    c.rect(0, 0, w, h, fill=1, stroke=0)

    # 繪製邊框
    if border_style != "none":
        c.setStrokeColorRGB(0.15, 0.15, 0.15)
        c.setLineWidth(1.5)
        c.rect(margin, margin, w - 2 * margin, h - 2 * margin, fill=0, stroke=1)
        if border_style == "double":
            inn = 5 * mm
            c.rect(margin + inn, margin + inn, w - 2 * (margin + inn), h - 2 * (margin + inn), fill=0, stroke=1)

    # 計算字型大小
    text_max_w = w - 24 * mm
    # 根據是否有其他附屬文字調整姓名最大高度
    has_others = bool(subtitle or dept)
    text_max_h = h * 0.55 if has_others else h * 0.7

    if fixed_fs:
        name_fs = fixed_fs
        while name_fs > 12 and c.stringWidth(name, font_name, name_fs) > text_max_w:
            name_fs -= 2
    else:
        for fs in range(260, 12, -2):
            if c.stringWidth(name, font_name, fs) <= text_max_w and fs <= text_max_h:
                break
        name_fs = fs

    sub_fs = min(max(12, name_fs // 4), 32) if subtitle else 0
    dept_fs = min(max(12, name_fs // 5), 26) if dept else 0
    gap = 6 * mm

    cx = w / 2
    cy = h / 2

    c.saveState()
    c.translate(cx, cy)  # 將原點移到該面中心

    if is_upside_down:
        c.rotate(180)  # 上半部旋轉 180 度

    # 在局部座標系中（Y 軸正方向固定為文字視覺上方）：
    # 排序由上至下：單位(dept) -> 職稱(subtitle) -> 姓名(name)
    total_text_h = name_fs
    if subtitle: total_text_h += gap + sub_fs
    if dept:     total_text_h += gap + dept_fs

    # 計算各自的相對垂直起點
    current_top = total_text_h / 2
    
    if dept:
        dept_y = current_top - dept_fs
        current_top -= (dept_fs + gap)
    else:
        dept_y = 0

    if subtitle:
        sub_y = current_top - sub_fs
        current_top -= (sub_fs + gap)
    else:
        sub_y = 0

    name_y = current_top - name_fs + (name_fs * 0.15) # 微調字體基線

    # 繪製文字
    c.setFillColorRGB(0.05, 0.05, 0.05)
    
    # 1. 單位
    if dept:
        c.setFont(font_name, dept_fs)
        c.setFillColorRGB(0.4, 0.4, 0.4)
        c.drawCentredString(0, dept_y, dept)
    
    # 2. 職稱
    if subtitle:
        c.setFont(font_name, sub_fs)
        c.setFillColorRGB(0.3, 0.3, 0.3)
        c.drawCentredString(0, sub_y, subtitle)

    # 3. 姓名
    c.setFont(font_name, name_fs)
    c.setFillColorRGB(0.05, 0.05, 0.05)
    c.drawCentredString(0, name_y, name)

    c.restoreState()

def make_pdf(entries, border_style, output_path, fixed_fs=None):
    font_name = get_font()
    page_w, page_h = A4  # 210mm x 297mm (直向)
    half_h = page_h / 2

    c = rl_canvas.Canvas(output_path, pagesize=(page_w, page_h))

    for name, subtitle, dept in entries:
        # 上半部（倒立面）
        c.saveState()
        c.translate(0, half_h)
        draw_panel(c, font_name, name, subtitle, dept, page_w, half_h, border_style, fixed_fs, is_upside_down=True)
        c.restoreState()

        # 下半部（正立面）
        c.saveState()
        c.translate(0, 0)
        draw_panel(c, font_name, name, subtitle, dept, page_w, half_h, border_style, fixed_fs, is_upside_down=False)
        c.restoreState()

        # 中間折線
        c.saveState()
        c.setStrokeColorRGB(0.6, 0.6, 0.6)
        c.setLineWidth(0.5)
        c.setDash(6, 4)
        c.line(0, half_h, page_w, half_h)
        c.restoreState()

        c.showPage()

    c.save()

# ── GUI 介面 ─────────────────────────────────────────────────────────────────
class App(tk.Tk):
    PREV_W = 280
    PREV_H = 396

    def __init__(self):
        super().__init__()
        self.title("A4 對折桌牌產生器")
        self.resizable(False, False)
        self.configure(bg="#f5f5f3")

        self.preview_idx = 0
        self.rows = []
        self.font_size_auto = tk.BooleanVar(value=True)
        self.font_size_val = tk.IntVar(value=80)
        self.border_style = tk.StringVar(value="single")

        self._build_ui()
        self._add_row("王大明", "組長", "資訊處")
        self._add_row("李小華", "研究員", "企劃部")
        self._add_row("張美玲", "", "")
        self._update_preview()

    def _build_ui(self):
        left = tk.Frame(self, bg="#f5f5f3")
        left.grid(row=0, column=0, sticky="nsew", padx=12, pady=12)

        tk.Label(left, text="A4 對折桌牌產生器", font=("", 13, "bold"), bg="#f5f5f3", fg="#111").grid(row=0, column=0, columnspan=3, sticky="w", pady=(0, 2))
        tk.Label(left, text="直向對折，由上至下：單位 ➔ 職稱 ➔ 姓名", font=("", 8), bg="#f5f5f3", fg="#888").grid(row=1, column=0, columnspan=3, sticky="w", pady=(0, 10))

        # 全體單位
        tk.Label(left, text="全體單位（選填）", bg="#f5f5f3", fg="#555", font=("", 9)).grid(row=2, column=0, columnspan=3, sticky="w")
        self.global_dept = tk.StringVar()
        self.global_dept.trace_add("write", lambda *_: self._update_preview())
        tk.Entry(left, textvariable=self.global_dept, width=28, font=("", 11)).grid(row=3, column=0, columnspan=3, sticky="ew", pady=(2, 6))

        # 全體職稱
        tk.Label(left, text="全體職稱（選填）", bg="#f5f5f3", fg="#555", font=("", 9)).grid(row=4, column=0, columnspan=3, sticky="w")
        self.global_sub = tk.StringVar()
        self.global_sub.trace_add("write", lambda *_: self._update_preview())
        tk.Entry(left, textvariable=self.global_sub, width=28, font=("", 11)).grid(row=5, column=0, columnspan=3, sticky="ew", pady=(2, 8))

        # 外框樣式
        tk.Label(left, text="外框樣式", bg="#f5f5f3", fg="#555", font=("", 9)).grid(row=6, column=0, columnspan=3, sticky="w")
        for col, (val, lbl) in enumerate([("single", "單線"), ("double", "雙線"), ("none", "無框")]):
            tk.Radiobutton(left, text=lbl, variable=self.border_style, value=val, bg="#f5f5f3", activebackground="#f5f5f3", command=self._update_preview).grid(row=7, column=col, sticky="w")

        # 字型大小
        tk.Label(left, text="姓名字型大小", bg="#f5f5f3", fg="#555", font=("", 9)).grid(row=8, column=0, columnspan=2, sticky="w", pady=(8, 0))
        tk.Checkbutton(left, text="自動", variable=self.font_size_auto, bg="#f5f5f3", activebackground="#f5f5f3", command=self._on_font_auto_toggle).grid(row=8, column=2, sticky="e", pady=(8, 0))

        sf = tk.Frame(left, bg="#f5f5f3")
        sf.grid(row=9, column=0, columnspan=3, sticky="ew", pady=(2, 8))
        self.font_slider = tk.Scale(sf, from_=20, to=200, orient="horizontal", variable=self.font_size_val, bg="#f5f5f3", highlightthickness=0, troughcolor="#d0d0ce", command=lambda _: self._on_slider_move(), length=220, showvalue=False)
        self.font_slider.pack(side="left")
        self.font_size_label = tk.Label(sf, text="自動", bg="#f5f5f3", fg="#aaa", font=("", 9), width=6)
        self.font_size_label.pack(side="left", padx=(4, 0))

        # 列表欄位標題
        tk.Label(left, text="姓名", bg="#f5f5f3", fg="#555", font=("", 9)).grid(row=10, column=0, sticky="w", pady=(4, 2))
        tk.Label(left, text="個別職稱", bg="#f5f5f3", fg="#555", font=("", 9)).grid(row=10, column=1, sticky="w", pady=(4, 2))
        tk.Label(left, text="個別單位", bg="#f5f5f3", fg="#555", font=("", 9)).grid(row=10, column=2, sticky="w", pady=(4, 2))

        outer = tk.Frame(left, bg="#f5f5f3")
        outer.grid(row=11, column=0, columnspan=3, sticky="ew")
        self.list_canvas = tk.Canvas(outer, bg="#f5f5f3", height=180, width=340, highlightthickness=0)
        sb = ttk.Scrollbar(outer, orient="vertical", command=self.list_canvas.yview)
        self.list_canvas.configure(yscrollcommand=sb.set)
        self.list_canvas.pack(side="left", fill="both", expand=True)
        sb.pack(side="right", fill="y")
        self.list_inner = tk.Frame(self.list_canvas, bg="#f5f5f3")
        self.list_canvas.create_window((0, 0), window=self.list_inner, anchor="nw")
        self.list_inner.bind("<Configure>", lambda e: self.list_canvas.configure(scrollregion=self.list_canvas.bbox("all")))

        tk.Button(left, text="+ 新增姓名", command=self._add_row, bg="#e8e8e6", relief="flat", padx=8, pady=4, cursor="hand2").grid(row=12, column=0, columnspan=3, sticky="ew", pady=(6, 0))

        # 右側預覽
        right = tk.Frame(self, bg="#e8e8e6")
        right.grid(row=0, column=1, sticky="nsew", padx=(0, 12), pady=12)

        tk.Label(right, text="預覽", font=("", 9), bg="#e8e8e6", fg="#555").pack(pady=(10, 6))
        self.canvas = tk.Canvas(right, width=self.PREV_W, height=self.PREV_H, bg="#e8e8e6", highlightthickness=0)
        self.canvas.pack(padx=12)

        nav = tk.Frame(right, bg="#e8e8e6")
        nav.pack(pady=6)
        tk.Button(nav, text="<-", command=self._prev, width=3, bg="#e8e8e6", relief="flat", cursor="hand2").pack(side="left")
        self.nav_label = tk.Label(nav, text="1 / 1", bg="#e8e8e6", fg="#555", font=("", 9), width=6)
        self.nav_label.pack(side="left")
        tk.Button(nav, text="->", command=self._next, width=3, bg="#e8e8e6", relief="flat", cursor="hand2").pack(side="left")

        bottom = tk.Frame(self, bg="#f5f5f3")
        bottom.grid(row=1, column=0, columnspan=2, sticky="ew", padx=12, pady=(0, 12))
        bottom.columnconfigure(0, weight=1)
        bottom.columnconfigure(1, weight=1)
        tk.Button(bottom, text="產生 PDF 並儲存", command=self._export, bg="#111", fg="#fff", relief="flat", font=("", 11, "bold"), padx=16, pady=8, cursor="hand2", activebackground="#333", activeforeground="#fff").grid(row=0, column=0, sticky="ew", padx=(0, 4))
        tk.Button(bottom, text="全部清除", command=self._clear_all, bg="#e8e8e6", relief="flat", font=("", 11), padx=16, pady=8, cursor="hand2").grid(row=0, column=1, sticky="ew", padx=(4, 0))

        self._on_font_auto_toggle()

    def _on_slider_move(self):
        self.font_size_label.config(text=f"{self.font_size_val.get()} pt")
        self._update_preview()

    def _on_font_auto_toggle(self):
        if self.font_size_auto.get():
            self.font_slider.config(state="disabled")
            self.font_size_label.config(fg="#aaa", text="自動")
        else:
            self.font_slider.config(state="normal")
            self.font_size_label.config(fg="#333", text=f"{self.font_size_val.get()} pt")
        self._update_preview()

    def _add_row(self, name="", sub="", dept=""):
        rf = tk.Frame(self.list_inner, bg="#f5f5f3")
        rf.pack(fill="x", pady=2)
        nv = tk.StringVar(value=name)
        sv = tk.StringVar(value=sub)
        dv = tk.StringVar(value=dept)
        nv.trace_add("write", lambda *_: self._update_preview())
        sv.trace_add("write", lambda *_: self._update_preview())
        dv.trace_add("write", lambda *_: self._update_preview())
        
        tk.Entry(rf, textvariable=nv, width=10, font=("", 11)).pack(side="left", padx=(0, 4))
        tk.Entry(rf, textvariable=sv, width=10, font=("", 11)).pack(side="left", padx=(0, 4))
        tk.Entry(rf, textvariable=dv, width=10, font=("", 11)).pack(side="left", padx=(0, 4))
        
        tk.Button(rf, text="✕", relief="flat", bg="#f5f5f3", fg="#aaa", font=("", 9), cursor="hand2", command=lambda r=rf, n=nv, s=sv, d=dv: self._del_row(r, n, s, d)).pack(side="left")
        self.rows.append((nv, sv, dv))
        self._update_preview()

    def _del_row(self, rf, nv, sv, dv):
        self.rows = [(n, s, d) for n, s, d in self.rows if not (n is nv and s is sv and d is dv)]
        rf.destroy()
        if self.preview_idx >= len(self._get_entries()):
            self.preview_idx = max(0, len(self._get_entries()) - 1)
        self._update_preview()

    def _clear_all(self):
        for w in self.list_inner.winfo_children():
            w.destroy()
        self.rows.clear()
        self.preview_idx = 0
        self._update_preview()

    def _get_entries(self):
        gsub = self.global_sub.get().strip()
        gdept = self.global_dept.get().strip()
        return [(n.get().strip(), s.get().strip() or gsub, d.get().strip() or gdept) for n, s, d in self.rows if n.get().strip()]

    def _prev(self):
        n = len(self._get_entries())
        if n: self.preview_idx = (self.preview_idx - 1) % n
        self._update_preview()

    def _next(self):
        n = len(self._get_entries())
        if n: self.preview_idx = (self.preview_idx + 1) % n
        self._update_preview()

    def _update_preview(self):
        entries = self._get_entries()
        total = len(entries)
        if self.preview_idx >= total:
            self.preview_idx = max(0, total - 1)
        self.nav_label.config(text=f"{self.preview_idx + 1} / {total}" if total else "0 / 0")
        name, sub, dept = entries[self.preview_idx] if total else ("(姓名)", "", "")
        self._draw_preview(name, sub, dept)

    def _draw_preview(self, name, sub, dept):
        cv = self.canvas
        W, H = self.PREV_W, self.PREV_H
        half = H // 2
        cv.delete("all")
        cv.create_rectangle(0, 0, W, H, fill="white", outline="#bbb")

        bstyle = self.border_style.get()
        mg = 6

        # 計算字級
        if not self.font_size_auto.get():
            pdf_w = A4[0]
            scale = (W - mg * 2) / pdf_w
            chosen_fs = max(8, int(self.font_size_val.get() * scale))
        else:
            has_others = bool(sub or dept)
            max_h = int(half * 0.55) if has_others else int(half * 0.7)
            chosen_fs = 12
            for f in range(65, 11, -1):
                t = tkfont.Font(size=-f)
                if t.measure(name) <= (W - 30) and f <= max_h:
                    chosen_fs = f
                    break

        sub_fs = max(8, min(chosen_fs // 4, 16)) if sub else 0
        dept_fs = max(8, min(chosen_fs // 5, 14)) if dept else 0
        gap = 4

        # 繪製上下半部
        for is_top in [True, False]:
            y_offset = 0 if is_top else half
            if bstyle != "none":
                cv.create_rectangle(mg, y_offset + mg, W - mg, y_offset + half - mg, outline="#333", width=1)
                if bstyle == "double":
                    cv.create_rectangle(mg + 4, y_offset + mg + 4, W - mg - 4, y_offset + half - mg - 4, outline="#333", width=1)

            cy = y_offset + half // 2

            # 計算總文字方塊高度
            th = chosen_fs
            if sub: th += gap + sub_fs
            if dept: th += gap + dept_fs

            if is_top:
                # 倒立面（上半）：視覺最上方的單位，在畫布上因為旋轉180度，Y座標反而要在最下面（靠中央折線）
                current_top = cy + th / 2
                
                if dept:
                    dept_y = current_top - dept_fs / 2
                    current_top -= (dept_fs + gap)
                else: dept_y = 0
                
                if sub:
                    sub_y = current_top - sub_fs / 2
                    current_top -= (sub_fs + gap)
                else: sub_y = 0
                
                name_y = current_top - chosen_fs / 2

                cv.create_text(W // 2, name_y, text=name, font=("", -chosen_fs, "bold"), fill="#111", angle=180)
                if sub:
                    cv.create_text(W // 2, sub_y, text=sub, font=("", -sub_fs), fill="#555", angle=180)
                if dept:
                    cv.create_text(W // 2, dept_y, text=dept, font=("", -dept_fs), fill="#777", angle=180)
            else:
                # 正立面（下半）：文字正常朝上，單位在最上面（靠中央折線），姓名在最下面
                current_top = cy - th / 2
                
                if dept:
                    dept_y = current_top + dept_fs / 2
                    current_top += (dept_fs + gap)
                else: dept_y = 0
                
                if sub:
                    sub_y = current_top + sub_fs / 2
                    current_top += (sub_fs + gap)
                else: sub_y = 0
                
                name_y = current_top + chosen_fs / 2

                cv.create_text(W // 2, name_y, text=name, font=("", -chosen_fs, "bold"), fill="#111")
                if sub:
                    cv.create_text(W // 2, sub_y, text=sub, font=("", -sub_fs), fill="#555")
                if dept:
                    cv.create_text(W // 2, dept_y, text=dept, font=("", -dept_fs), fill="#777")

        # 折線提示
        cv.create_line(0, half, W, half, dash=(6, 4), fill="#aaa", width=1)
        cv.create_text(W - 4, half - 8, text="折線", font=("", 7), fill="#aaa", anchor="e")

    def _export(self):
        entries = self._get_entries()
        if not entries:
            messagebox.showwarning("提示", "請至少輸入一個姓名")
            return
        path = filedialog.asksaveasfilename(
            defaultextension=".pdf",
            filetypes=[("PDF 檔案", "*.pdf")],
            initialfile="桌牌.pdf",
            title="儲存桌牌 PDF"
        )
        if not path:
            return
        try:
            fixed_fs = None if self.font_size_auto.get() else self.font_size_val.get()
            make_pdf(entries, self.border_style.get(), path, fixed_fs)
            messagebox.showinfo("完成", f"已輸出 {len(entries)} 張桌牌\n→ {path}")
        except Exception as e:
            messagebox.showerror("錯誤", str(e))

if __name__ == "__main__":
    app = App()
    app.mainloop()