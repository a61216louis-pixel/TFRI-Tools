#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
A4 三折三角體桌牌產生器
A4 橫向四等份：
  面1（空白）| 面2（文字面）| 面3（文字面）| 面4（空白）
  黏合面      | 左側可見    | 右側可見    | 黏合面
折法：面1+面4 黏合，三角柱站立，兩側各可見一個面
需求：pip install reportlab
"""

import os
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from tkinter import font as tkfont
from reportlab.lib.pagesizes import A4, landscape
from reportlab.lib.units import mm
from reportlab.pdfgen import canvas as rl_canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

FONT_CANDIDATES = [
    ("C:/Windows/Fonts/msjh.ttc",    "MSJH"),
    ("C:/Windows/Fonts/mingliu.ttc", "MingLiu"),
    ("C:/Windows/Fonts/kaiu.ttf",    "KaiU"),
    ("/usr/share/fonts/opentype/noto/NotoSansCJK-Black.ttc",   "NotoBlack"),
    ("/usr/share/fonts/opentype/noto/NotoSerifCJK-Bold.ttc",   "NotoSerifBold"),
    ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc", "NotoRegular"),
    ("/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc",           "WQYZenHei"),
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
    raise RuntimeError("找不到中文字型")


def draw_blank_panel(c, w, h, border_style):
    """空白面（黏合用）"""
    margin = 6 * mm
    c.setFillColorRGB(1, 1, 1)
    c.rect(0, 0, w, h, fill=1, stroke=0)
    if border_style != "none":
        c.setStrokeColorRGB(0.7, 0.7, 0.7)
        c.setLineWidth(0.8)
        c.setDash(4, 3)
        c.rect(margin, margin, w-2*margin, h-2*margin, fill=0, stroke=1)
        c.setDash()


def draw_rotated_panel(c, font_name, name, subtitle, w, h,
                       border_style, fixed_fs, mode_face2):
    """
    完全對應預覽視窗畫面的 PDF 繪製邏輯。
    mode_face2 = True  -> 面2：字頭朝右（順時針轉來看），人名在左、職稱在右。
    mode_face2 = False -> 面3：字頭朝左（逆時針轉來看），職稱在左、人名在右。
    """
    margin = 6 * mm
    c.setFillColorRGB(1, 1, 1)
    c.rect(0, 0, w, h, fill=1, stroke=0)

    if border_style != "none":
        c.setStrokeColorRGB(0.15, 0.15, 0.15)
        c.setLineWidth(1.2)
        c.rect(margin, margin, w-2*margin, h-2*margin, fill=0, stroke=1)
        if border_style == "double":
            inn = 4 * mm
            c.rect(margin+inn, margin+inn,
                   w-2*margin-2*inn, h-2*margin-2*inn, fill=0, stroke=1)

    # 計算字型大小
    text_max_w = h - 20 * mm
    text_max_h = w * 0.65 if subtitle else w * 0.75

    if fixed_fs:
        name_fs = fixed_fs
        while name_fs > 12 and c.stringWidth(name, font_name, name_fs) > text_max_w:
            name_fs -= 2
    else:
        for fs in range(260, 12, -2):
            if c.stringWidth(name, font_name, fs) <= text_max_w and fs <= text_max_h:
                break
        name_fs = fs

    sub_fs = min(max(12, name_fs // 4), 28) if subtitle else 0
    gap = 5 * mm

    cx = w / 2
    cy = h / 2

    c.saveState()
    c.translate(cx, cy)  # 移到物件中心

    if mode_face2:
        c.rotate(-90)   # 面2：對應預覽的 angle=-90
        # 旋轉後，原本短邊的寬度變成了垂直 Y 軸（向上為正）。
        # 為了讓職稱在人名的「上方（頭頂）」，職稱要往 Y 正方向移，人名往 Y 負方向移。
        if subtitle:
            # 考慮字高與間距，精確計算各自的基線位移
            total_text_h = name_fs + gap + sub_fs
            name_y = -(total_text_h / 2) + (name_fs * 0.2)
            sub_y  = (total_text_h / 2) - sub_fs
        else:
            name_y = -name_fs * 0.3
            sub_y  = 0
    else:
        c.rotate(90)    # 面3：對應預覽的 angle=90
        # 旋轉 90° 後，原本的短邊寬度依然對應 Y 軸（向上為正）。
        # 同樣為了讓職稱在人名的「上方（頭頂）」，職稱要往 Y 正方向移，人名往 Y 負方向移。
        if subtitle:
            total_text_h = name_fs + gap + sub_fs
            name_y = -(total_text_h / 2) + (name_fs * 0.2)
            sub_y  = (total_text_h / 2) - sub_fs
        else:
            name_y = -name_fs * 0.3
            sub_y  = 0

    # 繪製文字
    c.setFillColorRGB(0.05, 0.05, 0.05)
    c.setFont(font_name, name_fs)
    c.drawCentredString(0, name_y, name)

    if subtitle:
        c.setFont(font_name, sub_fs)
        c.setFillColorRGB(0.35, 0.35, 0.35)
        c.drawCentredString(0, sub_y, subtitle)

    c.restoreState()


def make_pdf(entries, border_style, output_path, fixed_fs=None):
    font_name       = get_font()
    page_w, page_h  = landscape(A4)
    panel_w         = page_w / 4

    c = rl_canvas.Canvas(output_path, pagesize=(page_w, page_h))

    for name, subtitle in entries:
        # 面1：空白黏合
        c.saveState()
        c.translate(0, 0)
        draw_blank_panel(c, panel_w, page_h, border_style)
        c.restoreState()

        # 面2：文字面（左側）
        c.saveState()
        c.translate(panel_w, 0)
        draw_rotated_panel(c, font_name, name, subtitle,
                           panel_w, page_h, border_style, fixed_fs,
                           mode_face2=True)
        c.restoreState()

        # 面3：文字面（右側）
        c.saveState()
        c.translate(panel_w * 2, 0)
        draw_rotated_panel(c, font_name, name, subtitle,
                           panel_w, page_h, border_style, fixed_fs,
                           mode_face2=False)
        c.restoreState()

        # 面4：空白黏合
        c.saveState()
        c.translate(panel_w * 3, 0)
        draw_blank_panel(c, panel_w, page_h, border_style)
        c.restoreState()

        # 三條折線
        for i in range(1, 4):
            x = panel_w * i
            c.saveState()
            c.setStrokeColorRGB(0.55, 0.55, 0.55)
            c.setLineWidth(0.5)
            c.setDash(6, 4)
            c.line(x, 0, x, page_h)
            c.restoreState()

        # 黏合提示
        c.saveState()
        c.setFont(font_name, 8)
        c.setFillColorRGB(0.65, 0.65, 0.65)
        c.drawCentredString(panel_w / 2,       5 * mm, "<- 黏合面 ->")
        c.drawCentredString(panel_w * 3.5,     5 * mm, "<- 黏合面 ->")
        c.restoreState()

        c.showPage()

    c.save()


# ── GUI ───────────────────────────────────────────────────────────────────────
class App(tk.Tk):
    PREV_W = 400
    PREV_H = 150

    def __init__(self):
        super().__init__()
        self.title("A4 三角體桌牌產生器（三折）")
        self.resizable(False, False)
        self.configure(bg="#f5f5f3")

        self.preview_idx    = 0
        self.rows           = []
        self.font_size_auto = tk.BooleanVar(value=True)
        self.font_size_val  = tk.IntVar(value=80)
        self.border_style   = tk.StringVar(value="single")

        self._build_ui()
        self._add_row("王大明", "組長")
        self._add_row("李小華", "研究員")
        self._add_row("張美玲", "")
        self._update_preview()

    def _build_ui(self):
        left = tk.Frame(self, bg="#f5f5f3")
        left.grid(row=0, column=0, sticky="nsew", padx=12, pady=12)

        tk.Label(left, text="A4 三角體桌牌（三折）",
                 font=("", 13, "bold"), bg="#f5f5f3", fg="#111"
                 ).grid(row=0, column=0, columnspan=3, sticky="w", pady=(0,2))
        tk.Label(left, text="面1空白│面2文字│面3文字│面4空白  .  面1+面4黏合",
                 font=("", 8), bg="#f5f5f3", fg="#888"
                 ).grid(row=1, column=0, columnspan=3, sticky="w", pady=(0,10))

        tk.Label(left, text="全體職稱（選填）", bg="#f5f5f3",
                 fg="#555", font=("", 9)).grid(row=2, column=0,
                 columnspan=3, sticky="w")
        self.global_sub = tk.StringVar()
        self.global_sub.trace_add("write", lambda *_: self._update_preview())
        tk.Entry(left, textvariable=self.global_sub, width=28,
                 font=("", 11)).grid(row=3, column=0, columnspan=3,
                 sticky="ew", pady=(2, 8))

        tk.Label(left, text="外框樣式", bg="#f5f5f3",
                 fg="#555", font=("", 9)).grid(row=4, column=0,
                 columnspan=3, sticky="w")
        for col, (val, lbl) in enumerate([("single","單線"),
                                           ("double","雙線"),
                                           ("none",  "無框")]):
            tk.Radiobutton(left, text=lbl, variable=self.border_style,
                           value=val, bg="#f5f5f3", activebackground="#f5f5f3",
                           command=self._update_preview
                           ).grid(row=5, column=col, sticky="w")

        tk.Label(left, text="姓名字型大小", bg="#f5f5f3",
                 fg="#555", font=("", 9)).grid(row=6, column=0,
                 columnspan=2, sticky="w", pady=(8,0))
        tk.Checkbutton(left, text="自動", variable=self.font_size_auto,
                       bg="#f5f5f3", activebackground="#f5f5f3",
                       command=self._on_font_auto_toggle
                       ).grid(row=6, column=2, sticky="e", pady=(8,0))

        sf = tk.Frame(left, bg="#f5f5f3")
        sf.grid(row=7, column=0, columnspan=3, sticky="ew", pady=(2,8))
        self.font_slider = tk.Scale(
            sf, from_=20, to=200, orient="horizontal",
            variable=self.font_size_val, bg="#f5f5f3",
            highlightthickness=0, troughcolor="#d0d0ce",
            command=lambda _: self._on_slider_move(),
            length=220, showvalue=False)
        self.font_slider.pack(side="left")
        self.font_size_label = tk.Label(sf, text="自動", bg="#f5f5f3",
                                        fg="#aaa", font=("", 9), width=6)
        self.font_size_label.pack(side="left", padx=(4,0))

        tk.Label(left, text="姓名", bg="#f5f5f3", fg="#555",
                 font=("", 9)).grid(row=8, column=0, sticky="w", pady=(4,2))
        tk.Label(left, text="個別職稱（選填）", bg="#f5f5f3", fg="#555",
                 font=("", 9)).grid(row=8, column=1, sticky="w", pady=(4,2))

        outer = tk.Frame(left, bg="#f5f5f3")
        outer.grid(row=9, column=0, columnspan=3, sticky="ew")
        self.list_canvas = tk.Canvas(outer, bg="#f5f5f3", height=180,
                                     width=320, highlightthickness=0)
        sb = ttk.Scrollbar(outer, orient="vertical",
                           command=self.list_canvas.yview)
        self.list_canvas.configure(yscrollcommand=sb.set)
        self.list_canvas.pack(side="left", fill="both", expand=True)
        sb.pack(side="right", fill="y")
        self.list_inner = tk.Frame(self.list_canvas, bg="#f5f5f3")
        self.list_canvas.create_window((0,0), window=self.list_inner, anchor="nw")
        self.list_inner.bind("<Configure>",
            lambda e: self.list_canvas.configure(
                scrollregion=self.list_canvas.bbox("all")))

        tk.Button(left, text="+ 新增姓名", command=self._add_row,
                  bg="#e8e8e6", relief="flat", padx=8, pady=4,
                  cursor="hand2").grid(row=10, column=0, columnspan=3,
                  sticky="ew", pady=(6,0))

        # 右側預覽
        right = tk.Frame(self, bg="#e8e8e6")
        right.grid(row=0, column=1, sticky="nsew", padx=(0,12), pady=12)

        tk.Label(right, text="預覽", font=("", 9), bg="#e8e8e6",
                 fg="#555").pack(pady=(10,6))
        self.canvas = tk.Canvas(right, width=self.PREV_W, height=self.PREV_H,
                                bg="#e8e8e6", highlightthickness=0)
        self.canvas.pack(padx=12)

        nav = tk.Frame(right, bg="#e8e8e6")
        nav.pack(pady=6)
        tk.Button(nav, text="<-", command=self._prev, width=3,
                  bg="#e8e8e6", relief="flat", cursor="hand2").pack(side="left")
        self.nav_label = tk.Label(nav, text="1 / 1", bg="#e8e8e6",
                                  fg="#555", font=("", 9), width=6)
        self.nav_label.pack(side="left")
        tk.Button(nav, text="->", command=self._next, width=3,
                  bg="#e8e8e6", relief="flat", cursor="hand2").pack(side="left")

        tk.Label(right,
                 text="空白│面2│面3│空白",
                 font=("", 8), bg="#e8e8e6", fg="#999").pack()

        bottom = tk.Frame(self, bg="#f5f5f3")
        bottom.grid(row=1, column=0, columnspan=2, sticky="ew",
                    padx=12, pady=(0,12))
        bottom.columnconfigure(0, weight=1)
        bottom.columnconfigure(1, weight=1)
        tk.Button(bottom, text="產生 PDF 並儲存",
                  command=self._export,
                  bg="#111", fg="#fff", relief="flat",
                  font=("", 11, "bold"), padx=16, pady=8,
                  cursor="hand2", activebackground="#333",
                  activeforeground="#fff").grid(
                  row=0, column=0, sticky="ew", padx=(0,4))
        tk.Button(bottom, text="全部清除", command=self._clear_all,
                  bg="#e8e8e6", relief="flat", font=("", 11),
                  padx=16, pady=8, cursor="hand2").grid(
                  row=0, column=1, sticky="ew", padx=(4,0))

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
            self.font_size_label.config(
                fg="#333", text=f"{self.font_size_val.get()} pt")
        self._update_preview()

    def _add_row(self, name="", sub=""):
        rf = tk.Frame(self.list_inner, bg="#f5f5f3")
        rf.pack(fill="x", pady=2)
        nv = tk.StringVar(value=name)
        sv = tk.StringVar(value=sub)
        nv.trace_add("write", lambda *_: self._update_preview())
        sv.trace_add("write", lambda *_: self._update_preview())
        tk.Entry(rf, textvariable=nv, width=10, font=("",11)).pack(side="left", padx=(0,4))
        tk.Entry(rf, textvariable=sv, width=10, font=("",11)).pack(side="left", padx=(0,4))
        tk.Button(rf, text="✕", relief="flat", bg="#f5f5f3", fg="#aaa",
                  font=("",9), cursor="hand2",
                  command=lambda r=rf, n=nv, s=sv: self._del_row(r,n,s)
                  ).pack(side="left")
        self.rows.append((nv, sv))
        self._update_preview()

    def _del_row(self, rf, nv, sv):
        self.rows = [(n,s) for n,s in self.rows if not (n is nv and s is sv)]
        rf.destroy()
        if self.preview_idx >= len(self._get_entries()):
            self.preview_idx = max(0, len(self._get_entries())-1)
        self._update_preview()

    def _clear_all(self):
        for w in self.list_inner.winfo_children():
            w.destroy()
        self.rows.clear()
        self.preview_idx = 0
        self._update_preview()

    def _get_entries(self):
        gsub = self.global_sub.get().strip()
        return [(n.get().strip(), s.get().strip() or gsub)
                for n,s in self.rows if n.get().strip()]

    def _prev(self):
        n = len(self._get_entries())
        if n: self.preview_idx = (self.preview_idx-1) % n
        self._update_preview()

    def _next(self):
        n = len(self._get_entries())
        if n: self.preview_idx = (self.preview_idx+1) % n
        self._update_preview()

    def _update_preview(self):
        entries = self._get_entries()
        total   = len(entries)
        if self.preview_idx >= total:
            self.preview_idx = max(0, total-1)
        self.nav_label.config(
            text=f"{self.preview_idx+1} / {total}" if total else "0 / 0")
        name, sub = entries[self.preview_idx] if total else ("(姓名)","")
        self._draw_preview(name, sub)

    def _draw_preview(self, name, sub):
        cv   = self.canvas
        W, H = self.PREV_W, self.PREV_H
        pw   = W // 4
        cv.delete("all")
        cv.create_rectangle(0, 0, W, H, fill="white", outline="#bbb")

        bstyle = self.border_style.get()
        mg     = 5

        face_labels = ["空白\n(黏合)", "面2", "面3", "空白\n(黏合)"]

        for i in range(4):
            x0, x1 = i*pw, (i+1)*pw
            is_blank = i in (0, 3)

            if bstyle != "none":
                outline_color = "#ccc" if is_blank else "#333"
                cv.create_rectangle(x0+mg, mg, x1-mg, H-mg,
                                    outline=outline_color,
                                    dash=(3,2) if is_blank else (),
                                    width=1)

            if not is_blank:
                area_h = H - mg*2

                if not self.font_size_auto.get():
                    pdf_h = landscape(A4)[1]
                    scale = (pw - mg*2) / pdf_h
                    fs = max(7, int(self.font_size_val.get() * scale))
                else:
                    max_h = int(area_h * 0.65) if sub else int(area_h * 0.75)
                    fs = 8
                    for f in range(60, 7, -1):
                        t = tkfont.Font(size=-f)
                        if t.measure(name) <= H-20 and f <= max_h:
                            fs = f
                            break

                sub_fs = max(7, min(fs//4, 13)) if sub else 0

                cx = (x0+x1)//2
                cy = H//2

                # 預覽窗格內的字級位移量與角度
                if sub:
                    total_h = fs + 3 + sub_fs
                    name_offset = -(total_h / 2) + (fs / 2)
                    sub_offset  = (total_h / 2) - (sub_fs / 2)
                else:
                    name_offset = 0
                    sub_offset  = 0

                if i == 1:
                    angle = -90
                    # angle=-90 時，向左平移 (-X) 在視覺上等同於「向上/頭頂方向」
                    name_x = cx + name_offset
                    sub_x  = cx + sub_offset
                else:
                    angle = 90
                    # angle=90 時，向右平移 (+X) 在視覺上等同於「向上/頭頂方向」
                    name_x = cx - name_offset
                    sub_x  = cx - sub_offset

                cv.create_text(name_x, cy, text=name,
                               font=("", -fs, "bold"),
                               fill="#111", angle=angle)
                if sub:
                    cv.create_text(sub_x, cy, text=sub,
                                   font=("", -sub_fs), fill="#555",
                                   angle=angle)

            cv.create_text((x0+x1)//2, H-15,
                           text=face_labels[i], font=("", 7), fill="#aaa")

        for i in range(1, 4):
            cv.create_line(i*pw, 0, i*pw, H, dash=(5,3), fill="#bbb", width=1)

    def _export(self):
        entries = self._get_entries()
        if not entries:
            messagebox.showwarning("提示", "請至少輸入一個姓名")
            return
        path = filedialog.asksaveasfilename(
            defaultextension=".pdf",
            filetypes=[("PDF 檔案","*.pdf")],
            initialfile="三角桌牌.pdf",
            title="儲存三角體桌牌 PDF")
        if not path:
            return
        try:
            fixed_fs = None if self.font_size_auto.get() \
                       else self.font_size_val.get()
            make_pdf(entries, self.border_style.get(), path, fixed_fs)
            messagebox.showinfo("完成", f"已輸出 {len(entries)} 張桌牌\n→ {path}")
        except Exception as e:
            messagebox.showerror("錯誤", str(e))


if __name__ == "__main__":
    app = App()
    app.mainloop()