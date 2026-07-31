#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
2026 森林永續發展與育林技術研討會
邀請函產生器 & 群發系統 GUI
執行方式：py invitation_gui.py
"""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox, scrolledtext
import threading
import os
import sys
import smtplib
import ssl
import time
import zipfile
import urllib.parse
import webbrowser
import openpyxl
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.base import MIMEBase
from email.mime.image import MIMEImage
from email import encoders
from email.header import Header

# ── PDF 產生（reportlab）──
from reportlab.lib.pagesizes import A4
from reportlab.lib import colors
from reportlab.lib.units import mm
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

# ════════════════════════════════════════════════
#  字型設定（Windows 系統字型）
# ════════════════════════════════════════════════
def setup_fonts():
    """嘗試在 Windows 上載入中文字型"""
    font_dirs = [
        r"C:\Windows\Fonts",
        os.path.expanduser("~\\AppData\\Local\\Microsoft\\Windows\\Fonts"),
    ]
    candidates_regular = ["msjh.ttc", "mingliu.ttc", "kaiu.ttf", "simsun.ttc", "msjhbd.ttc"]
    candidates_bold    = ["msjhbd.ttc", "mingliu.ttc", "kaiu.ttf", "simsun.ttc"]

    reg_path = None
    bold_path = None
    for d in font_dirs:
        for f in candidates_regular:
            p = os.path.join(d, f)
            if os.path.exists(p):
                reg_path = p
                break
        if reg_path:
            break
    for d in font_dirs:
        for f in candidates_bold:
            p = os.path.join(d, f)
            if os.path.exists(p):
                bold_path = p
                break
        if bold_path:
            break

    if not reg_path:
        raise RuntimeError("找不到中文字型，請確認 Windows 字型資料夾正常。")

    try:
        pdfmetrics.registerFont(TTFont("CJK",  reg_path))
        pdfmetrics.registerFont(TTFont("CJKB", bold_path or reg_path))
    except Exception:
        pdfmetrics.registerFont(TTFont("CJK",  reg_path, subfontIndex=0))
        pdfmetrics.registerFont(TTFont("CJKB", (bold_path or reg_path), subfontIndex=0))

# ════════════════════════════════════════════════
#  PDF 產生核心
# ════════════════════════════════════════════════
C_FOREST_DARK  = colors.HexColor('#1a3a2a')
C_FOREST_GREEN = colors.HexColor('#3d7a52')
C_MOSS         = colors.HexColor('#5a8f5f')
C_SAGE         = colors.HexColor('#8ab89a')
C_AMBER        = colors.HexColor('#c8852a')
C_TEXT         = colors.HexColor('#2c3e2d')
C_TEXT_MID     = colors.HexColor('#4a6b4a')
C_BG           = colors.HexColor('#faf8f3')

FORM_BASE = "https://docs.google.com/forms/d/e/1FAIpQLSeX54OgUv52IcAJrBRZZa-wkBcWHkQffqN9DU-YQfU4fr8lpQ/viewform"

def make_pdf(seq, name, title,
             event_date, event_weekday,
             venue, venue_addr,
             dinner_date, dinner_weekday, dinner_time,
             dinner_venue, dinner_addr,
             host1_org, host1_title, host1_name,
             host2_org, host2_title, host2_name,
             form_url, entry_name_id, entry_title_id,
             out_path):

    W, H = A4
    mx = 22 * mm
    rw = W - 2 * mx

    url = (form_url.strip()
           + "?usp=pp_url"
           + "&" + entry_name_id.strip()  + "=" + urllib.parse.quote(name)
           + "&" + entry_title_id.strip() + "=" + urllib.parse.quote(title))

    c = canvas.Canvas(out_path, pagesize=A4)
    c.setTitle(f"邀請函 - {name} {title}")

    # 背景
    c.setFillColor(C_BG)
    c.rect(0, 0, W, H, fill=1, stroke=0)

    # 頂部漸層帶
    band_colors = ['#1a3a2a','#2d5c3f','#3d7a52','#5a8f5f','#8ab89a','#5a8f5f','#3d7a52','#2d5c3f','#1a3a2a']
    seg_w = W / len(band_colors)
    band_h = 5
    for i, hc in enumerate(band_colors):
        c.setFillColor(colors.HexColor(hc))
        c.rect(i * seg_w, H - band_h, seg_w + 1, band_h, fill=1, stroke=0)

    y = H - band_h - 24 * mm

    # 敬邀貴賓
    c.setFont('CJK', 11)
    c.setFillColor(C_SAGE)
    label = '敬　邀　貴　賓'
    lw = c.stringWidth(label, 'CJK', 11)
    c.setStrokeColor(colors.HexColor('#c8dfc8'))
    c.setLineWidth(0.5)
    c.line(mx, y - 3, (W - lw) / 2 - 5, y - 3)
    c.line((W + lw) / 2 + 5, y - 3, W - mx, y - 3)
    c.drawCentredString(W / 2, y - 6, label)
    y -= 18 * mm

    # 姓名
    c.setFont('CJKB', 42)
    c.setFillColor(C_FOREST_DARK)
    c.drawCentredString(W / 2, y, name)
    y -= 42 * 0.6 + 5 * mm

    # 職稱
    c.setFont('CJK', 16)
    c.setFillColor(C_TEXT_MID)
    c.drawCentredString(W / 2, y, title)
    y -= 13 * mm

    # 分隔線
    c.setStrokeColor(colors.HexColor('#c8dfc8'))
    c.setLineWidth(0.5)
    c.line(mx, y, W - mx, y)
    y -= 8 * mm

    # 研討會資訊標籤
    c.setFont('CJK', 10)
    c.setFillColor(C_SAGE)
    c.drawString(mx, y, '研 討 會 資 訊')
    y -= 6 * mm

    # 資訊框
    box_gap = 4 * mm
    box_w = (rw - box_gap) / 2
    box_h = 28 * mm

    def green_box(bx, by, bw, bh, lbl, lines):
        c.setFillColor(colors.HexColor('#eaf4ee'))
        c.setStrokeColor(colors.HexColor('#b2d4bc'))
        c.setLineWidth(0.5)
        c.roundRect(bx, by - bh, bw, bh, 1*mm, fill=1, stroke=1)
        c.setFont('CJK', 9)
        c.setFillColor(C_MOSS)
        c.drawString(bx + 4*mm, by - 6*mm, lbl)
        ty = by - 13*mm
        for txt, bold in lines:
            c.setFont('CJKB' if bold else 'CJK', 13)
            c.setFillColor(C_FOREST_DARK if bold else C_TEXT)
            c.drawString(bx + 4*mm, ty, txt)
            ty -= 7*mm

    date_line1 = f"民國{event_date}"
    date_line2 = f"（{event_weekday}）"
    green_box(mx, y, box_w, box_h, '研討會日期',
              [(date_line1, True), (date_line2, False)])
    green_box(mx + box_w + box_gap, y, box_w, box_h, '研討會地點',
              [(venue, True), (venue_addr, False)])
    y -= box_h + 7 * mm

    # 餐敘框
    din_h = 34 * mm
    c.setFillColor(colors.HexColor('#fdf0e0'))
    c.setStrokeColor(colors.HexColor('#e8b87a'))
    c.setLineWidth(0.5)
    c.roundRect(mx, y - din_h, rw, din_h, 1*mm, fill=1, stroke=1)

    c.setFont('CJKB', 13)
    c.setFillColor(C_AMBER)
    c.drawString(mx + 5*mm, y - 8*mm, '並誠摯邀請您參加 · 會後餐敘')

    time_str = f'時間：民國{dinner_date}（{dinner_weekday}）'
    c.setFont('CJK', 13)
    c.setFillColor(C_TEXT)
    c.drawString(mx + 5*mm, y - 16*mm, time_str)
    tw = c.stringWidth(time_str, 'CJK', 13)
    c.setFont('CJKB', 13)
    c.drawString(mx + 5*mm + tw, y - 16*mm, dinner_time)

    c.setFont('CJK', 13)
    c.drawString(mx + 5*mm, y - 25*mm, '地點：')
    lbl_w = c.stringWidth('地點：', 'CJK', 13)
    c.setFont('CJKB', 13)
    c.drawString(mx + 5*mm + lbl_w, y - 25*mm, dinner_venue)
    dv_w = c.stringWidth(dinner_venue, 'CJKB', 13)
    c.setFont('CJK', 13)
    c.drawString(mx + 5*mm + lbl_w + dv_w, y - 25*mm, f'（{dinner_addr}）')
    y -= din_h + 9 * mm

    # 期待光臨
    c.setStrokeColor(colors.HexColor('#c8dfc8'))
    c.setLineWidth(0.5)
    c.line(mx, y, W - mx, y)
    y -= 11 * mm
    c.setFont('CJKB', 22)
    c.setFillColor(C_FOREST_DARK)
    c.drawCentredString(W / 2, y, '期　待　您　的　光　臨')
    y -= 10 * mm
    c.setFont('CJK', 12)
    c.setFillColor(C_SAGE)
    c.drawCentredString(W / 2, y, 'Your presence would be our greatest honour')
    y -= 9 * mm
    c.setStrokeColor(colors.HexColor('#c8dfc8'))
    c.line(mx, y, W - mx, y)
    y -= 11 * mm

    # 署名（右對齊）
    def right_line(text_normal, text_bold, suffix, yy):
        c.setFont('CJK', 13)
        c.setFillColor(C_TEXT_MID)
        nw = c.stringWidth(text_normal, 'CJK', 13)
        bw = c.stringWidth(text_bold, 'CJKB', 13)
        sw = c.stringWidth(suffix, 'CJK', 13)
        total = nw + bw + sw
        sx = W - mx - total
        c.drawString(sx, yy, text_normal)
        c.setFont('CJKB', 13)
        c.setFillColor(C_FOREST_DARK)
        c.drawString(sx + nw, yy, text_bold)
        c.setFont('CJK', 13)
        c.setFillColor(C_TEXT_MID)
        c.drawString(sx + nw + bw, yy, suffix)

    right_line(f'{host1_org}　{host1_title}　', host1_name, '　敬邀', y)
    y -= 9 * mm
    right_line(f'{host2_org}　{host2_title}　', host2_name, '　敬邀', y)
    y -= 18 * mm

    # 確認出席按鈕
    btn_w, btn_h = 58 * mm, 14 * mm
    btn_x = (W - btn_w) / 2
    btn_y = y - btn_h
    c.setFillColor(C_FOREST_GREEN)
    c.roundRect(btn_x, btn_y, btn_w, btn_h, 1.5*mm, fill=1, stroke=0)
    c.setFont('CJKB', 14)
    c.setFillColor(colors.white)
    c.drawCentredString(W / 2, btn_y + 4*mm, '確認出席')
    c.linkURL(url, (btn_x, btn_y, btn_x + btn_w, btn_y + btn_h), relative=0, thickness=0)

    # 頁尾
    foot_y = 10 * mm
    c.setStrokeColor(colors.HexColor('#c8dfc8'))
    c.setLineWidth(0.5)
    c.line(mx, foot_y + 6*mm, W - mx, foot_y + 6*mm)
    c.setFont('CJK', 9)
    c.setFillColor(C_MOSS)
    c.drawCentredString(W / 2, foot_y, '農業部林業試驗所 · 2026')

    c.save()


# ════════════════════════════════════════════════
#  Email 寄送
# ════════════════════════════════════════════════
def send_email(name, title, to_email, pdf_path, image_path,
               subject_tmpl, body_tmpl, sender):
    from email.message import EmailMessage

    msg = EmailMessage()
    msg["From"]    = sender
    msg["To"]      = to_email
    msg["Subject"] = subject_tmpl.format(name=name, title=title)

    plain_body = body_tmpl.format(name=name, title=title)

    # 讀取圖片
    img_data  = None
    img_main  = "image"
    img_sub   = "png"
    if image_path and os.path.exists(image_path):
        with open(image_path, "rb") as f:
            img_data = f.read()
        ext = os.path.splitext(image_path)[1].lower()
        if ext in (".jpg", ".jpeg"):
            img_sub = "jpeg"

    # 將換行符號轉為 <br>（最相容所有郵件客戶端的做法）
    formatted_body = plain_body.strip().replace("\n", "<br>")

    img_tag = ('<img src="cid:infocard" '
               'style="max-width:100%;height:auto;display:block;">'
               if img_data else "")

    html_body = (
        "<html><body>"
        "<div style=\"font-family:'微軟正黑體',Arial,sans-serif;"
        "font-size:16px;line-height:1.6;color:#000000;max-width:700px;\">"
        "<div style=\"margin-bottom:20px;\">"
        + formatted_body +
        "</div>"
        "<hr style=\"border:none;border-top:1px solid #c8dfc8;margin:25px 0;\">"
        + img_tag +
        "</div></body></html>"
    )

    msg.set_content(plain_body, charset="utf-8")
    msg.add_alternative(html_body, subtype="html", charset="utf-8")

    # 內嵌圖片掛在 html part 下
    if img_data:
        html_part = msg.get_payload(1)
        html_part.add_related(img_data,
                              maintype=img_main, subtype=img_sub,
                              cid="<infocard>", disposition="inline",
                              filename=os.path.basename(image_path))

    # PDF 附件
    with open(pdf_path, "rb") as f:
        pdf_data = f.read()
    msg.add_attachment(pdf_data,
                       maintype="application", subtype="octet-stream",
                       filename=os.path.basename(pdf_path))
    return msg


# ════════════════════════════════════════════════
#  GUI 主程式
# ════════════════════════════════════════════════
class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("邀請函產生 & 群發系統　｜　2026 森林永續發展與育林技術研討會")
        self.geometry("960x860")
        self.minsize(800, 650)
        self.resizable(True, True)
        self.configure(bg="#f5f3ee")

        self._recipients = []   # [(name, title, email, pdf_filename), ...]
        self._xlsx_path  = tk.StringVar()
        self._img_path   = tk.StringVar()
        self._out_dir    = tk.StringVar(value=os.path.expanduser("~/Desktop/邀請函輸出"))
        self._pdf_done   = False  # 是否已產生 PDF

        self._build_ui()
        setup_fonts()

    # ── UI 建構 ──────────────────────────────────
    def _build_ui(self):
        # 標題列
        hdr = tk.Frame(self, bg="#1a3a2a", height=54)
        hdr.pack(fill="x")
        tk.Label(hdr, text="🌿  邀請函產生 & 群發系統",
                 font=("微軟正黑體", 16, "bold"),
                 bg="#1a3a2a", fg="white").pack(side="left", padx=20, pady=12)

        # 按鈕列（固定在標題列下方，永遠可見）
        btn_frame = tk.Frame(self, bg="#2d5c3f", pady=8)
        btn_frame.pack(fill="x")

        btn_cfg = dict(font=("微軟正黑體", 12, "bold"), relief="flat",
                       cursor="hand2", padx=18, pady=7)
        tk.Button(btn_frame, text="📄  產生邀請函 PDF",
                  bg="#5a9e6f", fg="white", activebackground="#3d7a52",
                  command=self._run_generate, **btn_cfg).pack(side="left", padx=(12, 6))
        tk.Button(btn_frame, text="📧  寄出 Email",
                  bg="#e8a040", fg="white", activebackground="#c8852a",
                  command=self._run_send, **btn_cfg).pack(side="left", padx=6)
        tk.Button(btn_frame, text="🚀  一鍵產生並寄出",
                  bg="#ffffff", fg="#1a3a2a", activebackground="#eaf4ee",
                  command=self._run_all, **btn_cfg).pack(side="left", padx=6)

        # 進度條（緊接在按鈕下方）
        self._progress = ttk.Progressbar(self, mode="determinate")
        self._progress.pack(fill="x", padx=0, pady=0)

        # Notebook
        style = ttk.Style(self)
        style.theme_use("clam")
        style.configure("TNotebook",        background="#f5f3ee", borderwidth=0)
        style.configure("TNotebook.Tab",    font=("微軟正黑體", 11),
                        padding=[16, 6], background="#dde8dd", foreground="#1a3a2a")
        style.map("TNotebook.Tab",
                  background=[("selected", "#3d7a52")],
                  foreground=[("selected", "white")])

        nb = ttk.Notebook(self)
        nb.pack(fill="both", expand=True, padx=16, pady=(10, 0))

        self._tab1 = tk.Frame(nb, bg="#f5f3ee")
        self._tab2 = tk.Frame(nb, bg="#f5f3ee")
        self._tab3 = tk.Frame(nb, bg="#f5f3ee")
        nb.add(self._tab1, text="  📋 活動資訊  ")
        nb.add(self._tab2, text="  📂 檔案 & 名單  ")
        nb.add(self._tab3, text="  📧 Email 設定  ")

        self._build_tab1()
        self._build_tab2()
        self._build_tab3()

        # 執行記錄（固定在最底部，永遠可見）
        log_outer = tk.Frame(self, bg="#1a2a1e", height=160)
        log_outer.pack(fill="x", padx=0, pady=0)
        log_outer.pack_propagate(False)

        tk.Label(log_outer, text=" 執行記錄", bg="#1a2a1e",
                 fg="#5a9e6f", font=("微軟正黑體", 9, "bold"),
                 anchor="w").pack(fill="x", padx=6, pady=(4, 0))

        self._log = scrolledtext.ScrolledText(
            log_outer, font=("Consolas", 10),
            bg="#1a2a1e", fg="#a8d4b0", insertbackground="white",
            relief="flat", state="disabled")
        self._log.pack(fill="both", expand=True, padx=4, pady=(0, 4))

    def _field(self, parent, label, var, row, width=36, placeholder=""):
        tk.Label(parent, text=label, bg="#f5f3ee",
                 font=("微軟正黑體", 11), fg="#2c3e2d",
                 anchor="w").grid(row=row, column=0, sticky="w", padx=8, pady=4)
        e = tk.Entry(parent, textvariable=var, width=width,
                     font=("微軟正黑體", 11), relief="solid", bd=1)
        e.grid(row=row, column=1, sticky="ew", padx=8, pady=4)
        if placeholder and not var.get():
            var.set(placeholder)
        return e

    # ── Tab 1：活動資訊 ──────────────────────────
    def _build_tab1(self):
        # 加捲軸讓所有欄位都能看到
        outer = self._tab1
        canvas = tk.Canvas(outer, bg="#f5f3ee", highlightthickness=0)
        vsb = ttk.Scrollbar(outer, orient="vertical", command=canvas.yview)
        canvas.configure(yscrollcommand=vsb.set)
        vsb.pack(side="right", fill="y")
        canvas.pack(side="left", fill="both", expand=True)

        p = tk.Frame(canvas, bg="#f5f3ee")
        win = canvas.create_window((0, 0), window=p, anchor="nw")

        def _on_frame_configure(e):
            canvas.configure(scrollregion=canvas.bbox("all"))
        def _on_canvas_configure(e):
            canvas.itemconfig(win, width=e.width)
        p.bind("<Configure>", _on_frame_configure)
        canvas.bind("<Configure>", _on_canvas_configure)
        canvas.bind_all("<MouseWheel>",
            lambda e: canvas.yview_scroll(int(-1*(e.delta/120)), "units"))

        p.columnconfigure(1, weight=1)

        tk.Label(p, text="研討會基本資訊",
                 font=("微軟正黑體", 13, "bold"), bg="#f5f3ee",
                 fg="#1a3a2a").grid(row=0, column=0, columnspan=2,
                                    sticky="w", padx=8, pady=(16, 8))

        self.v_event_date     = tk.StringVar(value="115年 5月21日–22日")
        self.v_event_weekday  = tk.StringVar(value="星期四·五")
        self.v_venue          = tk.StringVar(value="農業部林業試驗所國際會議廳")
        self.v_venue_addr     = tk.StringVar(value="台北市中正區三元街67號12F")
        self.v_dinner_date    = tk.StringVar(value="115年 5月21日")
        self.v_dinner_weekday = tk.StringVar(value="星期四")
        self.v_dinner_time    = tk.StringVar(value="18:00")
        self.v_dinner_venue   = tk.StringVar(value="新東南海鮮餐廳")
        self.v_dinner_addr    = tk.StringVar(value="臺北市中正區汀州路一段105號")

        self._field(p, "研討會日期（民國）",   self.v_event_date,     1)
        self._field(p, "星期",                 self.v_event_weekday,  2)
        self._field(p, "會議廳名稱",           self.v_venue,          3)
        self._field(p, "會議廳地址",           self.v_venue_addr,     4)

        tk.Label(p, text="─" * 60, bg="#f5f3ee", fg="#c8dfc8").grid(
            row=5, column=0, columnspan=2, pady=4)

        self._field(p, "餐敘日期（民國）",     self.v_dinner_date,    6)
        self._field(p, "餐敘星期",             self.v_dinner_weekday, 7)
        self._field(p, "餐敘時間",             self.v_dinner_time,    8)
        self._field(p, "餐敘地點",             self.v_dinner_venue,   9)
        self._field(p, "餐敘地址",             self.v_dinner_addr,    10)

        tk.Label(p, text="─" * 60, bg="#f5f3ee", fg="#c8dfc8").grid(
            row=11, column=0, columnspan=2, pady=4)

        tk.Label(p, text="主辦單位署名",
                 font=("微軟正黑體", 13, "bold"), bg="#f5f3ee",
                 fg="#1a3a2a").grid(row=12, column=0, columnspan=2,
                                    sticky="w", padx=8, pady=(8, 4))

        self.v_h1_org   = tk.StringVar(value="農業部林業試驗所")
        self.v_h1_title = tk.StringVar(value="所長")
        self.v_h1_name  = tk.StringVar(value="曾彥學")
        self.v_h2_org   = tk.StringVar(value="農業部林業試驗所")
        self.v_h2_title = tk.StringVar(value="育林組組長")
        self.v_h2_name  = tk.StringVar(value="鍾振德")

        self._field(p, "第一位（機關）", self.v_h1_org,   13)
        self._field(p, "第一位（職稱）", self.v_h1_title, 14)
        self._field(p, "第一位（姓名）", self.v_h1_name,  15)
        self._field(p, "第二位（機關）", self.v_h2_org,   16)
        self._field(p, "第二位（職稱）", self.v_h2_title, 17)
        self._field(p, "第二位（姓名）", self.v_h2_name,  18)

        tk.Label(p, text="─" * 60, bg="#f5f3ee", fg="#c8dfc8").grid(
            row=19, column=0, columnspan=2, pady=4)

        tk.Label(p, text="出席確認表單",
                 font=("微軟正黑體", 13, "bold"), bg="#f5f3ee",
                 fg="#1a3a2a").grid(row=20, column=0, columnspan=2,
                                    sticky="w", padx=8, pady=(8, 4))

        self.v_form_url = tk.StringVar(
            value="https://docs.google.com/forms/d/e/1FAIpQLSeX54OgUv52IcAJrBRZZa-wkBcWHkQffqN9DU-YQfU4fr8lpQ/viewform")
        self.v_entry_name  = tk.StringVar(value="entry.1112468528")
        self.v_entry_title = tk.StringVar(value="entry.116035932")

        self._field(p, "表單 viewform 網址", self.v_form_url,    21, width=60)
        tk.Label(p, text="  ↑ 請使用 /viewform 結尾的網址（非 /edit）",
                 bg="#f5f3ee", font=("微軟正黑體", 9), fg="#8ab89a").grid(
            row=22, column=0, columnspan=2, sticky="w", padx=8)
        self._field(p, "姓名欄位 entry ID",  self.v_entry_name,  23, width=30)
        self._field(p, "職稱欄位 entry ID",  self.v_entry_title, 24, width=30)
        tk.Label(p, text="  ↑ 在表單原始碼搜尋 entry.xxxxxxx 取得",
                 bg="#f5f3ee", font=("微軟正黑體", 9), fg="#8ab89a").grid(
            row=25, column=0, columnspan=2, sticky="w", padx=8)

    # ── Tab 2：檔案 & 名單 ───────────────────────
    def _build_tab2(self):
        p = self._tab2
        p.columnconfigure(1, weight=1)

        def file_row(row, label, var, filetypes, btn_text):
            tk.Label(p, text=label, bg="#f5f3ee",
                     font=("微軟正黑體", 11), fg="#2c3e2d").grid(
                row=row, column=0, sticky="w", padx=8, pady=6)
            tk.Entry(p, textvariable=var, font=("微軟正黑體", 10),
                     relief="solid", bd=1, state="readonly").grid(
                row=row, column=1, sticky="ew", padx=8, pady=6)
            tk.Button(p, text=btn_text, font=("微軟正黑體", 10),
                      bg="#3d7a52", fg="white", relief="flat",
                      command=lambda v=var, ft=filetypes: self._browse(v, ft)).grid(
                row=row, column=2, padx=4)

        file_row(0, "📊 名單 Excel (.xlsx)",
                 self._xlsx_path,
                 [("Excel", "*.xlsx")], "選擇")

        # 下載範例按鈕（獨立一列）
        tk.Button(p, text="📥 下載範例 Excel（格式說明）",
                  font=("微軟正黑體", 10), bg="#1a3a2a", fg="white",
                  relief="flat", cursor="hand2",
                  command=self._download_sample_xlsx).grid(
            row=1, column=1, sticky="w", padx=8, pady=(0, 6))

        file_row(2, "🖼 活動資訊圖卡 (.png/.jpg)",
                 self._img_path,
                 [("圖片", "*.png *.jpg *.jpeg")], "選擇")

        tk.Label(p, text="📁 PDF 輸出資料夾", bg="#f5f3ee",
                 font=("微軟正黑體", 11), fg="#2c3e2d").grid(
            row=3, column=0, sticky="w", padx=8, pady=6)
        tk.Entry(p, textvariable=self._out_dir, font=("微軟正黑體", 10),
                 relief="solid", bd=1).grid(
            row=3, column=1, sticky="ew", padx=8, pady=6)
        tk.Button(p, text="選擇", font=("微軟正黑體", 10),
                  bg="#3d7a52", fg="white", relief="flat",
                  command=lambda: self._out_dir.set(
                      filedialog.askdirectory() or self._out_dir.get())).grid(
            row=3, column=2, padx=4)

        # 載入名單按鈕
        tk.Button(p, text="🔄  載入名單預覽",
                  font=("微軟正黑體", 11, "bold"),
                  bg="#1a3a2a", fg="white", relief="flat",
                  command=self._load_xlsx).grid(
            row=4, column=0, columnspan=3, pady=12, padx=8, sticky="w")

        # 名單預覽表格
        cols = ("序", "姓名", "職稱", "Email", "PDF 附件")
        self._tree = ttk.Treeview(p, columns=cols, show="headings", height=10)
        for col, w in zip(cols, [40, 80, 90, 200, 200]):
            self._tree.heading(col, text=col)
            self._tree.column(col, width=w, anchor="w")
        self._tree.grid(row=5, column=0, columnspan=3, sticky="nsew", padx=8)

        sb = ttk.Scrollbar(p, orient="vertical", command=self._tree.yview)
        self._tree.configure(yscrollcommand=sb.set)
        sb.grid(row=5, column=3, sticky="ns")
        p.rowconfigure(5, weight=1)

        self._count_label = tk.Label(p, text="尚未載入名單",
                                     font=("微軟正黑體", 10), bg="#f5f3ee",
                                     fg="#8ab89a")
        self._count_label.grid(row=6, column=0, columnspan=3,
                                sticky="w", padx=8, pady=4)

    # ── Tab 3：Email 設定 ─────────────────────────
    def _build_tab3(self):
        outer = self._tab3
        canvas = tk.Canvas(outer, bg="#f5f3ee", highlightthickness=0)
        vsb = ttk.Scrollbar(outer, orient="vertical", command=canvas.yview)
        canvas.configure(yscrollcommand=vsb.set)
        vsb.pack(side="right", fill="y")
        canvas.pack(side="left", fill="both", expand=True)
        p = tk.Frame(canvas, bg="#f5f3ee")
        win = canvas.create_window((0, 0), window=p, anchor="nw")
        def _on_fc(e): canvas.configure(scrollregion=canvas.bbox("all"))
        def _on_cc(e): canvas.itemconfig(win, width=e.width)
        p.bind("<Configure>", _on_fc)
        canvas.bind("<Configure>", _on_cc)
        canvas.bind_all("<MouseWheel>",
            lambda e: canvas.yview_scroll(int(-1*(e.delta/120)), "units"))
        p.columnconfigure(1, weight=1)

        self.v_sender   = tk.StringVar(value="a61216louis@gmail.com")
        self.v_passwd   = tk.StringVar(value="")
        self.v_smtp     = tk.StringVar(value="smtp.gmail.com")
        self.v_port     = tk.StringVar(value="465")
        self.v_test_to  = tk.StringVar(value="")
        self.v_delay    = tk.StringVar(value="3")

        tk.Label(p, text="Gmail SMTP 設定",
                 font=("微軟正黑體", 13, "bold"), bg="#f5f3ee",
                 fg="#1a3a2a").grid(row=0, column=0, columnspan=2,
                                    sticky="w", padx=8, pady=(16, 8))

        self._field(p, "發信 Email",      self.v_sender, 1)
        tk.Label(p, text="應用程式密碼", bg="#f5f3ee",
                 font=("微軟正黑體", 11), fg="#2c3e2d").grid(
            row=2, column=0, sticky="w", padx=8, pady=4)
        tk.Entry(p, textvariable=self.v_passwd, show="●", width=36,
                 font=("微軟正黑體", 11), relief="solid", bd=1).grid(
            row=2, column=1, sticky="ew", padx=8, pady=4)

        pw_help_frame = tk.Frame(p, bg="#f5f3ee")
        pw_help_frame.grid(row=3, column=0, columnspan=2, sticky="w", padx=8, pady=(0, 4))

        tk.Label(pw_help_frame, text="  ↑ 請至",
                 bg="#f5f3ee", font=("微軟正黑體", 9), fg="#8ab89a").pack(side="left")

        pw_link = tk.Label(
            pw_help_frame, text="myaccount.google.com/apppasswords",
            bg="#f5f3ee", font=("微軟正黑體", 9, "underline"), fg="#3d7a52",
            cursor="hand2")
        pw_link.pack(side="left")
        pw_link.bind("<Button-1>", lambda e: webbrowser.open(
            "https://myaccount.google.com/apppasswords"))

        tk.Label(pw_help_frame, text=" 取得應用程式密碼",
                 bg="#f5f3ee", font=("微軟正黑體", 9), fg="#8ab89a").pack(side="left")

        tk.Label(
            p,
            text=("     （需先開啟 Google 帳戶「兩步驟驗證」）→ 輸入應用程式名稱（如：邀請函系統）\n"
                  "     → 點擊「建立」→ 複製產生的 16 碼密碼並貼於此欄（此密碼不會被儲存於檔案中）"),
            bg="#f5f3ee", font=("微軟正黑體", 9), fg="#8ab89a",
            justify="left", anchor="w").grid(
            row=4, column=0, columnspan=2, sticky="w", padx=8, pady=(0, 4))

        self._field(p, "SMTP 伺服器",    self.v_smtp,  5)
        self._field(p, "Port",           self.v_port,  6, width=10)
        self._field(p, "寄信間隔（秒）", self.v_delay, 7, width=10)

        tk.Label(p, text="─" * 60, bg="#f5f3ee", fg="#c8dfc8").grid(
            row=8, column=0, columnspan=2, pady=4)

        tk.Label(p, text="試寄設定",
                 font=("微軟正黑體", 13, "bold"), bg="#f5f3ee",
                 fg="#1a3a2a").grid(row=9, column=0, columnspan=2,
                                    sticky="w", padx=8, pady=(8, 4))
        self._field(p, "試寄收件地址\n（留空則寄全部）", self.v_test_to, 10)
        tk.Label(p, text="  ↑ 填入 Email 後點「寄出 Email」，只寄一封測試信",
                 bg="#f5f3ee", font=("微軟正黑體", 9), fg="#8ab89a").grid(
            row=11, column=0, columnspan=2, sticky="w", padx=8)

        tk.Label(p, text="─" * 60, bg="#f5f3ee", fg="#c8dfc8").grid(
            row=12, column=0, columnspan=2, pady=4)

        tk.Label(p, text="Email 主旨範本",
                 font=("微軟正黑體", 11), bg="#f5f3ee",
                 fg="#2c3e2d").grid(row=13, column=0, sticky="nw", padx=8, pady=4)
        self._subject_txt = tk.Text(p, height=2, font=("微軟正黑體", 10),
                                    relief="solid", bd=1, wrap="word")
        self._subject_txt.insert("1.0",
            "【邀請函】誠摯邀請 {name} {title} 蒞臨「2026 森林永續發展與育林技術研討會」")
        self._subject_txt.grid(row=13, column=1, sticky="ew", padx=8, pady=4)

        tk.Label(p, text="Email 內文範本",
                 font=("微軟正黑體", 11), bg="#f5f3ee",
                 fg="#2c3e2d").grid(row=14, column=0, sticky="nw", padx=8, pady=4)
        self._body_txt = tk.Text(p, height=22, font=("微軟正黑體", 10),
                                 relief="solid", bd=1, wrap="word")
        default_body = """{name} {title} 您好：

農業部林業試驗所育林組將於民國 115 年 5 月 21 日至 22 日舉辦「2026 森林永續發展與育林技術研討會」。

隨信附上為您特別準備的專屬邀請函（PDF 附件），誠摯邀請您撥冗出席指導。

此外，5 月 21 日（星期四）晚間 18:00，我們將於新東南海鮮餐廳（臺北市中正區汀州路一段 105 號）舉辦會後餐敘，期待您的光臨。

研討會資訊摘要：

  ・日期：民國 115 年 5 月 21 日至 22 日（星期四、五）
  ・地點：農業部林業試驗所 國際會議廳
  ・餐敘：5 月 21 日（星期四）18:00 新東南海鮮餐廳

（報名連結：https://www.tfri.gov.tw/News_Content.aspx?n=7523&s=34855）

農業部林業試驗所 所長 曾彥學 敬邀
農業部林業試驗所 育林組組長 鍾振德 敬邀

（承辦人：育林組 林冠穎 李俊佑 敬上）"""
        self._body_txt.insert("1.0", default_body)
        self._body_txt.grid(row=14, column=1, sticky="ew", padx=8, pady=4)

    # ── 工具函式 ────────────────────────────────
    def _browse(self, var, filetypes):
        path = filedialog.askopenfilename(filetypes=filetypes)
        if path:
            var.set(path)

    def _log_msg(self, msg):
        self._log.configure(state="normal")
        self._log.insert("end", msg + "\n")
        self._log.see("end")
        self._log.configure(state="disabled")
        self.update_idletasks()

    def _load_xlsx(self):
        path = self._xlsx_path.get()
        if not path or not os.path.exists(path):
            messagebox.showerror("錯誤", "請先選擇名單 Excel 檔案")
            return
        try:
            wb = openpyxl.load_workbook(path)
            ws = wb.active
            rows = list(ws.iter_rows(values_only=True))
            data = []
            for i, row in enumerate(rows[1:], 1):  # 跳過標題
                if not row[0] or not row[1] or not row[2]:
                    continue
                name  = str(row[0]).strip()
                title = str(row[1]).strip()
                email = str(row[2]).strip()
                # 自動產生附件檔名（忽略 Excel 中的欄位）
                pdf_name = f"{i:02d}_{name}_{title}.pdf"
                data.append((name, title, email, pdf_name))

            self._recipients = data

            # 更新表格
            for item in self._tree.get_children():
                self._tree.delete(item)
            for i, (n, t, e, pdf) in enumerate(data, 1):
                self._tree.insert("", "end", values=(i, n, t, e, pdf))
            self._count_label.config(text=f"✅ 已載入 {len(data)} 位收件人")
            self._log_msg(f"✅ 名單載入完成，共 {len(data)} 位")
            self._log_msg(f"   PDF 檔名將自動產生為「序號_姓名_職稱.pdf」")
        except Exception as ex:
            messagebox.showerror("載入失敗", str(ex))

    def _download_sample_xlsx(self):
        save_path = filedialog.asksaveasfilename(
            title="儲存範例名單",
            defaultextension=".xlsx",
            filetypes=[("Excel", "*.xlsx")],
            initialfile="邀請名單範例.xlsx"
        )
        if not save_path:
            return
        try:
            wb = openpyxl.Workbook()
            ws = wb.active
            ws.title = "邀請名單"

            # 標題列
            headers = ["姓名", "職稱", "電子郵件", "附件名稱"]
            for col, h in enumerate(headers, 1):
                cell = ws.cell(row=1, column=col, value=h)
                cell.font = openpyxl.styles.Font(bold=True, color="FFFFFF")
                cell.fill = openpyxl.styles.PatternFill(
                    fill_type="solid", fgColor="1a3a2a")
                cell.alignment = openpyxl.styles.Alignment(horizontal="center")

            # 範例資料（3筆）
            samples = [
                ("曾彥學", "所長",     "example1@tfri.gov.tw", ""),
                ("鍾振德", "育林組組長", "example2@tfri.gov.tw", ""),
                ("張三",   "教授",     "example3@ntu.edu.tw",  ""),
            ]
            for row, (name, title, email, pdf) in enumerate(samples, 2):
                ws.cell(row=row, column=1, value=name)
                ws.cell(row=row, column=2, value=title)
                ws.cell(row=row, column=3, value=email)
                ws.cell(row=row, column=4, value=pdf)

            # 欄寬
            ws.column_dimensions["A"].width = 12
            ws.column_dimensions["B"].width = 16
            ws.column_dimensions["C"].width = 30
            ws.column_dimensions["D"].width = 28

            # 備註列
            ws.cell(row=6, column=1,
                    value="※ 附件名稱欄可留空，系統產生 PDF 後自動填入")
            ws.cell(row=6, column=1).font = openpyxl.styles.Font(
                color="888888", italic=True)
            ws.merge_cells("A6:D6")

            wb.save(save_path)
            messagebox.showinfo("完成", f"✅ 範例檔已儲存至：\n{save_path}")
        except Exception as ex:
            messagebox.showerror("錯誤", str(ex))

    def _validate(self):
        if not self._recipients:
            messagebox.showerror("錯誤", "請先載入收件人名單（Tab 2）")
            return False
        return True

    # ── 產生 PDF ────────────────────────────────
    def _run_generate(self):
        if not self._validate():
            return
        threading.Thread(target=self._do_generate, daemon=True).start()

    def _do_generate(self):
        out = self._out_dir.get()
        os.makedirs(out, exist_ok=True)
        total = len(self._recipients)
        self._progress["maximum"] = total
        self._log_msg(f"\n📄 開始產生 PDF，共 {total} 份...")

        for i, (name, title, email, pdf_name) in enumerate(self._recipients, 1):
            out_path = os.path.join(out, pdf_name)
            try:
                make_pdf(
                    seq=i, name=name, title=title,
                    event_date=self.v_event_date.get(),
                    event_weekday=self.v_event_weekday.get(),
                    venue=self.v_venue.get(),
                    venue_addr=self.v_venue_addr.get(),
                    dinner_date=self.v_dinner_date.get(),
                    dinner_weekday=self.v_dinner_weekday.get(),
                    dinner_time=self.v_dinner_time.get(),
                    dinner_venue=self.v_dinner_venue.get(),
                    dinner_addr=self.v_dinner_addr.get(),
                    host1_org=self.v_h1_org.get(),
                    host1_title=self.v_h1_title.get(),
                    host1_name=self.v_h1_name.get(),
                    host2_org=self.v_h2_org.get(),
                    host2_title=self.v_h2_title.get(),
                    host2_name=self.v_h2_name.get(),
                    form_url=self.v_form_url.get(),
                    entry_name_id=self.v_entry_name.get(),
                    entry_title_id=self.v_entry_title.get(),
                    out_path=out_path
                )
                self._log_msg(f"  [{i:02d}/{total}] ✓ {name} {title}")
            except Exception as ex:
                self._log_msg(f"  [{i:02d}/{total}] ✗ {name}：{ex}")
            self._progress["value"] = i

        self._pdf_done = True
        self._log_msg(f"\n✅ 所有 PDF 已輸出至：{out}")

        # 將附件檔名寫回 Excel
        try:
            xlsx_path = self._xlsx_path.get()
            if xlsx_path and os.path.exists(xlsx_path):
                wb = openpyxl.load_workbook(xlsx_path)
                ws = wb.active
                # 確保標題列第4欄有「附件名稱」
                ws.cell(row=1, column=4, value="附件名稱")
                for i, (name, title, email, pdf_name) in enumerate(self._recipients, 2):
                    ws.cell(row=i, column=4, value=pdf_name)
                wb.save(xlsx_path)
                self._log_msg(f"📊 附件檔名已寫回 Excel：{os.path.basename(xlsx_path)}")
        except Exception as ex:
            self._log_msg(f"⚠️  寫回 Excel 失敗：{ex}")

        messagebox.showinfo("完成", f"✅ {total} 份 PDF 已產生！\n位置：{out}")

    # ── 寄送 Email ───────────────────────────────
    def _run_send(self):
        if not self._validate():
            return
        if not self._pdf_done:
            if not messagebox.askyesno("確認",
                "尚未產生 PDF，請確認 PDF 已存在於輸出資料夾。\n是否繼續寄送？"):
                return
        threading.Thread(target=self._do_send, daemon=True).start()

    def _do_send(self):
        out    = self._out_dir.get()
        img    = self._img_path.get()
        sender = self.v_sender.get()
        passwd = self.v_passwd.get()
        smtp   = self.v_smtp.get()
        port   = int(self.v_port.get())
        delay  = float(self.v_delay.get())
        test_to = self.v_test_to.get().strip()
        subject_tmpl = self._subject_txt.get("1.0", "end").strip()
        body_tmpl    = self._body_txt.get("1.0", "end").strip()

        send_list = self._recipients
        if test_to:
            r = self._recipients[0]
            send_list = [(r[0], r[1], test_to, r[3])]
            self._log_msg(f"\n📧 單封試寄模式 → {test_to}")
        else:
            self._log_msg(f"\n📧 開始寄送，共 {len(send_list)} 封...")

        errors = []
        total  = len(send_list)
        self._progress["maximum"] = total

        try:
            ctx = ssl.create_default_context()
            with smtplib.SMTP_SSL(smtp, port, context=ctx,
                                  local_hostname="localhost") as server:
                server.login(sender, passwd)
                self._log_msg("✅ SMTP 登入成功")

                for i, (name, title, email, pdf_name) in enumerate(send_list, 1):
                    pdf_path = os.path.join(out, pdf_name)
                    if not os.path.exists(pdf_path):
                        self._log_msg(f"  [{i:02d}/{total}] ✗ 找不到 PDF：{pdf_name}")
                        errors.append((name, email, "找不到PDF"))
                        continue
                    try:
                        msg = send_email(name, title, email, pdf_path, img,
                                         subject_tmpl, body_tmpl, sender)
                        server.sendmail(sender, [email], msg.as_bytes())
                        self._log_msg(f"  [{i:02d}/{total}] ✓ {name} {title} → {email}")
                    except Exception as ex:
                        err_str = str(ex)
                        self._log_msg(f"  [{i:02d}/{total}] ✗ {email}：{err_str}")
                        errors.append((name, email, err_str))
                        # 單封試寄時直接彈出詳細錯誤
                        if test_to:
                            self.after(0, lambda e=err_str: messagebox.showerror(
                                "寄送失敗 — 詳細原因", e))
                    self._progress["value"] = i
                    if i < total:
                        time.sleep(delay)

        except Exception as ex:
            self._log_msg(f"❌ 連線失敗：{ex}")
            messagebox.showerror("連線失敗", str(ex))
            return

        ok = total - len(errors)
        self._log_msg(f"\n{'='*40}")
        self._log_msg(f"  完成！成功 {ok} / 共 {total} 封")
        if errors:
            self._log_msg("  失敗名單：")
            for n, e, err in errors:
                self._log_msg(f"    {n} <{e}>：{err}")
        msg = f"✅ 成功寄出 {ok}/{total} 封"
        if errors:
            msg += f"\n❌ 失敗 {len(errors)} 封，請查看記錄"
        messagebox.showinfo("寄送完成", msg)

    # ── 一鍵產生並寄出 ────────────────────────────
    def _run_all(self):
        if not self._validate():
            return
        threading.Thread(target=self._do_all, daemon=True).start()

    def _do_all(self):
        self._do_generate()
        self._do_send()


if __name__ == "__main__":
    app = App()
    app.mainloop()
