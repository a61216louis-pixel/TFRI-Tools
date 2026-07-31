#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
電子印章產生器 GUI
輸入姓名 -> 產生「長方橫式章」與「標楷體正方章(姓右/名左)」-> 預覽 -> 存檔

使用方式：
    python stamp_gui.py

需要套件：pillow, numpy
    pip install pillow numpy

字型需求：
    - 長方橫式章使用「粗明體/黑體」風格字型
    - 正方章使用「標楷體」風格字型 (Windows 內建 DFKai-SB, 檔名通常是 kaiu.ttf)
    程式會自動嘗試常見路徑，找不到時會跳出「選擇字型檔」對話框，
    選過一次之後會記住路徑（本次執行期間有效）。
"""

import os
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox

import numpy as np
from PIL import Image, ImageDraw, ImageFont, ImageTk

# ---------- 參數設定 ----------
STAMP_COLOR = (200, 30, 30)  # 印泥紅色
SIZE = 600
SUPERSAMPLE = 4

# 常見字型路徑候選（依序嘗試，找到第一個存在的就用）
BOLD_FONT_CANDIDATES = [
    r"C:\Windows\Fonts\msjhbd.ttc",   # 微軟正黑體 粗體 (Windows 常見)
    r"C:\Windows\Fonts\mingliub.ttc", # 新細明體 粗體
    "/usr/share/fonts/opentype/noto/NotoSerifCJK-Bold.ttc",  # Linux/Mac 常見路徑
    "/System/Library/Fonts/PingFang.ttc",
]
KAITI_FONT_CANDIDATES = [
    r"C:\Windows\Fonts\kaiu.ttf",     # 標楷體 DFKai-SB (Windows 內建)
    "/usr/share/fonts/truetype/cwtex/cwkai.ttf",  # Linux cwTeX 楷書
    "/System/Library/Fonts/STKaiti.ttc",
]


def find_font(candidates, fallback_title):
    for path in candidates:
        if os.path.exists(path):
            return path
    # 找不到，跳出選擇對話框
    messagebox.showinfo(
        "需要選擇字型",
        f"找不到內建的{fallback_title}字型，請手動選擇一個 .ttf/.ttc/.otf 字型檔。"
    )
    path = filedialog.askopenfilename(
        title=f"請選擇{fallback_title}字型檔",
        filetypes=[("字型檔", "*.ttf *.ttc *.otf"), ("全部檔案", "*.*")]
    )
    if not path:
        raise RuntimeError(f"未選擇{fallback_title}字型，無法產生印章。")
    return path


# ---------- 印章繪製核心邏輯 ----------

def add_ink_texture(rgba_img, seed=42, noise_strength=38, roughen=True):
    """在印章圖層疊加雜訊，模擬印泥不均勻、略帶斑駁的質感。"""
    rng = np.random.default_rng(seed)
    arr = np.array(rgba_img).astype(np.int16)
    alpha = arr[:, :, 3]

    mask = alpha > 0
    noise = rng.integers(-noise_strength, noise_strength + 1, size=alpha.shape)
    new_alpha = alpha.copy()
    new_alpha[mask] = np.clip(alpha[mask] + noise[mask], 0, 255)
    arr[:, :, 3] = new_alpha

    if roughen:
        h, w = alpha.shape
        n_holes = rng.integers(15, 30)
        for _ in range(n_holes):
            cx, cy = rng.integers(0, w), rng.integers(0, h)
            r = rng.integers(2, 6)
            yy, xx = np.ogrid[:h, :w]
            hole_mask = (xx - cx) ** 2 + (yy - cy) ** 2 <= r ** 2
            arr[:, :, 3][hole_mask] = np.clip(
                arr[:, :, 3][hole_mask].astype(np.int16) - rng.integers(60, 150), 0, 255
            )

    arr[:, :, 3] = np.clip(arr[:, :, 3], 0, 255)
    return Image.fromarray(arr.astype(np.uint8), mode="RGBA")


def make_horizontal_rect_stamp(name, font_path, w_ratio=1.8):
    """橫式長方章：外框為長方形，姓名橫式排列於正中央"""
    ss = SUPERSAMPLE
    canvas_h = SIZE * ss
    canvas_w = int(canvas_h * w_ratio)
    img = Image.new("RGBA", (canvas_w, canvas_h), (0, 0, 0, 0))
    draw = ImageDraw.Draw(img)

    margin = int(canvas_h * 0.08)
    draw.rectangle([margin, margin, canvas_w - margin, canvas_h - margin],
                    outline=STAMP_COLOR, width=int(10 * ss / 4))
    inner_margin = margin + int(16 * ss)
    draw.rectangle([inner_margin, inner_margin, canvas_w - inner_margin, canvas_h - inner_margin],
                    outline=STAMP_COLOR, width=int(4 * ss / 4))

    font_size = int(canvas_h * 0.42)
    font = ImageFont.truetype(font_path, font_size)
    letter_spacing = int(font_size * 0.15)

    chars = list(name)
    widths = []
    for ch in chars:
        bbox = draw.textbbox((0, 0), ch, font=font)
        widths.append(bbox[2] - bbox[0])
    total_w = sum(widths) + letter_spacing * (len(chars) - 1)

    cx, cy = canvas_w // 2, canvas_h // 2
    x = cx - total_w // 2
    for ch, w in zip(chars, widths):
        bbox = draw.textbbox((0, 0), ch, font=font)
        y = cy - (bbox[3] - bbox[1]) // 2 - bbox[1]
        draw.text((x - bbox[0], y), ch, font=font, fill=STAMP_COLOR)
        x += w + letter_spacing

    img = add_ink_texture(img, seed=3)
    img = img.resize((int(SIZE * w_ratio), SIZE), Image.LANCZOS)
    return img


def make_split_kaiti_square_stamp(surname, given_name, font_path,
                                    full_bleed=True, show_divider=False, max_fill=True,
                                    w_ratio=1.0):
    """
    標楷體印章：姓氏置於右半、佔滿全高；名字置於左半，上下堆疊。
    """
    ss = SUPERSAMPLE
    canvas_h = SIZE * ss
    canvas_w = int(canvas_h * w_ratio)
    canvas = canvas_h
    img = Image.new("RGBA", (canvas_w, canvas_h), (0, 0, 0, 0))
    draw = ImageDraw.Draw(img)

    if full_bleed:
        margin = int(canvas * (0.025 if max_fill else 0.035))
        border_w = int(14 * ss / 4)
        draw.rectangle([margin, margin, canvas_w - margin, canvas_h - margin],
                        outline=STAMP_COLOR, width=border_w)
        inner_margin = margin + int((3 if max_fill else 6) * ss)
    else:
        margin = int(canvas * 0.06)
        draw.rectangle([margin, margin, canvas_w - margin, canvas_h - margin],
                        outline=STAMP_COLOR, width=int(10 * ss / 4))
        inner_margin = margin + int(16 * ss)
        draw.rectangle([inner_margin, inner_margin, canvas_w - inner_margin, canvas_h - inner_margin],
                        outline=STAMP_COLOR, width=int(4 * ss / 4))

    mid_x = canvas_w // 2
    if show_divider:
        draw.line([(mid_x, inner_margin), (mid_x, canvas_h - inner_margin)],
                   fill=STAMP_COLOR, width=int(4 * ss / 4))

    content_top = inner_margin
    content_bottom = canvas_h - inner_margin
    content_h = content_bottom - content_top

    # 右半：姓氏
    right_scale = 1.06 if max_fill else (0.95 if full_bleed else 0.78)
    right_font_size = max(1, int(content_h * right_scale))
    right_font = ImageFont.truetype(font_path, right_font_size)
    bbox = draw.textbbox((0, 0), surname, font=right_font)
    w, h = bbox[2] - bbox[0], bbox[3] - bbox[1]

    safety_pad = canvas * (0.004 if max_fill else 0.012)
    right_area_left = mid_x + int(safety_pad)
    right_area_right = canvas_w - inner_margin - int(safety_pad)
    right_area_w = max(1, right_area_right - right_area_left)

    vpad = int(canvas * (0.004 if max_fill else 0.0))
    v_area_h = content_h - 2 * vpad

    glyph_img = Image.new("RGBA", (w + 4, h + 4), (0, 0, 0, 0))
    glyph_draw = ImageDraw.Draw(glyph_img)
    glyph_draw.text((-bbox[0] + 2, -bbox[1] + 2), surname, font=right_font, fill=STAMP_COLOR)

    if h > v_area_h:
        scale_factor = v_area_h / h
        glyph_img = glyph_img.resize((max(1, int((w + 4) * scale_factor)), max(1, int(v_area_h))), Image.LANCZOS)
        w = glyph_img.size[0] - 4

    if w > right_area_w:
        new_w = max(1, right_area_w)
        glyph_img = glyph_img.resize((new_w, glyph_img.size[1]), Image.LANCZOS)
        w = new_w

    right_cx = right_area_left + right_area_w // 2
    right_cy = content_top + content_h // 2
    img.alpha_composite(glyph_img, (right_cx - w // 2, right_cy - glyph_img.size[1] // 2))

    # 左半：名字，上下堆疊
    left_chars = list(given_name)
    n = max(1, len(left_chars))
    cell_h = content_h / n
    left_scale = 0.98 if max_fill else (0.92 if full_bleed else 0.72)
    left_font_size = max(1, int(cell_h * left_scale))
    left_font = ImageFont.truetype(font_path, left_font_size)
    left_cx = inner_margin + (mid_x - inner_margin) // 2
    left_area_w = max(1, (mid_x - int(safety_pad)) - (inner_margin + int(safety_pad)))

    for i, ch in enumerate(left_chars):
        bbox = draw.textbbox((0, 0), ch, font=left_font)
        w2, h2 = bbox[2] - bbox[0], bbox[3] - bbox[1]
        cell_cy = content_top + cell_h * i + cell_h / 2

        cglyph = Image.new("RGBA", (w2 + 4, h2 + 4), (0, 0, 0, 0))
        cdraw = ImageDraw.Draw(cglyph)
        cdraw.text((-bbox[0] + 2, -bbox[1] + 2), ch, font=left_font, fill=STAMP_COLOR)

        if w2 > left_area_w:
            new_w2 = max(1, left_area_w)
            cglyph = cglyph.resize((new_w2, cglyph.size[1]), Image.LANCZOS)
            w2 = new_w2

        img.alpha_composite(cglyph, (int(left_cx - w2 // 2), int(cell_cy - cglyph.size[1] // 2)))

    img = add_ink_texture(img, seed=4)
    img = img.resize((max(1, int(SIZE * w_ratio)), SIZE), Image.LANCZOS)
    return img


# ---------- GUI ----------

class StampApp:
    def __init__(self, root):
        self.root = root
        root.title("電子印章產生器")
        root.geometry("880x560")
        root.resizable(False, False)

        self.bold_font_path = None
        self.kaiti_font_path = None
        self.rect_image = None   # PIL Image，最新產生的長方章
        self.square_image = None  # PIL Image，最新產生的正方章

        self._build_ui()

    def _build_ui(self):
        pad = {"padx": 8, "pady": 6}

        top = ttk.Frame(self.root)
        top.pack(fill="x", **pad)

        ttk.Label(top, text="姓名：").pack(side="left")
        self.name_var = tk.StringVar()
        name_entry = ttk.Entry(top, textvariable=self.name_var, width=14, font=("Microsoft JhengHei", 14))
        name_entry.pack(side="left", padx=4)
        name_entry.bind("<KeyRelease>", self._on_name_change)

        ttk.Label(top, text="姓氏(可手動調整)：").pack(side="left", padx=(16, 0))
        self.surname_var = tk.StringVar()
        ttk.Entry(top, textvariable=self.surname_var, width=6, font=("Microsoft JhengHei", 14)).pack(side="left", padx=4)

        ttk.Label(top, text="名字：").pack(side="left")
        self.given_var = tk.StringVar()
        ttk.Entry(top, textvariable=self.given_var, width=10, font=("Microsoft JhengHei", 14)).pack(side="left", padx=4)

        gen_btn = ttk.Button(top, text="產生印章", command=self.generate)
        gen_btn.pack(side="left", padx=(16, 0))

        # 預覽區
        preview = ttk.Frame(self.root)
        preview.pack(fill="both", expand=True, **pad)

        rect_frame = ttk.LabelFrame(preview, text="長方橫式章")
        rect_frame.pack(side="left", fill="both", expand=True, padx=4)
        self.rect_canvas = tk.Canvas(rect_frame, width=380, height=220, bg="white")
        self.rect_canvas.pack(padx=6, pady=6)
        ttk.Button(rect_frame, text="另存新檔...", command=self.save_rect).pack(pady=4)

        square_frame = ttk.LabelFrame(preview, text="標楷體正方章")
        square_frame.pack(side="left", fill="both", expand=True, padx=4)
        self.square_canvas = tk.Canvas(square_frame, width=300, height=300, bg="white")
        self.square_canvas.pack(padx=6, pady=6)
        ttk.Button(square_frame, text="另存新檔...", command=self.save_square).pack(pady=4)

        # 狀態列
        self.status_var = tk.StringVar(value="請輸入姓名，然後按「產生印章」")
        ttk.Label(self.root, textvariable=self.status_var, foreground="gray").pack(fill="x", padx=8, pady=(0, 8))

    def _on_name_change(self, event=None):
        name = self.name_var.get().strip()
        if not name:
            return
        # 預設：第一個字當姓氏，其餘當名字（雙姓可在「姓氏」欄手動修正）
        self.surname_var.set(name[0])
        self.given_var.set(name[1:] if len(name) > 1 else "")

    def _ensure_fonts(self):
        if self.bold_font_path is None:
            self.bold_font_path = find_font(BOLD_FONT_CANDIDATES, "粗黑體/明體")
        if self.kaiti_font_path is None:
            self.kaiti_font_path = find_font(KAITI_FONT_CANDIDATES, "標楷體")

    def generate(self):
        name = self.name_var.get().strip()
        surname = self.surname_var.get().strip()
        given = self.given_var.get().strip()

        if not name:
            messagebox.showwarning("請輸入姓名", "請先輸入姓名再產生印章。")
            return
        if not surname:
            messagebox.showwarning("缺少姓氏", "姓氏欄位不能為空。")
            return

        try:
            self._ensure_fonts()
        except RuntimeError as e:
            messagebox.showerror("字型錯誤", str(e))
            return

        self.status_var.set("產生中，請稍候...")
        self.root.update_idletasks()

        try:
            self.rect_image = make_horizontal_rect_stamp(name, self.bold_font_path)
            self.square_image = make_split_kaiti_square_stamp(surname, given or " ", self.kaiti_font_path)
        except Exception as e:
            messagebox.showerror("產生失敗", f"發生錯誤：\n{e}")
            self.status_var.set("產生失敗，請檢查字型或姓名輸入。")
            return

        self._show_preview(self.rect_canvas, self.rect_image, 380, 220)
        self._show_preview(self.square_canvas, self.square_image, 300, 300)
        self.status_var.set(f"已產生「{name}」的印章，可按「另存新檔」下載。")

    def _show_preview(self, canvas, pil_img, max_w, max_h):
        img = pil_img.copy()
        img.thumbnail((max_w - 12, max_h - 12), Image.LANCZOS)
        # 棋盤格底，方便看清楚透明區域
        bg = Image.new("RGBA", img.size, (240, 240, 240, 255))
        composed = Image.alpha_composite(bg, img)
        tk_img = ImageTk.PhotoImage(composed)
        canvas.delete("all")
        canvas.image = tk_img  # 保留參照避免被回收
        canvas.create_image(max_w // 2, max_h // 2, image=tk_img)

    def save_rect(self):
        self._save_image(self.rect_image, "長方橫式章")

    def save_square(self):
        self._save_image(self.square_image, "標楷體正方章")

    def _save_image(self, img, label):
        if img is None:
            messagebox.showwarning("尚未產生", f"請先按「產生印章」，才能儲存{label}。")
            return
        name = self.name_var.get().strip() or "stamp"
        default_name = f"{label}_{name}.png"
        path = filedialog.asksaveasfilename(
            title=f"儲存{label}",
            defaultextension=".png",
            initialfile=default_name,
            filetypes=[("PNG 圖檔", "*.png")]
        )
        if not path:
            return
        img.save(path)
        messagebox.showinfo("已儲存", f"已儲存至：\n{path}")


def main():
    root = tk.Tk()
    app = StampApp(root)
    root.mainloop()


if __name__ == "__main__":
    main()
