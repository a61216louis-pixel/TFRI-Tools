#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
HTML 網頁發布工具 GUI（GitHub Pages）
選擇一個 .html 檔 -> 上傳到 GitHub 儲存庫 -> 自動開啟 GitHub Pages -> 取得公開網址 + QR Code

網址格式：
    https://<GitHub帳號>.github.io/<儲存庫名稱>/<網址代稱>/

使用方式：
    python html_publisher_gui.py

需要套件：無（只用 Python 內建模組）
    選用：pip install qrcode pillow   -> 顯示 / 另存 QR Code，方便手機掃描開啟

事前準備（只需做一次）：
    1. 註冊 GitHub 帳號
    2. 建立 Personal Access Token（classic）：
       GitHub 右上頭像 → Settings → Developer settings → Personal access tokens
       → Tokens (classic) → Generate new token，勾選「repo」權限
    3. 把 Token 貼到本程式的「GitHub Token」欄位

注意：
    - 免費帳號的 GitHub Pages 只能用「公開」儲存庫，上傳的網頁任何人拿到網址都看得到，
      請勿上傳含個資、帳密或未公開資料的 HTML。
    - 第一次發布約需 1～2 分鐘網站才會上線，程式會自動偵測並提示。
    - Token 若勾選「記住」，會以明碼存在使用者家目錄的 .tfri_html_publisher.json，請勿分享該檔。
"""

import base64
import json
import os
import re
import threading
import time
import tkinter as tk
import urllib.error
import urllib.parse
import urllib.request
import webbrowser
from datetime import datetime
from tkinter import ttk, filedialog, messagebox

try:
    import qrcode
    from PIL import Image, ImageTk
    HAS_QR = True
except ImportError:
    HAS_QR = False

# ---------- 參數設定 ----------
API = "https://api.github.com"
CONFIG_PATH = os.path.join(os.path.expanduser("~"), ".tfri_html_publisher.json")
DEFAULT_REPO = "tfri-pages"
SLUG_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_-]{0,63}$")
FONT = "微軟正黑體"
BG = "#f5f3ee"
DARK = "#1a3a2a"
GREEN = "#3d7a52"


# ---------- GitHub API ----------
class GitHubError(Exception):
    def __init__(self, status, message):
        super().__init__(f"HTTP {status}：{message}")
        self.status = status


class GitHub:
    def __init__(self, token):
        self.token = token.strip()

    def call(self, method, path, body=None):
        data = json.dumps(body).encode("utf-8") if body is not None else None
        req = urllib.request.Request(API + path, data=data, method=method)
        req.add_header("Authorization", f"Bearer {self.token}")
        req.add_header("Accept", "application/vnd.github+json")
        req.add_header("X-GitHub-Api-Version", "2022-11-28")
        req.add_header("User-Agent", "TFRI-html-publisher")
        if data is not None:
            req.add_header("Content-Type", "application/json")
        try:
            with urllib.request.urlopen(req, timeout=30) as r:
                raw = r.read()
                return json.loads(raw) if raw else {}
        except urllib.error.HTTPError as e:
            try:
                msg = json.loads(e.read()).get("message", "")
            except Exception:
                msg = e.reason
            raise GitHubError(e.code, msg) from None

    def login(self):
        return self.call("GET", "/user")["login"]

    def ensure_repo(self, owner, repo, log):
        try:
            self.call("GET", f"/repos/{owner}/{repo}")
            return
        except GitHubError as e:
            if e.status != 404:
                raise
        log(f"建立公開儲存庫 {owner}/{repo} …")
        self.call("POST", "/user/repos", {
            "name": repo,
            "description": "林業試驗所育林組 網頁發布（由 html_publisher_gui.py 產生）",
            "private": False,
            "auto_init": True,
        })
        # .nojekyll：讓 GitHub Pages 直接提供靜態檔，不經 Jekyll 處理（部署較快）
        self.put_file(owner, repo, ".nojekyll", b"", "Add .nojekyll")

    def ensure_pages(self, owner, repo, log):
        try:
            info = self.call("GET", f"/repos/{owner}/{repo}/pages")
            return info.get("html_url")
        except GitHubError as e:
            if e.status != 404:
                raise
        log("開啟 GitHub Pages …")
        branch = self.call("GET", f"/repos/{owner}/{repo}")["default_branch"]
        for _ in range(5):  # 剛建立的儲存庫有時需等幾秒才能開 Pages
            try:
                info = self.call("POST", f"/repos/{owner}/{repo}/pages",
                                 {"source": {"branch": branch, "path": "/"}})
                return info.get("html_url")
            except GitHubError as e:
                if e.status == 409:  # 已開啟
                    return None
                if e.status not in (404, 422):
                    raise
                time.sleep(3)
        raise GitHubError(422, "無法開啟 GitHub Pages，請到儲存庫 Settings → Pages 手動開啟")

    def get_sha(self, owner, repo, path):
        try:
            return self.call("GET", f"/repos/{owner}/{repo}/contents/{quote(path)}")["sha"]
        except GitHubError as e:
            if e.status == 404:
                return None
            raise

    def put_file(self, owner, repo, path, content, message):
        body = {"message": message, "content": base64.b64encode(content).decode("ascii")}
        sha = self.get_sha(owner, repo, path)
        if sha:
            body["sha"] = sha
        self.call("PUT", f"/repos/{owner}/{repo}/contents/{quote(path)}", body)
        return sha is not None

    def delete_file(self, owner, repo, path, message):
        sha = self.get_sha(owner, repo, path)
        if sha:
            self.call("DELETE", f"/repos/{owner}/{repo}/contents/{quote(path)}",
                      {"message": message, "sha": sha})

    def list_pages(self, owner, repo):
        try:
            items = self.call("GET", f"/repos/{owner}/{repo}/contents/")
        except GitHubError as e:
            if e.status == 404:
                return []
            raise
        return sorted(i["name"] for i in items if i["type"] == "dir")


def quote(path):
    return urllib.parse.quote(path, safe="/")


def page_url(owner, repo, slug):
    return f"https://{owner.lower()}.github.io/{repo}/{slug}/"


def wait_online(url, content, timeout=300):
    """反覆讀取公開網址，直到內容與剛上傳的檔案相同（代表已部署完成）。"""
    end = time.time() + timeout
    while time.time() < end:
        try:
            req = urllib.request.Request(f"{url}?t={int(time.time())}",
                                         headers={"Cache-Control": "no-cache"})
            with urllib.request.urlopen(req, timeout=15) as r:
                if r.read() == content:
                    return True
        except Exception:
            pass
        time.sleep(8)
    return False


def suggest_slug(filename):
    stem = os.path.splitext(os.path.basename(filename))[0]
    s = re.sub(r"[^A-Za-z0-9_-]+", "-", stem).strip("-_")
    return s[:64] if s and SLUG_RE.match(s) else "page-" + datetime.now().strftime("%Y%m%d-%H%M")


# ---------- 設定檔 ----------
def load_config():
    try:
        with open(CONFIG_PATH, encoding="utf-8") as f:
            return json.load(f)
    except Exception:
        return {}


def save_config(cfg):
    try:
        with open(CONFIG_PATH, "w", encoding="utf-8") as f:
            json.dump(cfg, f, ensure_ascii=False, indent=2)
        if os.name != "nt":
            os.chmod(CONFIG_PATH, 0o600)
    except Exception:
        pass


# ---------- GUI ----------
class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("HTML 網頁發布工具（GitHub Pages）")
        self.geometry("820x720")
        self.minsize(720, 640)
        self.configure(bg=BG)

        cfg = load_config()
        self._token = tk.StringVar(value=cfg.get("token", ""))
        self._remember = tk.BooleanVar(value=bool(cfg.get("token")))
        self._repo = tk.StringVar(value=cfg.get("repo", DEFAULT_REPO))
        self._file = tk.StringVar()
        self._slug = tk.StringVar()
        self._url = tk.StringVar()
        self._owner = None
        self._qr_img = None
        self._busy = False

        self._build_ui()

    # ── UI 建構 ──────────────────────────────────
    def _build_ui(self):
        hdr = tk.Frame(self, bg=DARK)
        hdr.pack(fill="x")
        tk.Label(hdr, text="🌐  HTML 網頁發布工具", font=(FONT, 16, "bold"),
                 bg=DARK, fg="white").pack(side="left", padx=20, pady=12)

        style = ttk.Style(self)
        style.theme_use("clam")
        style.configure("Treeview", font=(FONT, 10), rowheight=24)
        style.configure("Treeview.Heading", font=(FONT, 10, "bold"))

        body = tk.Frame(self, bg=BG)
        body.pack(fill="both", expand=True, padx=16, pady=10)
        body.columnconfigure(1, weight=1)
        lbl = dict(bg=BG, font=(FONT, 11), fg="#2c3e2d", anchor="w")
        ent = dict(font=(FONT, 10), relief="solid", bd=1)
        btn = dict(font=(FONT, 10), relief="flat", cursor="hand2", bg="#dde8dd", padx=10)

        # 1. GitHub 設定
        tk.Label(body, text="① GitHub 設定", bg=BG, font=(FONT, 13, "bold"), fg=DARK
                 ).grid(row=0, column=0, columnspan=3, sticky="w", pady=(0, 4))
        tk.Label(body, text="GitHub Token", **lbl).grid(row=1, column=0, sticky="w")
        tk.Entry(body, textvariable=self._token, show="•", **ent
                 ).grid(row=1, column=1, sticky="ew", padx=6, pady=3)
        tk.Checkbutton(body, text="記住", variable=self._remember, bg=BG, font=(FONT, 10)
                       ).grid(row=1, column=2, sticky="w")
        tk.Label(body, text="儲存庫名稱", **lbl).grid(row=2, column=0, sticky="w")
        tk.Entry(body, textvariable=self._repo, **ent
                 ).grid(row=2, column=1, sticky="ew", padx=6, pady=3)
        tk.Button(body, text="Token 怎麼申請？", command=self._help_token, **btn
                  ).grid(row=2, column=2, sticky="w")

        # 2. 選擇檔案
        tk.Label(body, text="② 選擇要發布的 HTML", bg=BG, font=(FONT, 13, "bold"), fg=DARK
                 ).grid(row=3, column=0, columnspan=3, sticky="w", pady=(12, 4))
        tk.Label(body, text="HTML 檔案", **lbl).grid(row=4, column=0, sticky="w")
        tk.Entry(body, textvariable=self._file, **ent
                 ).grid(row=4, column=1, sticky="ew", padx=6, pady=3)
        tk.Button(body, text="瀏覽…", command=self._pick_file, **btn
                  ).grid(row=4, column=2, sticky="w")
        tk.Label(body, text="網址代稱", **lbl).grid(row=5, column=0, sticky="w")
        tk.Entry(body, textvariable=self._slug, **ent
                 ).grid(row=5, column=1, sticky="ew", padx=6, pady=3)
        tk.Label(body, text="英數、- 、_", bg=BG, font=(FONT, 9), fg="#8a8a8a"
                 ).grid(row=5, column=2, sticky="w")
        tk.Label(body, text="同一個代稱再次發布 = 更新內容，網址不變（使用者瀏覽器內保存的資料也會保留）",
                 bg=BG, font=(FONT, 9), fg="#6b7d70"
                 ).grid(row=6, column=1, columnspan=2, sticky="w", padx=6)

        tk.Button(body, text="🚀  發布 / 更新", font=(FONT, 12, "bold"), relief="flat",
                  cursor="hand2", bg="#5a9e6f", fg="white", activebackground=GREEN,
                  padx=18, pady=6, command=self._publish
                  ).grid(row=7, column=0, columnspan=3, sticky="ew", pady=(10, 4))

        # 3. 結果
        res = tk.Frame(body, bg=BG)
        res.grid(row=8, column=0, columnspan=3, sticky="ew", pady=(6, 0))
        res.columnconfigure(0, weight=1)
        tk.Label(res, text="③ 線上網址", bg=BG, font=(FONT, 13, "bold"), fg=DARK
                 ).grid(row=0, column=0, sticky="w")
        tk.Entry(res, textvariable=self._url, font=("Consolas", 11), relief="solid", bd=1,
                 state="readonly").grid(row=1, column=0, sticky="ew", pady=3)
        bf = tk.Frame(res, bg=BG)
        bf.grid(row=2, column=0, sticky="w")
        tk.Button(bf, text="📋 複製網址", command=self._copy, **btn).pack(side="left", padx=(0, 6))
        tk.Button(bf, text="🔗 用瀏覽器開啟", command=self._open, **btn).pack(side="left", padx=6)
        if HAS_QR:
            tk.Button(bf, text="💾 另存 QR Code", command=self._save_qr, **btn).pack(side="left", padx=6)
        self._qr_label = tk.Label(res, bg=BG,
                                  text="" if HAS_QR else "（pip install qrcode pillow 可顯示 QR Code）",
                                  font=(FONT, 9), fg="#8a8a8a")
        self._qr_label.grid(row=0, column=1, rowspan=3, padx=(12, 0))

        # 4. 已發布清單
        lf = tk.Frame(body, bg=BG)
        lf.grid(row=9, column=0, columnspan=3, sticky="nsew", pady=(12, 0))
        body.rowconfigure(9, weight=1)
        lf.columnconfigure(0, weight=1)
        lf.rowconfigure(1, weight=1)
        top = tk.Frame(lf, bg=BG)
        top.grid(row=0, column=0, sticky="ew")
        tk.Label(top, text="④ 已發布的網頁", bg=BG, font=(FONT, 13, "bold"), fg=DARK).pack(side="left")
        tk.Button(top, text="🗑 刪除選取", command=self._delete, **btn).pack(side="right")
        tk.Button(top, text="🔄 重新整理", command=self._refresh, **btn).pack(side="right", padx=6)
        self._tree = ttk.Treeview(lf, columns=("slug", "url"), show="headings", height=5)
        self._tree.heading("slug", text="代稱")
        self._tree.heading("url", text="網址")
        self._tree.column("slug", width=160, stretch=False)
        self._tree.grid(row=1, column=0, sticky="nsew")
        self._tree.bind("<<TreeviewSelect>>", self._on_select)

        self._status = tk.Label(self, text="就緒", bg="#dde8dd", fg=DARK, font=(FONT, 10),
                                anchor="w", padx=12, pady=4)
        self._status.pack(fill="x", side="bottom")

    # ── 小工具 ────────────────────────────────────
    def _log(self, msg):
        self.after(0, lambda: self._status.config(text=msg))

    def _set_url(self, url):
        self._url.set(url)
        if not HAS_QR:
            return
        if not url:
            self._qr_img = None
            self._qr_label.config(image="")
            return
        qr = qrcode.make(url)
        img = getattr(qr, "get_image", lambda: qr._img)().convert("RGB").resize((140, 140), Image.NEAREST)
        self._qr_img = ImageTk.PhotoImage(img)
        self._qr_label.config(image=self._qr_img, text="")

    def _help_token(self):
        messagebox.showinfo("申請 GitHub Token", (
            "1. 登入 github.com，點右上角頭像 → Settings\n"
            "2. 左側最下方 Developer settings → Personal access tokens → Tokens (classic)\n"
            "3. Generate new token (classic)，Note 隨意填，Expiration 選期限\n"
            "4. 勾選「repo」權限 → Generate token\n"
            "5. 複製 ghp_ 開頭的字串，貼到本程式\n\n"
            "※ Token 等同密碼，請勿外流；若外流請立即到同一頁面刪除。"))
        webbrowser.open("https://github.com/settings/tokens/new?scopes=repo&description=TFRI%20html%20publisher")

    def _pick_file(self):
        path = filedialog.askopenfilename(title="選擇 HTML 檔",
                                          filetypes=[("HTML 檔", "*.html *.htm"), ("全部檔案", "*.*")])
        if path:
            self._file.set(path)
            if not self._slug.get().strip():
                self._slug.set(suggest_slug(path))

    def _copy(self):
        if self._url.get():
            self.clipboard_clear()
            self.clipboard_append(self._url.get())
            self._log("已複製網址到剪貼簿")

    def _open(self):
        if self._url.get():
            webbrowser.open(self._url.get())

    def _save_qr(self):
        if not self._url.get():
            return
        path = filedialog.asksaveasfilename(defaultextension=".png", filetypes=[("PNG", "*.png")],
                                            initialfile=f"QR_{self._url.get().rstrip('/').split('/')[-1]}.png")
        if path:
            qrcode.make(self._url.get(), box_size=12).save(path)
            self._log(f"已儲存 {path}")

    def _on_select(self, _):
        sel = self._tree.selection()
        if sel:
            slug, url = self._tree.item(sel[0], "values")
            self._slug.set(slug)
            self._set_url(url)

    def _check(self):
        if self._busy:
            return False
        if not self._token.get().strip():
            messagebox.showwarning("缺少 Token", "請先填入 GitHub Token（可按「Token 怎麼申請？」）")
            return False
        if not re.match(r"^[A-Za-z0-9._-]+$", self._repo.get().strip()):
            messagebox.showwarning("儲存庫名稱", "儲存庫名稱只能用英數、. 、- 、_")
            return False
        cfg = {"repo": self._repo.get().strip()}
        if self._remember.get():
            cfg["token"] = self._token.get().strip()
        save_config(cfg)
        return True

    def _run(self, work):
        """在背景執行緒執行網路工作，避免視窗卡住。"""
        self._busy = True

        def wrapper():
            try:
                gh = GitHub(self._token.get())
                if not self._owner:
                    self._log("驗證 Token …")
                    self._owner = gh.login()
                work(gh, self._owner, self._repo.get().strip())
            except GitHubError as e:
                hint = {401: "\n\nToken 無效或已過期，請重新申請。",
                        403: "\n\nToken 權限不足，請確認有勾選「repo」。"}.get(e.status, "")
                self.after(0, lambda: messagebox.showerror("GitHub 錯誤", f"{e}{hint}"))
                self._log("失敗")
            except urllib.error.URLError as e:
                self.after(0, lambda: messagebox.showerror("連線錯誤", f"無法連線到 GitHub：{e.reason}"))
                self._log("失敗")
            finally:
                self._busy = False

        threading.Thread(target=wrapper, daemon=True).start()

    # ── 主要動作 ──────────────────────────────────
    def _publish(self):
        path, slug = self._file.get().strip(), self._slug.get().strip()
        if not os.path.isfile(path):
            messagebox.showwarning("檔案", "請先選擇要發布的 HTML 檔")
            return
        if not SLUG_RE.match(slug):
            messagebox.showwarning("網址代稱", "網址代稱只能用英文、數字、- 、_（開頭須為英數），例如 attendance-115")
            return
        if not self._check():
            return
        with open(path, "rb") as f:
            content = f.read()

        def work(gh, owner, repo):
            gh.ensure_repo(owner, repo, self._log)
            gh.ensure_pages(owner, repo, self._log)
            self._log(f"上傳 {os.path.basename(path)} …")
            updated = gh.put_file(owner, repo, f"{slug}/index.html", content,
                                  f"Publish {slug} ({os.path.basename(path)})")
            url = page_url(owner, repo, slug)
            self.after(0, lambda: self._set_url(url))
            self.after(0, self._refresh_tree_later(gh, owner, repo))
            self._log(f"{'已更新' if updated else '已上傳'}，等待 GitHub Pages 部署（約 1～2 分鐘）…")
            if wait_online(url, content):
                self._log(f"✅ 已上線：{url}")
                self.after(0, lambda: messagebox.showinfo(
                    "發布完成", f"網頁已上線！\n\n{url}\n\n可複製網址或 QR Code 分享給同仁。"))
            else:
                self._log(f"⚠ 尚未偵測到上線，請稍後再開啟：{url}")

        self._run(work)

    def _refresh_tree_later(self, gh, owner, repo):
        pages = gh.list_pages(owner, repo)
        return lambda: self._fill_tree(owner, repo, pages)

    def _fill_tree(self, owner, repo, pages):
        self._tree.delete(*self._tree.get_children())
        for slug in pages:
            self._tree.insert("", "end", values=(slug, page_url(owner, repo, slug)))

    def _refresh(self):
        if not self._check():
            return

        def work(gh, owner, repo):
            self._log("讀取已發布清單 …")
            self.after(0, self._refresh_tree_later(gh, owner, repo))
            self._log("就緒")

        self._run(work)

    def _delete(self):
        sel = self._tree.selection()
        if not sel or not self._check():
            return
        slug = self._tree.item(sel[0], "values")[0]
        if not messagebox.askyesno("刪除", f"確定要下架「{slug}」？\n網址將失效，使用者也無法再開啟。"):
            return

        def work(gh, owner, repo):
            self._log(f"刪除 {slug} …")
            gh.delete_file(owner, repo, f"{slug}/index.html", f"Remove {slug}")
            self.after(0, self._refresh_tree_later(gh, owner, repo))
            self.after(0, lambda: self._set_url(""))
            self._log(f"已刪除 {slug}（GitHub Pages 約 1 分鐘後生效）")

        self._run(work)


if __name__ == "__main__":
    App().mainloop()
