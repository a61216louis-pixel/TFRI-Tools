import tkinter as tk
from tkinter import messagebox

def calculate():
    # 獲取使用者選擇的培養基、體積與額外 NaCl 比例
    selected_media = media_var.get()
    try:
        # 支援浮點數體積輸入
        user_input_vol = entry_volume.get()
        volume = float(user_input_vol)
        
        # 支援浮點數 NaCl 比例輸入 (%)
        extra_nacl_percent = entry_extra_nacl.get()
        extra_percent = float(extra_nacl_percent) if extra_nacl_percent else 0.0
        
        if volume <= 0:
            messagebox.showwarning("輸入錯誤", "體積必須大於 0")
            return
        if extra_percent < 0:
            messagebox.showwarning("輸入錯誤", "NaCl 比例不能為負數")
            return
            
    except ValueError:
        messagebox.showerror("輸入錯誤", "請輸入有效的數字")
        return

    # 根據最新資料表定義原始配方數據 (每 1.0 L 的重量 g)
    # 順序: NaCl, Tryptone, Yeast, CaCl2, Agar
    recipes = {
        "LB":  [10, 10, 5, 0, 15],
        "YEP": [5, 10, 10, 0, 15],
        "TY":  [0, 5, 3, 0.88, 15]
    }
    
    ingredients = ["NaCl", "Tryptone", "Yeast", "CaCl2", "Agar"]
    current_recipe = list(recipes[selected_media]) # 複製一份清單避免修改原始數據

    # 計算額外 NaCl 重量: (L * 1000 ml/L) * (percent / 100) = L * 10 * percent
    extra_nacl_g = volume * 10 * extra_percent

    # 清除畫布舊內容
    canvas.delete("all")

    # 繪製表格參數
    x_start = 40
    y_start = 40
    row_h = 40
    width = 340

    # 繪製標題
    title_text = f"{selected_media} 培養基配方 ({volume} L)"
    if extra_percent > 0:
        title_text += f" + {extra_percent}% NaCl"
    canvas.create_text(210, 20, text=title_text, font=("Arial", 11, "bold"))

    # 繪製表格外框
    canvas.create_rectangle(x_start, y_start, x_start + width, y_start + (len(ingredients) * row_h), outline="black", width=2)

    # 循環繪製每一列數據
    for i in range(len(ingredients)):
        y_curr = y_start + (i * row_h)
        
        # 畫格線
        if i > 0:
            canvas.create_line(x_start, y_curr, x_start + width, y_curr, fill="gray")

        name = ingredients[i]
        base_w = current_recipe[i]
        
        # 處理 NaCl 的特殊計算（基礎量 + 額外量）
        final_w = base_w * volume
        is_nacl = (name == "NaCl")
        if is_nacl:
            final_w += extra_nacl_g

        # 顯示邏輯
        if final_w == 0 and not is_nacl:
            display_w = "-"
        else:
            calc_w = round(final_w, 3)
            display_w = f"{calc_w} g"
            
            # 如果有額外添加 NaCl，在後方註記
            if is_nacl and extra_percent > 0:
                base_part = round(base_w * volume, 2)
                extra_part = round(extra_nacl_g, 2)
                display_w += f" ({base_part}+{extra_part})"

        # 繪製文字
        canvas.create_text(x_start + 10, y_curr + 20, text=name, anchor="w", font=("Arial", 10))
        canvas.create_text(x_start + width - 10, y_curr + 20, text=display_w, anchor="e", font=("Arial", 10, "bold"), fill="blue")

# 建立主視窗
root = tk.Tk()
root.title("培養基原料換算器 (進階版)")
root.geometry("450x580")

# 控制區域
ctrl_frame = tk.Frame(root, pady=10)
ctrl_frame.pack()

# 選擇培養基
tk.Label(ctrl_frame, text="1. 選擇培養基:").grid(row=0, column=0, padx=5, pady=5, sticky="e")
media_options = ["LB", "YEP", "TY"]
media_var = tk.StringVar(root)
media_var.set(media_options[0])
option_menu = tk.OptionMenu(ctrl_frame, media_var, *media_options)
option_menu.grid(row=0, column=1, padx=5, pady=5, sticky="w")

# 配置體積
tk.Label(ctrl_frame, text="2. 配置總體積 (L):").grid(row=1, column=0, padx=5, pady=5, sticky="e")
entry_volume = tk.Entry(ctrl_frame, width=15)
entry_volume.insert(0, "1.0")
entry_volume.grid(row=1, column=1, padx=5, pady=5, sticky="w")

# 額外 NaCl
tk.Label(ctrl_frame, text="3. 額外添加 NaCl (%):").grid(row=2, column=0, padx=5, pady=5, sticky="e")
entry_extra_nacl = tk.Entry(ctrl_frame, width=15)
entry_extra_nacl.insert(0, "0")
entry_extra_nacl.grid(row=2, column=1, padx=5, pady=5, sticky="w")
tk.Label(ctrl_frame, text="(w/v %)", fg="gray").grid(row=2, column=2, padx=2, sticky="w")

# 按鈕
btn_calc = tk.Button(ctrl_frame, text="執行換算", command=calculate, bg="#e1e1e1", height=2)
btn_calc.grid(row=3, column=0, columnspan=3, pady=15, sticky="ew")

# 畫布區域
canvas = tk.Canvas(root, width=420, height=300, bg="white", borderwidth=2, relief="ridge")
canvas.pack(padx=10, pady=10)

# 初始執行
calculate()

root.mainloop()