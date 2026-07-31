import math
import tkinter as tk
from tkinter import messagebox

# --- 1. 定義完整的表格數據 (pH 與 x, y 體積的對應關係) ---
PH_TABLE_DATA = {
    5.8: {'x_mL': 4.0,   'y_mL': 46.0},
    6.0: {'x_mL': 6.15,  'y_mL': 43.85},
    6.2: {'x_mL': 9.25,  'y_mL': 40.75},
    6.4: {'x_mL': 13.25, 'y_mL': 36.75},
    6.6: {'x_mL': 18.75, 'y_mL': 31.25},
    6.8: {'x_mL': 24.5,  'y_mL': 25.5},
    7.0: {'x_mL': 30.5,  'y_mL': 19.5},
    7.2: {'x_mL': 36.0,  'y_mL': 14.0},
    7.4: {'x_mL': 40.5,  'y_mL': 9.5},
    7.6: {'x_mL': 43.5,  'y_mL': 6.5},
    7.8: {'x_mL': 45.75, 'y_mL': 4.25},
    8.0: {'x_mL': 47.35, 'y_mL': 2.65}
}

# 提取並格式化可用的 pH 值清單
available_ph_list = sorted(PH_TABLE_DATA.keys())
# 格式化為 "5.8, 6.0, ..., 8.0"
available_ph_str = ", ".join(f"{p:.1f}" for p in available_ph_list)

# 表格的基礎參數
TABLE_STOCK_CONC_M = 0.2
TABLE_FINAL_VOLUME_ML = 100.0
TABLE_FINAL_CONC_M = 0.1

def calculate_buffer():
    """從 GUI 獲取輸入，進行計算並顯示結果"""
    try:
        # 從輸入欄位獲取數值
        target_volume_mL = float(entry_volume.get())
        target_conc_M = float(entry_target_conc.get())
        stock_conc_M = float(entry_stock_conc.get())
        
        # 處理 pH 輸入和查表
        target_pH_input = entry_ph.get()
        
        try:
            target_pH = float(target_pH_input)
        except ValueError:
            messagebox.showerror("輸入錯誤", "pH 值必須為數字。")
            return

        # 檢查 pH 是否在表格中
        found = False
        for key in PH_TABLE_DATA.keys():
            # 使用 math.isclose 進行浮點數的安全比較
            if math.isclose(target_pH, key):
                target_pH = key 
                found = True
                break
        
        if not found:
            messagebox.showerror("查表錯誤", f"pH 值 {target_pH_input} 不在表格數據中。\n可用的 pH 值為: {available_ph_str}")
            return

        # 取得表格基礎比例 (x 和 y)
        x_table_mL = PH_TABLE_DATA[target_pH]['x_mL']
        y_table_mL = PH_TABLE_DATA[target_pH]['y_mL']
        
        # --- 核心計算 (使用比例縮放) ---

        # 1) 比例因子計算 (Scale Factor)
        conc_ratio = target_conc_M / TABLE_FINAL_CONC_M 
        volume_ratio = target_volume_mL / TABLE_FINAL_VOLUME_ML
        stock_ratio = TABLE_STOCK_CONC_M / stock_conc_M
        
        # 2) 總縮放因子
        scale_factor = conc_ratio * volume_ratio * stock_ratio
        
        # 3) 計算最終所需的母液體積
        V_base_mL = x_table_mL * scale_factor
        V_acid_mL = y_table_mL * scale_factor
        V_mixed = V_base_mL + V_acid_mL
        
        # --- 輸出結果格式化 ---
        
        result_text = f"--- 計算結果 ---\n"
        result_text += f"目標 pH 值: {target_pH:.2f}\n"
        result_text += f"最終濃度: {target_conc_M:.3f} M\n"
        result_text += f"母液濃度: {stock_conc_M:.3f} M\n"
        result_text += "\n"
        result_text += f"1. Na₂HPO₄ (鹼式鹽, x) 所需體積: {V_base_mL:.2f} mL\n"
        result_text += f"2. NaH₂PO₄ (酸式鹽, y) 所需體積: {V_acid_mL:.2f} mL\n"
        result_text += f"兩種母液混合總體積: {V_mixed:.2f} mL\n"
        result_text += "\n"
        
        if V_mixed < target_volume_mL:
            result_text += f"配製步驟: 混合兩種母液，然後加水定容至 {target_volume_mL:.1f} mL。"
        else:
            result_text += f"配製步驟: 直接混合兩種母液 ({V_mixed:.2f} mL)，無需定容。"
        
        # 顯示結果
        text_result.set(result_text)

    except ValueError:
        messagebox.showerror("輸入錯誤", "所有欄位必須輸入有效的數字！")
    except ZeroDivisionError:
        messagebox.showerror("計算錯誤", "母液濃度或目標濃度不能為零。")
    except Exception as e:
        # 捕捉更廣泛的錯誤並提醒用戶
        messagebox.showerror("發生錯誤", f"計算過程中發生未知錯誤: {e}")


# --- 2. GUI 介面設計 (Tkinter) ---
root = tk.Tk()
root.title("磷酸鈉鹽緩衝液配製計算機 (表格查表版)")
root.geometry("480x480") # 稍微加大視窗以容納長標籤

# 設定標籤和輸入框
# *** 這裡修改了 pH 欄位的標籤，註明可輸入值 ***
labels = [
    "目標總體積 (mL):", 
    "目標總濃度 (M):", 
    f"目標 pH 值 (可輸入: {available_ph_str}):", 
    "母液濃度 (M):"
]
entries = []

# 輸入變數
entry_volume = tk.StringVar(root, value="1000")
entry_target_conc = tk.StringVar(root, value="0.2")
entry_ph = tk.StringVar(root, value="7.4")
entry_stock_conc = tk.StringVar(root, value="1.0")

row_counter = 0
for label_text, var in zip(labels, [entry_volume, entry_target_conc, entry_ph, entry_stock_conc]):
    # 增加 wraplength 讓長標籤可以換行顯示
    tk.Label(root, text=label_text, font=('Arial', 10), wraplength=150, justify=tk.LEFT).grid(row=row_counter, column=0, padx=10, pady=5, sticky="nw") 
    entry = tk.Entry(root, textvariable=var, font=('Arial', 10))
    entry.grid(row=row_counter, column=1, padx=10, pady=5, sticky="ew")
    entries.append(entry)
    row_counter += 1

# 計算按鈕
tk.Button(root, text="開始計算", command=calculate_buffer, font=('Arial', 12, 'bold'), bg='#4CAF50', fg='white').grid(row=row_counter, column=0, columnspan=2, pady=15, padx=10, sticky="ew")
row_counter += 1

# 結果顯示區
tk.Label(root, text="--- 配製結果 ---", font=('Arial', 10, 'bold')).grid(row=row_counter, column=0, columnspan=2, pady=5)
row_counter += 1

text_result = tk.StringVar()
tk.Label(root, textvariable=text_result, justify=tk.LEFT, relief=tk.RIDGE, bd=2, anchor="w", wraplength=450, font=('Courier New', 10)).grid(row=row_counter, column=0, columnspan=2, padx=10, pady=5, sticky="nsew")
row_counter += 1

# 設置資料來源註釋
tk.Label(root, text="資料來源: Sigma Aldrich 磷酸鹽緩衝液表格數據", font=('Arial', 8)).grid(row=row_counter, column=0, columnspan=2, pady=5)

# 設置列權重，使輸入框可隨視窗寬度調整
root.grid_columnconfigure(1, weight=1)

# 執行 GUI 迴圈
root.mainloop()