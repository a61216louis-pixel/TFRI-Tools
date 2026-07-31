import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog
import math

class QPcrCalculator:
    """
    qPCR 藥品用量動態計算器 (Tkinter GUI)
    """
    def __init__(self, master):
        self.master = master
        master.title("qPCR 用量動態計算器")
        
        # --- 固定藥品組件定義 (每個 10 uL 反應) ---
        self.mm_components = {
            "PowerTrack SYBR Green (2X)": 5.0,
            "Forward Primer (8000 nM)": 0.5,
            "Reverse Primer (8000 nM)": 0.5,
            "Nuclease-free Water (MM)": 3.0,
        }
        self.total_mm_per_reaction = sum(self.mm_components.values()) # 9.0 uL

        # --- 初始化可變參數變數 ---
        self.vars = {
            "N_Sample": tk.StringVar(value="16"),
            "R_Sample": tk.StringVar(value="2"),
            "N_Target": tk.StringVar(value="3"),
            "N_Std": tk.StringVar(value="8"),
            "R_Std": tk.StringVar(value="3"),
            "R_NTC": tk.StringVar(value="3"),
            "D_Sample": tk.StringVar(value="5"),
            "F_Overage": tk.StringVar(value="1.10"),
            "V_Sample_Total": tk.StringVar(value="50.0") 
        }
        
        self._create_widgets()

    def _create_widgets(self):
        # 設定輸入框架
        input_frame = ttk.LabelFrame(self.master, text="🧬 實驗參數輸入")
        input_frame.grid(row=0, column=0, columnspan=2, padx=10, pady=10, sticky="ew")
        
        labels = {
            "N_Sample": "實驗樣本數 (N):",
            "R_Sample": "樣品重複數 (R):",
            "N_Target": "目標基因數 (Target):",
            "N_Std": "標準品點數:",
            "R_Std": "標準品重複數:",
            "R_NTC": "NTC 重複數:",
            "D_Sample": "樣本稀釋倍率:",
            "F_Overage": "Master Mix 額外量比例 (如 1.10):",
            "V_Sample_Total": "樣本稀釋總體積 (uL/樣本):"
        }

        row = 0
        for key, text in labels.items():
            ttk.Label(input_frame, text=text).grid(row=row, column=0, padx=5, pady=2, sticky="w")
            entry = ttk.Entry(input_frame, textvariable=self.vars[key])
            entry.grid(row=row, column=1, padx=5, pady=2, sticky="ew")
            row += 1

        # --- 按鈕區域 ---
        button_frame = ttk.Frame(self.master)
        button_frame.grid(row=1, column=0, columnspan=2, padx=10, pady=5, sticky="ew")
        button_frame.grid_columnconfigure(0, weight=1)
        button_frame.grid_columnconfigure(1, weight=1)

        # 計算按鈕
        calculate_button = ttk.Button(button_frame, text="📈 執行計算並生成報告", command=self.calculate_usage)
        calculate_button.grid(row=0, column=0, padx=5, pady=5, sticky="ew")
        
        # 儲存/列印按鈕
        save_button = ttk.Button(button_frame, text="💾 儲存報告 (.txt)", command=self.save_report)
        save_button.grid(row=0, column=1, padx=5, pady=5, sticky="ew")


        # 設定結果框架
        self.result_text = tk.Text(self.master, height=30, width=80, state='disabled')
        self.result_text.grid(row=2, column=0, columnspan=2, padx=10, pady=10, sticky="nsew")
        
        # 讓視窗能夠縮放
        self.master.grid_columnconfigure(0, weight=1)
        self.master.grid_columnconfigure(1, weight=1)
        self.master.grid_rowconfigure(2, weight=1)

    def calculate_usage(self):
        """
        核心計算邏輯
        """
        try:
            params = {}
            for key, var in self.vars.items():
                params[key] = float(var.get())

            N_Sample = int(params["N_Sample"])
            R_Sample = int(params["R_Sample"])
            N_Target = int(params["N_Target"])
            N_Std = int(params["N_Std"])
            R_Std = int(params["R_Std"])
            R_NTC = int(params["R_NTC"])
            D_Sample = params["D_Sample"]
            F_Overage = params["F_Overage"]
            V_Sample_Total = params["V_Sample_Total"]

            # --- 1. 總孔數計算 (單一 Target) ---
            W_Total_Single = (N_Sample * R_Sample) + (N_Std * R_Std) + (1 * R_NTC)
            N_MM = math.ceil(W_Total_Single * F_Overage)
            
            output = f"=== qPCR 藥品用量計算報告 ===\n"
            output += f"--- 實驗參數確認 ---\n"
            output += f"  實驗樣本數: {N_Sample} | 樣品重複數: {R_Sample}\n"
            output += f"  目標基因數: {N_Target} | Master Mix 額外量: {F_Overage * 100 - 100:.0f}%\n"
            output += f"  單一 Target 總孔數: {W_Total_Single} 孔 (實際配置份數: {N_MM} 份)\n\n"

            # --- 3. 報表 1: Master Mix 配置清單 ---
            output += "### 報表 1: Master Mix 配置清單 (每個 Target)\n"
            
            # 修正後的標題確認行
            output += " (每個反應總體積 10.0 uL，其中 Master Mix 佔 {} uL，樣本佔 1 uL)\n".format(self.total_mm_per_reaction)

            # *** 新增 "單孔體積(uL)" 欄位 ***
            output += "{:<30} {:>10} {:>10} {:>10}\n".format("組件", "單孔體積(uL)", "單一 Target (uL)", "總計 (所有 Targets, uL)")
            output += "-"*65 + "\n" # 調整分隔線長度

            total_mm_volume_single = 0
            total_mm_volume_all = 0
            
            for name, vol_per_rxn in self.mm_components.items():
                vol_single_target = vol_per_rxn * N_MM
                vol_all_targets = vol_single_target * N_Target
                
                # *** 輸出 4 個欄位 ***
                output += "{:<30} {:>10.2f} {:>10.2f} {:>10.2f}\n".format(name, vol_per_rxn, vol_single_target, vol_all_targets)
                total_mm_volume_single += vol_single_target
                total_mm_volume_all += vol_all_targets

            output += "-"*65 + "\n" # 調整分隔線長度
            
            # *** 總計行也要新增單孔體積 (即 total_mm_per_reaction) ***
            output += "{:<30} {:>10.2f} {:>10.2f} {:>10.2f}\n".format("Master Mix 總體積", self.total_mm_per_reaction, total_mm_volume_single, total_mm_volume_all)
            output += "\n"
            
            # --- 4. 報表 2: 實驗樣本 DNA 稀釋清單 ---
            output += "### 報表 2: 實驗樣本 DNA 稀釋清單 (每個樣本)\n"
            required_template_per_sample = R_Sample * N_Target * (10.0 - self.total_mm_per_reaction) 

            stock_dna_vol = V_Sample_Total / D_Sample
            water_vol = V_Sample_Total - stock_dna_vol
            
            output += f"  **總體積** (V_Total) 建議製備: {V_Sample_Total:.1f} uL\n"
            output += f"  每個樣本總共需要模板量 (理論最小值): {required_template_per_sample:.1f} uL\n"
            output += f"  --------------------------------------------------\n"
            output += f"  **Stock DNA (原液) 用量:** {stock_dna_vol:.2f} uL\n"
            output += f"  **Nuclease-free Water 用量:** {water_vol:.2f} uL\n\n"
            
            # --- 5. 報表 3: 標準品稀釋提醒 ---
            output += "### 報表 3: 標準品稀釋清單 (1:10 連續稀釋提醒)\n"
            output += f"  需為每個 Target 製備 {N_Std} 個稀釋點 (S1 到 S{N_Std})，範圍從 $10^{{-4}}$ 到 $10^{{-11}}$。\n"
            output += f"  連續稀釋步驟 ($1:10$) 建議配置：總體積 100 µL (10 µL 稀釋液 + 90 µL 水)。\n"


        except ValueError:
            messagebox.showerror("輸入錯誤", "請檢查所有參數是否為有效的數字。")
            return
        except Exception as e:
            messagebox.showerror("計算錯誤", f"發生了一個錯誤: {e}")
            return

        # 輸出結果
        self.result_text.config(state='normal')
        self.result_text.delete('1.0', tk.END)
        self.result_text.insert(tk.END, output)
        self.result_text.config(state='disabled')
        
    def save_report(self):
        """
        將結果文本框的內容儲存為 .txt 文件
        """
        report_content = self.result_text.get('1.0', tk.END).strip() 

        if not report_content:
            messagebox.showwarning("操作提醒", "請先點擊 '執行計算' 以生成報告。")
            return

        # 彈出儲存檔案對話框
        file_path = filedialog.asksaveasfilename(
            defaultextension=".txt",
            filetypes=[("Text files", "*.txt"), ("All files", "*.*")],
            title="儲存 qPCR 用量計算報告",
            initialfile="qPCR_Report.txt"
        )

        if file_path:
            try:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(report_content)
                messagebox.showinfo("儲存成功", f"報告已儲存至:\n{file_path}\n\n您可以開啟此檔案進行列印。")
            except Exception as e:
                messagebox.showerror("儲存錯誤", f"儲存檔案時發生錯誤: {e}")


if __name__ == "__main__":
    root = tk.Tk()
    app = QPcrCalculator(root)
    root.mainloop()