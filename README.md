# Portfolio Audit: From Textbook "Safe Haven" to Systematic Vulnerability

## 📖 My Story
I started investing in **2018 at the age of 14**. By late 2025, my portfolio was **50% VOO / 50% QQQ**. 

Facing extreme volatility, I wanted to follow the textbook "safe haven" advice of adding bonds. But before doing so, I built this audit to test if the strategy still works. **Spoiler: It doesn't.** This project explains why I abandoned static defense for a dynamic approach.

---

## 📂 Project Navigation (What's in this folder)
To ensure the code runs out-of-the-box, I have kept all original filenames:

*   **`R code - final edition`**: The complete R script for backtesting and analysis.
*   **`Portfolio Audit Project.pdf`**: My final research report (The "Big Picture").
*   **`Portfolio Audit Project - script.pdf`**: Technical documentation of the R logic.

### 📊 Datasets (Crucial for Reproduction)
*   `VOO ETF Stock Price History`: Historical prices for S&P 500.
*   `QQQ ETF Stock Price History`: Historical prices for Nasdaq 100.
*   `VGLT ETF Stock Price History`: Historical prices for Long-term Treasuries.
*   `cpi`: Consumer Price Index data for regime detection.

---

## 🚀 How to Run
1.  Download the entire repository.
2.  Open **`R code - final edition`** in RStudio.
3.  **DO NOT RENAME the CSV files.** The script is calibrated to read these specific filenames locally.
4.  Run the script to reproduce all p-values, Sharpe ratios, and graphs shown in the PDF.

---

## 💡 Summary of Tactical Alpha
*   **Traditional Balanced (1/3)**: 7.07% Ann. Return | 32.17% Max Drawdown.
*   **My Dynamic Tactical Strategy**: **16.55% Ann. Return** | **19.86% Max Drawdown**.
