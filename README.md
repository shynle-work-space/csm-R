1. Đọc dữ liệu và profiling
2. Tiền xử lí cho mô hình chuẩn
    2.1 Rename, encode bằng binary - factorize
3. Xây dựng mô hình chuẩn
4. Kiểm tra giả thuyết (Normality test) => ⛔ Ko đạt
    4.1 Bằng QQPlot
    4.2 Bằng bài test
5. Normalize bằng Box-Cox
    5.1 Transform outlier bằng Winsorize
    5.2 Box-Cox transformation bằng "x ~ Y"
    5.3 Thực hiện bước `3` và `4`
6. Đa cộng tuyến trên tập dữ liệu transform
    6.1 CorrPlot
    6.2 VIF
    6.3 Loại biến thủ công ⏳