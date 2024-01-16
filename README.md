# Requirements

1. Tiền xử lý dữ liệu
2. Xem dữ liệu
3. Kiểm tra đa cộng tuyến
4. Corr plot
5. VIF (hệ số phóng đại)
6. Loại bỏ đa cộng tuyến
7. Kiểm tra dữ liệu thiếu
8. Kiểm tra là MCAR hay MAR
    8.1 Nếu là MCAR: thay bằng mean
    8.2 Nếu là MAR: multiple imputation
9. Kiểm tra và remove outliers [capping method]
10. Tạo biến dummy cho Genre và Movie
11. Chọn biến xây dựng mô hình
12. Kiểm tra giả thuyết của LR 
13. Phân phối chuẩn (qq plot, shapiro wik test
14. Phương sai sai số k đổi 
    ```
    Mean = 0
    => k thoa man => transform data voi boxcox transformation
    ```
15. Predict va evaluate (mse)