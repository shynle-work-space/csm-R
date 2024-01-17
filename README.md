Requirements						|	Approach
---------------------------------------------------------------------------------------------------------------
1• Xây dựng mô hình chuẩn				|	
2• Xem dữ liệu 						|	-> profilling using `Hmisc`
3• Preprocess data					|	-> remove `Movie`, encode `Genre` & `Year`
4• Xây dựng mô hình chuẩn				|
5• Predict va evaluate (mse)				|
6• Kiểm tra giả thuyết của LM				|
7• Kiểm tra đa cộng tuyến				|
    1.Corr plot						|
    2. VIF (hệ số phóng đại)				|
    3. Loại bỏ đa cộng tuyến				|
8• Kiểm tra dữ liệu thiếu (là MCAR hay MAR)		|
    1. Nếu là MCAR: thay bằng mean			|
    2. Nếu là MAR: multiple imputation			|
9• Kiểm tra và remove outliers [capping method]		|
10• Chọn biến xây dựng mô hình				|
11• Kiểm tra giả thuyết của LR 				|
    1. Phân phối chuẩn (qq plot, shapiro wik test	|
    2. Phương sai sai số k đổi 				|
	Mean = 0 => k thoa man 				|
	=> transform data voi boxcox transformation	|

