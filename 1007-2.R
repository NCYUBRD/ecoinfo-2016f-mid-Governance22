# R 資料結構
#取50個整數
x <- sample(50)
x
#轉換矩陣
x.m <- matrix(x, nrow = 10)
#轉換資料矩陣
x.df <- data.frame (x.m)
x.m
x.m[,3] <-
summaryx.m