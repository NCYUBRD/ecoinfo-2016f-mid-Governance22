#先將R Studio之前的背景資料刪除掉
rm(list = ls())
ts <- 2016111815


#1. 輸入資料：
#利用第三方套件data.table讀取所要的資料
library(data.table)


#將目標data所在的資料夾設為目錄，並建立一個空的檔案，作為等等所有data要放入的地方
setwd("c:\\Users/admin/Desktop/raw/")
list <- list.files('.')
wed <- list()
length(list)


#2. 整理資料：
#資料夾中的所有data都讀取到新建立的檔案中，並將-9991, -9995-9996, -9997, -9998, -9999的data替換成NA
for(i in 1:length(list)){ wed[[i]] <- fread (list[i],
  skip = 75,
  na.strings = c('-9991', '-9995', '-9996', '-9997', '-9998', '-9999'))
      }
#利用for迴圈及rbind將每個原始檔案內的data一個個往下堆疊合併成同一個表格
for (i in 2:length(list)){
  wed[[1]] <- rbind(wed[[1]], wed[[i]])
}

#將合併好後的第一到第九欄名做更改
setnames(wed[[1]], 1:9,
         c('stno', 'yyyymmddhh', 'PSO1', 'TX01', 'RH01',
           'WD01', 'WD02', 'PP01', 'SS01'))

#將第二欄的yyyymmddhh時間內容的小時減去一小時
wed[[1]]$yyyymmddhh <- wed[[1]]$yyyymmddhh-1

#將yyyymmddhh欄的data轉換成時間戳記，並新增成timestamp欄
wed[[1]][, timestamp:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d%H'))]

#將yyyymmddhh欄的data提取出年, 月, 日，並獨立新增成day欄
wed[[1]][, day:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d'))]

#將yyyymmddhh欄的data提取出年, 月，並獨立新增成mon欄
wed[[1]][, mon:= format.Date(timestamp, '%Y-%m')]





#開始分析：
#(1)計算每日平均氣溫：
#設立計算平均值的公式
mean_omit_na <- function(x){
  x <- as.numeric(x)
  return(mean(x, na.rm = T))
}

#利用aggregate將day欄內數字相同者合併做TX01數值的平均值的計算，即可得每日平均氣溫
DAYTX <- aggregate(wed[[1]]$TX01, by = list(wed[[1]]$day), FUN = mean_omit_na)





#(2)計算每月平均氣溫：
#利用aggregate將mon欄內數字相同者合併做TX01數值的平均值的計算，即可得每月平均氣溫
MONTHTX <- aggregate(wed[[1]]$TX01, by = list(wed[[1]]$mon), FUN = mean_omit_na)





#(3)計算每日最高溫的平均：
#設立計算最大值的公式
MAX <- function(x){
  x <- as.numeric(x)
  return(max(x, na.rm = T))
}

#利用aggregate將day欄內數字相同者合併取出TX01數值的最大值，即可得每日最高溫
MAXDAY <- aggregate(wed[[1]]$TX01, by = list (wed[[1]]$day), FUN = MAX)

#將以上所得的每日最高溫表格轉成換data.frame的格式
MAXDAYdf <- data.frame(MAXDAY)

#取出表格的第二欄，即每日最高溫之溫度做平均
meanMAXDAY <- mean(MAXDAYdf[,2])
list(meanMAXDAY)





#(4)計算每日最低溫的平均：
#設立計算最小值的公式
MIN <- function(x){
  x <- as.numeric(x)
  return(min(x, na.rm = T))
}

#利用aggregate將day欄內數字相同者合併取出TX01數值的最小值，即可得每日最低溫
MINDAY <- aggregate(wed[[1]]$TX01, by = list (wed[[1]]$day), FUN = MIN)

#將以上所得的每日最高溫表格轉換成data.frame的格式
MINDAYdf <- data.frame(MINDAY)

#取出表格的第二欄，即每日最低溫之溫度做平均
meanMINDAY <- mean(MINDAYdf[,2])
list(meanMINDAY)





#(5)計算每月累積降水：
#設立計算累積值的公式
SUM <- function(x){
  x <- as.numeric(x)
  return(sum(x, na.rm = T))
}

#利用aggregate將mon欄內數字相同者合併取出PP01數值做累加，即可得每月累積降水量
SUMMONTH <- aggregate(wed[[1]]$PP01, by = list (wed[[1]]$mon), FUN = SUM)

#將所得的每月累積降水量的表格轉換成data.frame的格式，並取出第二欄資料，即每月累積降水量，做平均
SUMMONTHdf <- data.frame(SUMMONTH)
meanSUMMONTH <- mean(SUMMONTHdf[,2])
list(meanSUMMONTH)






#(6)計算最暖月的每日最高溫平均：
#將前面所做每月平均氣溫的表格轉換成data.frame的格式
MONTHTXdf <- data.frame(MONTHTX)

#利用setorder,對每月平均氣溫的表格以第二欄，即氣溫做排序，由小至大
orderedMONTHTXdf <- setorder(MONTHTXdf,x)

#取出第一欄，最後一列的data，即得到月均溫最高的年月
orderedMONTHTXdf[length(list),1]



#利用fread讀取屬於orderedMONTHTXdf[length(list),1]所顯示之年月(最暖月)的氣象資料
MAXMONTHdayTX <- fread("c:\\Users/admin/Desktop/raw/", 
                       skip = 75,
                       na.strings = c('-9991', '-9995', '-9996', '-9997', '-9998', '-9999'))

#對讀取的資料做如一開始以for迴圈讀取整筆資料時相同的整理方式
setnames(MAXMONTHdayTX, 1:9,
         c('stno', 'yyyymmddhh', 'PSO1', 'TX01', 'RH01',
           'WD01', 'WD02', 'PP01', 'SS01'))
MAXMONTHdayTX$yyyymmddhh <- MAXMONTHdayTX$yyyymmddhh-1
MAXMONTHdayTX[, timestamp:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d%H'))]
MAXMONTHdayTX[, day:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d'))]

#利用aggregate將day欄內數字相同者合併取出TX01數值的最大值，即可得最暖月之每日最高溫
MAXMONTHdayMAXTX <- aggregate(MAXMONTHdayTX$TX01, by = list(MAXMONTHdayTX$day), FUN = MAX)

#將所得最暖月的每日最高溫平均
lastMAXanswer <- mean(MAXMONTHdayMAXTX[,2])
lastMAXanswer 



#(7)計算最冷月的每日最低溫平均：
#取出前一題中所得完成排序之每月平均氣溫表格的第一欄，第一列data，即得到月均溫最低的年月
orderedMONTHTXdf[1,1]


#利用fread讀取屬於orderedMONTHTXdf[1,1]所顯示之年月(最冷月)的氣象資料
MINMONTHdayTX <- fread("c:\\Users/admin/Desktop/raw/", 
                       skip = 75,
                       na.strings = c('-9991', '-9995', '-9996', '-9997', '-9998', '-9999'))

#對讀取的資料做如一開始以for迴圈讀取整筆資料時相同的整理方式
setnames(MINMONTHdayTX, 1:9,
         c('stno', 'yyyymmddhh', 'PSO1', 'TX01', 'RH01',
           'WD01', 'WD02', 'PP01', 'SS01'))
MINMONTHdayTX$yyyymmddhh <- MINMONTHdayTX$yyyymmddhh-1
MINMONTHdayTX[, timestamp:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d%H'))]
MINMONTHdayTX[, day:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d'))]

#利用aggregate將day欄內數字相同者合併取出TX01數值的最小值，即可得最冷月之每日最低溫
MINMONTHdayMINTX <- aggregate(MINMONTHdayTX$TX01, by = list(MINMONTHdayTX$day), FUN = MIN)

#將所得最冷月的每日最低溫平均
lastMINanswer <- mean(MINMONTHdayMINTX[,2])
lastMINanswer 













