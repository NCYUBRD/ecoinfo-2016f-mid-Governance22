data.table::fread("C:\\Users\\admin\\Desktop\\raw\\200601_auto_hr.txt", skip = 74)
read.csv("C:\\Users\\admin\\Desktop\\raw\\200601_auto_hr.txt", skip = 74)
rm(list = ls() )
ts <- '20161011'
ts_formated <-strptime(ts, '%Y%m%d%H')
timestamp <- as.POSIXct(ts_formated)
a201601 <- fread ("~/C:\\Users\\admin\\Desktop\\raw\\200601_auto_hr.txt", 
                 skip = 74,
                 na.strings = c('-9991','-9995','-9996','-9997','-9998','-9999'))
colnamesa <- c("stno",201601,"PS01", "TX01", "RH01","WD01","WD02","PP01", "S-S01"
setNames ("a, 1:9, colnamesa")

