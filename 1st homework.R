#¥ý²M°£¤§«eªº­I´º¸ê®Æ¡A¨Ã«Ø¥ßtsªí¥Ü¼gcodeªº¤é´Á¡B®É¶¡
rm(list = ls())
ts <- 2016111815

#1. Åª¨ú¸ê®Æ‹óŠÔ
#§Q¥Îddata.tableÅª¨ú¸ê®Æ—¿
library(data.table)

#³]¥ß¤u§@¥Ø¿ý¡A¥H«K¦b¦¹¥Ø¿ý¤¤¶i¦æ«ü¥Oªº¼¶¼g©M¾Þ§@
setwd("D:\\Users/admin/Desktop/raw/")

#«Ø¥ßªÅªºÀÉ®×ªÅ¶¡¡A±N¥Ø¿ý¤¤©Ò¦³ÀÉ®×ªºdata©ñ¶i¥h
list <- list.files('.')
wed <- list()

#­pºâÅª¤JªÅÀÉ®×¤¤ªºÀÉ®×¼Æ¬°¦h¤Ö
length(list)




#2. ¸ê®Æªºªì¨B¾ã²z
#§Q¥Îfor°j°é±N¸ê®Æ¤¤¬°ˆ×-9991, -9995, -9996, -9997, -9998, -9999ªº¿ù»~¼Æ­È§ï¬°NA
for(i in 1:length(list)){ wed[[i]] <- fread (list[i],
  skip = 75,
  na.strings = c('-9991', '-9995', '-9996', '-9997', '-9998', '-9999'))
      }

#§Q¥Îrbind±N¸ê®Æªí®æ¤@­Ó­Ó©¹¤UÅ|¥[¦X¨Ö°_¨Ó
for (i in 2:length(list)){
  wed[[1]] <- rbind(wed[[1]], wed[[i]])
}

#³]©w1~9ªºÄæ¦W
setnames(wed[[1]], 1:9,
         c('stno', 'yyyymmddhh', 'PSO1', 'TX01', 'RH01',
           'WD01', 'WD02', 'PP01', 'SS01'))

#±Nyyyymmddhh¦¹Äæªº¼Æ­È´î¥h¤@¤p®É¡A¥HÁ×§Kµ{¦¡©¿²¤Åã¥Ü¬°²Ä24¤p®Éªº¸ê®Æ
wed[[1]]$yyyymmddhh <- wed[[1]]$yyyymmddhh-1

#±Nyyyymmddhh¦¹Äæªº¼Æ­ÈÂà´«¬°®É¶¡ÂW°Oªº®æ¦¡¡A¨Ã³]¥ß®É¶¡ÂW°O¤@Äæ
wed[[1]][, timestamp:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d%H'))]

#±qyyyymmddhh¤@Äæ¤¤¥u¿ï¨ú¦~¡B¤ë¡B¤éªº¸ê®Æ¡A¨Ã¿W¥ß¬°day¤@Äæ
wed[[1]][, day:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d'))]

#¦Ûyyyymmddhh¤¤¿ï¨ú¦~¡B¤ëªº¸ê®Æ¡A¨Ã¿W¥ß¥X¨Ó¦¨mon¤@Äæ
wed[[1]][, mon:= format.Date(timestamp, '%Y-%m')]
#¥H¤W§¹¦¨¸ê®Æªº¾ã²z



#°ÝÃD¡G
#(1)­pºâ2006¦Ü2015¦~ªº¨C¤é¥­§¡®ð·Å¡G
#«Ø¥ß¥­§¡­Èªº­pºâ¤½¦¡
mean_omit_na <- function(x){
  x <- as.numeric(x)
  return(mean(x, na.rm = T))
}

#§Q¥Î”¨aggregat»E¦X­pºâdayÄædata¬Û¦PªÌ¦bTšTX01Äædataªº¥­§¡­È
#±N©Ò­pºâ¥Xªº¼Æ­È«Ø¥ßDAYTX¤@ªí®æ¡Aªí®æ¤º®e§Y¬°¨C¤éªº¥­§¡®ð·Å
DAYTX <- aggregate(wed[[1]]$TX01, by = list(wed[[1]]$day), FUN = mean_omit_na)


#(2)­pºâ2006¦Ü2015¦~ªº¨C¤é¥­§¡®ð·Å¡G
#§Q¥Î”¨aggregat»E¦X­pºâmonÄædata¬Û¦PªÌ¦bTšTX01Äædataªº¥­§¡­È
#±N©Ò­pºâ¥Xªº¼Æ­È«Ø¥ßMONTHTX¤@ªí®æ¡Aªí®æ¤º®e§Y¬°¨C¤ëªº¥­§¡®ð·Å
MONTHTX <- aggregate(wed[[1]]$TX01, by = list(wed[[1]]$mon), FUN = mean_omit_na)


#(3)­pºâ2006¦Ü2015¦~ªº¨C¤é³Ì°ª·Åªº¥­§¡¡G
#«Ø¥ß³Ì¤j­Èªº­pºâ¤½¦¡
MAX <- function(x){
  x <- as.numeric(x)
  return(max(x, na.rm = T))
}

#§Q¥Î”¨aggregat»E¦X­pºâdayÄædata¬Û¦PªÌ¦bTšTX01Äædataªº³Ì¤j­È
#±N©Ò­pºâ¥Xªº¼Æ­È«Ø¥ßMAXDAY¤@ªí®æ¡Aªí®æ¤º®e§Y¬°¨C¤éªº³Ì°ª·Å
MAXDAY <- aggregate(wed[[1]]$TX01, by = list (wed[[1]]$day), FUN = MAX)

#±NMAXDAYªº¸ê®ÆÂà´«¦¨data.frameªº®æ¦¡¡A¨Ã©R¦W¦¨·sªºªí®æ¢w¢wMAXDAYdf
MAXDAYdf <- data.frame(MAXDAY)

#¥H­pºâ¥­§¡­Èªº¤½¦¡¡AÅª¨ú¡B­pºâMAXDATdf²Ä¤GÄæªº¸ê®Æ¡A©Ò±o¼Æ­È©R¦W¦¨meanMAXDAY
meanMAXDAY <- mean(MAXDAYdf[,2])

#meanMAXDAY§Y¬°2006¦Ü2015¦~ªº¨C¤é³Ì°ª·Åªº¥­§¡
list(meanMAXDAY)


#(4)­pºâ2006¦Ü2015¦~ªº¨C¤é³Ì§C·Åªº¥­§¡¡G
#«Ø¥ß³Ì¤p­Èªº­pºâ¤½¦¡
MIN <- function(x){
  x <- as.numeric(x)
  return(min(x, na.rm = T))
}

#§Q¥Î”¨aggregat»E¦X­pºâdayÄædata¬Û¦PªÌ¦bTšTX01Äædataªº³Ì¤p­È
#±N©Ò­pºâ¥Xªº¼Æ­È«Ø¥ßMINDAY¤@ªí®æ¡Aªí®æ¤º®e§Y¬°¨C¤éªº³Ì§C·Å
MINDAY <- aggregate(wed[[1]]$TX01, by = list (wed[[1]]$day), FUN = MIN)

#±NMINDAYªº¸ê®ÆÂà´«¦¨data.frameªº®æ¦¡¡A¨Ã©R¦W¦¨·sªºªí®æ¢w¢wMINDAYdf
MINDAYdf <- data.frame(MINDAY)

#¥H­pºâ¥­§¡­Èªº¤½¦¡¡AÅª¨ú¡B­pºâMINDATdf²Ä¤GÄæªº¸ê®Æ¡A©Ò±o¼Æ­È©R¦W¦¨meanMINDAY
meanMINDAY <- mean(MINDAYdf[,2])

#meanMAXDAY§Y¬°2006¦Ü2015¦~ªº¨C¤é³Ì§C·Åªº¥­§¡
list(meanMINDAY)


#(5)­pºâ2006¦Ü2015¦~¥­§¡¨C¤ë²Ö¿n­°¤ô¡G
#«Ø¥ß­pºâ²Ö¥[­Èªº¤½¦¡
SUM <- function(x){
  x <- as.numeric(x)
  return(sum(x, na.rm = T))
}

#§Q¥Î”¨aggregat»E¦X­pºâmonÄædata¬Û¦PªÌ¦bPPX01Äædataªº²Ö¥[­È
#±N©Ò­pºâ¥Xªº¼Æ­È«Ø¥ßSUMMONTH¤@ªí®æ¡Aªí®æ¤º®e§Y¬°¨C¤ëªº²Ö²Ö£¡²Ö¿n­p«B¶q
SUMMONTH <- aggregate(wed[[1]]$PP01, by = list (wed[[1]]$mon), FUN = SUM)

#±NSUMMONTHªº¸ê®ÆÂà´«¦¨data.frameªº®æ¦¡¡A¨Ã©R¦W¦¨·sªºªí®æ¢w¢wSUMMONTHdf
SUMMONTHdf <- data.frame(SUMMONTH)

#¥H­pºâ¥­§¡­Èªº¤½¦¡¡AÅª¨ú¡B­pºâSUMMONTHdf²Ä¤GÄæªº¸ê®Æ¡A©Ò±o¼Æ­È©R¦W¦¨meanSUMMONTH
meanSUMMONTH <- mean(SUMMONTHdf[,2])

#meanSUMMONTH§Y¬°2006¦Ü2015¦~¨C¤ë²Ö¿n«B¶qªº¥­§¡
list(meanSUMMONTH)


#(6)­pºâ³Ì·x¤ëªº¨C¤é³Ì°ª·Å¥­§¡¡G
#±N°O¸ü¨C¤ë¥­§¡®ð·Åªºªí®æ¢w¢wMONTHTXÂà¶×¦¨data.frameªº®æ¦¡
MONTHTXdf <- data.frame(MONTHTX)

#¹ïMONTHTXdf¤¤°O¸ü¨C¤ë¥­§¡®ð·ÅªºxÄæ¶i¦æ±Æ§Ç¡A³Ì«á¤@¶µ¬°³Ì¤j­È
#¨Ã«Ø¥ß·sªí®æ¢w¢worderedMONTHTXdf
orderedMONTHTXdf <- setorder(MONTHTXdf,x)

#Åª¨ú°O¸ü¦~¤ë¥÷ªº²Ä¤@Äæ³Ì«á¤@¶µªº¼Æ­È¡A¦¹§Y¬°³Ì·x¤ëªº¦~¤ë¥÷
orderedMONTHTXdf[length(list),1]



#§Q¥Î”¨freadÅª¨úorderedMONTHTXdf[length(list),1]©Ò±o¥Xªº³Ì·x¤ëªº¦~¤ë¥÷ªº®ð¶H¸ê®Æ
#code¼gªkÀ³¬°fread("c:\\Users/admin/Desktop/raw/³Ì·x¤ëªº®ð¶H¸ê®ÆÀÉ®×¦W", ......)
#±N¸ê®Æ¤¤¬°ˆ×-9991, -9995, -9996, -9997, -9998, -9999ªº¿ù»~¼Æ­È§ï¬°NA
MAXMONTHdayTX <- fread("c:\\Users/admin/Desktop/raw/", 
                       skip = 75,
                       na.strings = c('-9991', '-9995', '-9996', '-9997', '-9998', '-9999'))

#³]©w1~9ÄæªºÄæ¦W
setnames(MAXMONTHdayTX, 1:9,
         c('stno', 'yyyymmddhh', 'PSO1', 'TX01', 'RH01',
           'WD01', 'WD02', 'PP01', 'SS01'))

#±Nyyyymmddhh¦¹Äæªº¼Æ­È´î¥h¤@¤p®É¡A¥HÁ×§Kµ{¦¡©¿²¤Åã¥Ü¬°²Ä24¤p®Éªº¸ê®Æ
MAXMONTHdayTX$yyyymmddhh <- MAXMONTHdayTX$yyyymmddhh-1

#±Nyyyymmddhh¦¹Äæªº¼Æ­ÈÂà´«¬°®É¶¡ÂW°Oªº®æ¦¡¡A¨Ã³]¥ß®É¶¡ÂW°O¤@Äæ
MAXMONTHdayTX[, timestamp:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d%H'))]

#±qyyyymmddhh¤@Äæ¤¤¥u¿ï¨ú¦~¡B¤ë¡B¤éªº¸ê®Æ¡A¨Ã¿W¥ß¬°day¤@Äæ
MAXMONTHdayTX[, day:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d'))]

#§Q¥Î”¨aggregat»E¦X­pºâdayÄædata¬Û¦PªÌ¦bTX01Äædataªº³Ì¤j­È
#«Ø¥ß¤@ªí®æ¢w¢wMAXMONTHdayMAXTX¡A°O¸ü³Ì·x¤ëªº¨C¤é³Ì°ª·Å
MAXMONTHdayMAXTX <- aggregate(MAXMONTHdayTX$TX01, by = list(MAXMONTHdayTX$day), FUN = MAX)


#¨Ï¥Î­pºâ¥­§¡­Èªº¤½¦¡­pºâMAXMONTHdayMAXTX²Ä¤GÄæªºdata¡A¤]´N¬O¹ï³Ì·x¤ëªº¨C¤é³Ì°ª·Å°µ¥­§¡­pºâ
lastMAXanswer <- mean(MAXMONTHdayMAXTX[,2])

#­pºâ¥Xªº³Ì«áµ²ªG§Y¬°³Ì·x¤ëªº¨C¤é³Ì°ª·Å¥­§¡
lastMAXanswer 


#(7)­pºâ³Ì§N¤ëªº¨C¤é³Ì§C·Å¥­§¡¡G
#©ÓÄò°ÝÃD(6)¡A¹ïMONTHTXdf¤¤°O¸ü¨C¤ë¥­§¡®ð·ÅªºxÄæ¶i¦æ±Æ§Ç¡A²Ä¤@¶µ¬°³Ì¤p­È
#«Ø¥ß·sªí®æ¢w¢worderedMONTHTXdf
#Åª¨úorderedMONTHTXdfªº²Ä¤@¶µ²Ä¤@¦Cdata§Y¬O³Ì§N¤ëªº¦~¤ë¥÷
orderedMONTHTXdf[1,1]

#§Q¥Î”¨freadÅª¨úorderedMONTHTXdf[1,1]©Ò±o¥Xªº³Ì§N¤ëªº¦~¤ë¥÷ªº®ð¶H¸ê®Æ
#code¼gªkÀ³¬°fread("c:\\Users/admin/Desktop/raw/³Ì§N¤ëªº®ð¶H¸ê®ÆÀÉ®×¦W", ......)
#±N¸ê®Æ¤¤¬°ˆ×-9991, -9995, -9996, -9997, -9998, -9999ªº¿ù»~¼Æ­È§ï¬°NA
MINMONTHdayTX <- fread("c:\\Users/admin/Desktop/raw/", 
                       skip = 75,
                       na.strings = c('-9991', '-9995', '-9996', '-9997', '-9998', '-9999'))

##³]©w1~9ÄæªºÄæ¦W
setnames(MINMONTHdayTX, 1:9,
         c('stno', 'yyyymmddhh', 'PSO1', 'TX01', 'RH01',
           'WD01', 'WD02', 'PP01', 'SS01'))

#±Nyyyymmddhh¦¹Äæªº¼Æ­È´î¥h¤@¤p®É¡A¥HÁ×§Kµ{¦¡©¿²¤Åã¥Ü¬°²Ä24¤p®Éªº¸ê®Æ
MINMONTHdayTX$yyyymmddhh <- MINMONTHdayTX$yyyymmddhh-1

#±Nyyyymmddhh¦¹Äæªº¼Æ­ÈÂà´«¬°®É¶¡ÂW°Oªº®æ¦¡¡A¨Ã³]¥ß®É¶¡ÂW°O¤@Äæ
MINMONTHdayTX[, timestamp:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d%H'))]

#±qyyyymmddhh¤@Äæ¤¤¥u¿ï¨ú¦~¡B¤ë¡B¤éªº¸ê®Æ¡A¨Ã¿W¥ß¬°day¤@Äæ
MINMONTHdayTX[, day:= as.POSIXct (strptime(yyyymmddhh, '%Y%m%d'))]

#§Q¥Î”¨aggregat»E¦X­pºâdayÄædata¬Û¦PªÌ¦bTX01Äædataªº³Ì¤p­È
#«Ø¥ß¤@ªí®æ¢w¢wMINMONTHdayMINTX¡A°O¸ü³Ì§N¤ëªº¨C¤é³Ì§C·Å
MINMONTHdayMINTX <- aggregate(MINMONTHdayTX$TX01, by = list(MINMONTHdayTX$day), FUN = MIN)

##¨Ï¥Î­pºâ¥­§¡­Èªº¤½¦¡­pºâMINMONTHdayMINTX²Ä¤GÄæªºdata¡A¤]´N¬O¹ï³Ì§N¤ëªº¨C¤é³Ì§C·Å°µ¥­§¡­pºâ
lastMINanswer <- mean(MINMONTHdayMINTX[,2])

#­pºâ¥Xªº³Ì«áµ²ªG§Y¬°³Ì§N¤ëªº¨C¤é³Ì§C·Å¥­§¡
lastMINanswer 












