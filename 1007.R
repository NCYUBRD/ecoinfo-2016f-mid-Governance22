#匯率換算
USDTW <- 31.53
#15 USDTW等於多少新台幣呢
15 * 31.53
15 * USDTW
#usdtwdConv function
#arguments:
# * usd 美金的數目
# *USDTWD 新台幣的匯率，預設值是31.5
usdtwdConv <- function(usd, USDTWD = 31.5){
  twd <- usd * USDTWD
  #
}