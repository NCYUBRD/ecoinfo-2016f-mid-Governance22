rm( list =  ls())

x <- c('下雨','颱風','下雪',
       '晴天','陰天','冰雹')
action <- c('帶傘','宅在家','出門玩')
#隨機排列，並且取出第一個「天氣」現象
#並命名為weather (變數)
#fi-ifelseif-else選擇
weather <- sample(x)[1]
weather
if(weather == '颱風' | '冰雹'){
  '宅在家'
}else if(weather == '下雨' | '下雪'){
  '帶傘'
}else if (weather == '晴天' | '陰天'){
  '出門玩'
}

  #帶傘
}
