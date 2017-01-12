rm (list = ls())
x <- c('下雨', '颱風', '下雪', '晴天', '陰天', '冰雹')
action <- c('帶傘', '宅在家', '出門玩')
print (weather <- sample(x)[1])
if (weather == '冰雹' | weather == '颱風'){
  print(paste('現在天氣是', weather, '所以', action[2], sep = ""))
}else if(weather == '下雨'){
    print(paste('現在天氣是', weather, '所以', action[1], sep = ""))
}else {
  print(paste('現在天氣是', weather, '所以', action[3], sep = ""))
  }
}

forecast
  }

