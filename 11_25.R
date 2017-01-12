#install dplyr??Œtidyr
install.packages(c('dplyr', 'tidyr'))

#install data set 
devtools::install_github('rstudio/EDAWR')

#fundamental 
iris.tbl <- dplyr::tbl_df(iris)
iris.tbl
require(tidyr)


#dplyr??„ç‰¹æ®Šç¬¦??Ÿï??%>%
#exï¼šx %>% f(y) ==> f(x,y)

iris %>% group_by(Species)
 == 
dplyr::group_by(iris, Species)

#f2(f1(x, y), k) == x %>% f1(y) %>% f2(k)
#f1(y) == mean(y)     f2(k) == (sd)   

dplyr::group_by(iris, Species)
browseVignettes(package = "dplyr")
install.packages('nycflights13')

library(nycflights13)
dim(flights)
quartz()
filter(flights, month == 1, day == 1)
flights <- rename(flights, dest = destination)
flight.m <- mutate(flights, 
                   gain = arr_delay - dep_delay, 
       speed = distance / air_time * 60)
require(dtplyr)
