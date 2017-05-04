library(dplyr)
setwd("~/Documents/Biologia/Tesis/dataAnalysis/")
verdosidad%>% group_by(T) %>% 
  summarise(meanClA=mean(cla), meanClB=mean(clb), 
            meanClT=mean(clt), meanCar=mean(car)) -> promediopigmentos
save(promedio, promediopigmentos, promediosPE, file="promedios.RData")