library(dplyr)
PE%>%group_by(T)%>%
  summarise(meanPE=mean(pe)) -> promediosPE
