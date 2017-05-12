detach("package:dplyr", unload=TRUE)
library(dplyr)

PE%>%group_by(T)%>%
  summarise(meanPE=mean(pe)) -> promediosPE

save(promediosPE, file="promediosPE.Rdata")
