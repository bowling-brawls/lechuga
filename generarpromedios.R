library(dplyr)
HojasConRGR %>% 
  group_by(T,DDS) %>%
  summarise(meanFvFm= mean(fv.fm), meanYII=mean(y.II.),
            meanETR=mean(etr), meanNPQ=mean(NPQ), meanqP=mean(qP),
            meanafol=mean(a.fol), meanmseca=mean(mstotal), 
            meanCRA=mean(CRA), meanAFE=mean(AFE), meanIAF=mean(IAF), 
            meanmfresca=mean(mf.hoj), meanprodm2=mean(prodm2)) -> promedios
dds75 %>% 
  group_by(T) %>%
  summarise(meanprodm2= mean(prodm2), meanprod=mean(mf.hoj)) -> productionmean

