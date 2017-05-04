library(dplyr)
HojasConRGR %>% 
  group_by(T,DDS) %>%
  summarise(meanFvFm= mean(fv.fm), meanYII=mean(y.II.),
            meanETR=mean(etr), meanNPQ=mean(NPQ), meanqP=mean(qP),
            meanafol=mean(a.fol), meanmseca=mean(mstotal), 
            meanCRA=mean(CRA), meanAFE=mean(AFE), meanIAF=mean(IAF)) -> promedio


