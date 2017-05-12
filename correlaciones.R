colsSE <- c( "fv.fm", "qP", "NPQ","a.fol", "AFE", "IAF", "y.II.", "etr",
             "CRA", "mstotal", "mf.hoj", 
             "RootShoot")

library(corrgram)
library(Hmisc)
library(xlsx)

corrgram(HojasConRGR[,colsSE],order = TRUE, upper.panel = NULL)
correlaciones <- rcorr(as.matrix(HojasConRGR[,colsSE]), as.matrix(HojasConRGR[,colsSE]), 
                       type="p")

correlacionesPearson <- as.data.frame(correlaciones$r)

significanciaPearson <- as.data.frame(correlaciones$P)

save(significanciaPearson, correlacionesPearson, file="correlaciones.RData")
write.xlsx(correlacionesPearson[1:12,1:12], file = "corrPearson.xlsx")
write.xlsx(significanciaPearson[1:12,1:12], file = "signPearson.xlsx")
