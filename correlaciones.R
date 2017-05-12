colsSE <- c( "fv.fm", "qP", "NPQ","a.fol", "AFE", "IAF", "y.II.", "etr",
             "CRA", "mstotal", "mf.hoj", 
             "RootShoot")

library(corrgram)
library(Hmisc)

corrgram(HojasConRGR[,colsSE],order = TRUE, upper.panel = NULL)
correlaciones <- rcorr(as.matrix(HojasConRGR[,colsSE]), 
                       as.matrix(HojasConRGR[,colsSE]), type="p")
correlaciones <-  as.matrix(correlaciones$V1$r)

