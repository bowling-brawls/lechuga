library(ggplot2)
library(reshape2)
library(dplyr)
library(RColorBrewer)

if(R.version$os=="linux-gnu"){ 
  load("~/Documents/Ignacio/lechuga/noRawdfs.RData")
  load("~/Documents/Ignacio/lechuga/promedios.RData")
  setwd("~/Documents/Ignacio/lechuga/")
} else if (R.version$os=="darwin15.6.0"){
  load("~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData")
  load("~/Documents/Biologia/Tesis/dataAnalysis/promedios.RData")
  setwd("~/Documents/Biologia/Tesis/dataAnalysis/")
  }



# Boxplots ----------------------------------------------------------------


print(qplot(as.factor(as.character(T)), rgrtot, data = HojasConRGR, 
            geom="boxplot",color=T))

print(qplot(as.factor(as.character(T)), IAF, data = HojasConRGR, 
            geom="boxplot",color=T))

# Lineas ------------------------------------------------------------------


# preparar los datos para el plot de lineas
promediosmelt <- melt(promedios, id.vars = c("T", "DDS"))
prodmelt <- melt(productionmean, id.vars = c("T"))

#graficar con ggplot2
library(dplyr)
df <- filter(promediosmelt, variable=="meanafol")
df$ID <- paste0(df$T,df$DDS)
detach("package:dplyr", unload=TRUE)
dfSE <- summarySE(HojasConRGR, measurevar="a.fol", groupvars=c("T", "DDS"))
dfSE$ID <- paste0(dfSE$T,dfSE$DDS)

df <- merge(df,dfSE[,c("ID","se")], by="ID")  

  afoliarplot <- ggplot(data = df, aes(x=DDS, y=value,
                                       colour=as.factor(as.character(T)))) + 
    geom_line() + 
    ylab(expression(paste("Área Foliar ", cm^{2}))) +
    xlab("Días después de siembra") + 
    scale_color_discrete(name="Tratamiento", breaks=c("1", "2", "3"),
                         labels=c("PolUV", "Control", "Polcom"))  +
    scale_x_continuous(breaks = c(30,45,60,75)) +
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(0.9))
  
  print(afoliarplot)
   
#intento lineas masa fresca
  library(dplyr)
  library(plyr)
  df <- filter(promediosmelt, variable=="meanmfresca")
  df$ID <- paste0(df$T,df$DDS)
  detach("package:dplyr", unload=TRUE)
  
  dfSE <- summarySE(HojasConRGR, measurevar= "mf.hoj", groupvars=c("T", "DDS"))
  dfSE$ID <- paste0(dfSE$T,dfSE$DDS)
  
  df <- merge(df,dfSE[,c("ID","se")], by="ID")  
  
  masafrescaplot <- ggplot(data = df, aes(x=DDS, y=value,
                                       colour=as.factor(as.character(T)))) + 
    geom_line() + 
    ylab(expression(paste("masa foliar fresca (g)"))) +
    xlab("Días después de siembra") + 
    scale_color_discrete(name="Tratamiento", breaks=c("1", "2", "3"),
                         labels=c("PolUV", "Control", "Polcom"))  +
    scale_x_continuous(breaks = c(30,45,60,75)) +
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(0.9))
  
  print(masafrescaplot)
  
  #masa seca linea ----
  
  library(dplyr)
  library(plyr)
  df <- filter(promediosmelt, variable=="meanmseca")
  df$ID <- paste0(df$T,df$DDS)
  detach("package:dplyr", unload=TRUE)
  
  dfSE <- summarySE(HojasConRGR, measurevar= "mstotal", groupvars=c("T", "DDS"))
  dfSE$ID <- paste0(dfSE$T,dfSE$DDS)
  
  df <- merge(df,dfSE[,c("ID","se")], by="ID")  
  
  masasecaplot <- ggplot(data = df, aes(x=DDS, y=value,
                                          colour=as.factor(as.character(T)))) + 
    geom_line() + 
    ylab(expression(paste("masa seca total (g)"))) +
    xlab("Días después de siembra") + 
    scale_color_discrete(name="Tratamiento", breaks=c("1", "2", "3"),
                         labels=c("PolUV", "Control", "Polcom"))  +
    scale_x_continuous(breaks = c(30,45,60,75)) +
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(0.9))
  
  print(masasecaplot)
  
  # AFE ----
  
  library(dplyr)
  library(plyr)
  df <- filter(promediosmelt, variable=="meanAFE")
  df$ID <- paste0(df$T,df$DDS)
  detach("package:dplyr", unload=TRUE)
  
  dfSE <- summarySE(HojasConRGR, measurevar= "AFE", groupvars=c("T", "DDS"))
  dfSE$ID <- paste0(dfSE$T,dfSE$DDS)
  
  df <- merge(df,dfSE[,c("ID","se")], by="ID")  
  
AFEplot <- ggplot(data = df, aes(x=DDS, y=value,
                                        colour=as.factor(as.character(T)))) + 
    geom_line() + 
    ylab(expression(paste("Area foliar específica"))) +
    xlab("Días después de siembra") + 
    scale_color_discrete(name="Tratamiento", breaks=c("1", "2", "3"),
                         labels=c("PolUV", "Control", "Polcom"))  +
    scale_x_continuous(breaks = c(30,45,60,75)) +
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(0.9))
  
  print(AFEplot)
  
  #IAF ----
  
  library(dplyr)
  library(plyr)
  df <- filter(promediosmelt, variable=="meanIAF")
  df$ID <- paste0(df$T,df$DDS)
  detach("package:dplyr", unload=TRUE)
  
  dfSE <- summarySE(HojasConRGR, measurevar= "IAF", groupvars=c("T", "DDS"))
  dfSE$ID <- paste0(dfSE$T,dfSE$DDS)
  
  df <- merge(df,dfSE[,c("ID","se")], by="ID")  
  
  IAFplot <- ggplot(data = df, aes(x=DDS, y=value,
                                   colour=as.factor(as.character(T)))) + 
    geom_line() + 
    ylab(expression(paste("Indice de área foliar"))) +
    xlab("Días después de siembra") + 
    scale_color_discrete(name="Tratamiento", breaks=c("1", "2", "3"),
                         labels=c("PolUV", "Control", "Polcom"))  +
    scale_x_continuous(breaks = c(30,45,60,75)) +
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(0.9))
  
  print(IAFplot)
  
  
  #masa seca raiz ----
  
  library(dplyr)
  library(plyr)
  df <- filter(promediosmelt, variable=="meanmsraiz")
  df$ID <- paste0(df$T,df$DDS)
  detach("package:dplyr", unload=TRUE)
  
  dfSE <- summarySE(HojasConRGR, measurevar= "ms.raiz", groupvars=c("T", "DDS"))
  dfSE$ID <- paste0(dfSE$T,dfSE$DDS)
  
  df <- merge(df,dfSE[,c("ID","se")], by="ID")  
  
  msraizplot <- ggplot(data = df, aes(x=DDS, y=value,
                                   colour=as.factor(as.character(T)))) + 
    geom_line() + 
    ylab(expression(paste("Masa seca raiz"))) +
    xlab("Días después de siembra") + 
    scale_color_discrete(name="Tratamiento", breaks=c("1", "2", "3"),
                         labels=c("PolUV", "Control", "Polcom"))  +
    scale_x_continuous(breaks = c(30,45,60,75)) +
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(0.9))
  
  print(msraizplot)
  
#histogramas-----
# pigmentos bars -------------------------------------------------------------
df <- melt(promediopigmentos, id.vars = c("T"))
pigmentosbars <- ggplot(data=df, aes(x=T, y=value, fill=variable)) +  
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
print(pigmentosbars)


#masa seca tallo ----

library(dplyr)
library(plyr)
df <- filter(promediosmelt, variable=="meanmstallo")
df$ID <- paste0(df$T,df$DDS)
detach("package:dplyr", unload=TRUE)

dfSE <- summarySE(HojasConRGR, measurevar= "ms.tallo", groupvars=c("T", "DDS"))
dfSE$ID <- paste0(dfSE$T,dfSE$DDS)

df <- merge(df,dfSE[,c("ID","se")], by="ID")  

mstalloplot <- ggplot(data = df, aes(x=DDS, y=value,
                                     colour=as.factor(as.character(T)))) + 
  geom_line() + 
  ylab(expression(paste("Masa seca tallo"))) +
  xlab("Días después de siembra") + 
  scale_color_discrete(name="Tratamiento", breaks=c("1", "2", "3"),
                       labels=c("PolUV", "Control", "Polcom"))  +
  scale_x_continuous(breaks = c(30,45,60,75)) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(0.9))

print(mstalloplot)


#histogramas-----
# pigmentos bars -------------------------------------------------------------
df <- melt(promediopigmentos, id.vars = c("T"))
pigmentosbars <- ggplot(data=df, aes(x=T, y=value, fill=variable)) +  
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  ylab(expression(paste("mg/g"))) +
  xlab("Tratamiento") 

print(pigmentosbars) 

#pigmentos bars 2

# df <- summarySE(promediopigmentos, measurevar="meanClA", "meanClB", "meanClT", 
#                 "meanCar", groupvars= (T))
# 
# pigmentos2bars <- ggplot(data=df, aes(x=as.factor(T), y=fv.fm, fill=as.factor(DDS))) +
#   geom_bar(stat="identity", position=position_dodge(), colour="black") + 
#   ylab("mg pigmento/g masa fresca") +  
#   scale_x_discrete(name="Tratamiento", 
#                    breaks=as.character(1:3), 
#                    labels=c("PolUV", "Control","PolCom"))+
#   scale_fill_discrete(name="DDS")+
#   geom_errorbar(aes(ymin=fv.fm-se, ymax=fv.fm+se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))
# 
# print(FvFmbars)

#fvfm bars ----

df <- filter(promediosmelt, variable=="meanFvFm")

# df <- summarySE(HojasConRGR, measurevar="fv.fm", groupvars=c("T", "DDS"))
# 
# FvFmbars <- ggplot(data=df, aes(x=as.factor(T), y=fv.fm, fill=as.factor(DDS))) +
#   geom_bar(stat="identity", position=position_dodge(), colour="black") + 
#   coord_cartesian(ylim=c(0.75, 0.85)) + ylab("Fv/Fm") +  
#   scale_x_discrete(name="Tratamiento", 
#                    breaks=as.character(1:3), 
#                    labels=c("PolUV", "Control","PolCom"))+
#   scale_fill_discrete(name="DDS")+
#   geom_errorbar(aes(ymin=fv.fm-se, ymax=fv.fm+se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))
# 
# print(FvFmbars)


#YII bars
df <- filter(promediosmelt, variable=="meanYII")

YIIbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") 
# + 
  # coord_cartesian(ylim=c(0, 0.4))
print(YIIbars)

# qP bars----
df <- filter(promediosmelt, variable=="meanqP")

qPbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  coord_cartesian(ylim=c(0, 0.6))
print(qPbars)

#NPQ bars ----

df <- filter(promediosmelt, variable=="meanNPQ")

NPQbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  coord_cartesian(ylim=c(0, 1.45))
print(NPQbars)

#masa seca bars----
df <- filter(promediosmelt, variable=="meanmseca")

msecabars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(msecabars)

#AFEbars----
df <- filter(promediosmelt, variable=="meanAFE")

AFEbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(AFEbars)

#IAF bars-----
df <- filter(promediosmelt, variable=="meanIAF")

IAFbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(IAFbars)

#productividad bars----
library(dplyr)
df <- filter(prodmelt, variable=="meanprod")

productivitybars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(T))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(productivitybars)

df <- filter(prodmelt, variable=="meanprodm2")

productivitybars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(T))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(productivitybars)

#

# df <- filter(prod, variable=="prodm2")
# 
# productivitybars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
#   geom_bar(stat="identity", position=position_dodge(), colour="black")
# print(productivitybars)

#PE bars -------------------------------------------------------------
  df <- melt(promediosPE, id.vars = c("T"))
PEbars <- ggplot(data=df, aes(x=T, y=value, fill=variable)) +  
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  ylab(expression(paste("%"))) +
  xlab("Tratamiento") 
print(PEbars)
