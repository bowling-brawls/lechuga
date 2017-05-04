library(ggplot2)
library(reshape2)
library(dplyr)

if(R.version$os=="linux-gnu"){ 
  load("~/Documents/Ignacio/lechuga/noRawdfs.RData")
  load("~/Documents/Ignacio/lechuga/promedios.RData")
} else if (R.version$os=="darwin15.6.0"){
  load("~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData")
  load("~/Documents/Biologia/Tesis/dataAnalysis/promedios.RData")
}


# Boxplots ----------------------------------------------------------------


print(qplot(as.factor(as.character(T)), rgrtot, data = HojasConRGR, 
            geom="boxplot",color=T))


# Lineas ------------------------------------------------------------------


# preparar los datos para el plot de lineas
promediosmelt <- melt(promedio, id.vars = c("T", "DDS"))

#graficar con ggplot2
df <- filter(promediosmelt, variable=="meanafol")


afoliarplot <- ggplot(data = df, aes(x=DDS, y=value, colour=as.factor(as.character(T)))) + geom_line() + 
  ylab("Área foliar (cm^2)") + xlab("Días después de siembra") + 
  scale_color_discrete(name="Tratamiento", breaks=c("1", "2", "3"),
                       labels=c("PolUV", "Control", "Polcom"))

print(afoliarplot)

# Histogramas -------------------------------------------------------------
df <- melt(promediopigmentos, id.vars = c("T"))
pigmentosbars <- ggplot(data=df, aes(x=T, y=value, fill=variable)) +  geom_bar(stat="identity", position=position_dodge(), colour="black")
print(pigmentosbars)


df <- filter(promediosmelt, variable=="meanFvFm")

FvFmbars <- ggplot(data=df, aes(x=T, y=value, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  coord_cartesian(ylim=c(0.75, 0.85))
print(FvFmbars)


