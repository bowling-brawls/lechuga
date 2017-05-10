library(ggplot2)
library(plyr)

if(R.version$os=="linux-gnu"){ 
  load("~/Documents/Ignacio/lechuga/noRawdfs.RData")
} else if (R.version$os=="darwin15.6.0"){
  load("~/Documents/Biologia/Tesis/dataAnalysis/noRawdfs.RData")
}


# Functions ---------------------------------------------------------------


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# 
# ddply(HojasConRGR, c("T", "DDS"), 
#       summarise,
#       promfvfm = mean(fv.fm),
#       SDfvfm = sd(fv.fm),
#       SEfvfm = SDfvfm/sqrt(N)
# )

# Plot --------------------------------------------------------------------

df <- summarySE(HojasConRGR, measurevar="fv.fm", groupvars=c("T", "DDS"))

FvFmbars <- ggplot(data=df, aes(x=as.factor(T), y=fv.fm, fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  coord_cartesian(ylim=c(0.75, 0.85)) + ylab("Fv/Fm") +  
  scale_x_discrete(name="Tratamiento", 
                   breaks=as.character(1:3), 
                   labels=c("PolUV", "Control","PolCom"))+
  scale_fill_discrete(name="DDS")+
  geom_errorbar(aes(ymin=fv.fm-se, ymax=fv.fm+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

print(FvFmbars)


#Los dos vectores de nombres de columna y labels deben estar en el MISMO ORDEN
colsSE <- c( "qP", "NPQ","a.fol", "AFE", "IAF", "y.II.", "etr",
  "CRA", "mstotal", "ms.hoja.tot", "ms.raiz", "ms.tallo")
labelscolsSE <- c("qP", "NPQ","Área foliar (cm2)", "Área Foliar Específica (cm2/g)", 
                  "Índice área foliar", "ACA VAMOS", "etr",
                  "CRA", "mstotal", "ms.hoja.tot", "ms.raiz", "ms.tallo")

numcols <-length(colsSE)

for(col in 1:numcols){
df <- summarySE(HojasConRGR, measurevar=colsSE[col], groupvars=c("T", "DDS"))

df$colmean <- df[,colsSE[col]]

objetoggplot <- ggplot(data=df, aes(x=as.factor(T), y=colmean, 
                                    fill=as.factor(DDS))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  ylab(labelscolsSE[col]) +  
  scale_x_discrete(name="Tratamiento", 
                   breaks=as.character(1:3), 
                   labels=c("PolUV", "Control","PolCom"))+
  scale_fill_discrete(name="DDS") +
  geom_errorbar(aes(ymin=colmean-se, ymax=colmean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

print(objetoggplot)
}