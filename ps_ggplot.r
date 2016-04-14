rm(list=ls())
library(twang)
library(reshape2)
library(ggplot2)
library(scales)
source("Rcode/makePlotDat.R")

# load some data
data(lalonde)

# fit a ps model
x = ps(treat~age+educ+black,data=lalonde)

# some things used in multiple plots
treat <- x$treat
propScores <- x$ps
vars <- x$variableNames
weights <- x$w
if (!all(weights[,1]==1)){
  weights   <- cbind(unw=rep(1,nrow(weights)),weights)
  propScores <- cbind(unw=rep(0.5,nrow(propScores)),propScores)
}
whichVar <- pVal <- weighted <- varb <- NULL   


##############
## figure 1 ##
optDat <- makePlotDat(x, whichPlot = 1)

p1 <- ggplot(optDat, aes(iteration, balance)) + 
      geom_point(shape = 19) +
      facet_grid(~stopRule) + labs(x="Iteration",y="Balance measure")
p1


##############
## figure 2 ##
boxDat = melt(data.frame(x$ps,treat=treat,id=1:nrow(x$ps)),id.vars=c("id","treat"))

p2 <- ggplot(boxDat, aes(as.factor(treat), value)) + 
      geom_boxplot(width=0.65,outlier.shape=1,outlier.size=2) + 
      coord_flip() +
      facet_grid(~variable) + 
      labs(x="Treatment",y="Propensity scores")
p2


##############
## figure 3 ##
esDat <- makePlotDat(x, whichPlot = 3, yOnly = FALSE)
esDat$sig = ifelse(esDat$pVal<0.05,19,1)

p3 <- ggplot(esDat) + 
      geom_line(aes(x=weighted, y=effectSize , group=whichVar , color=ifelse(esBig,"lightblue","red")),size=.75) +
      geom_point(aes(x=weighted, y=effectSize , group=whichVar,shape=sig),size=2,color="red") +
      facet_grid(~whichComp) + 
      labs(x="",y="Absolute standard difference") +
      scale_shape_identity() + 
      theme(legend.position = "none") + 
      scale_y_continuous(limits=c(0,min(3,max(esDat$effectSize))) , minor_breaks=NULL , breaks=c(0,.2,.5,.8))
p3



##############
## figure 4 ##
esDat <- makePlotDat(x, whichPlot = 4)
esDat$shape <- ifelse(esDat$weighted=='Weighted',1,19)
slope = 1/max(as.numeric(esDat$whichVar)-1)
p4 <- ggplot(esDat) + 
      geom_point(aes(x=tRank, y=tPVal ,shape=ifelse(esDat$weighted=='Weighted',1,19)),size=2) +
      facet_grid(~whichComp) + 
      labs(y="T test p-values",x="Rank of p-value for pretreatment variables\n (hollow is weighted, solid is unweighted)") +
      scale_shape_identity() + 
      theme(legend.position = "none") + 
      scale_y_continuous(limits=c(0,1)) +
      geom_abline(intercept=-slope,slope=slope,color="blue")
p4



##############
## figure 5 ##
esDat <- makePlotDat(x, whichPlot = 5)
esDat$shape <- ifelse(esDat$weighted=='Weighted',1,19)
slope = 1/max(as.numeric(esDat$whichVar)-1)
p5 <- ggplot(esDat) + 
  geom_point(aes(x=ksRank, y=ksPVal ,shape=ifelse(esDat$weighted=='Weighted',1,19)),size=2) +
  facet_grid(~whichComp) + 
  labs(y="KS test p-values",x="Rank of p-value for pretreatment variables\n (hollow is weighted, solid is unweighted)") +
  scale_shape_identity() + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits=c(0,1)) +
  geom_abline(intercept=-slope,slope=slope,color="blue")
p5


##############
## figure6 ##
histDat = melt(data.frame(x$w,treat=treat,id=1:nrow(x$ps)),id.vars=c("id","treat"))

p6 <- ggplot(histDat) + geom_histogram(aes(x=value),binwidth=1) + facet_grid(~variable) +
  scale_y_continuous(labels = percent)
p6

