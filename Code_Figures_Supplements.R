# Code producing figures for supplementary material, realized in Adobe Illustrator

Mytheme<- theme_bw()+
  theme(#legend.position = "none",
    panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=5),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", size = 16, hjust = 0.5))

# Figure S1 ----

library(ggplot2)
library(devtools)
library(easyGgplot2)
library(plyr)
library(dplyr)
library(grid)
library(gridExtra)
library(splitstackshape)
library(xtable)




source("C:/Users/CamilleAnna/Desktop/PBDproject/Manuscript/Functions.R")
setwd("c:/Users/CamilleAnna/Desktop/PBDproject/outputs/ML_estimates")



# make plots for b
limits = c(lim_b = 217, lim_mu = 150, lim_la = 12, lim_b_mu = 1, lim_tau = 10)
dat_b03<- MakeboxplotData(limit=limits, b=0.3)
p_b03_b<- MakeBoxPlot(lim = 217, b=0.3, data=dat_b03$b, param="b_accuracy", paramName = "Speciation initiation rate (b)", liminf= 2.5)

limits = c(lim_b = 110 , lim_mu = 150, lim_la = 12, lim_b_mu = 1, lim_tau = 10)
dat_b04<- MakeboxplotData(limit=limits, b=0.4)
p_b04_b<- MakeBoxPlot(lim = 110, b=0.4, data=dat_b04$b, param="b_accuracy", paramName = "Speciation initiation rate (b)", liminf= 2.5)


limits = c(lim_b = 60, lim_mu = 12, lim_la = 12, lim_b_mu = 1, lim_tau = 10)
dat_b05<- MakeboxplotData(limit=limits, b=0.5)
dat_b06<- MakeboxplotData(limit=limits, b=0.6)
dat_b07<- MakeboxplotData(limit=limits, b=0.7)

p_b05_b<- MakeBoxPlot(lim = 60, b=0.5, data=dat_b05$b, param="b_accuracy", paramName = "Speciation initiation rate (b)", liminf= 2.5)
p_b06_b<- MakeBoxPlot(lim = 60, b=0.6, data=dat_b06$b, param="b_accuracy", paramName = "Speciation initiation rate (b)", liminf= 2.5)
p_b07_b<- MakeBoxPlot(lim = 60, b=0.7, data=dat_b07$b, param="b_accuracy", paramName = "Speciation initiation rate (b)", liminf= 2.5)



# Make plots for mu
# set the graphical limits that allow to best visualise the results for each parameters

limits = c(lim_b = 220, lim_mu = 214 , lim_la = 12, lim_b_mu = 1, lim_tau = 10)
dat_b03<- MakeboxplotData(limit=limits, b=0.3)
dat_b04<- MakeboxplotData(limit=limits, b=0.4)


limits = c(lim_b = 220, lim_mu = 50 , lim_la = 12, lim_b_mu = 1, lim_tau = 10)
dat_b05<- MakeboxplotData(limit=limits, b=0.5)
dat_b06<- MakeboxplotData(limit=limits, b=0.6)
dat_b07<- MakeboxplotData(limit=limits, b=0.7)


p_b03_mu<- MakeBoxPlot(lim = 214, b=0.3, data=dat_b03$mu, param="mu_accuracy", paramName = "Extinction rate (µ)", liminf= 2.5)
p_b04_mu<- MakeBoxPlot(lim = 214, b=0.4, data=dat_b04$mu, param="mu_accuracy", paramName = "Extinction rate (µ)", liminf= 2.5)
p_b05_mu<- MakeBoxPlot(lim = 50, b=0.5, data=dat_b05$mu, param="mu_accuracy", paramName = "Extinction rate (µ)", liminf= 2.5)
p_b06_mu<- MakeBoxPlot(lim = 50, b=0.6, data=dat_b06$mu, param="mu_accuracy", paramName = "Extinction rate (µ)", liminf= 2.5)
p_b07_mu<- MakeBoxPlot(lim = 50, b=0.7, data=dat_b07$mu, param="mu_accuracy", paramName = "Extinction rate (µ)", liminf= 2.5)

# Make plots for lambda


p_b03_la<- MakeBoxPlot(lim = 12, b=0.3, data=dat_b03$la, param="la_accuracy", paramName = "Speciation completion rate (lambda)", liminf= 2.5)
p_b04_la<- MakeBoxPlot(lim = 12, b=0.4, data=dat_b04$la, param="la_accuracy", paramName = "Speciation completion rate (lambda)", liminf= 2.5)
p_b05_la<- MakeBoxPlot(lim = 12, b=0.5, data=dat_b05$la, param="la_accuracy", paramName = "Speciation completion rate (lambda)", liminf= 2.5)
p_b06_la<- MakeBoxPlot(lim = 12, b=0.6, data=dat_b06$la, param="la_accuracy", paramName = "Speciation completion rate (lambda)", liminf= 2.5)
p_b07_la<- MakeBoxPlot(lim = 12, b=0.7, data=dat_b07$la, param="la_accuracy", paramName = "Speciation completion rate (lambda)", liminf= 2.5)

# Make plots for b-µ
p_b03_b_mu<- MakeBoxPlot(lim = 1, b=0.3, data=dat_b03$b_mu, param="b_mu_accuracy", paramName = "Net diversification rate (b-µ)", liminf= 1)
p_b04_b_mu<- MakeBoxPlot(lim = 1, b=0.4, data=dat_b04$b_mu, param="b_mu_accuracy", paramName = "Net diversification rate (b-µ)", liminf= 1)
p_b05_b_mu<- MakeBoxPlot(lim = 1, b=0.5, data=dat_b05$b_mu, param="b_mu_accuracy", paramName = "Net diversification rate (b-µ)", liminf= 1)
p_b06_b_mu<- MakeBoxPlot(lim = 1, b=0.6, data=dat_b06$b_mu, param="b_mu_accuracy", paramName = "Net diversification rate (b-µ)", liminf= 1)
p_b07_b_mu<- MakeBoxPlot(lim = 1, b=0.7, data=dat_b07$b_mu, param="b_mu_accuracy", paramName = "Net diversification rate (b-µ)", liminf= 1)

# Make plots for tau
p_b03_tau<- MakeBoxPlot(lim = 10, b=0.3, data=dat_b03$tau, param="durspMean_accuracy", paramName = "Mean duration of speciation (tau)", liminf= 10)
p_b04_tau<- MakeBoxPlot(lim = 10, b=0.4, data=dat_b04$tau, param="durspMean_accuracy", paramName = "Mean duration of speciation (tau)", liminf= 10)
p_b05_tau<- MakeBoxPlot(lim = 10, b=0.5, data=dat_b05$tau, param="durspMean_accuracy", paramName = "Mean duration of speciation (tau)", liminf= 10)
p_b06_tau<- MakeBoxPlot(lim = 10, b=0.6, data=dat_b06$tau, param="durspMean_accuracy", paramName = "Mean duration of speciation (tau)", liminf= 10)
p_b07_tau<- MakeBoxPlot(lim = 10, b=0.7, data=dat_b07$tau, param="durspMean_accuracy", paramName = "Mean duration of speciation (tau)", liminf= 10)



S1a<- list()
S1a[[1]]<- p_b03_b
S1a[[2]]<- p_b04_b
S1a[[3]]<- p_b05_b
S1a[[4]]<- p_b06_b
S1a[[5]]<- p_b07_b


setwd("C:/Users/CamilleAnna/Desktop/Supplements/")

for(i in 1:5)
{
  jpeg(paste('S1a_b0', i+2, '.jpg', sep=""), width = 500, height = 500)
  print(S1a[[i]])
  dev.off()
  
}



S1b<- list()
S1b[[1]]<- p_b03_mu
S1b[[2]]<- p_b04_mu
S1b[[3]]<- p_b05_mu
S1b[[4]]<- p_b06_mu
S1b[[5]]<- p_b07_mu


setwd("C:/Users/CamilleAnna/Desktop/Supplements/")

for(i in 1:5)
{
  jpeg(paste('S1b_b0', i+2, '.jpg', sep=""), width = 500, height = 500)
  print(S1b[[i]])
  dev.off()
  
}


S1c<- list()
S1c[[1]]<- p_b03_la
S1c[[2]]<- p_b04_la
S1c[[3]]<- p_b05_la
S1c[[4]]<- p_b06_la
S1c[[5]]<- p_b07_la


setwd("C:/Users/CamilleAnna/Desktop/Supplements/")

for(i in 1:5)
{
  jpeg(paste('S1c_b0', i+2, '.jpg', sep=""), width = 500, height = 500)
  print(S1c[[i]])
  dev.off()
  
}


S1d<- list()
S1d[[1]]<- p_b03_b_mu
S1d[[2]]<- p_b04_b_mu
S1d[[3]]<- p_b05_b_mu
S1d[[4]]<- p_b06_b_mu
S1d[[5]]<- p_b07_b_mu


setwd("C:/Users/CamilleAnna/Desktop/Supplements/")

for(i in 1:5)
{
  jpeg(paste('S1d_b0', i+2, '.jpg', sep=""), width = 500, height = 500)
  print(S1d[[i]])
  dev.off()
  
}


S1e<- list()
S1e[[1]]<- p_b03_tau
S1e[[2]]<- p_b04_tau
S1e[[3]]<- p_b05_tau
S1e[[4]]<- p_b06_tau
S1e[[5]]<- p_b07_tau


setwd("C:/Users/CamilleAnna/Desktop/Supplements/")

for(i in 1:5)
{
  jpeg(paste('S1e_b0', i+2, '.jpg', sep=""), width = 500, height = 500)
  print(S1e[[i]])
  dev.off()
  
}





# Figure S2 ----


setwd("C:/Users/Camille/Desktop/ms_review_Nov2017/ML_estimates/")

estimations_all<- vector("list")
index<- 1

bs = c(0.3,0.4,0.5,0.6,0.7)

for (j in 1:5)  # load all tables of estimates done on recontree (these tables have all informations with the number of tips on each tree (recontree, prunned and not prunned))
{
  b = bs[j]
  
  estimations<- vector("list")
  
  for (mu in c(0,0.1,0.2))
  {
    for (la in c(0.1,0.3,1))
    {
      estimations[[index]]<- read.table(paste("estimates_recontree", b, mu, la, sep="_"), header=TRUE)
      
      index= index+1
      
    }
    
  }
  
  estimations_all[[j]]<- do.call(rbind, estimations) # re assemble tables for all combination in 5 big tables, one for each value of b
  
}



# make histogram of the tree size for each value of b

p1<- ggplot(estimations_all[[1]], aes(x=Size_stree_oldest)) +
  geom_histogram(colour="black") +
  xlab("") + ylab("Counts\n")+ggtitle("b = 0.3")+
  theme_bw()+
  theme(panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", size = 16, hjust = 0.5))

p2<- ggplot(estimations_all[[2]], aes(x=Size_stree_oldest)) +
  geom_histogram(colour="black") +
  xlab("") + ylab("Counts\n")+ggtitle("b = 0.4")+
  theme_bw()+
  theme(panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", size = 16))

p3<- ggplot(estimations_all[[3]], aes(x=Size_stree_oldest)) +
  geom_histogram(colour="black") +
  xlab("") + ylab("Counts\n")+ggtitle("b = 0.5")+ xlim(0,10000)+
  theme_bw()+
  theme(panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", size = 16))

p4<- ggplot(estimations_all[[4]], aes(x=Size_stree_oldest)) +
  geom_histogram(colour="black") +
  xlab("") + ylab("Counts\n")+ggtitle("b = 0.6")+
  theme_bw()+
  theme(panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", size = 16))

p5<- ggplot(estimations_all[[5]], aes(x=Size_stree_oldest)) +
  geom_histogram(colour="black") +
  xlab("") + ylab("Counts\n")+ggtitle("b = 0.7")+ xlim(0,80000)+
  theme_bw()+
  theme(panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", size = 16))


grid.arrange(p1,p2,p3,p4,p5, ncol=3, nrow=2)

#setwd("C:/Users/CamilleAnna/Desktop/PBDproject/Manuscript/")

setwd("C:/Users/CamilleAnna/Desktop/Supplements/")
jpeg('S2.jpg', width = 1100, height = 700)
print(grid.arrange(p1,p2,p3,p4,p5, ncol=3, nrow=2))
dev.off()









# Figure S3 ----


setwd("C:/Users/CamilleAnna/Desktop/PBDproject/outputs/ML_estimates")


estimations_all<- vector("list")
index<- 1

bs = c(0.3,0.4,0.5,0.6,0.7)

for (j in 1:5)  # load all tables of estimates done on recontree (these tables have all informations with the number of tips on each tree (recontree, prunned and not prunned))
{
  b = bs[j]
  
  estimations<- vector("list")
  
  for (mu in c(0,0.1,0.2))
  {
    for (la in c(0.1,0.3,1))
    {
      estimations[[index]]<- read.table(paste("estimates_recontree", b, mu, la, sep="_"), header=TRUE)
      
      index= index+1
      
    }
    
  }
  
  estimations_all[[j]]<- do.call(rbind, estimations) # re assemble tables for all combination in 5 big tables, one for each value of b
  
}

# re-use the tales previously produced, add a column with the info on the b values (cenessary for plotting after)

estimations_all[[1]]$b<- rep(0.3, nrow(estimations_all[[1]]))
estimations_all[[2]]$b<- rep(0.4, nrow(estimations_all[[2]]))
estimations_all[[3]]$b<- rep(0.5, nrow(estimations_all[[3]]))
estimations_all[[4]]$b<- rep(0.6, nrow(estimations_all[[4]]))
estimations_all[[5]]$b<- rep(0.7, nrow(estimations_all[[5]]))

# add also a column with infor on wich parameter combination it is (out of the 45)
estimations_all[[1]]$comb<- sort(rep(seq(1,9,1),1000))
estimations_all[[2]]$comb<- sort(rep(seq(10,18,1),1000))
estimations_all[[3]]$comb<- sort(rep(seq(19,27,1),1000))
estimations_all[[4]]$comb<- sort(rep(seq(28,36,1),1000))
estimations_all[[5]]$comb<- c(rep(37,62), rep(38,96),rep(39,69),sort(rep(seq(40,45,1),1000))) # did not have 1000 simulations for all parameters combinations when b = 0.7, adapted the code


# Add column with information on the set ( = which combination of mu and lambda, within each value of b)
set<- c(rep("la=0.1 mu=0", 1000),rep("la=0.3 mu=0", 1000),rep("la= 1 mu=0", 1000),
        rep("la=0.1 mu=0.1", 1000),rep("la=0.3 mu=0.1", 1000),rep("la= 1 mu=0.1", 1000),
        rep("la=0.1 mu=0.2", 1000),rep("la=0.3 mu=0.2", 1000),rep("la= 1 mu=0.2", 1000))

set_b07<- c(rep("la=0.1 mu=0", 62),rep("la=0.3 mu=0", 96),rep("la= 1 mu=0", 69),
            rep("la=0.1 mu=0.1", 1000),rep("la=0.3 mu=0.1", 1000),rep("la= 1 mu=0.1", 1000),
            rep("la=0.1 mu=0.2", 1000),rep("la=0.3 mu=0.2", 1000),rep("la= 1 mu=0.2", 1000))

estimations_all[[1]]$set<- set
estimations_all[[2]]$set<- set
estimations_all[[3]]$set<- set
estimations_all[[4]]$set<- set
estimations_all[[5]]$set<- set_b07


TreeSizes<- do.call(rbind, estimations_all)[,c(1,8,14,15)]
TreeSizes$logTreeSize<- log(TreeSizes$Size_stree_oldest)

# get summary statistics necessary for boxplot with ggplot
dat<- ddply(TreeSizes,.(as.factor(comb)),
            transform,
            ymin = min(logTreeSize),
            ymax = max(logTreeSize),
            middle = median(logTreeSize),
            lower = quantile(logTreeSize,0.05),
            upper = quantile(logTreeSize,0.95))



cbPalette<- c("darkred", "darksalmon", "orangered", "orange1", "chartreuse3", "darkgreen", "seagreen", "steelblue4", "deepskyblue")

# produce the plot
p<- ggplot(dat, aes(x=as.factor(b), y=logTreeSize, fill=set))+
  geom_boxplot(aes(ymin = ymin, ymax =ymax,middle = middle,upper = upper,lower= lower), stat="identity")+
  scale_fill_manual(values = cbPalette, name="Parameter\ncombination")+
  ylab("Log species tree size\n") + xlab("\nSpeciation initiation rate (b)")+
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=15),
        axis.text.x = element_text(face="bold", colour="black", size=15),
        axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.title.x = element_text(face="bold", colour="black", size=16),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))

setwd("C:/Users/CamilleAnna/Desktop/Supplements/")

jpeg('S3.jpg', width = 1200, height = 800)
print(p)
dev.off()




  


# Figure S4 ----


source("C:/Users/CamilleAnna/Desktop/PBDproject/Manuscript/Functions.R")

setwd("C:/Users/CamilleAnna/Desktop/PBDproject/outputs/ML_estimates")

# set the graphical limits that allow to best visualise the results for each parameters
limits = c(lim_b = 50, lim_mu = 50, lim_la = 10, lim_b_mu = 1, lim_tau = 10)

# See the "function script". Getting all tables ready for plotting
dat_b03<- MakeboxplotData(limit=limits, b=0.3)
dat_b04<- MakeboxplotData(limit=limits, b=0.4)
dat_b05<- MakeboxplotData(limit=limits, b=0.5)
dat_b06<- MakeboxplotData(limit=limits, b=0.6)
dat_b07<- MakeboxplotData(limit=limits, b=0.7)


# I CHOSE TO MAKE THESE PLOTS WITH THE ESTIMATIONS MADE ON STREE OLDEST

setwd("C:/Users/CamilleAnna/Desktop/PBDproject/outputs/ML_estimates")

estimations_all<- vector("list")
index<- 1

bs = c(0.3,0.4,0.5,0.6,0.7) # load all tables of estimates on stree_oldest

for (j in 1:5)
{
  b = bs[j]
  
  estimations<- vector("list")
  
  for (mu in c(0,0.1,0.2))
  {
    for (la in c(0.1,0.3,1))
    {
      estimations[[index]]<- read.table(paste("estimates_stree_oldest", b, mu, la, sep="_"), header=TRUE)
      estimations[[index]]$b_simul <- rep(b, nrow(estimations[[index]]))   # add columns on information of the simulated parameter, necessary for plotting after
      estimations[[index]]$la_simul <- rep(la, nrow(estimations[[index]]))
      estimations[[index]]$mu_simul <- rep(mu, nrow(estimations[[index]]))
      
      
      index= index+1
      
    }
    
  }
  
  estimations_all[[j]]<- do.call(rbind, estimations) # get one big table per value of b
  
}


# combination of table a bit useless but it was to cross check that it was doing the right thing. Purpose is to obtain a table with the measure of accuracy of estimates (on stree_oldest) for each parameters, with the tree size (stree_oldest) and the simulation parameters associated with each value.
d_b03<- cbind(dat_b03$b[which(dat_b03$b$treeType=="stree_oldest"),], estimations_all[[1]][,c(1,8,14,15,16)])
d_b04<- cbind(dat_b04$b[which(dat_b04$b$treeType=="stree_oldest"),], estimations_all[[2]][,c(1,8,14,15,16)])

d_b05<- cbind(dat_b05$b[which(dat_b05$b$treeType=="stree_oldest"),], estimations_all[[3]][,c(1,8,14,15,16)])

d_b06<- cbind(dat_b06$b[which(dat_b06$b$treeType=="stree_oldest"),], estimations_all[[4]][,c(1,8,14,15,16)])

d_b07<- cbind(dat_b07$b[which(dat_b07$b$treeType=="stree_oldest"),], estimations_all[[5]][which(estimations_all[[5]]$mu_simul != 0),c(1,8,14,15,16)])


dat1<- rbind(d_b03,d_b04, d_b05, d_b06, d_b07)
dat1 <- dat1[,c(1,2,3,4,5,7,8,9,10,11,12,21)]


dat<- do.call(rbind, estimations_all)
dat<- dat[,c(1,2,3,4,5,8,14,15,16)]
colnames(dat)<- c("b_estim", "la_estim", "mu_estim", "b_mu_estim", "durspMean_estim", "Size_stree_oldest", "b", "la", "mu")
dat<- dat[-which(dat$b==0.7 & dat$mu==0),]

dat<- cbind(dat,dat1)
dat<- dat[,c(1:9,14,20)]


#Make the plots
p0<- ggplot(dat, aes(x=log(Size_stree_oldest), y=log(b_estim)-log(b), colour=as.factor(b)))+
  geom_point(size=0.6)+ theme(legend.position="none")+
  labs(x="Log Species tree size", y = "Log (estimated b) - Log(simulated b)", title= "b")+
  scale_color_discrete(name ="Speciation initiation\n          rate (b)")+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme(axis.title=element_text(size=15))+
  
  theme_bw()+
  theme(panel.border= element_blank(),
        axis.text.y = element_text( colour="black", size=10),
        axis.text.x = element_text(face="bold", colour="black", size=10),
        axis.title.y = element_text(colour="black", size=11),
        axis.title.x = element_text(colour="black", size=11),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8))

leg<- g_legend(p0)


p1<- ggplot(dat, aes(x=log(Size_stree_oldest), y=log(b_estim)-log(b), colour=as.factor(b)))+
  geom_point(size=2)+ theme(legend.position="none")+
  #labs(x="Log Species tree size", y = "Log (estimated b) - Log(simulated b)", title= "b")+
  scale_color_discrete(name ="Speciation initiation\n          rate (b)")+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_bw()+
  labs(x="", y ="", title="")+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text( face="bold", colour="black", size = 15),
        axis.text.x = element_text(face="bold", colour="black", size = 15),
        axis.title.y = element_text(colour="black", size = 9),
        axis.title.x = element_text(colour="black", size = 9),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5)#,
        #plot.title = element_text(lineheight=.8)
  )

p2<- ggplot(dat, aes(x=log(Size_stree_oldest), y=log(mu_estim+1)-log(mu+1), colour=as.factor(b)))+
  geom_point(size=2)+ theme(legend.position="none")+
  #labs(x="Log Species tree size", y = "Log (estimated µ+1) - Log(simulated µ+1)", title= "µ")+
  scale_color_discrete(name ="Speciation initiation\n          rate (b)")+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  ylim(-10, 15)+
  theme_bw()+
  labs(x="", y ="", title="")+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text( face="bold", colour="black", size = 15),
        axis.text.x = element_text(face="bold", colour="black", size = 15),
        axis.title.y = element_text(colour="black", size = 9),
        axis.title.x = element_text(colour="black", size = 9),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5)#,
        #plot.title = element_text(lineheight=.8)
  )


p3<- ggplot(dat, aes(x=log(Size_stree_oldest), y=log(la_estim)-log(la), colour=as.factor(b)))+
  geom_point(size=2)+ theme(legend.position="none")+
  scale_color_discrete(name ="Speciation initiation\n          rate (b)")+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_bw()+
  labs(x="", y ="", title="")+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text( face="bold", colour="black", size = 15),
        axis.text.x = element_text(face="bold", colour="black", size = 15),
        axis.title.y = element_text(colour="black", size = 9),
        axis.title.x = element_text(colour="black", size = 9),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5)#,
        #plot.title = element_text(lineheight=.8)
  )


dat$b_mu<- dat$b - dat$mu

p4<- ggplot(dat, aes(x=log(Size_stree_oldest), y=log(b_mu_estim)-log(b_mu), colour=as.factor(b)))+
  geom_point(size=2)+ theme(legend.position="none")+
 # labs(x="Log Species tree size", y = "Log (estimated b-µ) - Log(simulated b-µ)", title= "b - µ")+
  scale_color_discrete(name ="Speciation initiation\n          rate (b)")+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  ylim(-10, 5)+
  theme_bw()+
  labs(x="", y ="", title="")+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text( face="bold", colour="black", size = 15),
        axis.text.x = element_text(face="bold", colour="black", size = 15),
        axis.title.y = element_text(colour="black", size = 9),
        axis.title.x = element_text(colour="black", size = 9),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5)#,
        #plot.title = element_text(lineheight=.8)
  )


p5<- ggplot(dat, aes(x=log(Size_stree_oldest), y=log(durspMean_estim)-log(durspMean), colour=as.factor(b)))+
  geom_point(size=2)+ theme(legend.position="none")+
  scale_color_discrete(name ="Speciation initiation\n          rate (b)")+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_bw()+
  labs(x="", y ="", title="")+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text( face="bold", colour="black", size = 15),
        axis.text.x = element_text(face="bold", colour="black", size = 15),
        axis.title.y = element_text(colour="black", size = 9),
        axis.title.x = element_text(colour="black", size = 9),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5)#,
        #plot.title = element_text(lineheight=.8)
        )


setwd("C:/Users/CamilleAnna/Desktop/Supplements/")

jpeg('S4_b.jpg', width = 500, height = 500)
p1
dev.off()

jpeg('S4_mu.jpg', width = 500, height = 500)
p2
dev.off()

jpeg('S4_la.jpg', width = 500, height = 500)
p3
dev.off()

jpeg('S4_b_mu.jpg', width = 500, height = 500)
p4
dev.off()

jpeg('S4_tau.jpg', width = 500, height = 500)
p5
dev.off()


# Figure S5a ----



setwd("C:/Users/CamilleAnna/Desktop/PBDproject/outputs/ML_estimates")


quant<- function(x){quantile(x, p=0.5)} # chose to do this plot with the MEDIAN and not the 95th percentile because it was best representing the statements in the discussion about the variable effect of b depending on the la and mu combinations

LMEs<- vector("list")      # a list to store the tables of SUMMARIES of LME distances
Samplings<- vector("list") # a list to store the tables of SUMMARIES sampling effects measures

index<- 1
bs<- seq(0.3,0.7,0.1)

for(j in 1:5)
{
  b = bs[j]
  
  pars.ls<- vector("list")  #a list to store simulation parameters
  LME.ls<- vector("list")   #a list to store tables of LME distances
  Sampling.ls<- vector("list") #a list to store sampling effect distances
  
  for (mu in c(0,0.1,0.2)) # Load all the tables
  {
    for (la in c(0.1,0.3,1))
    {
      pars.ls[[index]]<- read.table(paste("Simulation_parameters", b, mu, la, sep="_"), header=TRUE)
      LME.ls[[index]]<- read.table(paste("LME_effect", b, mu, la, sep="_"), header=TRUE)
      Sampling.ls[[index]]<- read.table(paste("Sampling_effect", b, mu, la, sep="_"), header=TRUE)
      
      index= index+1
      
    }
    
  }
  
  # reassemble all tables
  pars<- do.call(rbind, pars.ls)
  LME<- do.call(rbind, LME.ls)
  Sampling<- do.call(rbind, Sampling.ls)
  colnames(LME)<- c("b_LME", "la_LME", "mu_LME", "b_mu_LME", "durspMean_LME", "durspMedian_LME")
  colnames(Sampling)<- c("b_Sampling", "la_Sampling", "mu_Sampling", "b_mu_Sampling", "durspMean_Sampling", "durspMedian_Sampling")
  
  # count the number of simulations obtained for each parameters combinations
  counts<- ddply(pars,.(b,la,mu,b_mu,durspMean,durspMedian),nrow)[7]
  # add this infor to the table
  comb<- c(rep(1, counts[1,]), rep(2, counts[2,]),rep(3, counts[3,]),
           rep(4, counts[4,]),rep(5, counts[5,]),rep(6, counts[6,]),
           rep(7, counts[7,]),rep(8, counts[8,]),rep(9, counts[9,]))
  
  
  # for each value of b, store a table with: the 45 parameters combination and the median of the LME istancs for the corresponding combination (computed with dply)
  LMEs[[j]]<- cbind(unique(pars), ddply(cbind(LME, comb), .(comb), colwise(quant))[,-1] )
  Samplings[[j]]<- cbind(unique(pars), ddply(cbind(Sampling, comb), .(comb), colwise(quant))[,-1] )
  
}

# re assemble these tables
LME_all<- do.call(rbind, LMEs)
Sampling_all<- do.call(rbind, Samplings)

# remove combinations with b = 0.7 and mu = 0 because I have only few data points for these ones
LME_all<- LME_all[-which(LME_all$b==0.7 & LME_all$mu==0),]
Sampling_all<- Sampling_all[-which(Sampling_all$b==0.7 & Sampling_all$mu==0),]

# Make the plots

# For Parameter b-µ

cbPalette<- c("darkred", "darkorange2", "dodgerblue3" )

p1<- ggplot(LME_all[which(LME_all$la==0.1),], aes(x=b, y=b_mu_LME, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


p2<- ggplot(LME_all[which(LME_all$la==0.3),], aes(x=b, y=b_mu_LME, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
 ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, size = 17))

p3<- ggplot(LME_all[which(LME_all$la==1),], aes(x=b, y=b_mu_LME, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


# For Parameter lambda


p4<-ggplot(LME_all[which(LME_all$la==0.1),], aes(x=b, y=la_LME, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


p5<- ggplot(LME_all[which(LME_all$la==0.3),], aes(x=b, y=la_LME, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, size = 17))

p6<- ggplot(LME_all[which(LME_all$la==1),], aes(x=b, y=la_LME, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


# For Parameter tau


p7<- ggplot(LME_all[which(LME_all$la==0.1),], aes(x=b, y=durspMean_LME, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab(" ")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


p8<- ggplot(LME_all[which(LME_all$la==0.3),], aes(x=b, y=durspMean_LME, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  theme(axis.text=element_text(size=12))+
  theme(axis.title=element_text(size=15))+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, size = 17))

p9<- ggplot(LME_all[which(LME_all$la==1),], aes(x=b, y=durspMean_LME, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  theme(axis.text=element_text(size=12))+
  theme(axis.title=element_text(size=15))+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


library(gridExtra)

setwd("C:/Users/CamilleAnna/Desktop/Supplements")

jpeg('S5_a_1.jpg', width = 300, height = 280)
p1
dev.off()

jpeg('S5_a_2.jpg', width = 300, height = 280)
p2
dev.off()

jpeg('S5_a_3.jpg', width = 300, height = 280)
p3
dev.off()

jpeg('S5_a_4.jpg', width = 300, height = 280)
p4
dev.off()

jpeg('S5_a_5.jpg', width = 300, height = 280)
p5
dev.off()

jpeg('S5_a_6.jpg', width = 300, height = 280)
p6
dev.off()

jpeg('S5_a_7.jpg', width = 300, height = 280)
p7
dev.off()

jpeg('S5_a_8.jpg', width = 300, height = 280)
p8
dev.off()


jpeg('S5_a_9.jpg', width = 300, height = 280)
p9
dev.off()



# Figure 5b ---


setwd("C:/Users/CamilleAnna/Desktop/PBDproject/outputs/ML_estimates")

# For Parameter b-µ

cbPalette<- c("darkred", "darkorange2", "dodgerblue3" )

p10<- ggplot(Sampling_all[which(Sampling_all$la==0.1),], aes(x=b, y=b_mu_Sampling, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


p11<- ggplot(Sampling_all[which(Sampling_all$la==0.3),], aes(x=b, y=b_mu_Sampling, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, size = 17))

p12<- ggplot(Sampling_all[which(Sampling_all$la==1),], aes(x=b, y=b_mu_Sampling, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


# For Parameter lambda


p13<- ggplot(Sampling_all[which(Sampling_all$la==0.1),], aes(x=b, y=la_Sampling, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
 ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


p14<- ggplot(Sampling_all[which(Sampling_all$la==0.3),], aes(x=b, y=la_Sampling, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, size = 17))

p15<- ggplot(Sampling_all[which(Sampling_all$la==1),], aes(x=b, y=la_Sampling, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


# For Parameter tau


p16<- ggplot(Sampling_all[which(Sampling_all$la==0.1),], aes(x=b, y=durspMean_Sampling, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab(" ")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))


p17<- ggplot(Sampling_all[which(Sampling_all$la==0.3),], aes(x=b, y=durspMean_Sampling, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
 ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, size = 17))

p18<- ggplot(Sampling_all[which(Sampling_all$la==1),], aes(x=b, y=durspMean_Sampling, group=mu, color=as.factor(mu))) +
  geom_line(size=1) +
  geom_point(size=2)+
  xlab("")+
  ylab("")+
  ggtitle(" ")+
  scale_colour_manual(values=cbPalette, name="Extinction rate (µ)")+
  theme_bw()+
  theme(legend.position="none",
        panel.border= element_blank(),
        axis.text.y = element_text(face="bold", colour="black", size=13),
        axis.text.x = element_text(face="bold", colour="black", size=13),
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold"))




setwd("C:/Users/CamilleAnna/Desktop/Supplements")

jpeg('S5_b_1.jpg', width = 300, height = 280)
p10
dev.off()

jpeg('S5_b_2.jpg', width = 300, height = 280)
p11
dev.off()

jpeg('S5_b_3.jpg', width = 300, height = 280)
p12
dev.off()

jpeg('S5_b_4.jpg', width = 300, height = 280)
p13
dev.off()

jpeg('S5_b_5.jpg', width = 300, height = 280)
p14
dev.off()

jpeg('S5_b_6.jpg', width = 300, height = 280)
p15
dev.off()

jpeg('S5_b_7.jpg', width = 300, height = 280)
p16
dev.off()

jpeg('S5_b_8.jpg', width = 300, height = 280)
p17
dev.off()


jpeg('S5_b_9.jpg', width = 300, height = 280)
p18
dev.off()


# Essai ---- 


head(Sampling_all)

nrow(Sampling_all)

Sampling_all$Effect<- rep("Sampling", nrow(Sampling_all))
LME_all$Effect<- rep("LME", nrow(LME_all))

colnames(LME_all)<- c("b", "la", "mu", "b_mu", "durspMean", "durspMedian", "b_deviation", "la_deviation", "mu_deviation", "b_mu_deviation", "durspMean_deviation", "durspMedian_deviation", "Effect")
colnames(Sampling_all)<- c("b", "la", "mu", "b_mu", "durspMean", "durspMedian", "b_deviation", "la_deviation", "mu_deviation", "b_mu_deviation", "durspMean_deviation", "durspMedian_deviation", "Effect")

Effects<- rbind(Sampling_all, LME_all)



TestPlot_la<- function(la, mu)
{
  
  ggplot(Effects[which(Effects$la==la & Effects$mu==mu),], aes(x=b, y=la_deviation, group=Effect, color=Effect)) +
    geom_line(size=1) +
    geom_point(size=2)+
    xlab("")+
    ylab("")+
    #ggtitle(paste("lambda estimates: la = ",la, " mu = ", mu, sep=""))+
    scale_colour_manual(values=c("black", "grey"), name="Effect")+
    theme_bw()+
    theme(legend.position="none",
      panel.border= element_blank(),
      axis.text.y = element_text(face="bold", colour="black", size=13),
      axis.text.x = element_text(face="bold", colour="black", size=13),
      axis.title.y = element_text(face="bold", colour="black", size=14),
      axis.title.x = element_text(face="bold", colour="black", size=14),
      axis.line.y = element_line(color="black", size = 0.5),
      axis.line.x = element_line(color="black", size = 0.5),
      plot.title = element_text(lineheight=.8, face="bold"))
  
  
  
}

TestPlot_b_mu<- function(la, mu)
{
  
  ggplot(Effects[which(Effects$la==la & Effects$mu==mu),], aes(x=b, y=b_mu_deviation, group=Effect, color=Effect)) +
    geom_line(size=1) +
    geom_point(size=2)+
    xlab("")+
    ylab("")+
    #ggtitle(paste("b-mu estimates: la = ",la, " mu = ", mu, sep=""))+
    scale_colour_manual(values=c("black", "grey"), name="Effect")+
    theme_bw()+
    theme(legend.position="none",
      panel.border= element_blank(),
      axis.text.y = element_text(face="bold", colour="black", size=13),
      axis.text.x = element_text(face="bold", colour="black", size=13),
      axis.title.y = element_text(face="bold", colour="black", size=14),
      axis.title.x = element_text(face="bold", colour="black", size=14),
      axis.line.y = element_line(color="black", size = 0.5),
      axis.line.x = element_line(color="black", size = 0.5),
      plot.title = element_text(lineheight=.8, face="bold"))
  
  
  
}

TestPlot_tau<- function(la, mu)
{
  
  ggplot(Effects[which(Effects$la==la & Effects$mu==mu),], aes(x=b, y=durspMean_deviation, group=Effect, color=Effect)) +
    geom_line(size=1) +
    geom_point(size=2)+
    xlab("")+
    ylab("")+
    #ggtitle(paste("Tau estimates: la = ",la, " mu = ", mu, sep=""))+
    scale_colour_manual(values=c("black", "grey"), name="Effect")+
    theme_bw()+
    theme(legend.position="none",
      panel.border= element_blank(),
      axis.text.y = element_text(face="bold", colour="black", size=13),
      axis.text.x = element_text(face="bold", colour="black", size=13),
      axis.title.y = element_text(face="bold", colour="black", size=14),
      axis.title.x = element_text(face="bold", colour="black", size=14),
      axis.line.y = element_line(color="black", size = 0.5),
      axis.line.x = element_line(color="black", size = 0.5),
      plot.title = element_text(lineheight=.8, face="bold"))
  
  
  
}



k = 1

Plots_la<- list()

for (i in c(0.1,0.3,1))
{
  for (j in c(0,0.1,0.2))
  {    
    Plots_la[[k]]<- TestPlot_la(i,j)
    k = k+1
  }

}


grid.arrange(Plots_la[[1]],Plots_la[[2]],Plots_la[[3]],
             Plots_la[[4]],Plots_la[[5]],Plots_la[[6]],
             Plots_la[[7]],Plots_la[[8]],Plots_la[[9]],
             nrow=3, ncol=3)



setwd("C:/Users/CamilleAnna/Desktop/Supplements")

pdf('S5_c_1_bis.pdf', width = 5, height = 5)
Plots_la[[1]]
dev.off()

pdf('S5_c_2_bis.pdf', width = 5, height = 5)
Plots_la[[2]]
dev.off()

pdf('S5_c_3_bis.pdf', width = 5, height = 5)
Plots_la[[3]]
dev.off()

pdf('S5_c_4_bis.pdf', width = 5, height = 5)
Plots_la[[4]]
dev.off()

pdf('S5_c_5_bis.pdf', width = 5, height = 5)
Plots_la[[5]]
dev.off()

pdf('S5_c_6_bis.pdf', width = 5, height = 5)
Plots_la[[6]]
dev.off()

pdf('S5_c_7_bis.pdf', width = 5, height = 5)
Plots_la[[7]]
dev.off()

pdf('S5_c_8_bis.pdf', width = 5, height = 5)
Plots_la[[8]]
dev.off()


pdf('S5_c_9_bis.pdf', width = 5, height = 5)
Plots_la[[9]]
dev.off()






k = 1

Plots_b_mu<- list()

for (i in c(0.1,0.3,1))
{
  for (j in c(0,0.1,0.2))
  {    
    Plots_b_mu[[k]]<- TestPlot_b_mu(i,j)
    k = k+1
  }
  
}


grid.arrange(Plots_b_mu[[1]],Plots_b_mu[[2]],Plots_b_mu[[3]],
             Plots_b_mu[[4]],Plots_b_mu[[5]],Plots_b_mu[[6]],
             Plots_b_mu[[7]],Plots_b_mu[[8]],Plots_b_mu[[9]],
             nrow=3, ncol=3)


setwd("C:/Users/CamilleAnna/Desktop/Supplements")

pdf('S5_b_1_bis.pdf', width = 5, height = 5)
Plots_b_mu[[1]]
dev.off()

pdf('S5_b_2_bis.pdf', width = 5, height = 5)
Plots_b_mu[[2]]
dev.off()

pdf('S5_b_3_bis.pdf', width = 5, height = 5)
Plots_b_mu[[3]]
dev.off()

pdf('S5_b_4_bis.pdf', width = 5, height = 5)
Plots_b_mu[[4]]
dev.off()

pdf('S5_b_5_bis.pdf', width = 5, height = 5)
Plots_b_mu[[5]]
dev.off()

pdf('S5_b_6_bis.pdf', width = 5, height = 5)
Plots_b_mu[[6]]
dev.off()

pdf('S5_b_7_bis.pdf', width = 5, height = 5)
Plots_b_mu[[7]]
dev.off()

pdf('S5_b_8_bis.pdf', width = 5, height = 5)
Plots_b_mu[[8]]
dev.off()


pdf('S5_b_9_bis.pdf', width = 5, height = 5)
Plots_b_mu[[9]]
dev.off()











k = 1

Plots_tau<- list()

for (i in c(0.1,0.3,1))
{
  for (j in c(0,0.1,0.2))
  {    
    Plots_tau[[k]]<- TestPlot_tau(i,j)
    k = k+1
  }
  
}


grid.arrange(Plots_tau[[1]],Plots_tau[[2]],Plots_tau[[3]],
             Plots_tau[[4]],Plots_tau[[5]],Plots_tau[[6]],
             Plots_tau[[7]],Plots_tau[[8]],Plots_tau[[9]],
             nrow=3, ncol=3)



setwd("C:/Users/CamilleAnna/Desktop/Supplements")

pdf('S5_a_1_bis.pdf', width = 5, height = 5)
Plots_tau[[1]]
dev.off()

pdf('S5_a_2_bis.pdf', width = 5, height = 5)
Plots_tau[[2]]
dev.off()

pdf('S5_a_3_bis.pdf', width = 5, height = 5)
Plots_tau[[3]]
dev.off()

pdf('S5_a_4_bis.pdf', width = 5, height = 5)
Plots_tau[[4]]
dev.off()

pdf('S5_a_5_bis.pdf', width = 5, height = 5)
Plots_tau[[5]]
dev.off()

pdf('S5_a_6_bis.pdf', width = 5, height = 5)
Plots_tau[[6]]
dev.off()

pdf('S5_a_7_bis.pdf', width = 5, height = 5)
Plots_tau[[7]]
dev.off()

pdf('S5_a_8_bis.pdf', width = 5, height = 5)
Plots_tau[[8]]
dev.off()


pdf('S5_a_9_bis.pdf', width = 5, height = 5)
Plots_tau[[9]]
dev.off()


# Additional: tree sizes before and after sampling ----


for (i in 1:5)
{

nr<- nrow(estimations_all[[i]])
  
d<- data.frame( treeSize = c(estimations_all[[i]][,"Size_recontree"], estimations_all[[i]][,"Size_full_tree"], estimations_all[[i]][,"Size_stree_oldest"]),
                treeType = c(rep("Approximate",nr), rep("Full", nr), rep("Sampled", nr)),
                set = c(estimations_all[[i]]$set, estimations_all[[i]]$set, estimations_all[[i]]$set)
)
  
d$treeType <- factor(d$treeType, levels = c("Full", "Sampled", "Approximate"))


setwd("C:/Users/Camille/Desktop/ms_review_Nov2017/TreeSizes_FSA")

p<- ggplot(d, aes(x=set, y=log(treeSize), fill=treeType))+
  geom_boxplot()+
  scale_fill_manual(values = c("white", "grey50", "grey35"), name="Tree type")+
  expand_limits(y=c(2,12))+
  scale_y_continuous(breaks = c(3,6,9,12))+
  Mytheme

pdf(paste("TreeSize_FSA_", i, ".pdf", sep=""), width=6, height=6)
print(p)
dev.off()

}

  





