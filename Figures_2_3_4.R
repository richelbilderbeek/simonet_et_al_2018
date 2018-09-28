
# #####################################################
#    Robustness of the Approximate Likelihood of the  #
#           Protracted Speciation Model               #
# ----------------------------------------------------#
# Code for production of Figure 1                     #
# Each heatmap is produced and saved in independent   #
# files, that were then complied together with Adobe  #
# Illustrator                                         #
# --------------------------------------------------- #
#          Last date modified: 04/11/2017             #
# Camille Simonet - MEME student, cohort 2015-2017    #
#######################################################

Mytheme<- theme_bw()+
          theme(legend.title=element_blank(),
                legend.text=element_text(size = 21),
                legend.key.width=unit(1,"cm"),
                legend.key.height = unit(1.5, "cm"),
                panel.border= element_blank(),
                axis.text.y = element_text(face="bold", colour="black", size = 28),
                axis.text.x = element_text(face="bold", colour="black", size = 28),
                axis.title.y = element_text(face="bold", colour="black", size = 28),
                axis.title.x = element_text(face="bold", colour="black", size = 28),
                axis.line.y = element_line(color="white", size = 0.5),
                axis.line.x = element_line(color="white", size = 0.5),
                plot.title = element_text(lineheight=.8, face="bold", size = 32, hjust = 0.5))



# Getting the data and producing figures, saved in lists

setwd("C:/Users/Camille/Desktop/PBD_Nov2017_Review/ML_estimates")

heatmaps_LME_b_mu<- vector("list")   # list to store plots of LME effect, parameter b-µ
heatmaps_Sampling_b_mu<- vector("list") # list to store plots of sampling effect, parameter b-µ effect, for parameter b-µ

heatmaps_LME_tau<- vector("list")  # list to store plots of LME effect, parameter tau
heatmaps_Sampling_tau<- vector("list") # list to store plots of sampling effect, parameter b-µ effect, for parameter tau

heatmaps_LME_la<- vector("list") # list to store plots of LME effect, parameter lambda
heatmaps_Sampling_la<- vector("list") # list to store plots of sampling effect, parameter b-µ effect, for parameter lambda


quant<- function(x){quantile(x, p=0.95)} # function used in dply to extract the 95th quantile

cols <- c("midnightblue","darkslateblue","lightskyblue",
           "darkgreen", "green", "yellow", "orange", "red", "firebrick", "darkred" )

cols2 <- c("midnightblue","darkslateblue","lightskyblue",
           "orange", "red", "firebrick", "darkred" )

index<- 1
plotindex<- 1

# library(plyr)
# library(dplyr)
# library(ggplot2)
# library("grid")
# library(gridExtra)

inspect<- NULL

for(b in seq(0.3,0.8,0.1))
{
  
  pars.ls<- vector("list")   # list to store the tables of simulation parameters
  LME.ls<- vector("list")    # list to store the tables of LME effect
  Sampling.ls<- vector("list") # list to store the tables of sampling effect
  
  title_LME = "LME approximation effect" # for title of the plots
  title_Sampling = "Random sampling effect"
  
  for (mu in c(0,0.1,0.2)) # load all the tables
  {
    for (la in c(0.1,0.3,1))
    {
      pars.ls[[index]]<- read.table(paste("Simulation_parameters", b, mu, la, sep="_"), header=TRUE)
      LME.ls[[index]]<- read.table(paste("LME_effect", b, mu, la, sep="_"), header=TRUE)
      Sampling.ls[[index]]<- read.table(paste("Sampling_effect", b, mu, la, sep="_"), header=TRUE)
      
      index= index+1
      
      
      
    }
    
  }
  
  pars<- do.call(rbind, pars.ls) # re-assemble all tables
  LME<- do.call(rbind, LME.ls)
  Sampling<- do.call(rbind, Sampling.ls)
  colnames(LME)<- c("b_LME", "la_LME", "mu_LME", "b_mu_LME", "durspMean_LME", "durspMedian_LME")
  colnames(Sampling)<- c("b_Sampling", "la_Sampling", "mu_Sampling", "b_mu_Sampling", "durspMean_Sampling", "durspMedian_Sampling")
  
  
  counts<- ddply(pars,.(b,mu,la,b_mu,durspMean,durspMedian),nrow)[7] # count the number of simulation obtained for each parameter combination
  comb<- c(rep(1, counts[1,]), rep(2, counts[2,]),rep(3, counts[3,]),
           rep(4, counts[4,]),rep(5, counts[5,]),rep(6, counts[6,]),
           rep(7, counts[7,]),rep(8, counts[8,]),rep(9, counts[9,]))
  
  
  # assemble
  # i) the table of simulation parameters (collapse to unique combinations), and a summary table of the LME effect with the 95th percentile per parameter combination
  # ii) the same for the sampling effect
  
  LME_95th<- cbind(unique(pars), ddply(cbind(LME, comb), .(comb), colwise(quant))[,-1] )
  Sampling_95th<- cbind(unique(pars), ddply(cbind(Sampling, comb), .(comb), colwise(quant))[,-1] )
  
  
  # For manual inspection to prepare plots
  inspect<- rbind(inspect, 
                  data.frame(LME_95th[,c(1,2,3)], 
                             b_mu_LME = LME_95th[,8],
                             b_mu_Sampling = Sampling_95th[,8]))
  
  
  # Add to LME_95th and Sampling_95th, a constant bunch of values to set the scale for the heatmap
  
  dummy<- data.frame(  b = rep(b, 2),
                       la = rep(2, 2),
                       mu = c(0.4,0.5),
                       b_mu =  c(0.4,0.5),
                       durspMean = c(0.4,0.5),
                       durspMedian = c(0.4,0.5),
                       b_LME = c(0.4,0.5),
                       la_LME = c(0,2),
                       mu_LME =  c(0.4,0.5),
                       b_mu_LME = c(0,0.3),
                       durspMean_LME = c(0,5.5),
                       durspMedian_LME = c(0.4,0.5))

   LME_95th<- rbind(LME_95th, dummy)
   
   dummy_Sampling<- data.frame(  b = rep(b, 2),
                        la = rep(2, 2),
                        mu = c(0.4,0.5),
                        b_mu =  c(0.4,0.5),
                        durspMean = c(0.4,0.5),
                        durspMedian = c(0.4,0.5),
                        b_Sampling = c(0.4,0.5),
                        la_Sampling = c(0,2),
                        mu_Sampling =  c(0.4,0.5),
                        b_mu_Sampling = c(0,0.3),
                        durspMean_Sampling = c(0,5.5),
                        durspMedian_Sampling = c(0.4,0.5))
   
   Sampling_95th<- rbind(Sampling_95th, dummy_Sampling)
  
   
  # Transform data to map to color scale
   
  LME_95th$b_mu_LME_transf<-  LME_95th$b_mu_LME/(1+LME_95th$b_mu_LME)
  Sampling_95th$b_mu_Sampling_transf<-  Sampling_95th$b_mu_Sampling/(1+Sampling_95th$b_mu_Sampling)
  
  LME_95th$durspMean_LME_transf<-  LME_95th$durspMean_LME/(1+LME_95th$durspMean_LME)
  Sampling_95th$durspMean_Sampling_transf<-  Sampling_95th$durspMean_Sampling/(1+Sampling_95th$durspMean_Sampling)
  

  #max(inspect$b_mu_LME/(1+inspect$b_mu_LME))
  
  # do the plots and store them in lists
  
  if(plotindex > 1) 
  {
    title_LME = " " # give a title only for the plot of first value of b (for final layout)
  }
  
  theme_set(theme_gray(base_size = 5)) 
  
  heatmaps_LME_b_mu[[plotindex]]<- ggplot(data = LME_95th, aes(x=as.factor(mu), y=as.factor(la), fill=b_mu_LME)) + 
    geom_tile()+xlab("µ")+ylab(expression(lambda))+
    xlim(c("0", "0.1", "0.2")) + ylim(c("0.1", "0.3", "1"))+
    scale_fill_gradientn(colours=cols2, limits=c(0,0.3), breaks=c(0,0.1,0.2,0.3))+
    Mytheme
  


  heatmaps_LME_tau[[plotindex]]<- ggplot(data = LME_95th, aes(x=as.factor(mu), y=as.factor(la), fill=durspMean_LME)) + 
    geom_tile()+xlab("µ")+ylab(expression(lambda))+
    xlim(c("0", "0.1", "0.2")) + ylim(c("0.1", "0.3", "1"))+
    scale_fill_gradientn(colours=cols2, limits=c(0,5.6), breaks=c(0,1,2,3,4,5))+
    Mytheme
  
  heatmaps_LME_la[[plotindex]]<- ggplot(data = LME_95th, aes(x=as.factor(mu), y=as.factor(la), fill=la_LME)) + 
    geom_tile()+xlab("µ")+ylab(expression(lambda))+
    xlim(c("0", "0.1", "0.2")) + ylim(c("0.1", "0.3", "1"))+
    scale_fill_gradientn(colours=cols2, limits=c(0,2.04), breaks=c(0,1,2))+
    Mytheme
  
  
  # do the same for the sampling effects
  
  
  if(plotindex > 1)  
  {
    title_Sampling = " "
  }
  
  theme_set(theme_gray(base_size = 5))
  
  
  heatmaps_Sampling_b_mu[[plotindex]]<- ggplot(data = Sampling_95th, aes(x=as.factor(mu), y=as.factor(la), fill=b_mu_Sampling)) + 
    geom_tile()+xlab("µ")+ylab(expression(lambda))+
    xlim(c("0", "0.1", "0.2")) + ylim(c("0.1", "0.3", "1"))+
    scale_fill_gradientn(colours=cols2, limits=c(0,0.3), breaks=c(0,0.1,0.2,0.3))+
    Mytheme
  

  heatmaps_Sampling_tau[[plotindex]]<- ggplot(data = Sampling_95th, aes(x=as.factor(mu), y=as.factor(la), fill=durspMean_Sampling)) + 
    geom_tile()+xlab("µ")+ylab(expression(lambda))+
    xlim(c("0", "0.1", "0.2")) + ylim(c("0.1", "0.3", "1"))+
    scale_fill_gradientn(colours=cols2, limits=c(0,5.6), breaks=c(0,1,2,3,4,5))+
    Mytheme

  
  heatmaps_Sampling_la[[plotindex]]<- ggplot(data = Sampling_95th, aes(x=as.factor(mu), y=as.factor(la), fill=la_Sampling)) + 
    geom_tile()+xlab("µ")+ylab(expression(lambda))+
    xlim(c("0", "0.1", "0.2")) + ylim(c("0.1", "0.3", "1"))+
    scale_fill_gradientn(colours=cols2, limits=c(0,2.04), breaks=c(0,1,2))+
    Mytheme
  
  
  plotindex = plotindex+1
  
  
}





# Saving the files
  setwd("C:/Users/Camille/Desktop/PBD_Nov2017_Review")


# Net diversification rate (b-µ)

# LME


pdf("heatmaps_LME_b_mu_1.pdf", width=10, height=5)
plot(heatmaps_LME_b_mu[[1]])
dev.off()
pdf("heatmaps_LME_b_mu_2.pdf", width=10, height=5)
plot(heatmaps_LME_b_mu[[2]])
dev.off()
pdf("heatmaps_LME_b_mu_3.pdf", width=10, height=5)
plot(heatmaps_LME_b_mu[[3]])
dev.off()
pdf("heatmaps_LME_b_mu_4.pdf", width=10, height=5)
plot(heatmaps_LME_b_mu[[4]])
dev.off()
pdf("heatmaps_LME_b_mu_5.pdf", width=10, height=5)
plot(heatmaps_LME_b_mu[[5]])
dev.off()

# Sampling effect

pdf("heatmaps_Sampling_b_mu_1.pdf", width=10, height=5)
heatmaps_Sampling_b_mu[[1]]
dev.off()
pdf("heatmaps_Sampling_b_mu_2.pdf", width=10, height=5)
heatmaps_Sampling_b_mu[[2]]
dev.off()
pdf("heatmaps_Sampling_b_mu_3.pdf", width=10, height=5)
heatmaps_Sampling_b_mu[[3]]
dev.off()
pdf("heatmaps_Sampling_b_mu_4.pdf", width=10, height=5)
heatmaps_Sampling_b_mu[[4]]
dev.off()
pdf("heatmaps_Sampling_b_mu_5.pdf", width=10, height=5)
heatmaps_Sampling_b_mu[[5]]
dev.off()



# Mean duration of speciation

# LME


pdf("heatmaps_LME_tau_1.pdf", width=10, height=5)
plot(heatmaps_LME_tau[[1]])
dev.off()
pdf("heatmaps_LME_tau_2.pdf", width=10, height=5)
plot(heatmaps_LME_tau[[2]])
dev.off()
pdf("heatmaps_LME_tau_3.pdf", width=10, height=5)
plot(heatmaps_LME_tau[[3]])
dev.off()
pdf("heatmaps_LME_tau_4.pdf", width=10, height=5)
plot(heatmaps_LME_tau[[4]])
dev.off()
pdf("heatmaps_LME_tau_5.pdf", width=10, height=5)
plot(heatmaps_LME_tau[[5]])
dev.off()

# Sampling effect

pdf("heatmaps_Sampling_tau_1.pdf", width=10, height=5)
heatmaps_Sampling_tau[[1]]
dev.off()
pdf("heatmaps_Sampling_tau_2.pdf", width=10, height=5)
heatmaps_Sampling_tau[[2]]
dev.off()
pdf("heatmaps_Sampling_tau_3.pdf", width=10, height=5)
heatmaps_Sampling_tau[[3]]
dev.off()
pdf("heatmaps_Sampling_tau_4.pdf", width=10, height=5)
heatmaps_Sampling_tau[[4]]
dev.off()
pdf("heatmaps_Sampling_tau_5.pdf", width=10, height=5)
heatmaps_Sampling_tau[[5]]
dev.off()


# Speciation-completion rate

# LME


pdf("heatmaps_LME_la_1.pdf", width=10, height=5)
plot(heatmaps_LME_la[[1]])
dev.off()
pdf("heatmaps_LME_la_2.pdf", width=10, height=5)
plot(heatmaps_LME_la[[2]])
dev.off()
pdf("heatmaps_LME_la_3.pdf", width=10, height=5)
plot(heatmaps_LME_la[[3]])
dev.off()
pdf("heatmaps_LME_la_4.pdf", width=10, height=5)
plot(heatmaps_LME_la[[4]])
dev.off()
pdf("heatmaps_LME_la_5.pdf", width=10, height=5)
plot(heatmaps_LME_la[[5]])
dev.off()

# Sampling

pdf("heatmaps_Sampling_la_1.pdf", width=10, height=5)
heatmaps_Sampling_la[[1]]
dev.off()
pdf("heatmaps_Sampling_la_2.pdf", width=10, height=5)
heatmaps_Sampling_la[[2]]
dev.off()
pdf("heatmaps_Sampling_la_3.pdf", width=10, height=5)
heatmaps_Sampling_la[[3]]
dev.off()
pdf("heatmaps_Sampling_la_4.pdf", width=10, height=5)
heatmaps_Sampling_la[[4]]
dev.off()
pdf("heatmaps_Sampling_la_5.pdf", width=10, height=5)
heatmaps_Sampling_la[[5]]
dev.off()
