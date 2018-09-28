############################################################
#  Statistical analysis of the protracted speciation model #
#                      Camille Simonet                     #
#----------------------------------------------------------#  
#            Functions necessary for plottings             #
############################################################


Mytheme<- theme(legend.title=element_blank(),
      legend.text=element_text(size = 10),
      panel.border= element_blank(),
      axis.text.y = element_text(face="bold", colour="black", size = 10),
      axis.text.x = element_text(face="bold", colour="black", size = 10),
      axis.title.y = element_text(face="bold", colour="black", size = 10),
      axis.title.x = element_text(face="bold", colour="black", size = 10),
      axis.line.y = element_line(color="white", size = 0.5),
      axis.line.x = element_line(color="white", size = 0.5),
      plot.title = element_text(lineheight=.8, face="bold", size = 10, hjust = 0.5))
##

# Extract Legend from a ggplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}



# Prepare the data table for boxplots of accuracy of estimates (Figure S1)
MakeboxplotData<- function(limit, b)
{
  
  index<- 1
  
  pars.ls<- vector("list")         # list to store simulation parameters
  accuracy.Sr.ls<- vector("list")  # list to store accuracy of estimates on stree_random
  accuracy.So.ls<- vector("list")  # list to store accuracy of estimates on  stree_oldest
  accuracy.rec.ls<- vector("list") # list to store accuracy of estimates on recontree
  
  if(b<0.7)  # for b<0.7, I have all simulation, can load everything
  {
    
    for (mu in c(0,0.1,0.2))
    {
      for (la in c(0.1,0.3,1))   # Load all the tables and store them in the lists
      {
        pars.ls[[index]]<- read.table(paste("Simulation_parameters", b, mu, la, sep="_"), header=TRUE)
        accuracy.Sr.ls[[index]]<- read.table(paste("accuracy_stree_random", b, mu, la, sep="_"), header=TRUE)
        accuracy.So.ls[[index]]<- read.table(paste("accuracy_stree_oldest", b, mu, la, sep="_"), header=TRUE)
        accuracy.rec.ls[[index]]<- read.table(paste("accuracy_recontree", b, mu, la, sep="_"), header=TRUE)
        
        index=index+1
        
      }
    }
    
    pars<- do.call(rbind, pars.ls)   # reassemble the tables of simulation parameters
    counts<- ddply(pars,.(b,la,mu,b_mu,durspMean,durspMedian),nrow) # count how many simulation I got for each combination
    counts<- counts[order(counts$mu),]
    
    counts<- counts[order(counts$mu),7]
    pars$comb<- c(rep(1, counts[1]), rep(2, counts[2]),rep(3, counts[3]), # add a "parameter combination" column to the table (necessary for plotting after)
                  rep(4, counts[4]),rep(5, counts[5]),rep(6, counts[6]),
                  rep(7, counts[7]),rep(8, counts[8]),rep(9, counts[9]))
    
    
  } else  # When b = 0.7 , There are some simulations missing, adapt the script for that
  {
    
    
    for (mu in c(0.1,0.2))
    {
      for (la in c(0.1,0.3,1))   #Simulation_parameters_0.6_0.2_1"
      {
        pars.ls[[index]]<- read.table(paste("Simulation_parameters", b, mu, la, sep="_"), header=TRUE)
        accuracy.Sr.ls[[index]]<- read.table(paste("accuracy_stree_random", b, mu, la, sep="_"), header=TRUE)
        accuracy.So.ls[[index]]<- read.table(paste("accuracy_stree_oldest", b, mu, la, sep="_"), header=TRUE)
        accuracy.rec.ls[[index]]<- read.table(paste("accuracy_recontree", b, mu, la, sep="_"), header=TRUE)
        
        index=index+1
        
      }
    }
    
    pars<- do.call(rbind, pars.ls)
    counts<- ddply(pars,.(b,la,mu,b_mu,durspMean,durspMedian),nrow)
    counts<- counts[order(counts$mu),7]
    pars$comb<- c(rep(1, counts[1]), rep(2, counts[2]),rep(3, counts[3]),
                  rep(4, counts[4]),rep(5, counts[5]),rep(6, counts[6]))
    
    
  }
  
  
  
  
  accuracy.rec<- do.call(rbind, accuracy.rec.ls) # reassemble all tables of accuracy of estimates on recontree
  accuracy.rec<- -(accuracy.rec) # I computed (true value - estimate), I prefer to have (estimate - true value) so multiply everything by -1
  accuracy.rec$treeType<- rep("recontree", nrow(accuracy.rec)) # add info that these accuracies are for "recontree"
  colnames(accuracy.rec)<- c(paste(colnames(accuracy.rec)[1:6], "_accuracy", sep=""), "treeType")
  accuracy.rec<- cbind(pars, accuracy.rec) # bind it with the simulated parameters
  
  # doing the same for accuracy of estimates on stree_random
  accuracy.Sr<- do.call(rbind, accuracy.Sr.ls)
  accuracy.Sr<- -(accuracy.Sr)
  accuracy.Sr$treeType<- rep("stree_random", nrow(accuracy.Sr))
  colnames(accuracy.Sr)<- c(paste(colnames(accuracy.Sr)[1:6], "_accuracy", sep=""), "treeType")
  accuracy.Sr<- cbind(pars, accuracy.Sr)
  
  # doing the same for accuracy of estimates on stree_oldest
  accuracy.So<- do.call(rbind, accuracy.So.ls)
  accuracy.So<- -(accuracy.So)
  accuracy.So$treeType<- rep("stree_oldest", nrow(accuracy.So))
  colnames(accuracy.So)<- c(paste(colnames(accuracy.So)[1:6], "_accuracy", sep=""), "treeType")
  accuracy.So<- cbind(pars, accuracy.So)
  
  # assemble all the tables
  accuracy<- rbind(accuracy.rec, accuracy.Sr, accuracy.So)
  
  #View(accuracy[which(accuracy$treeType=="stree_oldest" & accuracy$mu==0),])
  
  lim<- limit # for the graphical limit
  
  
  # This part of the code is a series of manipulation to extract the 95th and 5th quantiles, the median, the max and min value for each parameter combination/each type of tree
  # necessary to do the boxplot with ggplot afterward
  accuracy_summary_b<- accuracy %>% group_by(comb,treeType) %>%
    summarize(ymax=max(b_accuracy), ymin = min(b_accuracy), 
              middle = median(b_accuracy), upper = quantile(b_accuracy, 0.95),
              lower = quantile(b_accuracy, 0.05))
  
  accuracy_summary_mu<- accuracy %>% group_by(comb,treeType) %>%
    summarize(ymax=max(mu_accuracy), ymin = min(mu_accuracy), 
              middle = median(mu_accuracy), upper = quantile(mu_accuracy, 0.95),
              lower = quantile(mu_accuracy, 0.05))
  
  accuracy_summary_la<- accuracy %>% group_by(comb,treeType) %>%
    summarize(ymax=max(la_accuracy), ymin = min(la_accuracy), 
              middle = median(la_accuracy), upper = quantile(la_accuracy, 0.95),
              lower = quantile(la_accuracy, 0.05))
  
  accuracy_summary_b_mu<- accuracy %>% group_by(comb,treeType) %>%
    summarize(ymax=max(b_mu_accuracy), ymin = min(b_mu_accuracy), 
              middle = median(b_mu_accuracy), upper = quantile(b_mu_accuracy, 0.95),
              lower = quantile(b_mu_accuracy, 0.05))
  
  
  accuracy_summary_tau<- accuracy %>% group_by(comb,treeType) %>%
    summarize(ymax=max(durspMean_accuracy), ymin = min(durspMean_accuracy), 
              middle = median(durspMean_accuracy), upper = quantile(durspMean_accuracy, 0.95),
              lower = quantile(durspMean_accuracy, 0.05))
  
  
  
  #  No w we re-order these recontree first, stree_random second and stree_oldest last (easier because other tables are organized like this)
  #accuracy_summary_b<- rbind(cbind(accuracy_summary_b[which(accuracy_summary_b$treeType=="recontree"),], counts),
  #                         cbind(accuracy_summary_b[which(accuracy_summary_b$treeType=="stree_random"),],counts),
   #                        cbind(accuracy_summary_b[which(accuracy_summary_b$treeType=="stree_oldest"),],counts))
  
  
  accuracy_summary_b<- rbind(data.frame(accuracy_summary_b[which(accuracy_summary_b$treeType=="recontree"),], counts),
                             data.frame(accuracy_summary_b[which(accuracy_summary_b$treeType=="stree_random"),],counts),
                             data.frame(accuracy_summary_b[which(accuracy_summary_b$treeType=="stree_oldest"),],counts))
  
  
  # adjust the ymin and ymax for plotting
  accuracy_summary_b[(accuracy_summary_b$ymax > lim[[1]]), 3] <- lim[[1]]   # "if the ymax is higher than the limit we chose, set it to "limit"
  accuracy_summary_b[(accuracy_summary_b$ymin < -lim[[1]]), 4] <- -lim[[1]] # "if the ymin is lower than the limit we chose, set it to "-limit"
  accuracy_summary2_b<- expandRows(accuracy_summary_b, "counts") # expands this summary statistics table by the number of simulation obtained for each parameter combination (normally 1000)
  accuracy_b<- cbind(accuracy, accuracy_summary2_b[,3:7]) # bind together the table of accuracy measures and the table of summary statistics
  
  
  # Do the same for the four other parameters
  #accuracy_summary_mu<- rbind(cbind(accuracy_summary_mu[which(accuracy_summary_mu$treeType=="recontree"),], counts),
  #                           cbind(accuracy_summary_mu[which(accuracy_summary_mu$treeType=="stree_random"),],counts),
  #                           cbind(accuracy_summary_mu[which(accuracy_summary_mu$treeType=="stree_oldest"),],counts))
  
  
  accuracy_summary_mu<- rbind(data.frame(accuracy_summary_mu[which(accuracy_summary_mu$treeType=="recontree"),], counts),
                              data.frame(accuracy_summary_mu[which(accuracy_summary_mu$treeType=="stree_random"),],counts),
                              data.frame(accuracy_summary_mu[which(accuracy_summary_mu$treeType=="stree_oldest"),],counts))
  
  
  accuracy_summary_mu[(accuracy_summary_mu$ymax > lim[[2]]), 3] <- lim[[2]]
  accuracy_summary_mu[(accuracy_summary_mu$ymin < -lim[[2]]), 4] <- -lim[[2]]
  
  accuracy_summary2_mu<- expandRows(accuracy_summary_mu, "counts")
  accuracy_mu<- cbind(accuracy, accuracy_summary2_mu[,3:7])
  
  
  
  #accuracy_summary_la<- rbind(cbind(accuracy_summary_la[which(accuracy_summary_la$treeType=="recontree"),], counts),
  #                            cbind(accuracy_summary_la[which(accuracy_summary_la$treeType=="stree_random"),],counts),
  #                            cbind(accuracy_summary_la[which(accuracy_summary_la$treeType=="stree_oldest"),],counts))
  
  accuracy_summary_la<- rbind(data.frame(accuracy_summary_la[which(accuracy_summary_la$treeType=="recontree"),], counts),
                              data.frame(accuracy_summary_la[which(accuracy_summary_la$treeType=="stree_random"),],counts),
                              data.frame(accuracy_summary_la[which(accuracy_summary_la$treeType=="stree_oldest"),],counts))
  
  accuracy_summary_la[(accuracy_summary_la$ymax > lim[[3]]), 3] <- lim[[3]]
  accuracy_summary_la[(accuracy_summary_la$ymin < -lim[[3]]), 4] <- -lim[[3]]
  
  accuracy_summary2_la<- expandRows(accuracy_summary_la, "counts")
  accuracy_la<- cbind(accuracy, accuracy_summary2_la[,3:7])
  
  
  
  #accuracy_summary_b_mu<- rbind(cbind(accuracy_summary_b_mu[which(accuracy_summary_b_mu$treeType=="recontree"),], counts),
  #                           cbind(accuracy_summary_b_mu[which(accuracy_summary_b_mu$treeType=="stree_random"),],counts),
  #                           cbind(accuracy_summary_b_mu[which(accuracy_summary_b_mu$treeType=="stree_oldest"),],counts))
  
  accuracy_summary_b_mu<- rbind(data.frame(accuracy_summary_b_mu[which(accuracy_summary_b_mu$treeType=="recontree"),], counts),
                                data.frame(accuracy_summary_b_mu[which(accuracy_summary_b_mu$treeType=="stree_random"),],counts),
                                data.frame(accuracy_summary_b_mu[which(accuracy_summary_b_mu$treeType=="stree_oldest"),],counts))
  
  
  accuracy_summary_b_mu[(accuracy_summary_b_mu$ymax > lim[[4]]), 3] <- lim[[4]]
  accuracy_summary_b_mu[(accuracy_summary_b_mu$ymin < -lim[[4]]), 4] <- -lim[[4]]
  
  accuracy_summary2_b_mu<- expandRows(accuracy_summary_b_mu, "counts")
  accuracy_b_mu<- cbind(accuracy, accuracy_summary2_b_mu[,3:7])
  
  
  #accuracy_summary_tau<- rbind(cbind(accuracy_summary_tau[which(accuracy_summary_tau$treeType=="recontree"),], counts),
  #                           cbind(accuracy_summary_tau[which(accuracy_summary_tau$treeType=="stree_random"),],counts),
  #                           cbind(accuracy_summary_tau[which(accuracy_summary_tau$treeType=="stree_oldest"),],counts))
  
  accuracy_summary_tau<- rbind(data.frame(accuracy_summary_tau[which(accuracy_summary_tau$treeType=="recontree"),], counts),
                               data.frame(accuracy_summary_tau[which(accuracy_summary_tau$treeType=="stree_random"),],counts),
                               data.frame(accuracy_summary_tau[which(accuracy_summary_tau$treeType=="stree_oldest"),],counts))
  
  accuracy_summary_tau[(accuracy_summary_tau$ymax > lim[[5]]), 3] <- lim[[5]]
  accuracy_summary_tau[(accuracy_summary_tau$ymin < -lim[[5]]), 4] <- -lim[[5]]
  
  accuracy_summary2_tau<- expandRows(accuracy_summary_tau, "counts")
  accuracy_tau<- cbind(accuracy, accuracy_summary2_tau[,3:7])
  
  
  # Assemble the tables in one list, each can then be called individually for plotting
  accuracies<- list(accuracy_b, accuracy_mu, accuracy_la, accuracy_b_mu, accuracy_tau)
  names(accuracies)<- c("b", "mu", "la", "b_mu", "tau")
  
  return(accuracies)
  
}



# Used in dply to return the mean of a given vector
give.n <- function(x){return(c(y = mean(x)))}


# Make the boxplots of the accuracy of estimates
MakeBoxPlot<- function(lim, b, data, param, paramName, liminf)
{
  
  theme_set(theme_gray(base_size = 10))
  
  ggplot(data, aes(x = as.factor(data$la), y = data[,param], fill = treeType)) +
    geom_boxplot(aes(ymin = ymin, ymax =ymax,middle = middle,upper = upper,lower= lower),
                 stat = 'identity')+
    labs(x="", y="")+
    facet_wrap(~ mu)+
    theme(legend.position="none", 
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=8),
          axis.text.x=element_text(face="bold", colour="black", size=15),
          axis.text.y=element_text(face="bold", colour="black", size=15),
          plot.title = element_text(size=15, face = "bold", hjust = 0.5),
          strip.text.x = element_text(face="bold", colour="black", size=15))+
    coord_cartesian(ylim = c(-liminf, lim))+
    scale_fill_manual(name = "Tree type", values = c("white", "grey50", "grey35"))
   
    #+theme()
    #stat_summary(fun.data = give.n, geom = "point", size=1,col="gray20", position=position_dodge(width=0.95))+
    #xlab(expression(lambda)) + ylab(paste("Accuracy in estimation of \n", paramName, sep=""))+ 
    #ggtitle(paste("b = ", b, sep=""))
}


