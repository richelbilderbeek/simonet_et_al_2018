
MLE<- function (args)
  
{ 
  ### Loading packages and setting directory
  library(DDD)
  library(PBD)
  library(apTreeshape)
  
  ### Setup parameters  
  
  b = args[1]
  la = args[2]
  mu = args[3]
  
  print(paste("Parameters:"," b=", b, " la=", la, " mu=", mu, sep=""))

  ### Loading the trees
  
  setwd("/home/s3033724/PBDproject/outputs/simulations")
  load(file= sprintf("trees_%s_%s_%s.RData", b, mu, la))

   
  ### Create matrices to be filled with the estimates

  estimates_recontree=matrix(nrow=length(trees), ncol=13) # to store the results of ML estimates of recontrees in a matrix 
  colnames(estimates_recontree)<- c("b", "la", "mu", "b_mu","durspMean", "durspMedian", "Size_recontree", "Size_stree_oldest", "Size_recontree", "Size_full_tree", "loglik", "df", "conv") 

  estimates_stree_random=matrix(nrow=length(trees), ncol=13) # Same for the strees with random sampling
  colnames(estimates_stree_random)<- c("b", "la", "mu", "b_mu","durspMean", "durspMedian", "Size_recontree", "Size_stree_oldest", "Size_recontree", "Size_full_tree", "loglik", "df", "conv") 
  
  estimates_stree_oldest=matrix(nrow=length(trees), ncol=13) # Same for the strees with sampling the oldest
  colnames(estimates_stree_oldest)<- c("b", "la", "mu", "b_mu", "durspMean", "durspMedian", "Size_recontree", "Size_stree_oldest", "Size_recontree", "Size_full_tree", "loglik", "df", "conv")
  
  simulation_parameters=matrix(nrow=length(trees), ncol=6) # Same for the strees with sampling the oldest
  colnames(simulation_parameters)<- c("b", "la", "mu", "b_mu","durspMean", "durspMedian") 

  ### Perform the ML estimates and store results in the matrices
  
i=1
while(i <= length(trees))
  
{ 
  current_recontree<- trees[[i]]$recontree 
  current_stree_random<- trees[[i]]$stree_random 
  current_stree_oldest<- trees[[i]]$stree_oldest
  current_full_tree<- trees[[i]]$tree
  

      ### 1) Compute branching times and ML estimates
      brts_recontree<- branching.times(current_recontree)   # extract branching times to put them in the ML
      brts_stree_random<- branching.times(current_stree_random)
      brts_stree_oldest<- branching.times(current_stree_oldest)
      
      MLE_recontree<- pbd_ML(brts_recontree,soc=2, initparsopt = c(b, mu, la, mu),idparsopt=1:4, exteq = 1)
      MLE_stree_random<- pbd_ML(brts_stree_random,soc=2, initparsopt = c(b, mu, la, mu),idparsopt=1:4, exteq = 1) 
      MLE_stree_oldest<- pbd_ML(brts_stree_oldest,soc=2, initparsopt = c(b, mu, la, mu),idparsopt=1:4, exteq = 1) 
      
      # Extract the values of parameters estimates from the ML function output
      Estimates_recontree    <- c(estim_b = MLE_recontree[[1]], estim_mu = MLE_recontree[[2]],estim_la = MLE_recontree[[3]], loglik = MLE_recontree[[5]], df = MLE_recontree[[6]], conv = MLE_recontree[[7]])
      Estimates_stree_random <- c(estim_b = MLE_stree_random[[1]], estim_mu = MLE_stree_random[[2]],estim_la = MLE_stree_random[[3]], loglik = MLE_stree_random[[5]], df = MLE_stree_random[[6]], conv = MLE_stree_random[[7]])
      Estimates_stree_oldest <- c(estim_b = MLE_stree_oldest[[1]], estim_mu = MLE_stree_oldest[[2]],estim_la = MLE_stree_oldest[[3]], loglik = MLE_stree_oldest[[5]], df = MLE_stree_oldest[[6]], conv = MLE_stree_oldest[[7]])

      ### 2) Record Tree Sizes
      treeSize_recontree<-length(current_recontree$tip.label) # extract the tree size number of tips)
      treeSize_stree_random<-length(current_stree_random$tip.label)
      treeSize_stree_oldest<- length(current_stree_oldest$tip.label)
      treeSize_full_tree<- length(current_full_tree$tip.label)
      
      ### 3) Mean and Median of duration of speciation of incipient species, measured, estimated
      # Measured
      Measured_Mean_Durspec<- pbd_durspec_mean(pars = c(b, la, mu))
      Measured_Median_Durspec<- pbd_durspec_quantile(pars = c(b, la, mu) ,p= 0.5)
      
      #Estimated, recontree
      Estim_Mean_Durspec_recontree<- pbd_durspec_mean(pars = c(Estimates_recontree[[1]], Estimates_recontree[[3]], Estimates_recontree[[4]])) # b, la, mu
      Estim_Median_Durspec_recontree<- pbd_durspec_quantile(pars = c(Estimates_recontree[[1]], Estimates_recontree[[3]], Estimates_recontree[[4]]) ,p= 0.5) 
      
      # Estimated, stree_oldest
      Estim_Mean_Durspec_stree_oldest<- pbd_durspec_mean(pars = c(Estimates_stree_oldest[[1]], Estimates_stree_oldest[[3]], Estimates_stree_oldest[[4]])) 
      Estim_Median_Durspec_stree_oldest<- pbd_durspec_quantile(pars = c(Estimates_stree_oldest[[1]], Estimates_stree_oldest[[3]], Estimates_stree_oldest[[4]]) ,p= 0.5)
      
      #Estimated, stree_random
      Estim_Mean_Durspec_stree_random<- pbd_durspec_mean(pars = c(Estimates_stree_random[[1]], Estimates_stree_random[[3]], Estimates_stree_random[[4]]))
      Estim_Median_Durspec_stree_random<- pbd_durspec_quantile(pars = c(Estimates_stree_random[[1]], Estimates_stree_random[[3]], Estimates_stree_random[[4]]) ,p= 0.5)
      
      ### 5) Fill the Matrices with these results
      
      # True parameters
      simulation_parameters[i,]<- c(b, la, mu, b-mu, Measured_Mean_Durspec, Measured_Median_Durspec)
      
      # Estimates for each type of tree
      estimates_recontree[i,]<- c(Estimates_recontree[c(1,3,2)], (Estimates_recontree[1] - Estimates_recontree[2]), Estim_Mean_Durspec_recontree, Estim_Median_Durspec_recontree, treeSize_recontree, treeSize_stree_oldest, treeSize_recontree, treeSize_full_tree, Estimates_recontree[c(5,6,7)])
      estimates_stree_oldest[i,]<- c(Estimates_stree_oldest[c(1,3,2)], (Estimates_stree_oldest[1] - Estimates_stree_oldest[2]), Estim_Mean_Durspec_stree_oldest, Estim_Median_Durspec_stree_oldest,treeSize_recontree, treeSize_stree_oldest, treeSize_recontree, treeSize_full_tree, Estimates_stree_oldest[c(5,6,7)])
      estimates_stree_random[i,]<- c(Estimates_stree_random[c(1,3,2)], (Estimates_stree_random[1] - Estimates_stree_random[2]), Estim_Mean_Durspec_stree_random, Estim_Median_Durspec_stree_random,treeSize_recontree, treeSize_stree_oldest, treeSize_recontree, treeSize_full_tree , Estimates_stree_random[c(5,6,7)])
      
      # Distance true - estimate (for boxplots, replication of Etienne et al)
      accuracy_recontree   <- cbind((simulation_parameters - estimates_recontree[,c(1:6)]))
      accuracy_stree_oldest<- cbind((simulation_parameters - estimates_stree_oldest[,c(1:6)]))
      accuracy_stree_random<- cbind((simulation_parameters - estimates_stree_random[,c(1:6)]))
      
      
      # Between tree comparisons - Take the absolute value of the difference as the distance between estimated
      
      # Approximate tree (recontree) vs. oldest_sampled tree = LME approximation effect
       LME_effect<- cbind(abs(estimates_recontree[,c(1:6)] - estimates_stree_oldest[,c(1:6)]))
      
       # Random_sampled_tree vs. oldest_sampled_tree = random sampling effect
       Sampling_effect<- cbind(abs(estimates_stree_random[,c(1:6)] - estimates_stree_oldest[,c(1:6)]))

      # recontre vs. random_sampled_tree = Combined effects of LME approximation and random sampling
       Combined_effect<- cbind(abs(estimates_recontree[,c(1:6)] - estimates_stree_random[,c(1:6)]))

           
  ### Save the ML estimates matrix 
   # (I write and overwrite the table everytime a new tree is treated)

   # True parameters
   write.table(simulation_parameters, file= sprintf("/home/s3033724/PBDproject/outputs/ML_estimates/Simulation_parameters_%s_%s_%s",b, mu, la), row.names=FALSE, col.names=TRUE,sep="\t")

   # Estimated parameters
   write.table(estimates_recontree, file= sprintf("/home/s3033724/PBDproject/outputs/ML_estimates/estimates_recontree_%s_%s_%s",b, mu, la), row.names=FALSE, col.names=TRUE,sep="\t")
   write.table(estimates_stree_oldest, file= sprintf("/home/s3033724/PBDproject/outputs/ML_estimates/estimates_stree_oldest_%s_%s_%s",b, mu, la), row.names=FALSE, col.names=TRUE,sep="\t")
   write.table(estimates_stree_random, file= sprintf("/home/s3033724/PBDproject/outputs/ML_estimates/estimates_stree_random_%s_%s_%s",b, mu, la), row.names=FALSE, col.names=TRUE,sep="\t")
   
   # Accuracy
   write.table(accuracy_recontree, file= sprintf("/home/s3033724/PBDproject/outputs/ML_estimates/accuracy_recontree_%s_%s_%s",b, mu, la), row.names=FALSE, col.names=TRUE,sep="\t")
   write.table(accuracy_stree_oldest, file= sprintf("/home/s3033724/PBDproject/outputs/ML_estimates/accuracy_stree_oldest_%s_%s_%s",b, mu, la), row.names=FALSE, col.names=TRUE,sep="\t")
   write.table(accuracy_stree_random, file= sprintf("/home/s3033724/PBDproject/outputs/ML_estimates/accuracy_stree_random_%s_%s_%s",b, mu, la), row.names=FALSE, col.names=TRUE,sep="\t")
   
   
   # LME effect
   write.table(LME_effect, file= sprintf("/home/s3033724/PBDproject/outputs/ML_estimates/LME_effect_%s_%s_%s",b, mu,la), row.names=FALSE, col.names=TRUE,sep="\t")
   
   # Sampling effect
   write.table(Sampling_effect, file= sprintf("/home/s3033724/PBDproject/outputs/ML_estimates/Sampling_effect_%s_%s_%s",b, mu, la), row.names=FALSE, col.names=TRUE,sep="\t")
   
   # Combined effect
   write.table(Combined_effect, file= sprintf("/home/s3033724/PBDproject/outputs/ML_estimates/Combined_effect_%s_%s_%s",b, mu, la), row.names=FALSE, col.names=TRUE,sep="\t")
   
   
   print(i)  # To keep track of where we are. 
   i=i+1
 
    }

}

