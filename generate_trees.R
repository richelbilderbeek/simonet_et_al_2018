
generate_trees<- function (args, nbTrees)
  
{

  require(PBD)
  
  b  = args[1]
  la = args[2]
  mu = args[3]

  set.seed(42)
  
  ### Set up list to store trees + current parameters

  print(paste("Parameters:"," b=", b, " la=", la, " mu=", mu, sep=""))
  trees<- list()


  ### Simulate nbTrees trees and store them
  i=1
  while(i<=nbTrees)
  { 
    
    simtree<- pbd_sim(pars=c(b,la,b,mu,mu), age=15, soc=2)
    trees[[i]]<- list(simtree$tree, simtree$recontree, simtree$stree_oldest, simtree$stree_random, simtree$L)
    names(trees[[i]])<- c("tree", "recontree", "stree_oldest", "stree_random", "L")
    
    ### Save the lists of trees
    #save(trees, file = sprintf("C:/Users/Camille/Desktop/PBDproject/outputs/simulations/trees_%s_%s_%s.RData", b, mu, la))
    save(trees, file = sprintf("/home/s3033724/PBDproject/outputs/simulations/trees_%s_%s_%s.RData", b, mu, la))

    print(i) 
    i=i+1
  }
  

}



  