
args = as.numeric(commandArgs(TRUE))
setwd("/home/s3033724/PBDproject/scripts")
source('generate_trees.R')
generate_trees(args,1000)
