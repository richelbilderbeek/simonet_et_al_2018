PBD project
Camille Simonet, Artur Rego-Costa, Raphaël Scherrer (MEME students cohort 2015-2017)
Rampal S. Etienne
#---------------------------------------------------------------------------------------
Last update of this README file: 08/11/2017
#---------------------------------------------------------------------------------------
Content:

PBD_pipeline_new
DoMyTrees.R
generate_trees.R
Estimate_Parameters.R
MLE.R
Functions.R
Figures_2_3_4
Code_Figures_Supplements
ML_estimates (contains 540 tables in .txt format)
Figure234.ai
Supplements_version2.ai
#---------------------------------------------------------------------------------------
Files description:


PBD_pipeline_new: launches two R scripts, a first one to generate the trees, and a second to estimates the parameters

DoMyTrees.R: short r script to take input arguments from command line to feed in "generate_trees.R". Arguments define parameters b, la, mu and number of trees to simulate

generate_trees.R: simulates trees with parameters defined by "DoMyTrees.R". As b1 = b2, and mu1 = mu2, age=15, soc=2. Each simulation results in a list storing "tree", "recontree", "stree_oldest", "stree_random", "L", saved as .RData file. There is NO SEED SET.


Estimate_Parameters.R: short r script to take input arguments from command line to feed in "MLE.R"

MLE.R: for values of b,mu,la given by "Estimate_Parameters.R", fetches the corresponding simulation (Rdata file) and estimates parameters. Each RData file has n number of trees. This will RETURNS 10 TABLES, with as many lines as there are trees in the RData.

simulation_parameters: simulation parameters of the tree
estimates_recontree: parameters estimated on recontree (b,la,mu,b-mu,tau,mediandursp) + size of recontree/stree/full tree + loglik, df, conv from LME function output
estimates_stree_oldest: same for stree oldest (information about tree size redundant with the other tables)
estimates_stree_random: same for stree random (information about tree size redundant with the other tables)
accuracy_recontree: simulation_parameters - estimates_recontre
accuracy_stree_oldest: simulation_parameters - estimates_stree_oldest
accuracy_stree_random: simulation_parameters - estimates_stree_random
LME_effect: abs(estimates_recontree - estimates_stree_oldest)
Sampling_effect: abs(estimates_stree_random - estimates_stree_oldest)
Combined_effect abs(estimates_recontree - estimates_stree_random)

--> ALL OF THESE TABLES ARE IN "ML_estimates" directory in this archive

Functions.R: a bunch of functions used for plotting

Figures_2_3_4: code producing heatmaps of figures 2, 3 and 4. This CANNOT BE RUN WITHOUT the "Function.R" script

Code_Figures_Supplements: script for producing all supplement figures

Figure234.ai: Adobe illustrator file used to generate embbedded figures from raw pdf output of the R script
Supplements_version2.ai: Adobe Illustrator file used to generate the supplementary material from raw pdf output of R script
#----------------------------------------------------------------------------------------------------------------------------------