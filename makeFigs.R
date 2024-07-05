#'  Functions to generate figures for: 
#'    
#'  Title: 	Insights from Fisher's Geometric Model into the scope
#'			for balancing selection in inbreeding populations
#'
#'
#'  Author: Colin Olito
#'
#'
#'  NOTES: Run this file, either from terminal using Rscript,
#'		  or interactively in R. This should create all the 
#'		  figures needed to correctly compile the mansucript
#'		  LaTeX file.  
#'

rm(list=ls())
###############
# DEPENDENCIES
###############
source('./R/functions-figures.R')
source('./R/functions-FGM-Simulations.R')

######################################
#' Create figures directories if they
#' do not already exist
figuresDirectoriesExist  <-  dir.exists("./figs")

if(!figuresDirectoriesExist) {
	dir.create("./figs")
}

########################
# Figures for the paper
########################



########################
# Priliminary figures
########################

# Illustration of weak-selection classical results
source('./R/functions-figures.R')
toPdf(classicParamSpaceFig(), 
			figPath(name='classicParamSpaceFig2.pdf'), width=11, height=8)
embed_fonts(figPath(name='classicParamSpaceFig2.pdf'))

# Relative proportion of parameter space resulting in
# balancing selection for: 
# (1) all mutations
toPdf(RelBalancingFig(favouredMuts=FALSE), 
			figPath(name='RelBalancingFig-AllMuts.pdf'), width=10, height=8)
embed_fonts(figPath(name='RelBalancingFig-AllMuts.pdf'))

# (2) Favored mutations
toPdf(RelBalancingFig(favouredMuts=TRUE), 
			figPath(name='RelBalancingFig-FavMuts.pdf'), width=10, height=8)
embed_fonts(figPath(name='RelBalancingFig-FavMuts.pdf'))

# Relative proportion of parameter space resulting in
# balancing selection for established mutations.
toPdf(RelBalancingEstablishedFig(), 
			figPath(name='RelBalancingFig-EstMuts.pdf'), width=10, height=8)
embed_fonts(figPath(name='RelBalancingFig-EstMuts.pdf'))

# Summary figure showing relative proportion of parameter
# space resluting in balancing selection for favored and
# established mutations, with variable mutation effect
# sizes. 
 source('./R/functions-figures.R')
 source('./R/functions-FGM-Simulations.R')
toPdf(RelBalancing_SummaryFig(Ne=10^5, sim.reps=10^5), 
			figPath(name='RelBalancing_SummaryFig.pdf'), width=7, height=7)
embed_fonts(figPath(name='RelBalancing_SummaryFig.pdf'))




# Kimura-Ohta invasion plot for arbitrary selection strength
toPdf(KimuraOhta_InvPlot(), 
			figPath(name='KimuraOhta_InvPlot.pdf'), width=10, height=7)
embed_fonts(figPath(name='KimuraOhta_InvPlot.pdf'))

# Ratio of mutation size with maximal probability of balancing selection
toPdf(relMutSizeMaxRBal(), 
			figPath(name='relMutSizeMaxRBal.pdf'), width=5, height=5)
embed_fonts(figPath(name='relMutSizeMaxRBal.pdf'))



#######################
# FGM 2D Visualizations
source('./R/functions-figures.R')
Fisher_2D_ExploreFig(z=0.5, reps=5*10^4)

toPdf(Fisher_2D_ExploreFig(z=0.75, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.5.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.5.pdf'))

toPdf(Fisher_2D_ExploreFig(z=0.5, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.5.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.5.pdf'))

toPdf(Fisher_2D_ExploreFig(z=1, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z1.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z1.pdf'))

toPdf(Fisher_2D_ExploreFig(z=1.25, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z1.25.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z1.25.pdf'))

toPdf(Fisher_2D_ExploreFig(z=1.5, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z1.5.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z1.5.pdf'))

toPdf(Fisher_2D_ExploreFig(z=2, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z2.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z2.pdf'))

toPdf(Fisher_2D_ExploreFig(z=3, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z3.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z3.pdf'))
