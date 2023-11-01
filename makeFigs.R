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

#   -- Illustration of weak-selection classical results
source('./R/functions-figures.R')
toPdf(classicParamSpaceFig(), 
			figPath(name='classicParamSpaceFig.pdf'), width=10, height=7)
embed_fonts(figPath(name='classicParamSpaceFig.pdf'))

toPdf(RelBalancingFig(), 
			figPath(name='RelBalancingFig.pdf'), width=10, height=7)
embed_fonts(figPath(name='RelBalancingFig.pdf'))



toPdf(Fisher_2D_ExploreFig(reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig.pdf'))
