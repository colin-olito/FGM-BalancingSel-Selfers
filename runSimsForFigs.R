#'  Run simulations with longer runtimes needed
#'  to generate figures
#'
#'  Author: Colin Olito
#'
#'  NOTES: Run this file, either from terminal using Rscript,
#'		  or interactively in R. This should create simulation
#'		  data needed for making figures for the mansucript.
#'

###############
# DEPENDENCIES
###############
rm(list=ls())
source('./R/functions-FGM-Simulations.R')

######################################
#' Create figures directories if they
#' do not already exist
simulationDirectoryExists  <-  dir.exists("./out")

if(!simulationDirectoryExists) {
	dir.create("./out")
}

# Established mutations, small mutation size limit
# Panel (A) -- R_f ~ x 
relBalancingMutSize_EstabMuts_Sims(Ne = 10^5, n = 50, z = 1, h = 1/2, reps=10^5, writeFile=TRUE)
relBalancingMutSize_EstabMuts_Sims(Ne = 10^6, n = 50, z = 1, h = 1/2, reps=10^5, writeFile=TRUE)
# Panel (B) -- R_f ~ F 
relBalancingSmallx_EstabMuts_F_Sims(Ne = 10^5, n = 50, z = 1, h = 1/2, reps=10^5, writeFile=TRUE)
relBalancingSmallx_EstabMuts_F_Sims(Ne = 10^6, n = 50, z = 1, h = 1/2, reps=10^5, writeFile=TRUE)

# Established mutations, variable mutation size
# R_f ~ F, showing both large- and small-x approximations
# with simulation results overlaid
relBalancingMutSize_variable_x_EstabMuts_Sims(Ne = 10^5, n = 50, z = 1, h = 1/2, sim.reps=10^4, writeFile = TRUE)
relBalancingMutSize_variable_x_EstabMuts_Sims(Ne = 10^6, n = 50, z = 1, h = 1/2, sim.reps=10^4, writeFile = TRUE)
