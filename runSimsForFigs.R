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
relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 0.05, Ne = 10^5, n = 50, z = 1, h = 1/2, sim.reps=10^5, writeFile = TRUE)
relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 5, Ne = 10^5, n = 50, z = 1, h = 1/2, sim.reps=10^5, writeFile = TRUE)


# Generate data for bivariate distribution 
# of s.het, s.hom for established mutations
BivariateSelFigSims_EstabMuts(xAvg=0.05, Ne = 10^5, n = 50, z = 1, h = 1/2, sim.reps=10^4, writeFile = TRUE)
BivariateSelFigSims_EstabMuts(xAvg=2, Ne = 10^5, n = 50, z = 1, h = 1/2, sim.reps=10^4, writeFile = TRUE)
BivariateSelFigSims_EstabMuts(xAvg=5, Ne = 10^5, n = 50, z = 1, h = 1/2, sim.reps=10^4, writeFile = TRUE)


# Generate data for V_G contributed by established mutations
VG_variable_x_EstabMuts_Sims(xBar=1,Ne = 1000, n = 50, z = 1, h = 1/2, nMuts=10^7, writeFile=TRUE)
VG_variable_x_EstabMuts_Sims(xBar=5,Ne = 1000, n = 50, z = 1, h = 1/2, nMuts=10^7, writeFile=TRUE)


# Generate data for R_estab. with variable phenotypic dominance
bs  <-  c(0.5, 1, 1.5, 5, 50)
for(i in 1:length(bs)) {
	relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 5, Ne = 10^5, n = 50, z = 1, h = 1/2, variableDom = TRUE, b=bs[i], sim.reps=10^5, writeFile = TRUE)
}

relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 5, Ne = 10^5, n = 50, z = 1, h = 1/2, variableDom = TRUE, b=100, sim.reps=10^5, writeFile = TRUE)



# Generate data for R_estab. with variable phenotypic dominance
ns  <-  c(20, 10, 5, 2)
for(i in 1:length(ns)) {
	relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 5, Ne = 10^5, n = ns[i], z = 1, h = 1/2, variableDom = FALSE, b=5, sim.reps=10^5, writeFile = TRUE)
}
	relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 5, Ne = 10^5, n = 2, z = 1, h = 1/2, variableDom = FALSE, b=5, sim.reps=10^5, writeFile = TRUE)

