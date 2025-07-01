#'  Run simulations with longer runtimes needed
#'  to generate figures
#'
#'  Author: Colin Olito
#'
#'  NOTES: Run this file, either from terminal using Rscript,
#' 		  or interactively in R. This should create simulation
#' 		  data needed for making figures in the mansucript.
#'

###############
# DEPENDENCIES
###############
rm(list = ls())
source("./R/functions-FGM-Simulations.R")

######################################
#' Create figures directories if they
#' do not already exist
simulationDirectoryExists <- dir.exists("./out")

if (!simulationDirectoryExists) {
  dir.create("./out")
}

# Established mutations, small mutation size limit
# Panel (A) -- R_f ~ x
relBalancingMutSize_EstabMuts_Sims(Ne = 10^5, n = 50, z = 1, h = 1 / 2, reps = 10^5, writeFile = TRUE)
relBalancingMutSize_EstabMuts_Sims(Ne = 10^6, n = 50, z = 1, h = 1 / 2, reps = 10^5, writeFile = TRUE)
# Panel (B) -- R_f ~ F
relBalancingSmallx_EstabMuts_F_Sims(Ne = 10^5, n = 50, z = 1, h = 1 / 2, reps = 10^5, writeFile = TRUE)
relBalancingSmallx_EstabMuts_F_Sims(Ne = 10^6, n = 50, z = 1, h = 1 / 2, reps = 10^5, writeFile = TRUE)

# Established mutations, variable mutation size
# R_f ~ F, showing both large- and small-x approximations
# with simulation results overlaid
relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 0.05, Ne = 10^5, n = 50, z = 1, h = 1 / 2, sim.reps = 10^5, writeFile = TRUE)
relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 5, Ne = 10^5, n = 50, z = 1, h = 1 / 2, sim.reps = 10^5, writeFile = TRUE)


# Generate data for bivariate distribution
# of s.het, s.hom for established mutations
BivariateSelFigSims_EstabMuts(xAvg = 0.05, Ne = 10^5, n = 50, z = 1, h = 1 / 2, sim.reps = 10^4, writeFile = TRUE)
BivariateSelFigSims_EstabMuts(xAvg = 2, Ne = 10^5, n = 50, z = 1, h = 1 / 2, sim.reps = 10^4, writeFile = TRUE)
BivariateSelFigSims_EstabMuts(xAvg = 5, Ne = 10^5, n = 50, z = 1, h = 1 / 2, sim.reps = 10^4, writeFile = TRUE)


# Generate data for V_G contributed by established mutations
VG_variable_x_EstabMuts_Sims(xBar = 1, Ne = 1000, n = 50, z = 1, h = 1 / 2, nMuts = 10^7, writeFile = TRUE)
VG_variable_x_EstabMuts_Sims(xBar = 5, Ne = 1000, n = 50, z = 1, h = 1 / 2, nMuts = 10^7, writeFile = TRUE)


# Generate data for R_estab. with variable phenotypic dominance
bs <- c(0.5, 1, 1.5, 5, 50)
for (i in 1:length(bs)) {
  relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 5, Ne = 10^5, n = 50, z = 1, h = 1 / 2, variableDom = TRUE, b = bs[i], sim.reps = 10^5, writeFile = TRUE)
}

relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 5, Ne = 10^5, n = 50, z = 1, h = 1 / 2, variableDom = TRUE, b = 100, sim.reps = 10^5, writeFile = TRUE)



# Generate data for R_estab. with variable phenotypic dominance
ns <- c(20, 10, 5, 2)
for (i in 1:length(ns)) {
  relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 5, Ne = 10^5, n = ns[i], z = 1, h = 1 / 2, variableDom = FALSE, b = 5, sim.reps = 10^5, writeFile = TRUE)
}
relBalancingMutSize_variable_x_EstabMuts_Sims(xAvg = 5, Ne = 10^5, n = 2, z = 1, h = 1 / 2, variableDom = FALSE, b = 5, sim.reps = 10^5, writeFile = TRUE)


# Generate simulation data for comparison of variable dominance with v ~ beta(),
# as well as different distributions of mutational effects
#%%

# Exponential vs. chi distribution supp fig.
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "exp", xAvg = 0.25), Ne = 10^5, n = 50, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "exp", xAvg = 2.5),  Ne = 10^5, n = 50, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "chi",    m = 0.01), Ne = 10^5, n = 100, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "chi",    m = 0.1),  Ne = 10^5, n = 50, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)

#%%

# Different distributions of v ~ beta supp fig.
# Less skewed beta distributions
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "exp", xAvg = 5), Ne = 10^5, n = 50, z = 1, vAvg = 1/4, variableDom = TRUE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "exp", xAvg = 5), Ne = 10^5, n = 50, z = 1, vAvg = 1/2, variableDom = TRUE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "exp", xAvg = 5), Ne = 10^5, n = 50, z = 1, vAvg = 3/4, variableDom = TRUE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
# More skewed beta distributions
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "exp", xAvg = 5), Ne = 10^5, n = 50, z = 1, vAvg = 1/4, variableDom = TRUE, sumAB = 2, estab.reps = 10^4, writeFile = TRUE)
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "exp", xAvg = 5), Ne = 10^5, n = 50, z = 1, vAvg = 1/2, variableDom = TRUE, sumAB = 2, estab.reps = 10^4, writeFile = TRUE)
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "exp", xAvg = 5), Ne = 10^5, n = 50, z = 1, vAvg = 3/4, variableDom = TRUE, sumAB = 2, estab.reps = 10^4, writeFile = TRUE)

#%%

# SAME, but with lower values of n = 3
# Exponential vs. chi distribution supp fig.
#R_bal_xDist_vBeta_Sims(x.dist = list(dist = "exp", xAvg = 0.25), Ne = 10^5, n = 3, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
#R_bal_xDist_vBeta_Sims(x.dist = list(dist = "exp", xAvg = 2.5),  Ne = 10^5, n = 3, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
#R_bal_xDist_vBeta_Sims(x.dist = list(dist = "chi",    m = 0.18), Ne = 10^5, n = 3, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
#R_bal_xDist_vBeta_Sims(x.dist = list(dist = "chi",    m = 1.8),  Ne = 10^5, n = 3, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "chi",    m = 0.281), Ne = 10^5, n = 2, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
R_bal_xDist_vBeta_Sims(x.dist = list(dist = "chi",    m = 2.81),  Ne = 10^5, n = 2, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
#R_bal_xDist_vBeta_Sims(x.dist = list(dist = "chi",    m = 0.4415), Ne = 10^5, n = 1, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)
#R_bal_xDist_vBeta_Sims(x.dist = list(dist = "chi",    m = 4.415),  Ne = 10^5, n = 1, z = 1, vAvg = 1/2, variableDom = FALSE, sumAB = 10, estab.reps = 10^4, writeFile = TRUE)

#%%