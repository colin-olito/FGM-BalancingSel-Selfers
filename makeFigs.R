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
#'		  figures for the article
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

# Fig. 1 -- Illustration of weak-selection classical results
toPdf(classicWkSel_VG_Fig(), 
			figPath(name='classicWkSel_VG_Fig.pdf'), width=10, height=6)
embed_fonts(figPath(name='RelBalancingFig-FavMuts.pdf'))


 
# Fig. 2 -- Balancing selection among new mutations: 
# (A) Pr(bal | x) ~ mutation size (x)
# (B) Relative proportion of new mutations under 
#	  as a function of inbreeding coefficient (F)
#	  balancing selection (R_new ~ F)
toPdf(BalSelNewMutationsFig(xAvg = 5, inclSmallMuts=FALSE), 
			figPath(name='BalSelNewMutationsFig.pdf'), width=10, height=8)
embed_fonts(figPath(name='BalSelNewMutationsFig.pdf'))


# Fig. 3 -- Bivariate distribution of selection coefficents 
# for new mutations(using FGM parameterization)
toPdf(BivariateSelFig(xAvg = 5, n = 50, z = 1, h = 1/2, reps=10^4, coeffs="FGM", yLim=c(-0.10, 0.15), xLim=c(-0.6, 0.12)), 
			figPath(name='BivariateSelFigFGM_xAvg5_z1_h0.5.pdf'), width=15, height=10)
embed_fonts(figPath(name='BivariateSelFigFGM_xAvg5_z1_h0.5.pdf'))


# Fig. 4 -- Relative fraction of balancing selection among favored mutations
#  R_adapt for the variable mutation sizes (Eq 9), w/ simulations
toPdf(RelBalancingFavored_Fig(xAvg = 5, favouredMuts=TRUE), 
			figPath(name='RelBalancingFig-FavMuts.pdf'), width=6, height=6)
embed_fonts(figPath(name='RelBalancingFig-FavMuts.pdf'))


# Fig. 5 -- Comparison of Relative fraction of balancing selection  
#			among favored vs. established mutations (R_adapt vs. R_estab), 
#			for variable mutation sizes
toPdf(RelBalancing_SummaryFig_wVG(xAvg = 5, Ne=10^6, sim.reps=10^7), 
			figPath(name='RelBalancingEstab_wVg_Fig.pdf'), width=10, height=6)
embed_fonts(figPath(name='RelBalancingEstab_wVg_Fig.pdf'))


# Fig. 6 -- Bivariate distribution of selection coefficents 
# for established mutations
toPdf(BivariateSelFig_EstabMuts(datafile = "./out/BivariateSelFigSims_EstabMuts_xAvg5_Ne1e+05_n50_z1_h0.5_reps10000.csv", coeffs = "FGM", xLim=NA, yLim=NA), 
			figPath(name='BivariateSelFigFGM_xAvg5_z1_h0.5_Estab.pdf'), width=12, height=8)
embed_fonts(figPath(name='BivariateSelFigFGM_xAvg5_z1_h0.5_Estab.pdf'))



########################
# Supplementary Figures
########################

# Fig. S1 -- Original classical parameter space figure
# (A) Weak selection invasion boundaries for het. adv.
# (B) Arb. selection inv. boundaries (Kimura & Ohta 1971)
toPdf(classicParamSpaceFig(), 
			figPath(name='classicParamSpaceFig2.pdf'), width=11, height=8)
embed_fonts(figPath(name='classicParamSpaceFig2.pdf'))

# Fig. S2 -- Relative proportion of parameter space resulting in
# balancing selection for new mutations. 
# (A) R_new ~ mut. size
# (B) R_new ~ F, for small-x
toPdf(RelBalancingFig(favouredMuts=FALSE), 
			figPath(name='RelBalancingFig-AllMuts.pdf'), width=10, height=7)
embed_fonts(figPath(name='RelBalancingFig-AllMuts.pdf'))


# Fig. S3 - Balancing selection summary figure for new mutations
# (A) Pr(Bal | x) ~ mutation size
# (B) R_new ~ F for both small- and variable-mutation size limits
toPdf(BalSelNewMutationsFig(xAvg = 5, inclSmallMuts=TRUE), 
			figPath(name='BalSelNewMutationsFig-wSmall.pdf'), width=10, height=8)
embed_fonts(figPath(name='BalSelNewMutationsFig-wSmall.pdf'))

# Fig. S4 -- Balancing selection for adaptive mutations
# (A) R_adapt ~ F for small-x
# (B) R_adapt ~ F for variable-x
toPdf(RelBalancingFavored_wSmall_Fig(xAvg = 5, favouredMuts=TRUE), 
			figPath(name='RelBalancingFig-FavMuts-wSmall.pdf'), width=10, height=8)


# Fig. S5 -- Bivariate distribution of selection coeffs. for new mutations (classical parameterization) 
toPdf(BivariateSelFig(xAvg = 5, n = 50, z = 1, h = 1/2, reps=10^4, coeffs="CLASSIC", xLim=c(-0.4,0.2), yLim=c(-0.05,0.8)), 
			figPath(name='BivariateSelFigClassic_xAvg5_z1.0.pdf'), width=15, height=10)
embed_fonts(figPath(name='BivariateSelFigClassic_xAvg5_z1.0.pdf'))

# Fig. S6 -- Bivariate distribution of selection coeffs. for new mutations (FGM parameterization) 
#			 with recessive phenotypic effects (v = 0.1) 
toPdf(BivariateSelFig(xAvg=5, n = 50, z = 1, h = 1/10, reps=10^4, coeffs="FGM", yLim=c(-0.1, 0.15), xLim=NA), 
			figPath(name='BivariateSelFigFGM_xAvg5_z1_h0.1.pdf'), width=15, height=10)
embed_fonts(figPath(name='BivariateSelFigFGM_xAvg5_z1_h0.1.pdf'))

# Fig. S7 -- R_bal ~ F summary figure for both small-x and variable-x limits
toPdf(RelBalancing_SummaryFig(Ne=10^6, sim.reps=10^6), 
			figPath(name='RelBalancing_SummaryFig.pdf'), width=7, height=7)
#embed_fonts(figPath(name='RelBalancing_SummaryFig.pdf'))


# Fig. S8 -- Bivariate distribution of selection coefficients for established mutations (Classic param.)
toPdf(BivariateSelFig_EstabMuts(datafile = "./out/BivariateSelFigSims_EstabMuts_xAvg5_Ne1e+05_n50_z1_h0.5_reps10000.csv", coeffs = "CLASSIC", xLim=c(-0.01,0.2), yLim=c(-0.1,0.8)), 
			figPath(name='BivariateSelFigClassic_xAvg5_z1_h0.5_Estab.pdf'), width=12, height=8)
embed_fonts(figPath(name='BivariateSelFigClassic_xAvg5_z1_h0.5_Estab.pdf'))

# Fig. S9 -- Summary of effects underlying the decline with inbreeding
#			 of balancing selection among established relative to new adaptive
#			 mutations (variable-x)
toPdf(Enrichment_EstabMutsFig(xAvg=5), 
			figPath(name='Enrichment_Estab_FavMuts_Fig_xAvg5_z1_h0.5_Estab.pdf'), width=6, height=9)
embed_fonts(figPath(name='Enrichment_Estab_FavMuts_Fig_xAvg5_z1_h0.5_Estab.pdf'))




########################
# Exploratory figures
########################

# Note: the remainder of this file are functions to generate
#		exploratory plots that didn't end up in the final 
#		paper. Some may be deprecated.

# Relative proportion of parameter space resulting in
# balancing selection for: 
# (1) new mutations
toPdf(RelBalancingFig(favouredMuts=FALSE), 
			figPath(name='RelBalancingFig-AllMuts.pdf'), width=10, height=7)
embed_fonts(figPath(name='RelBalancingFig-AllMuts.pdf'))

# (2) Adaptive mutations
toPdf(RelBalancingFig(favouredMuts=TRUE), 
			figPath(name='RelBalancingFig-FavMuts.pdf'), width=10, height=8)
embed_fonts(figPath(name='RelBalancingFig-FavMuts.pdf'))

# (3) Established nutations
toPdf(RelBalancingEstablishedFig(), 
			figPath(name='RelBalancingFig-EstMuts.pdf'), width=10, height=8)
embed_fonts(figPath(name='RelBalancingFig-EstMuts.pdf'))


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

# "Moonrise over flatland"
toPdf(Fisher_2D_ExploreFig(z=0.01, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.01.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.01.pdf'))

toPdf(Fisher_2D_ExploreFig(z=0.05, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.05.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.05.pdf'))

toPdf(Fisher_2D_ExploreFig(z=0.1, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.1.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.1.pdf'))

toPdf(Fisher_2D_ExploreFig(z=0.15, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.15.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.15.pdf'))

toPdf(Fisher_2D_ExploreFig(z=0.2, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.2.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.2.pdf'))

toPdf(Fisher_2D_ExploreFig(z=0.25, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.25.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.25.pdf'))

toPdf(Fisher_2D_ExploreFig(z=0.3, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.3.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.3.pdf'))

toPdf(Fisher_2D_ExploreFig(z=0.4, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.4.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.4.pdf'))

toPdf(Fisher_2D_ExploreFig(z=0.5, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.5.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.5.pdf'))

toPdf(Fisher_2D_ExploreFig(z=0.75, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.75.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.75.pdf'))


# Moonrise over flatland for larger initial displacements.
# Things get weird.
toPdf(Fisher_2D_ExploreFig(z=0.75, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreFig_z0.75.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreFig_z0.75.pdf'))

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



# Supplementary Figure - backtransforming "Moonrise" 
# to classic heterozygote advantage parameterization
# and visualizing on Kimura-Ohta invasion plot
toPdf(Fisher_2D_Explore_KimuraPlotFig(z = 0.05, h = 1/2, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreKimuraFig_z0.05.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreKimuraFig_z0.05.pdf'))

toPdf(Fisher_2D_Explore_KimuraPlotFig(z = 0.5, h = 1/2, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreKimuraFig_z0.5.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreKimuraFig_z0.5.pdf'))

toPdf(Fisher_2D_Explore_KimuraPlotFig(z = 1, h = 1/2, reps=5*10^4), 
			figPath(name='Fisher_2D_ExploreKimuraFig_z1.0.pdf'), width=15, height=10)
embed_fonts(figPath(name='Fisher_2D_ExploreKimuraFig_z1.0.pdf'))


# Possible Supplementary Figure - Bivariate Distribution of Selection coefficients
## Using Classic parameterization with s1, s2
toPdf(BivariateSelFig(xAvg = 2, n = 50, z = 0.05, h = 1/2, reps=10^4, coeffs="CLASSIC", xLim=c(-0.002,0.002), yLim=c(-0.0005,0.005)), 
			figPath(name='BivariateSelFigClassic_z0.05.pdf'), width=15, height=10)
embed_fonts(figPath(name='BivariateSelFigClassic_z0.05.pdf'))

toPdf(BivariateSelFig(xAvg = 2, n = 50, z = 1, h = 1/2, reps=10^4, coeffs="CLASSIC", xLim=c(-0.4,0.2), yLim=c(-0.05,0.8)), 
			figPath(name='BivariateSelFigClassic_z1.0.pdf'), width=15, height=10)
embed_fonts(figPath(name='BivariateSelFigClassic_z1.0.pdf'))

## Using FGM parameterization with s.het, s.hom
toPdf(BivariateSelFig(xAvg=2, n = 50, z = 0.05, h = 1/2, reps=10^4, coeffs="FGM", xLim=c(-0.002,0.00025), yLim=c(-0.0005,5e-4)), 
			figPath(name='BivariateSelFigFGM_z0.05_h0.5.pdf'), width=15, height=10)
embed_fonts(figPath(name='BivariateSelFigFGM_z0.05_h0.5.pdf'))

toPdf(BivariateSelFig(xAvg=2, n = 50, z = 1, h = 1/2, reps=10^4, coeffs="FGM", yLim=c(-0.3, 0.15), xLim=NA), 
			figPath(name='BivariateSelFigFGM_z1_h0.5.pdf'), width=15, height=10)
embed_fonts(figPath(name='BivariateSelFigFGM_z1_h0.5.pdf'))

toPdf(BivariateSelFig(xAvg=2, n = 50, z = 1, h = 1/10, reps=10^4, coeffs="FGM", yLim=c(-0.2, 0.15), xLim=NA), 
			figPath(name='BivariateSelFigFGM_z1_h0.1.pdf'), width=15, height=10)
embed_fonts(figPath(name='BivariateSelFigFGM_z1_h0.1.pdf'))



# Possible Supplementary Figure - Bivariate Distribution of Selection coefficients
## For ESTABLISHED MUTATIONS - Using Classic parameterization with s1, s2
toPdf(BivariateSelFig_EstabMuts(datafile = "./out/BivariateSelFigSims_EstabMuts_xAvg2_Ne1e+05_n50_z1_h0.5_reps10000.csv", coeffs = "CLASSIC", xLim=c(-0.01,0.2), yLim=c(-0.1,0.8)), 
			figPath(name='BivariateSelFigClassic_z1_h0.5_Estab.pdf'), width=12, height=8)
embed_fonts(figPath(name='BivariateSelFigClassic_z1_h0.5_Estab.pdf'))

toPdf(BivariateSelFig_EstabMuts(datafile = "./out/BivariateSelFigSims_EstabMuts_xAvg2_Ne1e+05_n50_z1_h0.5_reps10000.csv", coeffs = "FGM", xLim=NA, yLim=NA), 
			figPath(name='BivariateSelFigFGM_z1_h0.5_Estab.pdf'), width=12, height=8)
embed_fonts(figPath(name='BivariateSelFigFGM_z1_h0.5_Estab.pdf'))

toPdf(Enrichment_EstabMutsFig(xAvg=5), 
			figPath(name='Enrichment_Estab_FavMuts_Fig_xAvg5_z1_h0.5_Estab.pdf'), width=6, height=9)
embed_fonts(figPath(name='Enrichment_Estab_FavMuts_Fig_xAvg5_z1_h0.5_Estab.pdf'))



# Playing with looking at the distribution of seleciton coefficients and dominance. Meh.
toPdf(SelectionAndDominanceFig(xAvg=2, n = 50, z = 1, h = 1/2, reps = 10^4, estMuts = FALSE, datafile = "./out/BivariateSelFigSims_EstabMuts_xAvg2_Ne1e+05_n50_z1_h0.5_reps10000.csv"), 
			figPath(name='SelectionAndDominanceFig_z1_h0.5_New.pdf'), width=12, height=8)
embed_fonts(figPath(name='SelectionAndDominanceFig_z1_h0.5_New'))

toPdf(SelectionAndDominanceFig(xAvg=2, n = 50, z = 1, h = 1/2, reps = 10^4, estMuts = TRUE, datafile = "./out/BivariateSelFigSims_EstabMuts_xAvg2_Ne1e+05_n50_z1_h0.5_reps10000.csv"), 
			figPath(name='SelectionAndDominanceFig_z1_h0.5_Estab.pdf'), width=12, height=8)
embed_fonts(figPath(name='SelectionAndDominanceFig_z1_h0.5_Estab.pdf'))

