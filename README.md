# Effects of inbreeding on the probability of balancing selection: Insights from Fisher’s geometric model 

## Overview

This is the GitHub repository for a theoretical population genetics research project with working title "*Effects of inbreeding on the probability of balancing selection: Insights from Fisher’s geometric model*" (doi to be provided after acceptance). Here you can find all of the computer code necessary to reproduce the simulations and main figures presented in the published paper and appendices. The most current Version of Record is archived on Zenodo here: [![DOI](https://zenodo.org/badge/535606024.svg)](https://doi.org/10.5281/zenodo.14000423). 


## Abstract

Balancing selection is a potentially important factor in the maintenance of genetic variation for fitness and the genetic basis of inbreeding depression. Classic population genetics theory predicts that inbreeding restricts the range of conditions leading to balancing selection. For example, in models of heterozygote advantage, the classic theory shows that inbreeding reduces the parameter conditions for balancing selection by a factor of $1 – F$, where $F$ is Wright’s inbreeding coefficient. However, without a model for the distribution of fitness effects of mutations or genotypes, this classic theory tells us little about the actual probability that new or segregating mutations meet criteria for balancing selection. Here, we develop an extension of Fisher’s geometric model for nonrandomly mating populations and explore how inbreeding affects the probability of balancing selection by heterozygote advantage. Accounting for the distribution of fitness effects among new mutations, adaptive mutations, and established mutations, we find that the prevalence of balancing selection is consistently, and often substantially, below the $1 – F$ baseline implied by classic theory. The reduction is consistently greater for established mutations relative to new adaptive mutations, which strongly reinforces the idea that balanced genetic polymorphisms are far more likely to occur in outbred than inbred species.



## Citing information

*Citation info will be provided after the resulting article has passed through peer review and been accepted for publication*:


##  Instructions

This repository provides all code necessary to (1) rerun the simulations and (2) produce figures as .pdf's. To do so, please follow these basic steps:

1. Clone the repo using the following: `git clone https://https://github.com/colin-olito/FGM-BalancingSel-Selfers`. Alternatively, on the project main page on GitHub, click on the green button `clone` or `download` and then click on `Download ZIP`.  
2. Check that you have a recent version of [`R`](https://www.r-project.org/) installed. 
3. Make sure that the working directory for your R session is the root directory of this repo (e.g., `FGM-BalancingSel-Selfers-master/`).
4. Run `./runSimsForFigs.R` either interactively in R or via terminal. This will generate output .csv files for some of the simulations that take some time to run.
5. *Note*: We use Times New Roman fonts in the figures. Other fonts, including Computer Modern are possible, but use CM you need to correctly install the `R` font packages `extrafont` and `fontcm`. Alternatively, comment out L.4-5 in `./R/functions-figures`, and change the default font family to whatever you like by editing the font family on L.24 & L.25.
7. Run `makeFigs.R` up to L.64 (to produce only figs in the final paper) or in its entirety (to produce all supplementary and exploratory figures).  



## Repository structure and contents 

The directories/files in this repository needed to reproduce the results for this study are as follows:  

- **`R`**   
	- `functions-figures.R`  
	- `functions-FGM-Simulations.R`  
- **`out`***  
- **`figs`***  
- `makeFigs.R`  
- `runSimsForFigs.R`  
- `LICENSE.txt`   

**Note:** * `out` and `figs` directories will be created locally the first time `runSimsForFigs.R`  and `makeFigs.R` are run (*if needed*).


### File & variable descriptions

Plotting function files
- `./R/functions-figures.R`: plotting functions for figures.  

Simulation function files
- `./R/functions-FGM-Simulations.R`: simulation functions needed to generate figures.   

Executables
- `runSimsForFigs.R`: executable functions to perform W-F simulations for established mutations.   
- `makeFigs.R`: executable functions to create .pdf figures using simulation output files.

License    
- `LICENSE.txt`: MIT license for this repository.  


## Contact & bug reporting

Please report any bugs, problems, or issues by opening an issue on the inversionSize github [issues page](https://github.com/colin-olito/FGM-Balancing-Selfers/issues). If you would like to report a bug/issue, and do not have a github account (and don't want to get one), please send a brief email detailing the problem you encountered to colin.olito at biol dot lu dot se.



## Licence information

This repository is provided by the authors under the MIT License ([MIT](https://opensource.org/licenses/MIT)).