#'  Functions to perform simulations 
#'    
#'  Author: Colin Olito
#'
#'
#'  NOTES: 
#'


###############################################
# FGM simulations for classical weak-selection
# approximation (i.e., high-dimensional FGM)
###############################################

# parameters
# n = 50   --  no. dimensions
# z = 1    --  wild-type displacement from optimum
# h = 0.5  --  dominance
relBalancingMutSize_Sims  <-  function(n = 50, z = 1, F = 1/2, h = 1/2, reps=10^5) {

	# Initial wild-type phenotype
	A.wt = c(-z, rep(0, (n - 1))) 
#	A.wt = rep(-z, (n - 1))
	# Phenotypic optimum at 0
	Opt = rep(0, n) 
	# Vector of mutation sizes
	Fisher.x = c(0.05, seq(0.25, 5, by = 0.25))
	absolute.r = 2*z*Fisher.x/sqrt(n)

	# Vector for output values
	rBal  <-  rep(0, times=length(Fisher.x))
	for(i in 1:length(Fisher.x)){
		# current mutation size
		r = absolute.r[i]
	    # random mutations
	    muts   <-  matrix(data=rnorm(n*reps), nrow=reps, ncol=n)
	        # het-/homo-zygote phenotypes
	    z.het  <-  apply(muts, MARGIN=1, function(x) {sqrt(sum((A.wt + r*h*x/sqrt(sum(x^2)) - Opt)^2))})
		z.hom  <-  apply(muts, MARGIN=1, function(x) {sqrt(sum((A.wt + r*x/sqrt(sum(x^2)) - Opt)^2))})
	    # het-/homo-zygote selection coefficients
	    s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
	    s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
	    # Do selection coefficients result in balancing selection?
	    # outcrossing
	    out.cond1  <-  as.numeric(s.hom < s.het)
	    out.cond2  <-  as.numeric(s.het > 0)
	    PrBal      <-  sum((out.cond1 + out.cond2) == 2) / reps
	    # inbreeding
	    cond1    <-  as.numeric(-F*s.hom < (1 - F)*s.het)
	    cond2    <-  as.numeric((1 - F)*s.het > s.hom)
		PrBal.F  <-  sum((cond1 + cond2) == 2) / reps
		# Relative probability of balancing selection
		rBal[i]  <-  PrBal.F/PrBal
	}

	res.df  <-  data.frame("x" = Fisher.x,
						   "rBal" = rBal)
	return(res.df)
}



# parameters
# n = 50   --  no. dimensions
# z = 1    --  wild-type displacement from optimum
# h = 0.5  --  dominance
relBalancingSmallx_F_Sims  <-  function(n = 50, z = 1, h = 1/2, reps=10^5) {

	# Initial wild-type phenotype
	A.wt = c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt = rep(0, n) 
	# Mutation size
	Fisher.x = 0.05
	absolute.r = 2*z*Fisher.x/sqrt(n)
	r  <-  absolute.r
	# Inbreeding values
	F.I  <-  1:20/20

	# Vector for output values
	rBal  <-  rep(0, times=length(F.I))
	# random mutations
	muts   <-  matrix(data=rnorm(n*reps), nrow=reps, ncol=n)
	# het-/homo-zygote phenotypes
	z.het  <-  apply(muts, MARGIN=1, function(x) {sqrt(sum((A.wt + r*h*x/sqrt(sum(x^2)) - Opt)^2))})
	z.hom  <-  apply(muts, MARGIN=1, function(x) {sqrt(sum((A.wt + r*x/sqrt(sum(x^2)) - Opt)^2))})
	# het-/homo-zygote selection coefficients
	s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
	s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
	# Do selection coefficients result in balancing selection?
	# outcrossing
	out.cond1  <-  as.numeric(s.hom < s.het)
	out.cond2  <-  as.numeric(s.het > 0)
	PrBal      <-  sum((out.cond1 + out.cond2) == 2) / reps
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F = F.I[i]
	    # inbreeding
	    cond1    <-  as.numeric(-F*s.hom < (1 - F)*s.het)
	    cond2    <-  as.numeric((1 - F)*s.het > s.hom)
		PrBal.F  <-  sum((cond1 + cond2) == 2) / reps
		# Relative probability of balancing selection
		rBal[i]  <-  PrBal.F/PrBal
	}
	# Output dataframe
	res.df  <-  data.frame("F" = F.I,
						   "rBal" = rBal)
	# return results
	return(res.df)
}
