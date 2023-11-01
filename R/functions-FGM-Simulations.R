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

# Relative probability of balancing selection ~ Mutation size
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


# Relative probability of balancing selection ~ Inbreeding Coefficient (F)
# infinitesimal mutation size limit
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







# Exploration of parameter space in 2-dimensions
# showing where we get balancing selection as we
# vary mutation size and F.

# Ancillary funcitons
hetAdvCondInbreeding <-  function(F, t) {
#	S  <-  2*F/(F + 1)
#	2*t*(1 − t − S/2) / S*(1 − 2*t)
	t*(F*t + t - 1) / (F*(2*t - 1))
}
hetAdvCondInbreeding2 <-  function(F, s.het, s.hom) {
	cond1  <-  (s.het > 0 & s.hom > 0)
	cond2  <-  s.het > (F*s.hom / (1 - F*(1 + s.hom)))
	cond3  <-  s.het > s.hom*(1 + s.hom*(1 + F)) / (1 - F + s.hom)
	(cond1 == 1 & (cond2 + cond3) == 2)
}

# parameters
# n = 50   --  no. dimensions
# z = 1    --  wild-type displacement from optimum
# h = 0.5  --  dominance
Fisher_2D_ExploreFigSims  <-  function(z = 1, h = 1/2, reps=100, ...) {

	# Constrain to n = 2 dimensions
	n     <-  2
	# Initial wild-type phenotype
	A.wt  <-  c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt   <-  rep(0, n) 
	# Inbreeding values
	F.I         <-  1:5/5

	# Vector for output values
	PosSel.res      <-  c() #rep(0, times=length(F.I)*length(Fisher.x)*reps)
	BalSel.out.res  <-  c() #rep(0, times=length(F.I)*length(Fisher.x)*reps)
	BalSel.F.res    <-  c() #rep(0, times=length(F.I)*length(Fisher.x)*reps)
	x.res           <-  c()
	F.res           <-  c()
	z.hom.raw.1.res   <-  c()
	z.hom.raw.2.res   <-  c()
	# loop over inbreeding values
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F  <-  F.I[i]
		# Mutation size
		Fisher.x  <-  runif(0.05, 5, n = reps)
		r         <-  2*z*Fisher.x/sqrt(n)
		# random mutations
		muts   <-  matrix(data=rnorm(n*reps), nrow=reps, ncol=n)
		# homo-zygote phenotypes in 2-d space
		z.hom.raw  <-  matrix(0,nrow=reps, ncol=2)
		z.het  <-  rep(0, times=reps)
		z.hom  <-  rep(0, times=reps)
		for(j in 1:reps) {
			z.hom.raw[j,] <-  A.wt + r[j]*muts[j,]/sqrt(sum(muts[j,]^2))
			z.het[j]      <-  sqrt(sum((A.wt + r[j]*h*muts[j,]/sqrt(sum(muts[j,]^2)) - Opt)^2))
			z.hom[j]      <-  sqrt(sum((A.wt + r[j]*muts[j,]/sqrt(sum(muts[j,]^2)) - Opt)^2))
		}
		z.hom.raw.1.res   <-  c(z.hom.raw.1.res, z.hom.raw[,1])
		z.hom.raw.2.res   <-  c(z.hom.raw.2.res, z.hom.raw[,2])
		# het-/homo-zygote phenotypes
		# het-/homo-zygote selection coefficients
		s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
		s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
		t.wt   <-  exp(-0.5*z^2)/exp(-0.5*z.het^2) - 1
		t.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z.het^2) - 1
		# Do selection coefficients result in balancing selection?
		# outcrossing
		out.cond1    <-  as.numeric(s.hom < s.het)
		out.cond2    <-  as.numeric(s.het > 0)
		pos.cond     <-  as.numeric(s.hom > s.het)
		PosSel       <-  as.numeric((out.cond2 + pos.cond) == 2)
		BalSel.out   <-  as.numeric((out.cond1 + out.cond2) == 2)
		# inbreeding
#		cond1     <-  as.numeric(t.wt  < 0 & t.hom  < 0)
#		cond2     <-  as.numeric(-t.wt  > hetAdvCondInbreeding(F=F, t=-t.hom))
#		cond3     <-  as.numeric(-t.hom > hetAdvCondInbreeding(F=F, t=-t.wt))
#		BalSel.F  <-  as.numeric(cond1 == 1 & (cond2 + cond3) == 2)
		BalSel.F  <-  as.numeric(hetAdvCondInbreeding2(F=F, s.het=s.het, s.hom=s.hom))
		# Concatenate results
		x.res      <-  c(x.res, Fisher.x)
		F.res      <-  c(F.res, rep(F, times=length(BalSel.F)))
		PosSel.res      <-  c(PosSel.res, PosSel) #rep(0, times=length(F.I)*length(Fisher.x)*reps)
		BalSel.out.res  <-  c(BalSel.out.res, BalSel.out) #rep(0, times=length(F.I)*length(Fisher.x)*reps)
		BalSel.F.res    <-  c(BalSel.F.res, BalSel.F) #rep(0, times=length(F.I)*length(Fisher.x)*reps)
	}
	# Output dataframe
	res.df  <-  data.frame("F"            =  F.res,
						   "z.hom.raw.1"  =  z.hom.raw.1.res, 
						   "z.hom.raw.2"  =  z.hom.raw.2.res, 
						   "PosSel"       =  PosSel.res,
						   "BalSel.out"   =  BalSel.out.res,
						   "BalSel.F"     =  BalSel.F.res)
	# return results
	return(res.df)
}

########### CONTINUE EDITING HERE ###########
