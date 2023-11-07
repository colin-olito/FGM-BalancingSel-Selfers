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
KO_InvCond_up  <-  function(s1, S) {
  (s1*(2 - S - 2*s1))/(S*(1 - 2*s1))
}
KO_InvCond_low  <-  function(s1, S) {
  (1/2)*(1 - sqrt(1 - S + (S^2)*(1/2 - s1)^2) - S*(1/2 - s1))
}
# Ancillary funcitons
hetAdvCondInbreeding <-  function(F, t1, t2) {
	S  <-  (2*F)/(F + 1)
#	2*t*(1 − t − S/2) / S*(1 − 2*t)
	cond1  <-  (t1 < 0 & t2 < 0)
	t1[!cond1]  <-  NA
	t2[!cond1]  <-  NA
	t1          <-  abs(t1)
	t2          <-  abs(t2)
	cond2  <-  ( t2 <  (t1*(2 - S - 2*t1))/(S*(1 - 2*t1)) )
	cond3  <-  ( t2 >  (1/2)*(1 - sqrt(1 - S + (S^2)*(1/2 - t1)^2) - S*(1/2 - t1)) )
#	cond2  <-  (t1 < (t2*(F*t2 + t2 - 1) / (F*(2*t2 - 1))))
#	cond3  <-  (t2 < (t1*(F*t1 + t1 - 1) / (F*(2*t1 - 1))))
#	cond2  <-  (t1 > ((F + 1)*(-sqrt((F*(F*(4*(t2 - 1)*t2 + 3) + 2))/((F + 1)^2) ))+2*F*t2+1) / (2*(F+1)))
#	cond3  <-  (t2 > ((F + 1)*(-sqrt((F*(F*(4*(t1 - 1)*t1 + 3) + 2))/((F + 1)^2) ))+2*F*t1+1) / (2*(F+1)))
	(cond1 == 1 & (cond2 + cond3) == 2)
}
hetAdvCondInbreeding2 <-  function(F, s.het, s.hom) {
	cond1  <-  (s.het > 0)
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
	F.I         <-  c(0.2, 0.4, 0.6, 0.8, 0.98)

	# Vector for output values
	PosSel.out.res  <-  c() 
	BalSel.out.res  <-  c() 
	PosSel.F.res    <-  c() 
	BalSel.F.res    <-  c() 
	invade.F.res    <-  c() 
	x.res           <-  c()
	F.res           <-  c()
	z.hom.raw.1.res   <-  c()
	z.hom.raw.2.res   <-  c()
	s.het.res   <-  c()
	s.hom.res   <-  c()
	t.wt.res   <-  c()
	t.hom.res   <-  c()
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
		# calculate mutation-specific displacement values
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
		out.cond1   <-  as.numeric(s.hom < s.het)
		out.cond2   <-  as.numeric(s.het > 0)
		pos.cond    <-  as.numeric(s.hom > s.het)
		PosSel.out  <-  as.numeric((out.cond2 + pos.cond) == 2)
		BalSel.out  <-  as.numeric((out.cond1 + out.cond2) == 2)
		# inbreeding
#		BalSel.F    <-  as.numeric(hetAdvCondInbreeding2(F=F, s.het=s.het, s.hom=s.hom))
		BalSel.F    <-  as.numeric(hetAdvCondInbreeding(F=F, t1=t.wt, t2=t.hom))
		PosSel.F    <-  as.numeric(((1 - F)*s.het + F*s.hom) > 0 & BalSel.F == FALSE)
		invade.F    <-  as.numeric(((1 - F)*s.het + F*s.hom) > 0)
		# Concatenate results
		x.res           <-  c(x.res, Fisher.x)
		F.res           <-  c(F.res, rep(F, times=length(BalSel.F)))
		PosSel.out.res  <-  c(PosSel.out.res, PosSel.out) #rep(0, times=length(F.I)*length(Fisher.x)*reps)
		BalSel.out.res  <-  c(BalSel.out.res, BalSel.out) #rep(0, times=length(F.I)*length(Fisher.x)*reps)
		BalSel.F.res    <-  c(BalSel.F.res, BalSel.F) #rep(0, times=length(F.I)*length(Fisher.x)*reps)
		PosSel.F.res    <-  c(PosSel.F.res, PosSel.F) #rep(0, times=length(F.I)*length(Fisher.x)*reps)
		invade.F.res    <-  c(invade.F.res, invade.F) #rep(0, times=length(F.I)*length(Fisher.x)*reps)
		s.het.res       <-  c(s.het.res, s.het)
		s.hom.res       <-  c(s.hom.res, s.hom)
		t.wt.res        <-  c(t.wt.res, t.wt)
		t.hom.res       <-  c(t.hom.res, t.hom)
	}
	# Output dataframe
	res.df  <-  data.frame("F"            =  F.res,
						   "z.hom.raw.1"  =  z.hom.raw.1.res, 
						   "z.hom.raw.2"  =  z.hom.raw.2.res, 
						   "s.het"        =  s.het.res, 
						   "s.hom"        =  s.hom.res, 
						   "t.wt"         =  t.wt.res, 
						   "t.hom"        =  t.hom.res, 
						   "z.hom.raw.2"  =  z.hom.raw.2.res, 
						   "PosSel.out"   =  PosSel.out.res,
						   "BalSel.out"   =  BalSel.out.res,
						   "PosSel.F"     =  PosSel.F.res,
						   "invade.F"     =  invade.F.res,
						   "BalSel.F"     =  BalSel.F.res)
	# return results
	return(res.df)
}

########### CONTINUE EDITING HERE ###########
