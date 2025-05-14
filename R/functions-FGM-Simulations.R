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
	# Phenotypic optimum at 0
	Opt = rep(0, n) 
	# Vector of mutation sizes
	Fisher.x = c(0.05, seq(0.25, 5, by = 0.5))
	absolute.r = 2*z*Fisher.x/sqrt(n)

	# Vector for output values
	rBal      <-  rep(0, times=length(Fisher.x))
	rBal.fav  <-  rep(0, times=length(Fisher.x))
	for(i in 1:length(Fisher.x)) {
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
		## What about among selectively favoured mutations?
	    # outcrossing
	    PrInv     <-  sum(out.cond2 == 1) / reps
	    fBal.fav  <-  PrBal / PrInv
	    # inbreeding
		PrInv.F     <-  sum(cond1 == 1) / reps
		fBal.F.fav  <-  PrBal.F / PrInv.F
		# Relative probability of balancing selection
		rBal[i]  <-  PrBal.F/PrBal
		rBal.fav[i]  <-  fBal.F.fav/fBal.fav
	}

	res.df  <-  data.frame("x"         =  Fisher.x,
						   "rBal"      =  rBal,
						   "rBal.fav"  =  rBal.fav)
	return(res.df)
}


# Simulate relative probability of balancing selection (R_bal) ~ Inbreeding Coefficient (F)
# For either small- or large-mutation limit
# parameters
# n = 50   --  no. dimensions
# z = 1    --  wild-type displacement from optimum
# h = 0.5  --  dominance
# reps = 10^5  --  number of mutations to simulate
# largeMut = FALSE -- Should new mutations all be small, or drawn from uniform distribution over x in (0,5)
# variableDom = FALSE -- draw phenotypic dominance values from a beta distribution with E(h^2) = 1/2 (i.e., E[h] = 1/2?
# b = 5 -- 2nd shape parameter for Beta distribution; assume a = b/3, which forces E[h^2] = 1/4. 
#			possible values for b = {0.5, 1, 1.5, 5, 50}, which span possible distribution shapes
relBalancing_F_Sims  <-  function(xAvg = 1, n = 50, z = 1, h = 1/2, reps=10^5, largeMut = FALSE, variableDom = FALSE, b=5) {

	# Initial wild-type phenotype
	A.wt = c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt = rep(0, n) 
	# Mutation size
	if(largeMut) {
		Fisher.x = rexp(rate=1/xAvg, n=reps)
#	} else(Fisher.x = rep(0.05, times=reps))
	} else(Fisher.x = rexp(rate=1/0.05, n=reps))
	absolute.r = 2*z*Fisher.x/sqrt(n)
	r  <-  absolute.r
	# Inbreeding values
	F.I  <-  1:10/10
	# Vector for output values
	rBal      <-  rep(0, times=length(F.I))
	rBal.fav  <-  rep(0, times=length(F.I)) 
	# random mutations
	muts   <-  matrix(data=rnorm(n*reps), nrow=reps, ncol=n)
	# Variable phenotypic dominance
	if(variableDom) {
		a  <- b/3
		h   <-  sqrt(rbeta(n=reps, shape1=b/3, shape2=b))
	} 
	if(!variableDom){
		h  <-  rep(h, times=reps)
	}
	# het-/homo-zygote phenotypes
	z.het  <-  rep(0, times=reps)
	z.hom  <-  rep(0, times=reps)	
	for(j in 1:reps) {
		z.het[j]      <-  sqrt(sum((A.wt + r[j]*h[j]*muts[j,]/sqrt(sum(muts[j,]^2)) - Opt)^2))
		z.hom[j]      <-  sqrt(sum((A.wt + r[j]*muts[j,]/sqrt(sum(muts[j,]^2)) - Opt)^2))
	}
	# het-/homo-zygote selection coefficients
	s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
	s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
	# Do selection coefficients result in balancing selection?
	# outcrossing
	out.cond1  <-  as.numeric(s.hom < s.het)
	out.cond2  <-  as.numeric(s.het > 0)
	PrBal      <-  sum((out.cond1 + out.cond2) == 2) / reps
	# among favored mutations
	PrInv      <-  sum(out.cond2 == 1) / reps
	fBal.fav   <-  PrBal / PrInv 
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F = F.I[i]
	    # inbreeding
	    cond1    <-  as.numeric(-F*s.hom < (1 - F)*s.het)
	    cond2    <-  as.numeric((1 - F)*s.het > s.hom)
		PrBal.F  <-  sum((cond1 + cond2) == 2) / reps
		# Relative probability of balancing selection
		rBal[i]  <-  PrBal.F/PrBal
		# among favoured mutations
		PrInv.F     <-  sum(cond1 == 1) / reps
		fBal.F.fav  <-  PrBal.F / PrInv.F 
		# Relative probability of balancing selection among favoured mutations
		rBal.fav[i]  <-  fBal.F.fav/fBal.fav
	}
	# Output dataframe
	res.df  <-  data.frame("F"         =  F.I,
						   "rBal"      =  rBal,
						   "rBal.fav"  =  rBal.fav)
	# return results
	return(res.df)
}




###############################################
# ESABLISHED MUTATIONS
# FGM simulations for classical weak-selection
# approximation (i.e., high-dimensional FGM)
###############################################

pThreshold  <-  function(Ne, s, prFix) {
	p  <-  prFix*100
	log((100*exp(2*Ne*s))/(p + exp(2*Ne*s)))/(2*Ne*s) 
}

qHatBal.F  <-  function(F, s.het, s.hom) {
	s_1   <-  s.het
	s_2   <-  s.het - s.hom
	qHat  <-  (s_1 - F*s_2) / ((1 - F)*(s_1 + s_2))
	min(qHat, 1)
}

# Deterministic recursion
qPrime  <-  function(q, F, s.het, s.hom) {
	p  <-  1-q
	(q*(q*(1-F)+F)*(1 + s.hom) + p*q*(1-F)*(1 + s.het)) / (p*(p*(1-F) + F) + 2*p*q*(1-F)*(1 + s.het) + q*(q*(1-F) + F)*(1 + s.hom) )
}
# Wright-Fisher Simulation for mutation establisment
WF_sim_estab_F  <-  function(F, s.het, s.hom, Ne) {
	# calc N
	N  <-  round(Ne * (1+F))
	# initial mut freq
	q  <-  1/(2*N)
	# BalSel Equil.
	qHat  <-  qHatBal.F(F=F, s.het=s.het, s.hom=s.hom)
	# Forward simulation until loss or establishment criteria is met
	while(q > 0 && q < qHat) {
		p           <-  1 - q
		w.avg       <-  p*(p*(1-F) + F) + 2*p*q*(1-F)*(1 + s.het) + q*(q*(1-F) + F)*(1 + s.hom)
		P.ij.det    <-  c(p*(p*(1-F) + F) , 2*p*q*(1-F)*(1 + s.het), q*(q*(1-F) + F)*(1 + s.hom)) / w.avg
		P.ij.drift  <-  rmultinom(1, round(N), P.ij.det)/round(N)
		q           <-  P.ij.drift[3] + P.ij.drift[2]/2
	}
	# Did mutation establish?
	q > 0
}


# Relative probability of balancing selection ~ Mutation size
# -- Simulations take a while, so output can be written as a .csv file 
#    to subdirectory './out', or returned as an object by setting
#    'writeFile' accordingly
#
# parameters
# n = 50   --  no. dimensions
# z = 1    --  wild-type displacement from optimum
# h = 0.5  --  dominance
# variableDom = FALSE -- draw phenotypic dominance values from a beta distribution with E(h^2) = 1/2 (i.e., E[h] = 1/2?
# b = 5 -- 2nd shape parameter for Beta distribution; assume a = b/3, which forces E[h^2] = 1/4. 
#			possible values for b = {0.5, 1, 1.5, 5, 50}, which span possible distribution shapes
relBalancingMutSize_EstabMuts_Sims  <-  function(Ne = 1000, n = 50, z = 1, F = 1/2, h = 1/2, variableDom = FALSE, b=5, reps=10^2, writeFile=FALSE) {

	# Initial wild-type phenotype
	A.wt = c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt = rep(0, n) 
	# shape param. for beta distribution
	a  <- b/3
	# Vector of mutation sizes
	Fisher.x = c(0.05, seq(0.5, 4.5, by = 0.5))
	absolute.r = 2*z*Fisher.x/sqrt(n)
	# Vector for output values
	rBal.est  <-  rep(0, times=length(Fisher.x))
	# loop over mutation sizes
	for(i in 1:length(Fisher.x)) {
		# current mutation size
		r = absolute.r[i]

		## For outcrossing
		# vector for selection coefficients
		s.het.est  <-  rep(0, times=reps)
		s.hom.est  <-  rep(0, times=reps)
	    # counter
	    estCount  <-  0
	    while(estCount < reps) {
			# random mutations
			mut    <-  rnorm(n)
			# Variable phenotypic dominance
			if(variableDom) {
				h   <-  sqrt(rbeta(n=1, shape1=b/3, shape2=b))
			}
			# het-/homo-zygote phenotypes
			z.het  <-  sqrt(sum((A.wt + r*h*mut/sqrt(sum(mut^2)) - Opt)^2))
			z.hom  <-  sqrt(sum((A.wt + r*mut/sqrt(sum(mut^2)) - Opt)^2))
			# het-/homo-zygote selection coefficients
			s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
			s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
			# is mutation favoured?
			if(s.het > 0) {
				# Does mutation establish?
				mutEstablishes       <-  WF_sim_estab_F(F = 0, s.het = s.het, s.hom = s.hom, Ne = Ne)
				# if mut establishes, record sel. coeffs.
				if(mutEstablishes) {
					estCount             <-  estCount + as.numeric(mutEstablishes)
					s.het.est[estCount]  <-  s.het
					s.hom.est[estCount]  <-  s.hom
				cat('\r', paste(100*(estCount/reps),'% Complete'))					
				}
			}
	    }
	    # Do selection coefficients result in balancing selection
		# among established mutations?
	    # outcrossing
		out.cond1  <-  as.numeric(s.hom.est < s.het.est)
		out.cond2  <-  as.numeric(s.het.est > 0)
	    fBal.est  <-  sum((out.cond1 + out.cond2) == 2) /reps
		## For Inbreeding
		# vector for selection coefficients
		s.het.est  <-  rep(0, times=reps)
		s.hom.est  <-  rep(0, times=reps)
	    # counter
	    estCount  <-  0
	    while(estCount < reps) {
			# random mutations
			mut   <-  rnorm(n)
			# het-/homo-zygote phenotypes
			z.het  <-  sqrt(sum((A.wt + r*h*mut/sqrt(sum(mut^2)) - Opt)^2))
			z.hom  <-  sqrt(sum((A.wt + r*mut/sqrt(sum(mut^2)) - Opt)^2))
			# het-/homo-zygote selection coefficients
			s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
			s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
			# is mutation favoured?
			if(-F*s.hom < (1 - F)*s.het) {
				# Does mutation establish?
				mutEstablishes       <-  WF_sim_estab_F(F = F, s.het = s.het, s.hom = s.hom, Ne = Ne)
				# if mut establishes, record sel. coeffs.
				if(mutEstablishes == 1) {
					estCount             <-  estCount + as.numeric(mutEstablishes)
					s.het.est[estCount]  <-  s.het
					s.hom.est[estCount]  <-  s.hom
				cat('\r', paste(100*(estCount/reps),'% Complete'))					
				}
			}
	    }
#	    cond1       <-  as.numeric(-F*s.hom.est < (1 - F)*s.het.est)
	    cond       <-  as.numeric((1 - F)*s.het.est > s.hom.est)
		fBal.F.est  <-  sum(cond) / reps

		# Relative probability of balancing selection
		rBal.est[i]  <-  fBal.F.est/fBal.est
print(paste('mut. size ', i, "/", length(Fisher.x)))
	}
 
	# results
	res.df  <-  data.frame("x"         =  Fisher.x,
						   "rBal.est"  =  rBal.est)

	# export data as .csv to ./out
	if(writeFile) {
			filename <-  paste("./out/relBal_smallMut_EstabMuts", "_Ne", Ne, "_F", F, "_n", n, "_z", z, "_h", h, "_VarDom", variableDom, "_b", b, "_reps", reps, ".csv", sep="")
			write.csv(res.df, file=filename, row.names = FALSE)
	} else{
			# return dataframe
			return(res.df)
	}

}


# Relative probability of balancing selection ~ Inbreeding Coefficient (F)
# Infinitesimal mutation size limit
# -- Simulations take a while, so output can be written as a .csv file 
#    to subdirectory './out', or returned as an object by setting
#    'writeFile' accordingly
#
# parameters
# n = 50   --  no. dimensions
# z = 1    --  wild-type displacement from optimum
# h = 0.5  --  dominance
# variableDom = FALSE -- draw phenotypic dominance values from a beta distribution with E(h^2) = 1/2 (i.e., E[h] = 1/2?
# b = 5 -- 2nd shape parameter for Beta distribution; assume a = b/3, which forces E[h^2] = 1/4. 
#			possible values for b = {0.5, 1, 1.5, 5, 50}, which span possible distribution shapes
relBalancingSmallx_EstabMuts_F_Sims  <-  function(Ne = 1000, n = 50, z = 1, h = 1/2, variableDom = FALSE, b=5, reps=10^2, writeFile=FALSE) {

	# Initial wild-type phenotype
	A.wt = c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt = rep(0, n) 
	# Mutation size
	Fisher.x = 0.05
	absolute.r = 2*z*Fisher.x/sqrt(n)
	r  <-  absolute.r
	a  <- b/3
	# Inbreeding values
	F.I  <-  0:10/10
	# Vector for output values
	fBal.F.est  <-  rep(0, times=length(F.I)) 
	# loop over inbreeding coefficients
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F = F.I[i]

		# vector for selection coefficients
		s.het.est  <-  rep(0, times=reps)
		s.hom.est  <-  rep(0, times=reps)
	    # counter
	    estCount  <-  0
	    while(estCount < reps) {
			# random mutations
			mut   <-  rnorm(n)
			# Variable phenotypic dominance
			if(variableDom) {
				h   <-  sqrt(rbeta(n=1, shape1=b/3, shape2=b))
			}
			# het-/homo-zygote phenotypes
			z.het  <-  sqrt(sum((A.wt + r*h*mut/sqrt(sum(mut^2)) - Opt)^2))
			z.hom  <-  sqrt(sum((A.wt + r*mut/sqrt(sum(mut^2)) - Opt)^2))
			# het-/homo-zygote selection coefficients
			s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
			s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
			# is mutation favoured?
			if(-F*s.hom < (1 - F)*s.het) {
				# Does mutation establish?
				mutEstablishes       <-  WF_sim_estab_F(F = F, s.het = s.het, s.hom = s.hom, Ne = Ne)
				# if mut establishes, record sel. coeffs.
				if(mutEstablishes == 1) {
					estCount             <-  estCount + as.numeric(mutEstablishes)
					s.het.est[estCount]  <-  s.het
					s.hom.est[estCount]  <-  s.hom
				cat('\r', paste(100*(estCount/reps),'% Complete'))					
				}
			}
	    }
	    cond1       <-  as.numeric(-F*s.hom.est < (1 - F)*s.het.est)
	    cond2       <-  as.numeric((1 - F)*s.het.est > s.hom.est)
		fBal.F.est[i]  <-  sum((cond1 + cond2) == 2) / reps

print(paste('Inbreeding Coefficient ', i, "/", length(F.I)))
	}

	# Relative probability of balancing selection
	rBal.est  <-  fBal.F.est/fBal.F.est[1]
	# Output dataframe
	res.df  <-  data.frame("F"         =  F.I,
						   "rBal.est"  =  rBal.est)

	# export data as .csv to ./out
	if(writeFile) {
			filename <-  paste("./out/relBal_smallMut_F_EstabMuts", "_Ne", Ne, "_n", n, "_z", z, "_h", h, "_VarDom", variableDom, "_b", b, "_reps", reps, ".csv", sep="")
			write.csv(res.df, file=filename, row.names = FALSE)
	} else{
			# return dataframe
			return(res.df)
	}
}



# Relative probability of balancing selection ~ Inbreeding Coefficient (F)
# Variable mutation size
# -- Simulations take a while, so output can be written as a .csv file 
#    to subdirectory './out', or returned as an object by setting
#    'writeFile' accordingly
#
# parameters
# n = 50   --  no. dimensions
# z = 1    --  wild-type displacement from optimum
# h = 0.5  --  dominance
# variableDom = FALSE -- draw phenotypic dominance values from a beta distribution with E(h^2) = 1/2 (i.e., E[h] = 1/2?
# b = 5 -- 2nd shape parameter for Beta distribution; assume a = b/3, which forces E[h^2] = 1/4. 
#			possible values for b = {0.5, 1, 1.5, 5, 50}, which span possible distribution shapes
relBalancingMutSize_variable_x_EstabMuts_Sims  <-  function(xAvg = 2, Ne = 1000, n = 50, z = 1, h = 1/2, variableDom = FALSE, b=5, sim.reps=10^2, writeFile = FALSE) {

	# reps for new & favoured mutations
	reps  <-  10^5
	# Initial wild-type phenotype
	A.wt = c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt = rep(0, n) 

	### New & Favoured Mutations ###
	# Mutation size
	Fisher.x = rexp(rate=1/xAvg, n = reps)
	absolute.r = 2*z*Fisher.x/sqrt(n)
	r  <-  absolute.r
	a  <- b/3
	# Inbreeding values
	F.I  <-  0:10/10

	# Vector for output values
	rBal.new  <-  rep(0, times=length(F.I))
	rBal.fav  <-  rep(0, times=length(F.I)) 
	# random mutations
	muts   <-  matrix(data=rnorm(n*reps), nrow=reps, ncol=n)
	# Variable phenotypic dominance
	if(variableDom) {
		a  <- b/3
		h   <-  sqrt(rbeta(n=reps, shape1=b/3, shape2=b))
	}
	if(!variableDom){
		h  <-  rep(h, times=reps)
	}
	# het-/homo-zygote phenotypes
	z.het  <-  rep(0, times=reps)
	z.hom  <-  rep(0, times=reps)	
		for(j in 1:reps) {
			z.het[j]      <-  sqrt(sum((A.wt + r[j]*h[j]*muts[j,]/sqrt(sum(muts[j,]^2)) - Opt)^2))
			z.hom[j]      <-  sqrt(sum((A.wt + r[j]*muts[j,]/sqrt(sum(muts[j,]^2)) - Opt)^2))
		}
	# het-/homo-zygote selection coefficients
	s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
	s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
	# Do selection coefficients result in balancing selection?
	# outcrossing
	out.cond1  <-  as.numeric(s.hom < s.het)
	out.cond2  <-  as.numeric(s.het > 0)
	PrBal      <-  sum((out.cond1 + out.cond2) == 2) / reps
	# among favored mutations
	PrInv       <-  sum(out.cond2 == 1) / reps
	fBal.fav   <-  PrBal / PrInv 
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F = F.I[i]
	    # inbreeding
	    cond1    <-  as.numeric(-F*s.hom < (1 - F)*s.het)
	    cond2    <-  as.numeric((1 - F)*s.het > s.hom)
		PrBal.F  <-  sum((cond1 + cond2) == 2) / reps
		# Relative probability of balancing selection
		rBal.new[i]  <-  PrBal.F/PrBal
		# among favoured mutations
		PrInv.F     <-  sum(cond1 == 1) / reps
		fBal.F.fav  <-  PrBal.F / PrInv.F 
		# Relative probability of balancing selection among favoured mutations
		rBal.fav[i]  <-  fBal.F.fav/fBal.fav
	}

	### Established Mutations ###
	reps  <-  sim.reps
	# Vector for output values
	fBal.F.est  <-  rep(0, times=length(F.I)) 
	# loop over inbreeding coefficients
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F = F.I[i]

		# vector for selection coefficients
		s.het.est  <-  rep(0, times=reps)
		s.hom.est  <-  rep(0, times=reps)

	    # counter
	    estCount  <-  0
	    while(estCount < reps) {

			# Mutation size
			Fisher.x = rexp(rate = 1/xAvg, n=1)
			absolute.r = 2*z*Fisher.x/sqrt(n)
			r  <-  absolute.r
			# random mutations
			mut   <-  rnorm(n)
			# Variable phenotypic dominance
			if(variableDom) {
				h   <-  sqrt(rbeta(n=1, shape1=b/3, shape2=b))
			}
			if(!variableDom) {
				h   <-  h[1]
			}
			# het-/homo-zygote phenotypes
			z.het  <-  sqrt(sum((A.wt + r*h*mut/sqrt(sum(mut^2)) - Opt)^2))
			z.hom  <-  sqrt(sum((A.wt + r*mut/sqrt(sum(mut^2)) - Opt)^2))
			# het-/homo-zygote selection coefficients
			s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
			s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
			# is mutation favoured?
			if(-F*s.hom < (1 - F)*s.het) {
				# Does mutation establish?
				mutEstablishes       <-  WF_sim_estab_F(F = F, s.het = s.het, s.hom = s.hom, Ne = Ne)
				# if mut establishes, record sel. coeffs.
				if(mutEstablishes == 1) {
					estCount             <-  estCount + as.numeric(mutEstablishes)
					s.het.est[estCount]  <-  s.het
					s.hom.est[estCount]  <-  s.hom
				cat('\r', paste(100*(estCount/reps),'% Complete'))					
				}
			}
	    }
	    cond1       <-  as.numeric(-F*s.hom.est < (1 - F)*s.het.est)
	    cond2       <-  as.numeric((1 - F)*s.het.est > s.hom.est)
		fBal.F.est[i]  <-  sum((cond1 + cond2) == 2) / reps

print(paste('Inbreeding Coefficient ', i, "/", length(F.I)))
	}

	# Relative probability of balancing selection
	rBal.est  <-  fBal.F.est/fBal.F.est[1]

	# Output dataframe
	res.df  <-  data.frame("F"         =  F.I,
						   "rBal.new"  =  rBal.new,
						   "rBal.fav"  =  rBal.fav,
						   "rBal.est"  =  rBal.est)
	
	# export data as .csv to ./out
	if(writeFile) {
		if(variableDom) {
			filename <-  paste("./out/relBal_variable_x_EstabMuts", "_xAvg", xAvg, "_Ne", Ne, "_n", n, "_z", z, "_VarDom", variableDom, "_b", b, "_reps", sim.reps, ".csv", sep="")
		} else{filename <-  paste("./out/relBal_variable_x_EstabMuts", "_xAvg", xAvg, "_Ne", Ne, "_n", n, "_z", z, "_h", h, "_reps", sim.reps, ".csv", sep="")}			
			write.csv(res.df, file=filename, row.names = FALSE)
	} else{
			# return dataframe
			return(res.df)
	}

}


#################################################
# Classical weak selection invasion conditions
#################################################

upper.classic = function(F, s.2){
  s.2/F
}
lower.classic = function(F, s.2){
  F*s.2
}

upper.FGM = function(F, s.het){
  (s.het*(F - 1))/F
}
lower.FGM = function(F, s.het){
  (1 - F)*s.het
}

lower.FGM2 = function(F, s.hom){
  -F*s.hom/(1 - F)
}
upper.FGM2 = function(F, s.hom){
  s.hom/(1 - F)
}


#################################################
#################################################

# Exploration of parameter space in 2-dimensions
# showing where we get balancing selection as we
# vary mutation size and F.
# KO_InvCond_up  <-  function(s1, S) {
#   (s1*(2 - S - 2*s1))/(S*(1 - 2*s1))
# }
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
Fisher_2D_ExploreFigSims  <-  function(xAvg = 1, z = 1, h = 1/2, reps=100, ...) {

	# Constrain n to 2 traits
	n  <-  2
	# Initial wild-type phenotype
	A.wt  <-  c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt   <-  rep(0, n) 
	# Inbreeding values
	F.I         <-  c(0.2, 0.4, 0.6, 0.8, 0.98)

	# Vector for output values
	PosSel.out.res   <-  c() 
	BalSel.out.res   <-  c() 
	PosSel.F.res     <-  c() 
	BalSel.F.res     <-  c() 
	invade.F.res     <-  c() 
	x.res            <-  c()
	F.res            <-  c()
	z.hom.raw.1.res  <-  c()
	z.hom.raw.2.res  <-  c()
	s.het.res        <-  c()
	s.hom.res        <-  c()
	t.wt.res         <-  c()
	t.hom.res        <-  c()
	# loop over inbreeding values
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F  <-  F.I[i]
		# Mutation size
		Fisher.x  <-  rexp(rate=1/xAvg, n = reps)
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
	    invade.F    <-  as.numeric(-F*s.hom < (1 - F)*s.het)
	    cond2       <-  as.numeric((1 - F)*s.het > s.hom)
		BalSel.F    <-  as.numeric((invade.F + cond2) == 2)
		PosSel.F    <-  as.numeric((1 - F)*s.het < s.hom & BalSel.F == FALSE)
		# Concatenate results
		x.res           <-  c(x.res, Fisher.x)
		F.res           <-  c(F.res, rep(F, times=length(BalSel.F)))
		PosSel.out.res  <-  c(PosSel.out.res, PosSel.out) 
		BalSel.out.res  <-  c(BalSel.out.res, BalSel.out) 
		BalSel.F.res    <-  c(BalSel.F.res, BalSel.F) 
		PosSel.F.res    <-  c(PosSel.F.res, PosSel.F) 
		invade.F.res    <-  c(invade.F.res, invade.F) 
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




# Generate data for bivariate distribution of selection coefficients
BivariateSelFigSims  <-  function(xAvg = 1, n = 50, z = 1, h = 1/2, reps=100, smallMuts = FALSE, ...) {

	# Initial wild-type phenotype
	A.wt  <-  c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt   <-  rep(0, n) 
	# Inbreeding values
	F.I         <-  c(0, 0.2, 0.4, 0.6, 0.8, 0.9)

	# Vector for output values
	PosSel.out.res   <-  c() 
	BalSel.out.res   <-  c() 
	PosSel.F.res     <-  c() 
	BalSel.F.res     <-  c() 
	invade.F.res     <-  c() 
	x.res            <-  c()
	F.res            <-  c()
	s.het.res        <-  c()
	s.hom.res        <-  c()
	t.wt.res         <-  c()
	t.hom.res        <-  c()
	# loop over inbreeding values
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F  <-  F.I[i]
		# Mutation size
		if(smallMuts) {
			Fisher.x  <-  rep(0.05, times=reps)
		}
		if(!smallMuts) {
			Fisher.x  <-  rexp(rate = 1/xAvg, n = reps)
		}
		r         <-  2*z*Fisher.x/sqrt(n)
		# random mutations
		muts   <-  matrix(data=rnorm(n*reps), nrow=reps, ncol=n)
		# homo-zygote phenotypes in 2-d space
		z.het  <-  rep(0, times=reps)
		z.hom  <-  rep(0, times=reps)
		# calculate mutation-specific displacement values
		for(j in 1:reps) {
			z.het[j]      <-  sqrt(sum((A.wt + r[j]*h*muts[j,]/sqrt(sum(muts[j,]^2)) - Opt)^2))
			z.hom[j]      <-  sqrt(sum((A.wt + r[j]*muts[j,]/sqrt(sum(muts[j,]^2)) - Opt)^2))
		}
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
	    invade.F    <-  as.numeric(-F*s.hom < (1 - F)*s.het)
	    cond2       <-  as.numeric((1 - F)*s.het > s.hom)
		BalSel.F    <-  as.numeric((invade.F + cond2) == 2)
		PosSel.F    <-  as.numeric((1 - F)*s.het < s.hom & BalSel.F == FALSE)
		# Concatenate results
		x.res           <-  c(x.res, Fisher.x)
		F.res           <-  c(F.res, rep(F, times=length(BalSel.F)))
		PosSel.out.res  <-  c(PosSel.out.res, PosSel.out) 
		BalSel.out.res  <-  c(BalSel.out.res, BalSel.out) 
		BalSel.F.res    <-  c(BalSel.F.res, BalSel.F) 
		PosSel.F.res    <-  c(PosSel.F.res, PosSel.F) 
		invade.F.res    <-  c(invade.F.res, invade.F) 
		s.het.res       <-  c(s.het.res, s.het)
		s.hom.res       <-  c(s.hom.res, s.hom)
		t.wt.res        <-  c(t.wt.res, t.wt)
		t.hom.res       <-  c(t.hom.res, t.hom)
	}
	# Output dataframe
	res.df  <-  data.frame("F"            =  F.res,
						   "s.het"        =  s.het.res, 
						   "s.hom"        =  s.hom.res, 
						   "t.wt"         =  t.wt.res, 
						   "t.hom"        =  t.hom.res, 
						   "PosSel.out"   =  PosSel.out.res,
						   "BalSel.out"   =  BalSel.out.res,
						   "PosSel.F"     =  PosSel.F.res,
						   "invade.F"     =  invade.F.res,
						   "BalSel.F"     =  BalSel.F.res)
	# return results
	return(res.df)
}







# Relative probability of balancing selection ~ Inbreeding Coefficient (F)
# Variable mutation size
# -- Simulations take a while, so output can be written as a .csv file 
#    to subdirectory './out', or returned as an object by setting
#    'writeFile' accordingly
#
# parameters
# n = 50   --  no. dimensions
# z = 1    --  wild-type displacement from optimum
# h = 0.5  --  dominance
BivariateSelFigSims_EstabMuts  <-  function(xAvg = 1, Ne = 1000, n = 50, z = 1, h = 1/2, sim.reps=10^2, writeFile = FALSE) {

	# Initial wild-type phenotype
	A.wt = c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt = rep(0, n) 
	# Inbreeding values
	F.I         <-  c(0, 0.2, 0.4, 0.6, 0.8, 0.9)

	# Vector for output values
	BalSel.F.res     <-  c() 
	x.res            <-  c()
	F.res            <-  c()
	s.het.res        <-  c()
	s.hom.res        <-  c()
	t.wt.res         <-  c()
	t.hom.res        <-  c()

	### Established Mutations ###
	reps  <-  sim.reps
	# Vector for output values
	fBal.F.est  <-  rep(0, times=length(F.I)) 
	# loop over inbreeding coefficients
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F = F.I[i]

		# vector for selection coefficients
		s.het.est  <-  rep(0, times=reps)
		s.hom.est  <-  rep(0, times=reps)
		t.wt.est   <-  rep(0, times=reps)
		t.hom.est  <-  rep(0, times=reps)

	    # counter
	    estCount  <-  0
	    while(estCount < reps) {

			# Mutation size
			Fisher.x = rexp(rate = 1/xAvg, n=1)
			absolute.r = 2*z*Fisher.x/sqrt(n)
			r  <-  absolute.r

			# random mutations
			mut   <-  rnorm(n)
			# het-/homo-zygote phenotypes
			z.het  <-  sqrt(sum((A.wt + r*h*mut/sqrt(sum(mut^2)) - Opt)^2))
			z.hom  <-  sqrt(sum((A.wt + r*mut/sqrt(sum(mut^2)) - Opt)^2))
			# het-/homo-zygote selection coefficients
			s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
			s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
			t.wt   <-  exp(-0.5*z^2)/exp(-0.5*z.het^2) - 1
			t.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z.het^2) - 1
			# is mutation favoured?
			if(-F*s.hom < (1 - F)*s.het) {
				# Does mutation establish?
				mutEstablishes       <-  WF_sim_estab_F(F = F, s.het = s.het, s.hom = s.hom, Ne = Ne)
				# if mut establishes, record sel. coeffs.
				if(mutEstablishes == 1) {
					estCount             <-  estCount + as.numeric(mutEstablishes)
					s.het.est[estCount]  <-  s.het
					s.hom.est[estCount]  <-  s.hom
					t.wt.est[estCount]   <-  t.wt
					t.hom.est[estCount]  <-  t.hom
				cat('\r', paste(100*(estCount/reps),'% Complete'))					
				}
			}
	    }
	    # Are conditions for balancing selection met?
	    cond1       <-  as.numeric(-F*s.hom.est < (1 - F)*s.het.est)
	    cond2       <-  as.numeric((1 - F)*s.het.est > s.hom.est)
	    BalSel.F    <-  as.numeric((cond1 + cond2) == 2)
		fBal.F.est[i]  <-  sum(BalSel.F) / reps

		# Concatenate results
		BalSel.F.res  <-  c(BalSel.F.res, BalSel.F) 
		s.het.res     <-  c(s.het.res, s.het.est)
		s.hom.res     <-  c(s.hom.res, s.hom.est)
		t.wt.res      <-  c(t.wt.res, t.wt.est)
		t.hom.res     <-  c(t.hom.res, t.hom.est)

	# Relative probability of balancing selection
	rBal.est  <-  fBal.F.est[i]/fBal.F.est[1]
	print(paste('Inbreeding Coefficient ', i, "/", length(F.I), ', Rbal = ', rBal.est))
	}

	# Output dataframe
	res.df  <-  data.frame("F"            =  rep(F.I, each=reps),
						   "s.het"        =  s.het.res, 
						   "s.hom"        =  s.hom.res, 
						   "t.wt"         =  t.wt.res, 
						   "t.hom"        =  t.hom.res, 
						   "BalSel.F"     =  BalSel.F.res)
	
	# export data as .csv to ./out
	if(writeFile) {
			filename <-  paste("./out/BivariateSelFigSims_EstabMuts", "_xAvg", xAvg, "_Ne", Ne, "_n", n, "_z", z, "_h", h, "_reps", sim.reps, ".csv", sep="")
			write.csv(res.df, file=filename, row.names = FALSE)
	} else{
			# return dataframe
			return(res.df)
	}

}

# Relative probability of balancing selection ~ Inbreeding Coefficient (F)
# Small mutation size
# -- Simulations take a while, so output can be written as a .csv file 
#    to subdirectory './out', or returned as an object by setting
#    'writeFile' accordingly
#
# parameters
# n = 50   --  no. dimensions
# z = 1    --  wild-type displacement from optimum
# h = 0.5  --  dominance
BivariateSelFigSims_EstabMutsSmall  <-  function(Ne = 1000, n = 50, z = 1, h = 1/2, sim.reps=10^2, writeFile = FALSE) {

	# Initial wild-type phenotype
	A.wt = c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt = rep(0, n) 
	# Inbreeding values
	F.I         <-  c(0, 0.2, 0.4, 0.6, 0.8, 0.9)

	# Vector for output values
	BalSel.F.res     <-  c() 
	x.res            <-  c()
	F.res            <-  c()
	s.het.res        <-  c()
	s.hom.res        <-  c()
	t.wt.res         <-  c()
	t.hom.res        <-  c()

	### Established Mutations ###
	reps  <-  sim.reps
	# Vector for output values
	fBal.F.est  <-  rep(0, times=length(F.I)) 
	# loop over inbreeding coefficients
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F = F.I[i]

		# vector for selection coefficients
		s.het.est  <-  rep(0, times=reps)
		s.hom.est  <-  rep(0, times=reps)
		t.wt.est   <-  rep(0, times=reps)
		t.hom.est  <-  rep(0, times=reps)

		# Mutation size
		Fisher.x = 0.05
		absolute.r = 2*z*Fisher.x/sqrt(n)
		r  <-  absolute.r

	    # counter
	    estCount  <-  0
	    while(estCount < reps) {

			# random mutations
			mut   <-  rnorm(n)
			# het-/homo-zygote phenotypes
			z.het  <-  sqrt(sum((A.wt + r*h*mut/sqrt(sum(mut^2)) - Opt)^2))
			z.hom  <-  sqrt(sum((A.wt + r*mut/sqrt(sum(mut^2)) - Opt)^2))
			# het-/homo-zygote selection coefficients
			s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
			s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
			t.wt   <-  exp(-0.5*z^2)/exp(-0.5*z.het^2) - 1
			t.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z.het^2) - 1
			# is mutation favoured?
			if(-F*s.hom < (1 - F)*s.het) {
				# Does mutation establish?
				mutEstablishes       <-  WF_sim_estab_F(F = F, s.het = s.het, s.hom = s.hom, Ne = Ne)
				# if mut establishes, record sel. coeffs.
				if(mutEstablishes == 1) {
					estCount             <-  estCount + as.numeric(mutEstablishes)
					s.het.est[estCount]  <-  s.het
					s.hom.est[estCount]  <-  s.hom
					t.wt.est[estCount]   <-  t.wt
					t.hom.est[estCount]  <-  t.hom
				cat('\r', paste(100*(estCount/reps),'% Complete'))					
				}
			}
	    }
	    # Are conditions for balancing selection met?
	    cond1       <-  as.numeric(-F*s.hom.est < (1 - F)*s.het.est)
	    cond2       <-  as.numeric((1 - F)*s.het.est > s.hom.est)
	    BalSel.F    <-  as.numeric((cond1 + cond2) == 2)
		fBal.F.est[i]  <-  sum(BalSel.F) / reps

		# Concatenate results
		BalSel.F.res  <-  c(BalSel.F.res, BalSel.F) 
		s.het.res     <-  c(s.het.res, s.het.est)
		s.hom.res     <-  c(s.hom.res, s.hom.est)
		t.wt.res      <-  c(t.wt.res, t.wt.est)
		t.hom.res     <-  c(t.hom.res, t.hom.est)

	# Relative probability of balancing selection
	rBal.est  <-  fBal.F.est[i]/fBal.F.est[1]
	print(paste('Inbreeding Coefficient ', i, "/", length(F.I), ', Rbal = ', rBal.est))
	}

	# Output dataframe
	res.df  <-  data.frame("F"            =  rep(F.I, each=reps),
						   "s.het"        =  s.het.res, 
						   "s.hom"        =  s.hom.res, 
						   "t.wt"         =  t.wt.res, 
						   "t.hom"        =  t.hom.res, 
						   "BalSel.F"     =  BalSel.F.res)
	
	# export data as .csv to ./out
	if(writeFile) {
			filename <-  paste("./out/BivariateSelFigSims_EstabMutsSmall", "_Ne", Ne, "_n", n, "_z", z, "_h", h, "_reps", sim.reps, ".csv", sep="")
			write.csv(res.df, file=filename, row.names = FALSE)
	} else{
			# return dataframe
			return(res.df)
	}

}


# Relative probability of balancing selection ~ Inbreeding Coefficient (F)
# Variable mutation size
BivariateSelFigSims_FavMuts  <-  function(xAvg=2, n = 10, z = 1, h = 1/2, reps=100, smallMuts = FALSE, ...) {


	# Initial wild-type phenotype
	A.wt = c(-z, rep(0, (n - 1))) 
	# Phenotypic optimum at 0
	Opt = rep(0, n) 
	# Inbreeding values
	F.I         <-  c(0, 0.2, 0.4, 0.6, 0.8, 0.9)

	# Vector for output values
	BalSel.F.res     <-  c() 
	x.res            <-  c()
	F.res            <-  c()
	s.het.res        <-  c()
	s.hom.res        <-  c()
	t.wt.res         <-  c()
	t.hom.res        <-  c()

	# Vector for output values
	fBal.F.fav  <-  rep(0, times=length(F.I)) 
	# loop over inbreeding coefficients
	for(i in 1:length(F.I)){
		# Inbreeding Coefficient
		F = F.I[i]

		# vector for selection coefficients
		s.het.fav  <-  rep(0, times=reps)
		s.hom.fav  <-  rep(0, times=reps)
		t.wt.fav   <-  rep(0, times=reps)
		t.hom.fav  <-  rep(0, times=reps)

	    # counter
	    nFav  <-  0
	    # loop until reps favourable mutations are sampled
	    while(nFav < reps) {

			# Mutation size
			if(smallMuts) {
				Fisher.x = 0.05	
			}
			if(!smallMuts) {
				Fisher.x = rexp(rate = 1/xAvg, n=1)
			}
			absolute.r = 2*z*Fisher.x/sqrt(n)
			r  <-  absolute.r

			# random mutations
			mut   <-  rnorm(n)
			# het-/homo-zygote phenotypes
			z.het  <-  sqrt(sum((A.wt + r*h*mut/sqrt(sum(mut^2)) - Opt)^2))
			z.hom  <-  sqrt(sum((A.wt + r*mut/sqrt(sum(mut^2)) - Opt)^2))
			# het-/homo-zygote selection coefficients
			s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
			s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
			t.wt   <-  exp(-0.5*z^2)/exp(-0.5*z.het^2) - 1
			t.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z.het^2) - 1
			# is mutation favoured?
			if(-F*s.hom < (1 - F)*s.het) {
					nFav             <-  nFav + 1
					s.het.fav[nFav]  <-  s.het
					s.hom.fav[nFav]  <-  s.hom
					t.wt.fav[nFav]   <-  t.wt
					t.hom.fav[nFav]  <-  t.hom
				cat('\r', paste(100*(nFav/reps),'% Complete'))					
			}
	    }
	    # Are conditions for balancing selection met?
	    cond1       <-  as.numeric(-F*s.hom.fav < (1 - F)*s.het.fav)
	    cond2       <-  as.numeric((1 - F)*s.het.fav > s.hom.fav)
	    BalSel.F    <-  as.numeric((cond1 + cond2) == 2)
		PosSel.F    <-  as.numeric((1 - F)*s.het < s.hom & BalSel.F == FALSE)
		fBal.F.fav[i]  <-  sum(BalSel.F) / reps

		# Concatenate results
		BalSel.F.res  <-  c(BalSel.F.res, BalSel.F) 
		PosSel.F.res  <-  c(PosSel.F, PosSel.F) 
		s.het.res     <-  c(s.het.res, s.het.fav)
		s.hom.res     <-  c(s.hom.res, s.hom.fav)
		t.wt.res      <-  c(t.wt.res, t.wt.fav)
		t.hom.res     <-  c(t.hom.res, t.hom.fav)

	# Relative probability of balancing selection
	rBal.fav  <-  fBal.F.fav[i]/fBal.F.fav[1]
	print(paste('Inbreeding Coefficient ', i, "/", length(F.I), ', Rbal = ', rBal.fav))
	}

	# Output dataframe
	res.df  <-  data.frame("F"            =  rep(F.I, each=reps),
						   "s.het"        =  s.het.res, 
						   "s.hom"        =  s.hom.res, 
						   "t.wt"         =  t.wt.res, 
						   "t.hom"        =  t.hom.res, 
						   "PosSel.F"     =  PosSel.F.res,
						   "BalSel.F"     =  BalSel.F.res)
	# return results
	return(res.df)
}







###################################
# Genetic Variance Simulations
VG_variable_x_EstabMuts_Sims  <-  function(xBar=0.2, Ne = 1000, n = 50, z = 1, h = 1/2, nMuts=10^2, writeFile=TRUE) {

    # Initial wild-type phenotype
    A.wt = c(-z, rep(0, (n - 1))) 
    # Phenotypic optimum at 0
    Opt = rep(0, n) 

    F.I  <-  0:10/10
    ### Established Mutations ###
    # Vector for output values
    fBal.F.est  <-  rep(0, times=length(F.I)) 
    expectedVG  <-  rep(0, times=length(F.I)) 
    # loop over inbreeding coefficients
    for(i in 1:length(F.I)){
        # Inbreeding Coefficient
        F = F.I[i]

        # vector for selection coefficients
        s.het.est  <-  rep(0, times=nMuts)
        s.hom.est  <-  rep(0, times=nMuts)

        # loop over mutations
        VGvals  <-  rep(0, times=nMuts)
        for(j in 1:nMuts) {

            # Mutation size
            Fisher.x = rexp(n=1, rate=1/xBar)
            absolute.r = 2*z*Fisher.x/sqrt(n)
            r  <-  absolute.r

            # random mutations
            mut   <-  rnorm(n)
            # het-/homo-zygote phenotypes
            z.het  <-  sqrt(sum((A.wt + r*h*mut/sqrt(sum(mut^2)) - Opt)^2))
            z.hom  <-  sqrt(sum((A.wt + r*mut/sqrt(sum(mut^2)) - Opt)^2))
            # het-/homo-zygote selection coefficients
            s.het  <-  exp(-0.5*z.het^2)/exp(-0.5*z^2) - 1
            s.hom  <-  exp(-0.5*z.hom^2)/exp(-0.5*z^2) - 1
            # is mutation favoured?
            if(-F*s.hom < (1 - F)*s.het) {
                # Does mutation establish?
                mutEstablishes  <-  WF_sim_estab_F(F = F, s.het = s.het, s.hom = s.hom, Ne = Ne)
                # if mut establishes, record sel. coeffs.
                if(mutEstablishes == 1) {
                    VGvals[j]  <-  VGHat.fisher(sHet=s.het, sHom=s.hom, F=F)
                cat('\r', paste(100*(j/nMuts),'% Complete'))                    
                }
            }
        }
        expectedVG[i]  <-  mean(VGvals)

    print(paste('F = ', F.I[i]))
    }

    # Output dataframe
    res.df  <-  data.frame("F"         =  F.I,
                           "ExpVG"  =  expectedVG)

	# export data as .csv to ./out
	if(writeFile) {
			filename <-  paste("./out/EstabMut_VG", "_xBar", xBar, "_Ne", Ne, "_n", n, "_z", z, "_h", h, "_nMuts", nMuts, ".csv", sep="")
			write.csv(res.df, file=filename, row.names = FALSE)
	} else{
			# return dataframe
			return(res.df)
	}
}
