erf <- function(x){2*pnorm(x*sqrt(2)) - 1}
z = 1
v = 0.5
x.avg.exp = 10
lambda = 1/x.avg.exp
n = 50
m = 0.01

#Predicted mean and variance of x for the chi distribution model
x.avg.chisq = m*sqrt(n)/(2*z)*sqrt(2)*gamma((n + 1)/2)/gamma(n/2)
x.var.chisq = (m*sqrt(n)/(2*z))^2*n - x.avg.chisq^2

x.chisq = m*sqrt(n)/(2*z)*sqrt(rchisq(10^5, n))
hist(x.chisq, main = NA, breaks = 50, freq = F)
lines(c(x.avg.chisq, x.avg.chisq), c(0, 10^5), lty = 2, col = "red", lwd = 2)
points(mean(x.chisq), 0, pch = 21, bg = "white", col = "red")
print(c(var(x.chisq), x.var.chisq, mean(x.chisq), x.avg.chisq))

F.series = seq(0.05, 0.95, 0.05)

#NEW MUTATIONS
Ratio.new.exp = vector()
Ratio.new.chisq = vector()

for(i in 1:length(F.series)){
  f = F.series[i]
  Pr.bal.new.exp = function(x){
    0.5*(erf(x/sqrt(2)*(1 - (1 - f)*v^2)/(1 - (1 - f)*v)) - erf(x/sqrt(2)*(f + (1 - f)*v^2)/(f + (1 - f)*v)))*lambda*exp(-lambda*x)
  }
  
  Pr.bal.new.exp.0 = function(x){
    0.5*(erf(x/sqrt(2)*(1 + v)) - erf(x/sqrt(2)*v))*lambda*exp(-lambda*x)
  }
  
  Ratio.new.exp[i] = integrate(Pr.bal.new.exp, lower = 0, upper = Inf)$value/integrate(Pr.bal.new.exp.0, lower = 0, upper = Inf)$value
  
  #chi distribution of r/m
  #x = m*sqrt(n)/(2*z)*sqrt(y)
  Pr.bal.new.chisq = function(y){
    0.5*(erf(m*sqrt(n)/(2*z)*sqrt(y)/sqrt(2)*(1 - (1 - f)*v^2)/(1 - (1 - f)*v)) - erf(m*sqrt(n)/(2*z)*sqrt(y)/sqrt(2)*(f + (1 - f)*v^2)/(f + (1 - f)*v)))*pchisq(y, n)
  }
  
  Pr.bal.new.chisq.0 = function(y){
    0.5*(erf(m*sqrt(n)/(2*z)*sqrt(y)/sqrt(2)*(1 + v)) - erf(m*sqrt(n)/(2*z)*sqrt(y)/sqrt(2)*v))*pchisq(y, n)
  }
  
  Ratio.new.chisq[i] = integrate(Pr.bal.new.chisq, lower = 0, upper = Inf)$value/integrate(Pr.bal.new.chisq.0, lower = 0, upper = Inf)$value
}

#Uniform x prediction
R.new = function(f){
  (1 - f^2)*v^2*(1 - v^2)/((f + (1 - f)*v^2)*(1 - (1 - f)*v^2))
}

plot(c(0,1), c(1, 0), type = "l", ylab = "R.new", xlab = "F")
curve(R.new, add = T, lty = 3)
points(F.series, Ratio.new.exp, pch = 21, bg = "white")
points(F.series, Ratio.new.chisq, pch = 21, bg = "grey")


#BENEFICIAL MUTATIONS
Ratio.ben.exp = vector()
Ratio.ben.chisq = vector()

for(i in 1:length(F.series)){
  f = F.series[i]
  
  Pr.pos.exp = function(x){
    0.5*(1 - erf(x/sqrt(2)*(1 - (1 - f)*v^2)/(1 - (1 - f)*v)))*lambda*exp(-lambda*x)
  }
  
  Pr.ben.exp = function(x){
    0.5*(1 - erf(x/sqrt(2)*(f + (1 - f)*v^2)/(f + (1 - f)*v)))*lambda*exp(-lambda*x)
  }
  
  Pr.pos.exp.0 = function(x){
    0.5*(1 - erf(x/sqrt(2)*(1 + v)))*lambda*exp(-lambda*x)
  }
  
  Pr.ben.exp.0 = function(x){
    0.5*(1 - erf(x/sqrt(2)*v))*lambda*exp(-lambda*x)
  }
  
  Ratio.ben.exp[i] = (1 - integrate(Pr.pos.exp, lower = 0, upper = Inf)$value/integrate(Pr.ben.exp, lower = 0, upper = Inf)$value)/(1 - integrate(Pr.pos.exp.0, lower = 0, upper = Inf)$value/integrate(Pr.ben.exp.0, lower = 0, upper = Inf)$value)
  
  
  #x = m*sqrt(n)/(2*z)*sqrt(y)
  Pr.pos.chisq = function(y){
    0.5*(1 - erf(m*sqrt(n)/(2*z)*sqrt(y)/sqrt(2)*(1 - (1 - f)*v^2)/(1 - (1 - f)*v)))*pchisq(y, n)
  }
  
  Pr.ben.chisq = function(y){
    0.5*(1 - erf(m*sqrt(n)/(2*z)*sqrt(y)/sqrt(2)*(f + (1 - f)*v^2)/(f + (1 - f)*v)))*pchisq(y, n)
  }
  
  Pr.pos.chisq.0 = function(y){
    0.5*(1 - erf(m*sqrt(n)/(2*z)*sqrt(y)/sqrt(2)*(1 + v)))*pchisq(y, n)
  }
  
  Pr.ben.chisq.0 = function(y){
    0.5*(1 - erf(m*sqrt(n)/(2*z)*sqrt(y)/sqrt(2)*v))*pchisq(y, n)
  }
  
  Ratio.ben.chisq[i] = (1 - integrate(Pr.pos.chisq, lower = 0, upper = Inf)$value/integrate(Pr.ben.chisq, lower = 0, upper = Inf)$value)/(1 - integrate(Pr.pos.chisq.0, lower = 0, upper = Inf)$value/integrate(Pr.ben.chisq.0, lower = 0, upper = Inf)$value)
}

#Uniform x prediction
R.ben = function(f){
  (1 - f^2)*v*(1 - v^2)/((f + (1 - f)*v)*(1 - (1 - f)*v^2))
}

plot(c(0,1), c(1, 0), type = "l", ylab = "R.adapt", xlab = "F")
curve(R.ben, add = T, lty = 3)
points(F.series, Ratio.ben.exp, pch = 21, bg = "white")
points(F.series, Ratio.ben.chisq, pch = 21, bg = "grey")



#ESTABLISHED MUTATIONS
Ratio.est.exp = vector()
Ratio.est.chisq = vector()

for(i in 1:length(F.series)){
  f = F.series[i]
  
  Pr.fix.exp = function(x){
    z^2*x/n*(sqrt(2/pi)*(1 + f)*exp(-x^2/2*(3 + f)^2/(2 + 2*f)^2) - x*(1 + 3*f)/2*(1 - erf(x*(3 + f)/(2*sqrt(2)*(1 + f)))))*lambda*exp(-lambda*x)
  }
  
  Pr.est.exp = function(x){
    z^2*x/n*(sqrt(2/pi)*(1 + f)*exp(-x^2/2*(1 + 3*f)^2/(2 + 2*f)^2) - x*(1 + 3*f)/2*(1 - erf(x*(1 + 3*f)/(2*sqrt(2)*(1 + f)))))*lambda*exp(-lambda*x)
  }
  
  Pr.fix.exp.0 = function(x){
    z^2*x/n*(sqrt(2/pi)*exp(-x^2*9/8) - x/2*(1 - erf(x*3/(2*sqrt(2)))))*lambda*exp(-lambda*x)
  }
  
  Pr.est.exp.0 = function(x){
    z^2*x/n*(sqrt(2/pi)*exp(-x^2/8) - x/2*(1 - erf(x/(2*sqrt(2)))))*lambda*exp(-lambda*x)
  }
  
  Ratio.est.exp[i] = (1 - integrate(Pr.fix.exp, lower = 0, upper = Inf)$value/integrate(Pr.est.exp, lower = 0, upper = Inf)$value)/(1 - integrate(Pr.fix.exp.0, lower = 0, upper = Inf)$value/integrate(Pr.est.exp.0, lower = 0, upper = Inf)$value)
  
  
  #x = m*sqrt(n)/(2*z)*sqrt(y)
  Pr.fix.chisq = function(y){
    z^2*m*sqrt(n)/(2*z)*sqrt(y)/n*(sqrt(2/pi)*(1 + f)*exp(-(m*sqrt(n)/(2*z)*sqrt(y))^2/2*(3 + f)^2/(2 + 2*f)^2) - m*sqrt(n)/(2*z)*sqrt(y)*(1 + 3*f)/2*(1 - erf(m*sqrt(n)/(2*z)*sqrt(y)*(3 + f)/(2*sqrt(2)*(1 + f)))))*pchisq(y, n)
  }
  
  Pr.est.chisq = function(y){
    z^2*m*sqrt(n)/(2*z)*sqrt(y)/n*(sqrt(2/pi)*(1 + f)*exp(-(m*sqrt(n)/(2*z)*sqrt(y))^2/2*(1 + 3*f)^2/(2 + 2*f)^2) - m*sqrt(n)/(2*z)*sqrt(y)*(1 + 3*f)/2*(1 - erf(m*sqrt(n)/(2*z)*sqrt(y)*(1 + 3*f)/(2*sqrt(2)*(1 + f)))))*pchisq(y, n)
  }
  
  Pr.fix.chisq.0 = function(y){
    z^2*m*sqrt(n)/(2*z)*sqrt(y)/n*(sqrt(2/pi)*exp(-(m*sqrt(n)/(2*z)*sqrt(y))^2*9/8) - m*sqrt(n)/(2*z)*sqrt(y)/2*(1 - erf(m*sqrt(n)/(2*z)*sqrt(y)*3/(2*sqrt(2)))))*pchisq(y, n)
  }
  
  Pr.est.chisq.0 = function(y){
    z^2*m*sqrt(n)/(2*z)*sqrt(y)/n*(sqrt(2/pi)*exp(-(m*sqrt(n)/(2*z)*sqrt(y))^2/8) - m*sqrt(n)/(2*z)*sqrt(y)/2*(1 - erf(m*sqrt(n)/(2*z)*sqrt(y)/(2*sqrt(2)))))*pchisq(y, n)
  }
  
  Ratio.est.chisq[i] = (1 - integrate(Pr.fix.chisq, lower = 0, upper = Inf)$value/integrate(Pr.est.chisq, lower = 0, upper = Inf)$value)/(1 - integrate(Pr.fix.chisq.0, lower = 0, upper = Inf)$value/integrate(Pr.est.chisq.0, lower = 0, upper = Inf)$value)
}

#Uniform x prediction
R.est = function(f){
  27*(1 - f)^2*(7*f + 5)/(5*(3 + f)^3)
}

plot(c(0,1), c(1, 0), type = "l", ylab = "R.est", xlab = "F")
curve(R.est, add = T, lty = 3)
points(F.series, Ratio.est.exp, pch = 21, bg = "white")
points(F.series, Ratio.est.chisq, pch = 21, bg = "grey")
