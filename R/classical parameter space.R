inbreeding.states = c(0.1, 0.3, 0.5, 0.7)

F.I = inbreeding.states[1]

upper = function(t.2){
  t.2/F.I
}

lower = function(t.2){
  F.I*t.2
}

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

par(mfrow = c(1, 2))
curve(upper, 0, F.I, xlim = c(0, 1), lwd = 3, xlab = "t.2", ylab = "t.1")
curve(lower, 0, 1, add = TRUE, lwd = 3)

for(i in 2:4){
  F.I = inbreeding.states[i]
  
  curve(upper, 0, F.I, add = TRUE, lwd = 3, col = colorBlindBlack8[i])
  curve(lower, 0, 1, add = TRUE, lwd = 3, col = colorBlindBlack8[i])
}


inbreeding.states = 0.05 + 0:9/10
Pr.balancing = rep(0, length(inbreeding.states))

for(i in 1:length(inbreeding.states)){
  F.I = inbreeding.states[i]
  t.1 = runif(10^6)
  t.2 = runif(10^6)
  condition = t.1 > t.2*F.I & t.1 < t.2/F.I
  Pr.balancing[i] = length(condition[!condition == FALSE])/10^6
}

plot(0:1000/1000, 1 - 0:1000/1000, type = "l", lwd = 3, xlab = "inbreeding coefficient (F)", ylab = "Pr.balancing")
points(inbreeding.states, Pr.balancing, pch = 16, col = "RED")
