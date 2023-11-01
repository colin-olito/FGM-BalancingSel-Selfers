F.I = 0.5 #0 < F < 1
h = 0.5 #0 < h < 1

a = (1 - (1 - F.I)*h^2)/(1 - (1 - F.I)*h)
b = (F.I + (1 - F.I)*h^2)/(F.I + (1 - F.I)*h)
x.max.F = sqrt(2*log(a/b)/(a^2 - b^2))
x.max = sqrt(2*log((1 + h)/h)/(1 + 2*h))

Pr.inv.F = function(x, b = (F.I + (1 - F.I)*h^2)/(F.I + (1 - F.I)*h)){
  1 - pnorm(x*b)
}

Pr.bal.F = function(x, a = (1 - (1 - F.I)*h^2)/(1 - (1 - F.I)*h), b = (F.I + (1 - F.I)*h^2)/(F.I + (1 - F.I)*h)){
  pnorm(x*a) - pnorm(x*b)
}

Pr.pos.F = function(x, a = (1 - (1 - F.I)*h^2)/(1 - (1 - F.I)*h)){
  1 - pnorm(x*a)
}

Pr.inv = function(x, b = h){
  1 - pnorm(x*b)
}

Pr.bal = function(x, a = (1 + h), b = h){
  pnorm(x*a) - pnorm(x*b)
}

Pr.pos = function(x, a = (1 + h), b = h){
  1 - pnorm(x*a)
}

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#pdf(file = "./notes/img/MutationsClassDistributions.pdf",   # The directory you want to save the file in
#    width = 7, # The width of the plot in inches
#    height = 4) # The height of the plot in inches

par(mfrow = c(1, 2))
curve(Pr.inv.F, 0, 5, lwd = 3, ylim = c(0, 0.5), lty = 3, ylab = "probability", xlab = "mutation size (x)")
curve(Pr.bal.F, add = TRUE, col = "BLUE", lwd = 3)
curve(Pr.pos.F, add = TRUE, col = "RED", lwd = 3)
lines(c(x.max.F, x.max.F), c(0, Pr.bal.F(x.max.F)), lty = 3, col = "BLUE")

curve(Pr.inv, 0, 5, lwd = 3, ylim = c(0, 0.5), lty = 3, ylab = "probability", xlab = "mutation size (x)")
curve(Pr.bal, add = TRUE, col = "BLUE", lwd = 3)
curve(Pr.pos, add = TRUE, col = "RED", lwd = 3)
lines(c(x.max, x.max), c(0, Pr.bal(x.max)), lty = 3, col = "BLUE")

#dev.off()

par(mfrow = c(1, 2))
#Ratio of balancing selection ((F > 0) vs. (F = 0) in black); 1 - F in red
sizes = 1:1000/200
plot(sizes, Pr.bal.F(sizes)/Pr.bal(sizes), type = "l", ylim = c(0, 1), col = "GREY", lwd = 3)
lines(sizes, rep(1 - F.I, length(sizes)), lwd = 3)
lines(sizes, rep((1 - F.I)*h*(1 - h)*(1/(1 - (1 - F.I)*h) + 1/(F.I + (1 - F.I)*h)), length(sizes)), lty = 3)

F.values = 0:100/100
h.1 = 0.5
h.2 = 0.25
h.3 = 0.1
plot(F.values, (1 - F.values)*h.1*(1 - h.1)*(1/(1 - (1 - F.values)*h.1) + 1/(F.values + (1 - F.values)*h.1)), type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = colorBlindBlack8[2], lwd = 3)
lines(F.values, (1 - F.values)*h.2*(1 - h.2)*(1/(1 - (1 - F.values)*h.2) + 1/(F.values + (1 - F.values)*h.2)), col = colorBlindBlack8[3], lwd = 3)
lines(F.values, (1 - F.values)*h.3*(1 - h.3)*(1/(1 - (1 - F.values)*h.3) + 1/(F.values + (1 - F.values)*h.3)), col = colorBlindBlack8[4], lwd = 3)
lines(F.values, (1 - F.values), col = colorBlindBlack8[1], lwd = 3)


xMaxF  <- function(a,b){
  sqrt(2*log(a/b)/(a^2 - b^2))
}

xMax  <-  function(h){
  sqrt(2*log((1 + h)/h)/(1 + 2*h))
}

funa  <-  function(F,h) {
  (1 - (1 - F)*h^2)/(1 - (1 - F)*h)
}
funb  <-  function(F,h) {
  (F + (1 - F)*h^2)/(F + (1 - F)*h)
}

F.values = 0:99/100
h.1 = 0.5
h.2 = 0.25
h.3 = 0.1

xMaxFs.1  <-  xMaxF(a = funa(F=F.values, h=h.1), b=funb(F = F.values, h = h.1))
xMaxs.1  <-  xMax(h=h.1)
xMaxFs.2  <-  xMaxF(a = funa(F=F.values, h=h.2), b=funb(F = F.values, h = h.2))
xMaxs.2  <-  xMax(h=h.2)
xMaxFs.3  <-  xMaxF(a = funa(F=F.values, h=h.3), b=funb(F = F.values, h = h.3))
xMaxs.3  <-  xMax(h=h.3)

# ratio of mutation size with maximal probability of balancing selection
# for inbred relative to outcrossing populations
# pdf(file = "./notes/img/xMaxPlot.pdf",   # The directory you want to save the file in
#     width = 6, # The width of the plot in inches
#     height = 5) # The height of the plot in inches
par(omi=c(0.5, 0.25, 0.25, 0.5), mar = c(5,5,1,1))
plot((xMaxFs.1/xMaxs.1) ~ F.values, ylim=c(0, 1), type='l', lwd=2, col=colorBlindBlack8[1], ylab=expression(paste(italic(hat(x)[F]), "/", hat(x)[out])), xlab=expression(italic(F)))
lines((xMaxFs.2/xMaxs.2) ~ F.values, ylim=c(0, 1), lwd=2, col=colorBlindBlack8[2])
lines((xMaxFs.3/xMaxs.3) ~ F.values, ylim=c(0, 1), lwd=2, col=colorBlindBlack8[3])
legend(
               x       =  1,
               y       =  0.25,
               legend  =  c(
                            expression(paste(italic(h), " = 1/2")),
                            expression(paste(italic(h), " = 1/4")),
                            expression(paste(italic(h), " = 1/10"))),
               lty     =  1,
               lwd     =  2,
               col     =  c(colorBlindBlack8[1], colorBlindBlack8[2], colorBlindBlack8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA,
               xpd=NA
               )
# dev.off()


toPdf <- function(expr, filename, ...) {
  toDev(expr, pdf, filename, ...)
}

figPath  <-  function(name) {
  file.path('./img/', name)
}

toDev <- function(expr, dev, filename, ..., verbose=TRUE) {
  if ( verbose )
    cat(sprintf('Creating %s\n', filename))
  dev(filename, family='CM Roman', ...)
#    dev(filename, family='Arial', ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

KO_InvCond_up  <-  function(s1, S) {
  (s1*(2 - S - 2*s1))/(S*(1 - 2*s1))
}
KO_InvCond_low  <-  function(s1, S) {
  (1/2)*(1 - sqrt(1 - S + (S^2)*(1/2 - s1)^2) - S*(1/2 - s1))
}

KO_InvPlot  <- function() {
 pdf(file = "./notes/img/KOInvPlot.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6) # The height of the plot in inches
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
par(omi=c(0.5, 0.25, 0.25, 0.5), mar = c(5,5,1,1))
s1.vals  <-  seq(0,1,0.01)
plot(KO_InvCond_low(s1 = s1.vals, S=0.2) ~ s1.vals, ylim=c(0, 1), type='l', lwd=2, col=colorBlindBlack8[1], ylab=expression(italic(s[2])), xlab=expression(italic(s[1])))
lines(KO_InvCond_low(s1 = s1.vals, S=0.4) ~ s1.vals, ylim=c(0, 1), lwd=2, col=colorBlindBlack8[2])
lines(KO_InvCond_low(s1 = s1.vals, S=0.6) ~ s1.vals, ylim=c(0, 1), lwd=2, col=colorBlindBlack8[3])
lines(KO_InvCond_low(s1 = s1.vals, S=0.8) ~ s1.vals, ylim=c(0, 1), lwd=2, col=colorBlindBlack8[4])
lines(KO_InvCond_low(s1 = s1.vals, S=0.98) ~ s1.vals, ylim=c(0, 1), lwd=2, col=colorBlindBlack8[6])
#segments(0.5,0.5,0.5,1, lwd=2)
#segments(0.5,0.5,1,0.5, lwd=2)
segments(0.0,0.0,1,1, lwd=2, lty=3)
polygon(c(0.5,0.5,1,1), c(0.5,1,1,0.5), col=adjustcolor(colorBlindBlack8[1], alpha=0.25))
s1.vals  <-  seq(0,0.5,0.001)
lines(KO_InvCond_up(s1 = s1.vals, S=0.2)[KO_InvCond_up(s1 = s1.vals, S=0.2) <=1] ~ s1.vals[KO_InvCond_up(s1 = s1.vals, S=0.2) <=1], ylim=c(0, 1), lwd=2, col=colorBlindBlack8[1])
lines(KO_InvCond_up(s1 = s1.vals, S=0.4)[KO_InvCond_up(s1 = s1.vals, S=0.4) <=1] ~ s1.vals[KO_InvCond_up(s1 = s1.vals, S=0.4) <=1], ylim=c(0, 1), lwd=2, col=colorBlindBlack8[2])
lines(KO_InvCond_up(s1 = s1.vals, S=0.6)[KO_InvCond_up(s1 = s1.vals, S=0.6) <=1] ~ s1.vals[KO_InvCond_up(s1 = s1.vals, S=0.6) <=1], ylim=c(0, 1), lwd=2, col=colorBlindBlack8[3])
lines(KO_InvCond_up(s1 = s1.vals, S=0.8)[KO_InvCond_up(s1 = s1.vals, S=0.8) <=1] ~ s1.vals[KO_InvCond_up(s1 = s1.vals, S=0.8) <=1], ylim=c(0, 1), lwd=2, col=colorBlindBlack8[4])
lines(KO_InvCond_up(s1 = s1.vals, S=0.98)[KO_InvCond_up(s1 = s1.vals, S=0.98) <=1] ~ s1.vals[KO_InvCond_up(s1 = s1.vals, S=0.98) <=1], ylim=c(0, 1), lwd=2, col=colorBlindBlack8[6])
#dev.off()
 
}
KO_InvPlot()