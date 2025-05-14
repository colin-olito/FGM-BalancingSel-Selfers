###############
# DEPENDENCIES
###############
library(extrafont)
library(fontcm)
loadfonts(quiet = TRUE)
library(wesanderson)
#library(TeachingDemos)

#######################
# AUXILLIARY FUNCTIONS
#######################

toPdf <- function(expr, filename, ...) {
  toDev(expr, pdf, filename, ...)
}

figPath  <-  function(name) {
  file.path('./figs', name)
}

toDev <- function(expr, dev, filename, ..., verbose=TRUE) {
  if ( verbose )
    cat(sprintf('Creating %s\n', filename))
   dev(filename, family="Times", ...)
#    dev(filename, family='Arial', ...)
    on.exit(dev.off())
    eval.parent(substitute(expr))
}



####################
# PLOTTING FUNCTIONS
####################

#' Plot text or points according to relative axis position.
#'
#' @title Plot text or points according to relative axis position
#' @param px Relative x-axis position (in proportion) where character is to be plotted.
#' @param py Relative y-axis position (in proportion) where character is to be plotted.
#' @param lab Plotted text. Works if argument \code{\link[graphics]{text}} is \code{TRUE}.
#' @param adj See argument of same name in R base function \code{\link[graphics]{par}}.
#' @param text Logical. Should text or points be plotted?
#' @param log Used if the original plot uses the argument log, e.g. \code{log='x'}, \code{log='y'} or \code{log='xy'}.
#' @param ... Additional arguments to R base function \code{\link[graphics]{text}}.
#' @export
proportionalLabel <- function(px, py, lab, adj=c(0, 1), text=TRUE, log=FALSE, ...) {
    usr  <-  par('usr')
    x.p  <-  usr[1] + px*(usr[2] - usr[1])
    y.p  <-  usr[3] + py*(usr[4] - usr[3])
    if(log=='x') {
        x.p<-10^(x.p)
    }
    if(log=='y') {
        y.p<-10^(y.p)
    }
    if(log=='xy') {
        x.p<-10^(x.p)
        y.p<-10^(y.p)
    }
    if(text){
        text(x.p, y.p, lab, adj=adj, ...)
    } else {
        points(x.p, y.p, ...)
    }
}



proportionalArrows <- function(px1, py1, px2, py2, adj=c(0, 1), log=FALSE, length=length, ...) {
    usr  <-  par('usr')
    x.p1  <-  usr[1] + px1*(usr[2] - usr[1])
    y.p1  <-  usr[3] + py1*(usr[4] - usr[3])
    x.p2  <-  usr[1] + px2*(usr[2] - usr[1])
    y.p2  <-  usr[3] + py2*(usr[4] - usr[3])
    if(log=='x') {
        x.p1  <-  10^(x.p1)
        x.p2  <-  10^(x.p2)
    }
    if(log=='y') {
        y.p1  <-  10^(y.p1)
        y.p2  <-  10^(y.p2)
    }
    if(log=='xy') {
        x.p1  <-  10^(x.p1)
        y.p1  <-  10^(y.p1)
        x.p2  <-  10^(x.p2)
        y.p2  <-  10^(y.p2)
    }
    arrows(x0=x.p1, y0=y.p1, x1=x.p2, y1=y.p2, length=length,...)
}

#' Draw equally-spaced white lines on plot window.
#'
#' @title Equally-spaced white lines on plot window
#' @param ... Additional arguments to internal function \code{\link{proportionalLabel}}.
#' @author Diego Barneche
#' @export
plotGrid  <-  function(lineCol='white',...) {
    proportionalLabel(rep(0.2, 2), c(0,1), text=FALSE, type='l', col=lineCol, lwd=0.5, ...)
    proportionalLabel(rep(0.4, 2), c(0,1), text=FALSE, type='l', col=lineCol, lwd=0.5, ...)
    proportionalLabel(rep(0.6, 2), c(0,1), text=FALSE, type='l', col=lineCol, lwd=0.5, ...)
    proportionalLabel(rep(0.8, 2), c(0,1), text=FALSE, type='l', col=lineCol, lwd=0.5, ...)
    proportionalLabel(c(0,1), rep(0.2, 2), text=FALSE, type='l', col=lineCol, lwd=0.5, ...)
    proportionalLabel(c(0,1), rep(0.4, 2), text=FALSE, type='l', col=lineCol, lwd=0.5, ...)
    proportionalLabel(c(0,1), rep(0.6, 2), text=FALSE, type='l', col=lineCol, lwd=0.5, ...)
    proportionalLabel(c(0,1), rep(0.8, 2), text=FALSE, type='l', col=lineCol, lwd=0.5, ...)
}


#' Internal. Create nice rounded numbers for plotting.
#'
#' @title Rounded numbers for plotting
#' @param value A numeric vector.
#' @param precision Number of rounding digits.
#' @return A character vector.
#' @author Diego Barneche.
rounded  <-  function(value, precision=1) {
  sprintf(paste0('%.', precision, 'f'), round(value, precision))
}


#' Creates transparent colours
#'
#' @title Creates transparent colours
#' @param col Colour.
#' @param opacity equivalent to alpha transparency parameter
#' @export
transparentColor <- function(col, opacity=0.5) {
    if (length(opacity) > 1 && any(is.na(opacity))) {
        n        <-  max(length(col), length(opacity))
        opacity  <-  rep(opacity, length.out=n)
        col      <-  rep(col, length.out=n)
        ok       <-  !is.na(opacity)
        ret      <-  rep(NA, length(col))
        ret[ok]  <-  Recall(col[ok], opacity[ok])
        ret
    } else {
        tmp  <-  col2rgb(col)/255
        rgb(tmp[1,], tmp[2,], tmp[3,], alpha=opacity)
    }
}

fibonacci.scale  <-  function(n) {
    fibs  <-  c(0,1)
    for(i in 2:n) {
        fibs  <-  c(fibs, (fibs[i] + fibs[i-1]))
    }
    (fibs/max(fibs))[-2]
}


###########################################
# Additional functions used for plotting
###########################################

#' Gaussian Error Function 
erf <- function(x) {
    2*pnorm(x * sqrt(2)) - 1
}

# Convenience functions a & b from main text
funa  <-  function(F,h) {
  (1 - (1 - F)*h^2)/(1 - (1 - F)*h)
}
funb  <-  function(F,h) {
  (F + (1 - F)*h^2)/(F + (1 - F)*h)
}



############################################
# Figures for paper
############################################

######################################
# Figure 1.
# (A) Illustration of invasion conditions and proportion of parameter
# space permitting balancing selection under classic weak-selection
# approximation
# (B) Contribution of polymorphic locus to V_G under inbreeding
VGHat  <-  function(s1, s2, F) {
                pHat  <-  (s2 - F*s1)/((1 - F)*(s2 + s1))
                pHat[pHat > 1]  <-  1
                pHat[pHat < 0]  <-  0
                qHat  <-  1-pHat
                pHat*qHat*s1*s2*(1 - F^2)
               }

ExpVGHat  <-  function(F, smax) {
                xterm= (1+F)/2
                ( ((1 + F)^2 * (3 - 2*F - F^2))/(4*(1 - F)) + (1 + F)^3/(1 - F) * log(x=xterm) )*smax^2
               }

classicWkSel_VG_Fig  <-  function() {

    # Set inbreeding values
    F.I = c(0.1, 0.3, 0.5, 0.7, 0.9)

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,2), bty='o', xaxt='s', yaxt='s')
     # Panel (A) Polymorphic space
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,0.1), ylim = c(0,0.1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        s2  <-  0:1000/10000
        for(i in 1:length(F.I)) {
            upper  <-  upper.classic(F=F.I[i], s.2 = s2)
            lower  <-  lower.classic(F=F.I[i], s.2 = s2)
            lines(upper[upper < 0.1 ] ~ s2[upper < 0.1 ], lwd=2, col=COL8[i])
            lines(lower ~ s2, lwd=2, col=COL8[i])
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075,  expression(paste("A")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.175,   expression(paste("Parameter space")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("of balancing selection")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( -0.25,  0.5,  expression(paste("Selection against ", italic(aa), " ",(italic(s[2])))), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)
        proportionalLabel( 0.5,  -0.25,  expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=1.3, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel( 0.875,  0.17, expression(paste(italic(F), " = ", 0.1)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=8, col=COL8[1])
        proportionalLabel( 0.875,  0.33, expression(paste(italic(F), " = ", 0.3)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=13, col=COL8[2])
        proportionalLabel( 0.875,  0.5,  expression(paste(italic(F), " = ", 0.5)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=25, col=COL8[3])
        proportionalLabel( 0.875,  0.67, expression(paste(italic(F), " = ", 0.7)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=30, col=COL8[4])
        proportionalLabel( 0.875,  0.84, expression(paste(italic(F), " = ", 0.9)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=40, col=COL8[5])

     # Panel (B) Genetic Variance under the classic model
    # Set inbreeding values
    Fvals = 0:100/100
    smax  = 0.1     
     plot(NA, axes=FALSE, type='n', main='', xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        expVG  <-  ExpVGHat(smax=smax, F=Fvals)
        expVG.out  <-  ExpVGHat(smax=smax, F=0)
        points(expVG/expVG.out ~ Fvals, type='l', lwd=2, col=COL8[1])
        #  Calculate E[\hat{V}_G] from simulated selection coefficients
        F.sim      <-  0:10/10
        expVG.sim  <- c()
        for(i in 1:length(F.sim)) {
            s1vals        <-  runif(min=0, max=smax, n=10^6)
            s2vals        <-  runif(min=0, max=smax, n=10^6)
            VGvals        <-  VGHat(s1=s1vals, s2=s2vals, F=F.sim[i])
            expVG.sim[i]  <-  mean(VGvals)
        }
        points((expVG.sim[c(1,3,5,7,9,11)]/expVG.sim[1]) ~ F.sim[c(1,3,5,7,9,11)], pch=21, col=transparentColor(COL8[1], opacity=0.5), bg=transparentColor(COL8[1], opacity=0.4), cex=1.5)
        points((expVG.sim[2]/expVG.sim[1]) ~ F.sim[2], pch=21, col=COL8[1], bg=transparentColor(COL8[1], opacity=0.8), cex=1.5)
            segments(x0=F.sim[2], F.sim[2], y0=0, x1 = F.sim[2], y1 = (expVG.sim[2]/expVG.sim[1]), lty=2, lwd=2, col=transparentColor(COL8[1], opacity=0.8))
        points((expVG.sim[4]/expVG.sim[1]) ~ F.sim[4], pch=21, col=COL8[2], bg=transparentColor(COL8[2], opacity=0.8), cex=1.5)
            segments(x0=F.sim[4], F.sim[4], y0=0, x1 = F.sim[4], y1 = (expVG.sim[4]/expVG.sim[1]), lty=2, lwd=2, col=transparentColor(COL8[2], opacity=0.8))
        points((expVG.sim[6]/expVG.sim[1]) ~ F.sim[6], pch=21, col=COL8[3], bg=transparentColor(COL8[3], opacity=0.8), cex=1.5)
            segments(x0=F.sim[6], F.sim[6], y0=0, x1 = F.sim[6], y1 = (expVG.sim[6]/expVG.sim[1]), lty=2, lwd=2, col=transparentColor(COL8[3], opacity=0.8))
        points((expVG.sim[8]/expVG.sim[1]) ~ F.sim[8], pch=21, col=COL8[4], bg=transparentColor(COL8[4], opacity=0.8), cex=1.5)
            segments(x0=F.sim[8], F.sim[8], y0=0, x1 = F.sim[8], y1 = (expVG.sim[8]/expVG.sim[1]), lty=2, lwd=2, col=transparentColor(COL8[4], opacity=0.8))
        points((expVG.sim[10]/expVG.sim[1]) ~ F.sim[10], pch=21, col=COL8[5], bg=transparentColor(COL8[5], opacity=0.8), cex=1.5)
            segments(x0=F.sim[10], F.sim[10], y0=0, x1 = F.sim[10], y1 = (expVG.sim[10]/expVG.sim[1]), lty=2, lwd=2, col=transparentColor(COL8[5], opacity=0.8))
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075,   expression(paste("B")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.175,   expression(paste("Expected contribution")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("to genetic variance")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.25,  0.5,   expression(paste("E[", italic(V[G]), "] / E[", italic(V[G])," | ", italic(F)==0, "]")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
#        proportionalLabel(-0.25,  0.5,   expression(E*group("(",hat(V)[G],")")/E*group("(",hat(V)[G]*" | "*F==0,")"), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)

}



######################################
# Figure 2.
# Balancing Selection among new mutations
PrBalPredNewMut = function(x, a, b){
  0.5*(erf(a*x/sqrt(2)) - erf(b*x/sqrt(2)))
}

R.new.smallMut = function(F, h){
  (1 - F^2)*h*(1 - h)/((F + (1 - F)*h)*(1 - (1 - F)*h))
}
R.new.largeMut = function(F, h){
  (1 - F^2)*h^2*(1 - h^2)/((F + (1 - F)*h^2)*(1 - (1 - F)*h^2))
}

BalSelNewMutationsFig  <-  function(xAvg = 2, n = 50, z = 1, F = 1/2, h = 1/2, reps=10^5, inclSmallMuts=FALSE) {

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate data to plot
    x.series    <- c(0.05, seq(0.25,5,0.25))
    Pr.bal.sim  <-  vector()
    Fvals       <-  c(0, 0.25, 0.5)
    O = rep(0, n) #optimal phenotype
    A = c(-z, rep(0, n - 1)) #wild-type phenotype
    for(f in 1:length(Fvals)) {
        s.het = vector()
        s.hom = vector()
        for(j in 1:length(x.series)){
        r = x.series[j]*(2*z)/sqrt(n)
            for(i in 1:reps){
                mut = rnorm(n)
                A.het = A + r*h*mut/sqrt(sum(mut^2))
                z.het = sqrt(sum((A.het - O)^2))
                A.hom = A + r*mut/sqrt(sum(mut^2))
                z.hom = sqrt(sum((A.hom - O)^2))
                s.het[i] = exp(-z.het^2/2)/exp(-z^2/2) - 1
                s.hom[i] = exp(-z.hom^2/2)/exp(-z^2/2) - 1
                }
            Pr.bal.sim  <-  c(Pr.bal.sim, sum(s.het*(1 - Fvals[f]) > s.hom & s.het*(1 - Fvals[f]) > -Fvals[f]*s.hom)/reps)
            }
        }
        plotDat  <-  as.data.frame(cbind(Pr.bal.sim, rep(x.series, times=3), rep(Fvals, each=length(x.series))))
        names(plotDat)  <-  c("Pr.bal.sim", "xSeries", "F")
    # Generate predicted lines
    pred.x  <-  c(0:100)/20
    Pred1  <-  PrBalPredNewMut(x=pred.x, a=funa(F=Fvals[1], h=1/2), b=funb(F=Fvals[1], h=1/2))
    Pred2  <-  PrBalPredNewMut(x=pred.x, a=funa(F=Fvals[2], h=1/2), b=funb(F=Fvals[2], h=1/2))
    Pred3  <-  PrBalPredNewMut(x=pred.x, a=funa(F=Fvals[3], h=1/2), b=funb(F=Fvals[3], h=1/2))

    # set plot layout
    layout.mat  <-  matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Create L=Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,2), bty='o', xaxt='s', yaxt='s')
     # Panel (A) Polymorphic space
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,5), ylim = c(0,0.25), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Predicted Curves
        lines(Pred1 ~ pred.x, lwd=2, col = COL8[1])
        lines(Pred2 ~ pred.x, lwd=2, col = COL8[2])
        lines(Pred3 ~ pred.x, lwd=2, col = COL8[3])
        # Simulation Points
        points(Pr.bal.sim[F==0] ~ xSeries[F==0], pch=21, col = transparentColor(COL8[1], opacity=0.8), bg = transparentColor(COL8[1], opacity=0.5), data=plotDat)
        points(Pr.bal.sim[F==0.25] ~ xSeries[F==0.25], pch=21, col = transparentColor(COL8[2], opacity=0.8), bg = transparentColor(COL8[2], opacity=0.5), data=plotDat)
        points(Pr.bal.sim[F==0.5] ~ xSeries[F==0.5], pch=21, col = transparentColor(COL8[3], opacity=0.8), bg = transparentColor(COL8[3], opacity=0.5), data=plotDat)
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.05,   expression(paste("A")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( -0.25,  0.5, expression(paste("Pr(bal. | ", italic(x), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)
        proportionalLabel( 0.5,  -0.25, expression(paste("Scaled mutation size (", italic(x), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)        
        # Legend
        legend(
               x       =  usr[2]*0.78,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(" ")),
                            expression(paste(" ")),
                            expression(paste(" "))),
               lty     =  1,
               lwd     =  2,
               col     =  c(COL8[1], COL8[2], COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        legend(
               x       =  usr[2]*0.99,
               y       =  usr[4]*0.99,
               legend  =  c(expression(italic(F)==0.0),
                            expression(italic(F)==0.25),
                            expression(italic(F)==0.5)),
               pch     =  21,
               col     =  c(transparentColor(COL8[1], opacity=0.8),
                            transparentColor(COL8[2], opacity=0.8),
                            transparentColor(COL8[3], opacity=0.8)),
               pt.bg   =  c(transparentColor(COL8[1], opacity=0.5),
                            transparentColor(COL8[2], opacity=0.5),
                            transparentColor(COL8[3], opacity=0.5)),
               pt.cex  =  1,
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

    # Generate data to plot
    F.series    <- c(0:100)/100
    if(inclSmallMuts) {
        R.new.smallMut.Sims.h0.5  <-  relBalancing_F_Sims(xAvg = xAvg, h = 1/2)
        R.new.smallMut.Sims.h0.1  <-  relBalancing_F_Sims(xAvg = xAvg, h = 1/10)
    }
    R.new.largeMut.Sims.h0.5  <-  relBalancing_F_Sims(xAvg = xAvg, h = 1/2, largeMut=TRUE)
    R.new.largeMut.Sims.h0.1  <-  relBalancing_F_Sims(xAvg = xAvg, h = 1/10, largeMut=TRUE)

     # Panel (B) Rbal for new mutations
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Predicted Curves
        if(inclSmallMuts){
            lines(R.new.smallMut(F=F.series, h=0.5) ~ F.series, lty=1, lwd=2, col = COL8[2])
            lines(R.new.smallMut(F=F.series, h=0.1) ~ F.series, lty=1, lwd=2, col = COL8[3])
            points(rBal ~ F, pch=21, col = transparentColor(COL8[2], opacity=0.8), bg = transparentColor(COL8[2], opacity=0.5), data=R.new.smallMut.Sims.h0.5)
            points(rBal ~ F, pch=21, col = transparentColor(COL8[3], opacity=0.8), bg = transparentColor(COL8[3], opacity=0.5), data=R.new.smallMut.Sims.h0.1)
            lines(R.new.largeMut(F=F.series, h=0.5) ~ F.series, lty=2, lwd=2, col = COL8[2])
            lines(R.new.largeMut(F=F.series, h=0.1) ~ F.series, lty=2, lwd=2, col = COL8[3])
        }
        if(!inclSmallMuts){
            lines(R.new.largeMut(F=F.series, h=0.5) ~ F.series, lty=1, lwd=2, col = COL8[2])
            lines(R.new.largeMut(F=F.series, h=0.1) ~ F.series, lty=1, lwd=2, col = COL8[3])
        }
        lines((1 - F.series) ~ F.series, lwd=2, col = COL8[1])
        # Simulation Points
        points(rBal ~ F, pch=21, col = transparentColor(COL8[2], opacity=0.8), bg = transparentColor(COL8[2], opacity=0.5), data=R.new.largeMut.Sims.h0.5)
        points(rBal ~ F, pch=21, col = transparentColor(COL8[3], opacity=0.8), bg = transparentColor(COL8[3], opacity=0.5), data=R.new.largeMut.Sims.h0.1)
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.05,   expression(paste("B")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.25,  0.5,  expression(paste("Relative fraction (", italic(R[new]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        if(inclSmallMuts){
        legend(
               x       =  usr[2]*0.55,
               y       =  usr[4]*0.99,
               legend  =  c(" ",
                            " ",
                            " ",
                            " ",
                            " "),
               lty     =  c(1, 1, 2, 1, 2),
               lwd     =  2,
               col     =  c(COL8[1],
                            COL8[2],
                            COL8[2],
                            COL8[3],
                            COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Legend
        legend(
               x       =  usr[2],
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste("small-", italic(x), " (", italic(nu)==0.5,")", sep="")),
                            expression(paste("variable-", italic(x), " (", italic(nu)==0.5,")", sep="")),
                            expression(paste("small-", italic(x), " (", italic(nu)==0.1,")", sep="")),
                            expression(paste("variable-", italic(x), " (", italic(nu)==0.1,")", sep=""))),
               pch     =  21,
               pt.bg   =  c(NA,
                            transparentColor(COL8[2], opacity=0.5),
                            transparentColor(COL8[2], opacity=0.5),
                            transparentColor(COL8[3], opacity=0.5),
                            transparentColor(COL8[3], opacity=0.5)),
               col     =  c(NA,
                            COL8[2],
                            COL8[2],
                            COL8[3],
                            COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
           }
    if(!inclSmallMuts) {
        legend(
               x       =  usr[2]*0.825,
               y       =  usr[4]*0.99,
               legend  =  c(" ",
                            " ",
                            " "),
               lty     =  c(1, 1, 1),
               lwd     =  2,
               col     =  c(COL8[1],
                            COL8[2],
                            COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Legend
        legend(
               x       =  usr[2],
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste(italic(nu)==0.5, sep="")),
                            expression(paste(italic(nu)==0.1, sep=""))),
               pch     =  21,
               pt.bg   =  c(NA,
                            transparentColor(COL8[2], opacity=0.5),
                            transparentColor(COL8[3], opacity=0.5)),
               col     =  c(NA,
                            COL8[2],
                            COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
    }
}


######################################
# Figure 3. Ratio of balancing selection among 
#           Favoured Mutations

# Ancillary functions for this figure
Pr.inv.F  <-  function(x, F.I, h){
  b  <-  (F.I + (1 - F.I)*h^2)/(F.I + (1 - F.I)*h)
  1 - pnorm(x*b)
}
Pr.bal.F = function(x, F.I, h){
  a  <-  (1 - (1 - F.I)*h^2)/(1 - (1 - F.I)*h)
  b  <-  (F.I + (1 - F.I)*h^2)/(F.I + (1 - F.I)*h)
  pnorm(x*a) - pnorm(x*b)
}
Pr.pos.F = function(x, F.I, h){
  a  <-  (1 - (1 - F.I)*h^2)/(1 - (1 - F.I)*h)
  1 - pnorm(x*a)
}
Pr.inv = function(x, h){
  b  <-  h
  1 - pnorm(x*b)
}
Pr.bal = function(x, h){
  a  <-  (1 + h)
  b  <-  h
  pnorm(x*a) - pnorm(x*b)
}
Pr.pos = function(x, h){
  a  <-  (1 + h)
  b  <-  h
  1 - pnorm(x*a)
}
#Rbal_smallMutLimit  <-  function(F.I, h){
#    (1 - F.I)*h*(1 - h)*(1/(1 - (1 - F.I)*h) + 1/(F.I + (1 - F.I)*h))
#}
Rbal_smallMutLimit  <-  function(F, h){
    ((1 - F^2)*h*(1 - h)) / ((F + (1 - F)*h) * (1 - (1 - F)*h))
}

Rbal_largex_fav  <-  function(F, h) {
    ((1 - F^2)*h*(1 - h^2)) / ((F + (1 - F)*h) * (1 - (1 - F)*h^2))

}

# Ratio of inbred/outcross balancing selection
RelBalancingFavored_Fig  <-  function(xAvg = 1, favouredMuts = TRUE) {
    
    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(1, nrow=1, ncol=1, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,1), bty='o', xaxt='s', yaxt='s')
     # Panel (A) -- Small Mutation Limit 
     # Proportion of parameter space where balancing selection occurs
    F.values = 0:100/100
    h.i = c(0.5, 0.25, 0.1)
     # Panel (A) -- Variable Mutation Size 
     # Proportion of parameter space where balancing selection occurs
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        lines(Rbal_largex_fav(F = F.values, h = h.i[1]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[2], lwd = 3)
        lines(Rbal_largex_fav(F = F.values, h = h.i[2]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[3], lwd = 3)
        lines(Rbal_largex_fav(F = F.values, h = h.i[3]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[4], lwd = 3)
        lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 3)
        # Simulations
        RbalSim_h1  <-  relBalancing_F_Sims(xAvg=xAvg, h=1/2, largeMut = TRUE)
        RbalSim_h2  <-  relBalancing_F_Sims(xAvg=xAvg, h=1/4, largeMut = TRUE)
        RbalSim_h3  <-  relBalancing_F_Sims(xAvg=xAvg, h=1/10, largeMut = TRUE)
        if(favouredMuts) {
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim_h1)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim_h2)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[4], opacity=0.6), bg=transparentColor(COL8[4], opacity=0.4), data=RbalSim_h3)
            } else {
            points(rBal ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim_h1)
            points(rBal ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim_h2)
            points(rBal ~ F, pch=21, col=transparentColor(COL8[4], opacity=0.6), bg=transparentColor(COL8[4], opacity=0.4), data=RbalSim_h3)
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
#        proportionalLabel(0.05,  1.075,   expression(paste("B")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
#        proportionalLabel(0.5,  1.075,   expression(paste("Variable mutation size", sep="")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel(-0.25,  0.5,  expression(paste("Relative fraction (", italic(R[adapt.]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.82,
               y       =  usr[4]*0.99,
               legend  =  c(" ",
                            " ",
                            " ",
                            " "),
               lty     =  1,
               lwd     =  3,
               col     =  c(COL8[1], COL8[2], COL8[3], COL8[4]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Legend
        legend(
               x       =  usr[2]*0.99,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste(italic(nu)==1/2)),
                            expression(paste(italic(nu)==1/4)),
                            expression(paste(italic(nu)==1/10))),
               pch     =  21,
               pt.bg   =  c(NA,
                            transparentColor(COL8[2], opacity=0.5),
                            transparentColor(COL8[3], opacity=0.5),
                            transparentColor(COL8[4], opacity=0.5)),
               col     =  c(NA,
                            COL8[2],
                            COL8[3],
                            COL8[4]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
}



# Summary Figure showing small & large mutation size results
# for the relative fraction of favored and established mutations
# under balancing selection

# Ancillary functions for this figure
Rbal_largeMut_est  <-  function(F) {
    (27* (1 - F)^2 * (7*F + 5)) / (5*(3 + F)^3 )
}


# Summary Figure for ratio of inbred/outcross balancing selection
RelBalancing_SummaryFig_wVG  <-  function(xAvg = 2, h=1/2, Ne = 10^4, sim.reps=10^3) {
    
    # Import VG data
    dat1     <-  read.csv(file="./out/EstabMut_VG_xBar1_Ne1000_n50_z1_h0.5_nMuts1e+07.csv", header=TRUE)    
    dat2     <-  read.csv(file="./out/EstabMut_VG_xBar5_Ne1000_n50_z1_h0.5_nMuts1e+07.csv", header=TRUE)    

    # Inbreeding values
    F.values = 0:100/100

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,2), bty='o', xaxt='s', yaxt='s')
     # Ratio of Balancing selection for inbreeding to outcrossing
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
            # Favoured mutations
            lines(Rbal_largex_fav(F=F.values, h=1/2) ~ F.values, lty = 1, lwd = 3, col = COL8[2])
            # Established mutations
            lines(Rbal_largeMut_est(F=F.values) ~ F.values, lty = 1, lwd = 3, col = COL8[3])
        # Simulations
            RbalSim.F        <-  read.csv("./out/relBal_variable_x_EstabMuts_xAvg5_Ne1e+05_n50_z1_h0.5_reps10000.csv", header=TRUE)
            RbalSim.large    <-  relBalancing_F_Sims(xAvg=xAvg, h=1/2, largeMut = TRUE)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim.large)
            points(rBal.est ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.9), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim.F)
            # Naive expectation
            lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 3)
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.05,   expression(paste("A")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.175,   expression(paste("Relative probability")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("of balancing selection")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.25,  0.5,  expression(paste("Relative fraction (", italic(R[bal.]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legends
        legend(
               x       =  usr[2]*0.73,
               y       =  usr[4],
               legend  =  c(" ", " ", " "),
               lty     =  1,
               lwd     =  3,
               col     =  c(COL8[1], COL8[2], COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        legend(
               x       =  usr[2],
               y       =  usr[4],
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste("Adaptive")),
                            expression(paste("Established"))),
               pch     =  21,
               pt.bg   =  c(NA, 
                            transparentColor(COL8[2], opacity=0.5), 
                            transparentColor(COL8[3], opacity=0.5)),
               col     =  c(NA,
                            COL8[2],
                            COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )


     # Panel (B) VG contributed by established loci
     plot(NA, axes=FALSE, type='n', main='', xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        relVG  <-  relative_VG(F=F.values)
        points(relVG ~ F.values, type='l', lwd=2, col=COL8[1])
        #  Calculate E[\hat{V}_G] from simulated selection coefficients
        lines((ExpVG/ExpVG[1]) ~ F, lty=2, lwd=2, col=transparentColor(COL8[1], opacity=0.6), data=dat1)
        points((ExpVG/ExpVG[1]) ~ F, pch=21, col=COL8[1], bg=transparentColor(COL8[1], opacity=0.6), cex=1.5, data=dat1)
        lines((ExpVG/ExpVG[1]) ~ F, lty=2, lwd=2, col=transparentColor(COL8[1], opacity=0.3), data=dat2)
        points((ExpVG/ExpVG[1]) ~ F, pch=21, col=transparentColor(COL8[1], opacity=0.6), bg=transparentColor(COL8[1], opacity=0.3), cex=1.5, data=dat2)
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.05,   expression(paste("B")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.175,   expression(paste("Expected contribution")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("to genetic variance")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.2,  0.5,   expression(paste("E[", italic(V[G]), "] / E[", italic(V[G]),"; ",italic(F)==0, "]")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.955,
               y       =  usr[4]*0.975,
               legend  =  c(expression(paste(bar(italic(x))==1)),
                            expression(paste(bar(italic(x))==5))),
               lty     =  c(2,2),
               seg.len =  2,
               lwd     =  2,
               col     =  c(transparentColor(COL8[1], opacity=0.6),
                            transparentColor(COL8[1], opacity=0.3)),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        legend(
               x       =  usr[2]*0.8,
               y       =  usr[4]*0.975,
               legend  =  c(" ",
                            " "),
               pch     =  21,
               col     =  c(transparentColor(COL8[1], opacity=0.6),
                            transparentColor(COL8[1], opacity=0.3)),
               pt.bg     =  c(transparentColor(COL8[1], opacity=0.6),
                            transparentColor(COL8[1], opacity=0.3)),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

}





# Summary Figure for ratio of inbred/outcross balancing selection
RelBalancing_SummaryFig_presentation  <-  function(xAvg = 5, h=1/2, Ne = 10^4, sim.reps=10^3) {
    
    # Import VG data
    dat1     <-  read.csv(file="./out/EstabMut_VG_xBar1_Ne1000_n50_z1_h0.5_nMuts1e+07.csv", header=TRUE)    
    dat2     <-  read.csv(file="./out/EstabMut_VG_xBar5_Ne1000_n50_z1_h0.5_nMuts1e+07.csv", header=TRUE)    
    R.new.largeMut.Sims.h0.5  <-  relBalancing_F_Sims(xAvg = xAvg, h = 1/2, largeMut=TRUE)

    # Inbreeding values
    F.values = 0:100/100

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1), nrow=1, ncol=1, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,2), bty='o', xaxt='s', yaxt='s')
     # Ratio of Balancing selection for inbreeding to outcrossing
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
            # New mutations
            lines(R.new.largeMut(F=F.values, h=0.5) ~ F.values, lty=1, lwd=3, col = COL8[4])
            # Favoured mutations
            lines(Rbal_largex_fav(F=F.values, h=1/2) ~ F.values, lty = 1, lwd = 3, col = COL8[2])
            # Established mutations
            lines(Rbal_largeMut_est(F=F.values) ~ F.values, lty = 1, lwd = 3, col = COL8[3])
        # Simulations
            RbalSim.Estab        <-  read.csv("./out/relBal_variable_x_EstabMuts_xAvg5_Ne1e+05_n50_z1_h0.5_reps10000.csv", header=TRUE)
            RbalSim.large    <-  relBalancing_F_Sims(xAvg=xAvg, h=1/2, largeMut = TRUE)
            points(rBal ~ F, pch=21, col = transparentColor(COL8[4], opacity=0.8), bg = transparentColor(COL8[4], opacity=0.5), data=R.new.largeMut.Sims.h0.5)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim.large)
            points(rBal.est ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.9), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim.Estab)
            # Naive expectation
            lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 3)
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
#        proportionalLabel(0.05,  1.05,   expression(paste("A")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.175,   expression(paste("Relative probability")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("of balancing selection")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.25,  0.5,  expression(paste("Relative fraction (", italic(R[bal.]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legends
        legend(
               x       =  usr[2]*0.75,
               y       =  usr[4],
               legend  =  c(" ", " ", " ", " "),
               lty     =  1,
               lwd     =  3,
               col     =  c(COL8[1], COL8[4], COL8[2], COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        legend(
               x       =  usr[2],
               y       =  usr[4],
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste("New")),
                            expression(paste("Adaptive")),
                            expression(paste("Established"))),
               pch     =  21,
               pt.bg   =  c(NA, 
                            transparentColor(COL8[4], opacity=0.5), 
                            transparentColor(COL8[2], opacity=0.5), 
                            transparentColor(COL8[3], opacity=0.5)),
               col     =  c(NA,
                            COL8[4],
                            COL8[2],
                            COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

}


############################################
# Supplementary Figures
############################################
KO_InvCond_F_up  <-  function(s1, F) {
  S  <-  (2*F)/(F + 1)
  (s1*(2 - S - 2*s1))/(S*(1 - 2*s1))
}
KO_InvCond_F_low  <-  function(s1, F) {
  S  <-  (2*F)/(F + 1)
  (1/2)*(1 - sqrt(1 - S + (S^2)*(1/2 - s1)^2) - S*(1/2 - s1))
}
classicParamSpaceFig  <-  function() {

    # Set inbreeding values
    F.I = c(0.1, 0.3, 0.5, 0.7)

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2,3), nrow=1, ncol=3, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,2), bty='o', xaxt='s', yaxt='s')
     # Panel (A) Polymorphic space
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        s2  <-  0:1000/1000
        for(i in 1:4){
            upper  <-  upper.classic(F=F.I[i], s.2 = s2)
            lower  <-  lower.classic(F=F.I[i], s.2 = s2)
            lines(upper[upper < 1 ] ~ s2[upper < 1 ], lwd=2, col=COL8[i])
            lines(lower ~ s2, lwd=2, col=COL8[i])
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075,   expression(paste("A")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("Weak-selection approx.")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( -0.25,  0.5, expression(paste("Selection against ", italic(aa), " ",(italic(s[2])))), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)
        proportionalLabel( 0.5,  -0.25, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=1.3, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel( 0.875,  0.17, expression(paste(italic(F), " = ", 0.1)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=8, col=COL8[1])
        proportionalLabel( 0.875,  0.33,  expression(paste(italic(F), " = ", 0.3)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=13, col=COL8[2])
        proportionalLabel( 0.875,  0.5,  expression(paste(italic(F), " = ", 0.5)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=25, col=COL8[3])
        proportionalLabel( 0.875,  0.67,  expression(paste(italic(F), " = ", 0.7)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=30, col=COL8[4])


    # Panel (B) Kimura & Ohto (1971) invasion plot
    plot(NA, axes=FALSE, type='n', main='', xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot lower invasion boundaries
        s1.vals  <-  seq(0,1,0.01)
        lines(KO_InvCond_F_low(s1 = s1.vals, F=0.1) ~ s1.vals, ylim=c(0, 1), lwd=2, col=COL8[1])
        lines(KO_InvCond_F_low(s1 = s1.vals, F=0.3) ~ s1.vals, ylim=c(0, 1), lwd=2, col=COL8[2])
        lines(KO_InvCond_F_low(s1 = s1.vals, F=0.5) ~ s1.vals, ylim=c(0, 1), lwd=2, col=COL8[3])
        lines(KO_InvCond_F_low(s1 = s1.vals, F=0.7) ~ s1.vals, ylim=c(0, 1), lwd=2, col=COL8[4])
        lines(KO_InvCond_F_low(s1 = s1.vals, F=0.98) ~ s1.vals, ylim=c(0, 1), lwd=2, col=COL8[6])
        # Polygon showing always polymorphic space
        polygon(c(0.5,0.5,1,1), c(0.5,1,1,0.5), col=adjustcolor(COL8[1], alpha=0.25))
        # Plot lower invasion boundaries
        s1.vals  <-  seq(0,0.5,0.001)
        lines(KO_InvCond_F_up(s1 = s1.vals, F=0.1)[KO_InvCond_F_up(s1 = s1.vals, F=0.1) <=1] ~ s1.vals[KO_InvCond_F_up(s1 = s1.vals, F=0.1) <=1], ylim=c(0, 1), lwd=2, col=COL8[1])
        lines(KO_InvCond_F_up(s1 = s1.vals, F=0.3)[KO_InvCond_F_up(s1 = s1.vals, F=0.3) <=1] ~ s1.vals[KO_InvCond_F_up(s1 = s1.vals, F=0.3) <=1], ylim=c(0, 1), lwd=2, col=COL8[2])
        lines(KO_InvCond_F_up(s1 = s1.vals, F=0.5)[KO_InvCond_F_up(s1 = s1.vals, F=0.5) <=1] ~ s1.vals[KO_InvCond_F_up(s1 = s1.vals, F=0.5) <=1], ylim=c(0, 1), lwd=2, col=COL8[3])
        lines(KO_InvCond_F_up(s1 = s1.vals, F=0.7)[KO_InvCond_F_up(s1 = s1.vals, F=0.7) <=1] ~ s1.vals[KO_InvCond_F_up(s1 = s1.vals, F=0.7) <=1], ylim=c(0, 1), lwd=2, col=COL8[4])
        s1.vals  <-  seq(0,0.5,0.0001)
        lines(KO_InvCond_F_up(s1 = s1.vals, F=0.98)[KO_InvCond_F_up(s1 = s1.vals, F=0.98) <=1] ~ s1.vals[KO_InvCond_F_up(s1 = s1.vals, F=0.98) <=1], ylim=c(0, 1), lwd=2, col=COL8[6])
        # Inv Cond. Annotations
        proportionalLabel( 0.875,  0.15, expression(paste(italic(F), " = ", 0.1)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=6, col=COL8[1])
        proportionalLabel( 0.875,  0.275,  expression(paste(italic(F), " = ", 0.3)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=9, col=COL8[2])
        proportionalLabel( 0.875,  0.365,  expression(paste(italic(F), " = ", 0.5)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=9, col=COL8[3])
        proportionalLabel( 0.875,  0.44,  expression(paste(italic(F), " = ", 0.7)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=8, col=COL8[4])
        proportionalLabel( 0.65,  0.45,  expression(paste(italic(F), " = ", 0.98)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=3, col=COL8[6])
        # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075,   expression(paste("B")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("Arbitrary selection strength")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.75,  0.78,   expression(paste(italic("Always"))), cex=1.2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.75,  0.72,   expression(paste(italic("Polymorphic"))), cex=1.2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( 0.5,  -0.25, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=1.3, adj=c(0.5, 0.5), xpd=NA)        
 
     # Panel (c) Proportion of parameter space where balancing selection occurs
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        F.I = 0.05 + 0:9/10
        Pr.balancing = rep(0, length(F.I))
        Pr.balancing.KO = rep(0, length(F.I))
        for(i in 1:length(F.I)){
          s.1 = runif(10^6)
          s.2 = runif(10^6)
          condition = s.1 > s.2*F.I[i] & s.1 < s.2/F.I[i]
          KOcondition = s.2 > KO_InvCond_F_low(F=F.I[i], s1=s.1) & s.1 > KO_InvCond_F_low(F=F.I[i], s1=s.2)
          Pr.balancing[i] = sum(condition)/10^6
          Pr.balancing.KO[i] = sum(KOcondition)/10^6
        }
        lines(0:1000/1000, 1 - 0:1000/1000, lwd = 2)
        points(Pr.balancing ~ F.I, cex=1.5, pch = 21, col = transparentColor(COL8[1], opacity=0.8), bg=transparentColor(COL8[1], opacity=0.4))
        points(Pr.balancing.KO ~ F.I, cex=1.5, pch = 21, col = transparentColor(COL8[2], opacity=0.8), bg=transparentColor(COL8[2], opacity=0.4))
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075,   expression(paste("C")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("Fraction of parameter space")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.2,  0.5,   expression(paste("Proportion under balancing sel.")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.94,
               y       =  usr[4]*0.99,
               legend  =  expression(paste(1-italic(F))),
               lty     =  1,
               lwd     =  2,
               col     =  COL8[1],
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        proportionalLabel(0.83,  0.84,   expression(paste("Simulations")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=0)        
        legend(
               x       =  usr[2]*0.98,
               y       =  usr[4]*0.815,
               legend  =  c(expression(paste("Weak sel.")),
                            expression(paste("Arb. sel."))),
               pch     =  21,
               col     =  c(transparentColor(COL8[1], opacity=0.8),
                            transparentColor(COL8[2], opacity=0.8)),
               pt.bg   =  c(transparentColor(COL8[1], opacity=0.4),
                            transparentColor(COL8[2], opacity=0.4)),
               pt.cex  =  1,
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

}

classicWkSelFig  <-  function() {

    # Set inbreeding values
    F.I = c(0.1, 0.3, 0.5, 0.7, 0.98)

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1), nrow=1, ncol=1, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,2), bty='o', xaxt='s', yaxt='s')
     # Panel (A) Polymorphic space
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        s2  <-  0:1000/1000
        for(i in 1:length(F.I)) {
            upper  <-  upper.classic(F=F.I[i], s.2 = s2)
            lower  <-  lower.classic(F=F.I[i], s.2 = s2)
            lines(upper[upper < 1 ] ~ s2[upper < 1 ], lwd=2, col=COL8[i])
            lines(lower ~ s2, lwd=2, col=COL8[i])
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
#        proportionalLabel(0.05,  1.075,   expression(paste("A")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
#        proportionalLabel(0.5,  1.075,   expression(paste("Weak-selection approx.")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( -0.25,  0.5, expression(paste("Selection against ", italic(aa), " ",(italic(s[2])))), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)
        proportionalLabel( 0.5,  -0.25, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=1.3, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel( 0.875,  0.17, expression(paste(italic(F), " = ", 0.1)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=8, col=COL8[1])
        proportionalLabel( 0.875,  0.33,  expression(paste(italic(F), " = ", 0.3)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=13, col=COL8[2])
        proportionalLabel( 0.875,  0.5,  expression(paste(italic(F), " = ", 0.5)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=25, col=COL8[3])
        proportionalLabel( 0.875,  0.67,  expression(paste(italic(F), " = ", 0.7)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=30, col=COL8[4])
        proportionalLabel( 0.875,  0.81,  expression(paste(italic(F), " = ", 0.98)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=40, col=COL8[5])
}



# Rbal for adaptive mutations
RelBalancingFavored_wSmall_Fig  <-  function(xAvg=2, favouredMuts = TRUE) {
    
    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,1), bty='o', xaxt='s', yaxt='s')
     # Panel (A) -- Small Mutation Limit 
     # Proportion of parameter space where balancing selection occurs
    F.values = 0:100/100
    h.i = c(0.5, 0.25, 0.1)
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        lines(Rbal_smallMutLimit(F = F.values, h = h.i[1]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[2], lwd = 3)
        lines(Rbal_smallMutLimit(F = F.values, h = h.i[2]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[3], lwd = 3)
        lines(Rbal_smallMutLimit(F = F.values, h = h.i[3]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[4], lwd = 3)
        lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 3)
        # Simulations
        RbalSim_h1  <-  relBalancing_F_Sims(xAvg=0.05, h=1/2, largeMut = FALSE)
        RbalSim_h2  <-  relBalancing_F_Sims(xAvg=0.05, h=1/4, largeMut = FALSE)
        RbalSim_h3  <-  relBalancing_F_Sims(xAvg=0.05, h=1/10, largeMut = FALSE)
        if(favouredMuts) {
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim_h1)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim_h2)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[4], opacity=0.6), bg=transparentColor(COL8[4], opacity=0.4), data=RbalSim_h3)
            } else {
            points(rBal ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim_h1)
            points(rBal ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim_h2)
            points(rBal ~ F, pch=21, col=transparentColor(COL8[4], opacity=0.6), bg=transparentColor(COL8[4], opacity=0.4), data=RbalSim_h3)
        }
        # axes
        axis(1, las=1)
        axis(2, las=1, labels=TRUE)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075,   expression(paste("A")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("Small-mutation limit")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel(-0.25,  0.5,  expression(paste("Relative fraction (", italic(R[adapt.]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.61,
               y       =  usr[4]*0.99,
               legend  =  c(" ",
                            " ",
                            " ",
                            " "),
               lty     =  1,
               lwd     =  3,
               col     =  c(COL8[1], COL8[2], COL8[3], COL8[4]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Legend
        legend(
               x       =  usr[2]*0.99,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste(italic(nu)==1/2, " (Eq.B15)")),
                            expression(paste(italic(nu)==1/4, " (Eq.B15)")),
                            expression(paste(italic(nu)==1/10, " (Eq.B15)"))),
               pch     =  21,
               pt.bg   =  c(NA,
                            transparentColor(COL8[2], opacity=0.5),
                            transparentColor(COL8[3], opacity=0.5),
                            transparentColor(COL8[4], opacity=0.5)),
               col     =  c(NA,
                            COL8[2],
                            COL8[3],
                            COL8[4]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
     # Panel (B) -- Large Mutation Limit 
     # Proportion of parameter space where balancing selection occurs
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        lines(Rbal_largex_fav(F = F.values, h = h.i[1]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[2], lwd = 3)
        lines(Rbal_largex_fav(F = F.values, h = h.i[2]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[3], lwd = 3)
        lines(Rbal_largex_fav(F = F.values, h = h.i[3]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[4], lwd = 3)
        lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 3)
        # Simulations
        RbalSim_h1  <-  relBalancing_F_Sims(xAvg=xAvg, h=1/2, largeMut = TRUE)
        RbalSim_h2  <-  relBalancing_F_Sims(xAvg=xAvg, h=1/4, largeMut = TRUE)
        RbalSim_h3  <-  relBalancing_F_Sims(xAvg=xAvg, h=1/10, largeMut = TRUE)
        if(favouredMuts) {
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim_h1)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim_h2)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[4], opacity=0.6), bg=transparentColor(COL8[4], opacity=0.4), data=RbalSim_h3)
            } else {
            points(rBal ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim_h1)
            points(rBal ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim_h2)
            points(rBal ~ F, pch=21, col=transparentColor(COL8[4], opacity=0.6), bg=transparentColor(COL8[4], opacity=0.4), data=RbalSim_h3)
        }
        # axes
        axis(1, las=1)
        axis(2, las=1, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075,   expression(paste("B")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("Variable mutation size", sep="")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.61,
               y       =  usr[4]*0.99,
               legend  =  c(" ",
                            " ",
                            " ",
                            " "),
               lty     =  1,
               lwd     =  3,
               col     =  c(COL8[1], COL8[2], COL8[3], COL8[4]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Legend
        legend(
               x       =  usr[2]*0.99,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste(italic(nu)==1/2, " (Eq.B17)")),
                            expression(paste(italic(nu)==1/4, " (Eq.B17)")),
                            expression(paste(italic(nu)==1/10, " (Eq.B17)"))),
               pch     =  21,
               pt.bg   =  c(NA,
                            transparentColor(COL8[2], opacity=0.5),
                            transparentColor(COL8[3], opacity=0.5),
                            transparentColor(COL8[4], opacity=0.5)),
               col     =  c(NA,
                            COL8[2],
                            COL8[3],
                            COL8[4]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
}



# Summary Figure for ratio of inbred/outcross balancing selection
RelBalancing_SummaryFig  <-  function(h=1/2, Ne = 10^4, sim.reps=10^3) {
    
    # Inbreeding values
    F.values = 0:100/100

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(1, nrow=1, ncol=1, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,1), bty='o', xaxt='s', yaxt='s')
     # Ratio of Balancing selection for inbreeding to outcrossing
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
            # Favoured mutations
            lines(Rbal_smallMutLimit(F=F.values, h=1/2) ~ F.values, lty = 1, lwd = 3, col = COL8[2])
            lines(Rbal_largex_fav(F=F.values, h=1/2) ~ F.values, lty = 2, lwd = 3, col = COL8[2])
            # Established mutations
            lines(((1 - F.values)^2/(1 + F.values)^2) ~ F.values, lty = 1, lwd = 3, col = COL8[3])
            lines(Rbal_largeMut_est(F=F.values) ~ F.values, lty = 2, lwd = 3, col = COL8[3])
        # Simulations
            RbalSim.F.small  <-  read.csv("./out/relBal_variable_x_EstabMuts_xAvg0.05_Ne1e+05_n50_z1_h0.5_reps1e+05.csv", header=TRUE)
            RbalSim.F        <-  read.csv("./out/relBal_variable_x_EstabMuts_xAvg5_Ne1e+05_n50_z1_h0.5_reps1e+05.csv", header=TRUE)
            RbalSim.small    <-  relBalancing_F_Sims(h=1/2, largeMut = FALSE)
            RbalSim.large    <-  relBalancing_F_Sims(xAvg=2, h=1/2, largeMut = TRUE)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.9), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim.small)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim.large)
#            RbalSim.F  <-  relBalancingMutSize_variable_x_EstabMuts_Sims(h=1/2, sim.reps=sim.reps)
#            points(rBal.new ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.9), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim.F)
            points(rBal.est ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.9), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim.F.small)
            points(rBal.est ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.9), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim.F)
            # Naive expectation
            lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 3)
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(-0.15,  0.5,  expression(paste("Relative fraction (", italic(R[bal.]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.15,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legends
        # Naive predictions
        proportionalLabel( 0.73,  0.95,  expression(paste("Na\u{00EF}ve prediction")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        legend(
               x       =  usr[2]*0.75,
               y       =  usr[4]*0.925,
               legend  =  c(expression(paste(1-italic(F)))),
               lty     =  1,
               lwd     =  3,
               col     =  COL8[1],
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Favored mutations
        proportionalLabel( 0.75,  0.82,  expression(paste("Adaptive mutations")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        legend(
               x       =  usr[2]*0.67,
               y       =  usr[4]*0.79,
               legend  =  c(" ",
                            " "),
               lty     =  c(2,1),
               lwd     =  3,
               col     =  COL8[2],
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        legend(
               x       =  usr[2]*0.98,
               y       =  usr[4]*0.79,
               legend  =  c(expression(paste("Variable-", italic(x), " (Eq.B17)")),
                            expression(paste("Small-", italic(x), " (Eq.B15)"))),
               pch     =  21,
               pt.bg   =  c(transparentColor(COL8[2], opacity=0.5),
                            transparentColor(COL8[2], opacity=0.5)),
               col     =  c(COL8[2],
                            COL8[2]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Established mutations
        proportionalLabel( 0.78,  0.64,  expression(paste("Established mutations")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        legend(
               x       =  usr[2]*0.67,
               y       =  usr[4]*0.6,
               legend  =  c(" ",
                            " "),
               lty     =  c(2,1),
               lwd     =  3,
               col     =  COL8[3],
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        legend(
               x       =  usr[2]*0.98,
               y       =  usr[4]*0.6,
               legend  =  c(expression(paste("Variable-", italic(x), " (Eq.B28)")),
                            expression(paste("Small-", italic(x), " (Eq.B26)"))),
               pch     =  21,
               pt.bg   =  c(transparentColor(COL8[3], opacity=0.5),
                            transparentColor(COL8[3], opacity=0.5)),
               col     =  c(COL8[3],
                            COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
}




######################################
## Supp. Figs for variable dominance


# Ratio of inbred/outcross balancing selection
RelBalancing_VariableDom_Fig  <-  function(xAvg = 5, bs = c(0.5, 1, 1.5, 5, 50)) {
    
    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2,3,
                             4,4,4), nrow=2, ncol=3, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,1), bty='o', xaxt='s', yaxt='s')
     
    F.values = 0:100/100
    h = 1/2
     # Panel (A) -- New Mutations
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
# Mathematica R_new predicted Lines
#        VarDomLines  <-  list()

#            file  <-  paste0('./Mathematica/NewMuts_b', bs[i], '.csv')
#            VarDomLines[[i]]  <-  read.csv(file, header=FALSE)
#            Fnew  <-  VarDomLines[[i]][,1][seq_along(VarDomLines[[i]][,1])%%2 == 0]
#            Rnew  <-  VarDomLines[[i]][,1][seq_along(VarDomLines[[i]][,1])%%2 == 1]
#            VarDomLines[[i]]  <-  as.data.frame(cbind(Fnew, Rnew))
#        lines(Rnew ~ Fnew, col = COL8[i+2], lwd = 2 ,data = VarDomLines[[i]])
#        }
        # Sage
        RnewLines  <-  list()
        RnewLines[[1]]  <-  read.csv('./sage/R_new_b0.50.csv', header=FALSE)
        RnewLines[[2]]  <-  read.csv('./sage/R_new_b1.0.csv', header=FALSE)
        RnewLines[[3]]  <-  read.csv('./sage/R_new_b1.5.csv', header=FALSE)
        RnewLines[[4]]  <-  read.csv('./sage/R_new_b5.0.csv', header=FALSE)
        RnewLines[[5]]  <-  read.csv('./sage/R_new_b50..csv', header=FALSE)
        for(i in 1:length(bs)) {
            lines(V2 ~ V1, col = COL8[i+1], lwd = 2, data = RnewLines[[i]])
        }
        lines(R.new.largeMut(F = F.values, h = h) ~ F.values, type = "l", col = COL8[1], lwd = 2, lty=2)
        lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 2)
        # Simulations
        RbalSimData  <-  list()
        for(i in 1:length(bs)) {
            RbalSimData[[i]]  <-  relBalancing_F_Sims(xAvg=xAvg, h=1/2, largeMut = TRUE, variableDom = TRUE, b = bs[i])
        }
        for(i in 1:length(bs)) {
            points(rBal ~ F, pch=21, col=transparentColor(COL8[i+1], opacity=0.6), bg=transparentColor(COL8[i+1], opacity=0.4), data=RbalSimData[[i]])
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075,   expression(paste("A")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,   expression(paste("New Mutations", sep="")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.2,  0.5,  expression(paste("Relative fraction (", italic(R[bal]), ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=90)
        proportionalLabel( 0.5,  -0.2,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.96,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste(italic(nu)^2==1/4))),
               seg.len =  3,
               lty     =  c(1,2),
               lwd     =  c(2,2),
               col     =  COL8[1],
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        proportionalLabel(0.82,  0.8,   expression(paste(italic(nu)^2,"~ Beta(", italic(b)/3,", ",italic(b), ")", sep="")), cex=1.25, adj=c(0.5, 0.5), xpd=NA)        
        legend(
               x       =  usr[2]*0.99,
               y       =  usr[4]*0.76,
               legend  =  c(
                            expression(paste(italic(b)==50)),
                            expression(paste(italic(b)==5)),
                            expression(paste(italic(b)==1.5)),
                            expression(paste(italic(b)==1)),
                            expression(paste(italic(b)==0.5))),
               pch     =  21,
               pt.bg   =  c(transparentColor(COL8[6], opacity=0.5),
                            transparentColor(COL8[5], opacity=0.5),
                            transparentColor(COL8[4], opacity=0.5),
                            transparentColor(COL8[3], opacity=0.5),
                            transparentColor(COL8[2], opacity=0.5)),
               col     =  c(transparentColor(COL8[6], opacity=1),
                            transparentColor(COL8[5], opacity=1),
                            transparentColor(COL8[4], opacity=1),
                            transparentColor(COL8[3], opacity=1),
                            transparentColor(COL8[2], opacity=1)),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Legend
        legend(
               x       =  usr[2]*0.83,
               y       =  usr[4]*0.76,
               legend  =  c(" ",
                            " ",
                            " ",
                            " ",
                            " "),
               seg.len = 3,
               lty     =  c(1,1,1,1,1),
               lwd     =  c(2,2,2,2,2),
               col     =  c(rev(COL8[2:6])),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

    # Panel (B) -- Adaptive Mutations
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        RadaptLines  <-  list()
        RadaptLines[[1]]  <-  read.csv('./sage/R_adapt_b0.50.csv', header=FALSE)
        RadaptLines[[2]]  <-  read.csv('./sage/R_adapt_b1.0.csv', header=FALSE)
        RadaptLines[[3]]  <-  read.csv('./sage/R_adapt_b1.5.csv', header=FALSE)
        RadaptLines[[4]]  <-  read.csv('./sage/R_adapt_b5.0.csv', header=FALSE)
        RadaptLines[[5]]  <-  read.csv('./sage/R_adapt_b50..csv', header=FALSE)
        for(i in 1:length(bs)) {
            lines(V2 ~ V1, col = COL8[i+1], lwd = 2, data = RadaptLines[[i]])
        }        
        lines(Rbal_largex_fav(F = F.values, h = h) ~ F.values, type = "l", col = COL8[1], lty=2, lwd = 2)
        lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 2)
        # Simulations
        for(i in 1:length(bs)) {
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[i+1], opacity=0.6), bg=transparentColor(COL8[i+1], opacity=0.4), data=RbalSimData[[i]])
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075, expression(paste("B")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,  expression(paste("Adaptive Mutations", sep="")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
#        proportionalLabel(-0.25,  0.5,  expression(paste("Relative fraction (", italic(R[adapt.]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.2, expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)


    # Panel (C) -- Established Mutations
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        lines(Rbal_largeMut_est(F = F.values) ~ F.values, type = "l", col = COL8[1], lty=2, lwd = 2)
        lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 2)
        # Simulations
        RbalEstabSimData  <-  list()
        for(i in 1:length(bs)) {
            file  <-  paste0('./out/relBal_variable_x_EstabMuts_xAvg5_Ne1e+05_n50_z1_VarDomTRUE_b', bs[i], '_reps1e+05.csv')
            RbalEstabSimData[[i]]  <-  read.csv(file, header=TRUE)
        }
        for(i in 1:length(bs)) {
            points(rBal.est ~ F, pch=21, col=transparentColor(COL8[i+1], opacity=0.9), bg=transparentColor(COL8[i+1], opacity=0.4), data=RbalEstabSimData[[i]])
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075, expression(paste("C")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,  expression(paste("Established Mutations", sep="")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
#        proportionalLabel(-0.25,  0.5,  expression(paste("Relative fraction (", italic(R[estab.]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.2,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)


    # Panel D -- Distributions of nu^2
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,5), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        # Generate Lines + points
        nuSquaredDistLines  <-  list()
        nuSquaredDistLines[[1]]  <-  read.csv('./sage/nu_squared_Dist_b0.50.csv', header=FALSE)
        nuSquaredDistLines[[2]]  <-  read.csv('./sage/nu_squared_Dist_b1.0.csv', header=FALSE)
        nuSquaredDistLines[[3]]  <-  read.csv('./sage/nu_squared_Dist_b1.5.csv', header=FALSE)
        nuSquaredDistLines[[4]]  <-  read.csv('./sage/nu_squared_Dist_b5.0.csv', header=FALSE)
        nuSquaredDistLines[[5]]  <-  read.csv('./sage/nu_squared_Dist_b50..csv', header=FALSE)
        for(i in 1:length(bs)) {
            lines(sqrt(V2) ~ V1, col = COL8[i+1], lwd = 2, data = nuSquaredDistLines[[i]])
        }        
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075, expression(paste("D")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,  expression(paste("Distribution of ", italic(nu), " given that ", italic(nu)^2,"~ Beta(", italic(b)/3,", ",italic(b), ")", sep="")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel(-0.05,  0.5,  expression(paste("Density")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.2,  expression(paste(nu)), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
#        proportionalLabel(0.82,  0.82,   expression(paste(italic(nu)^2,"~ Beta, ", italic(b), sep="")), cex=1.25, adj=c(0.5, 0.5), xpd=NA)        
        # Legend
        legend(
               x       =  usr[2]*0.95,
               y       =  usr[4]*0.95,
               legend  =  c(
                            expression(paste(italic(b)==50)),
                            expression(paste(italic(b)==5)),
                            expression(paste(italic(b)==1.5)),
                            expression(paste(italic(b)==1)),
                            expression(paste(italic(b)==0.5))),
               seg.len = 3,
               lty     =  c(1,1,1,1,1),
               lwd     =  c(2,2,2,2,2),
               col     =  c(rev(COL8[2:6])),
               cex     =  1.25,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
}


## NEW SUPP FIGURE with v ~ Beta
# Ratio of inbred/outcross balancing selection
RelBalancing_BetaDom_Fig  <-  function(xAvg = 5, bs = c(0.5, 1, 1.5, 5, 50)) {

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2,3,
                             4,4,4), nrow=2, ncol=3, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,1), bty='o', xaxt='s', yaxt='s')
     
    F.values = 0:100/100
    h = 1/2
     # Panel (A) -- New Mutations
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
 

######################################
## Supp. Figs for lower trait dimensionality


# Ratio of inbred/outcross balancing selection
RelBalancing_lower_n_Fig  <-  function(xAvg = 5, ns = c(50, 20, 10, 5, 2)) {
    
    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1:3), nrow=1, ncol=3, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)
    F.values = 0:100/100
    h = 1/2

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,1), bty='o', xaxt='s', yaxt='s')
     
     # Panel (A) -- New Mutations
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        lines(R.new.largeMut(F = F.values, h = h) ~ F.values, type = "l", col = COL8[2], lwd = 2, lty=1)
        lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 2)
        # Simulations
        RnewSimData  <-  list()
        for(i in 1:length(ns)) {
            RnewSimData[[i]]  <-  relBalancing_F_Sims(xAvg=xAvg, h=1/2, n=ns[i], largeMut = TRUE)
        }
        for(i in 1:length(ns)) {
            points(rBal ~ F, pch=21, cex=1.5, col=transparentColor(COL8[i+1], opacity=0.6), bg=transparentColor(COL8[i+1], opacity=0.4), data=RnewSimData[[i]])
        }
        # axes
        axis(1, las=1, cex=2)
        axis(2, las=1, cex=2)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075, expression(paste("A")), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,  expression(paste("New Mutations", sep="")), cex=2, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel(-0.2,  0.5,   expression(paste("Relative fraction (", italic(R[bal]), ")")), cex=2, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.2,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=2, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.99,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste(italic(nu)==0.5))),
               seg.len =  3,
               lty     =  1,
               lwd     =  2,
               col     =  COL8[1:2],
               cex     =  1.25,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )        
        proportionalLabel(0.8,  0.78,   expression(paste("Number of traits", sep="")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        legend(
               x       =  usr[2]*0.93,
               y       =  usr[4]*0.76,
               legend  =  c(
                            expression(paste(italic(n)==2)),
                            expression(paste(italic(n)==5)),
                            expression(paste(italic(n)==10)),
                            expression(paste(italic(n)==20)),
                            expression(paste(italic(n)==50))),
               pch     =  21,
               pt.bg   =  c(transparentColor(COL8[6], opacity=0.5),
                            transparentColor(COL8[5], opacity=0.5),
                            transparentColor(COL8[4], opacity=0.5),
                            transparentColor(COL8[3], opacity=0.5),
                            transparentColor(COL8[2], opacity=0.5)),
               col     =  c(transparentColor(COL8[6], opacity=1),
                            transparentColor(COL8[5], opacity=1),
                            transparentColor(COL8[4], opacity=1),
                            transparentColor(COL8[3], opacity=1),
                            transparentColor(COL8[2], opacity=1)),
               cex     =  1.25,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
 
    # Panel (B) -- Adaptive Mutations
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot lines
        lines(Rbal_largex_fav(F = F.values, h = h) ~ F.values, type = "l", col = COL8[2], lty=1, lwd = 2)
        lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 2)
        # Simulations
        for(i in 1:length(ns)) {
            points(rBal.fav ~ F, pch=21, cex=1.5, col=transparentColor(COL8[i+1], opacity=0.6), bg=transparentColor(COL8[i+1], opacity=0.4), data=RnewSimData[[i]])
        }
        # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075, expression(paste("B")), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,  expression(paste("Adaptive Mutations", sep="")), cex=2, adj=c(0.5, 0.5), xpd=NA)        
#        proportionalLabel(-0.25,  0.5,  expression(paste("Relative fraction (", italic(R[adapt.]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.2, expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=2, adj=c(0.5, 0.5), xpd=NA)


    # Panel (C) -- Established Mutations
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        lines(Rbal_largeMut_est(F = F.values) ~ F.values, type = "l", col = COL8[2], lty=1, lwd = 2)
        lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 2)
        # Simulations
        RestabSimData  <-  list()
        for(i in 1:length(ns)) {
            file  <-  paste0('./out/relBal_variable_x_EstabMuts_xAvg5_Ne1e+05_n',ns[i],'_z1_h0.5_reps1e+05.csv')
            RestabSimData[[i]]  <-  read.csv(file, header=TRUE)
        }
        for(i in 1:length(ns)) {
            points(rBal.est ~ F, pch=21, cex=1.5, col=transparentColor(COL8[i+1], opacity=0.9), bg=transparentColor(COL8[i+1], opacity=0.4), data=RestabSimData[[i]])
        }
        # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.05,  1.075, expression(paste("C")), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.5,  1.075,  expression(paste("Established Mutations", sep="")), cex=2, adj=c(0.5, 0.5), xpd=NA)        
#        proportionalLabel(-0.25,  0.5,  expression(paste("Relative fraction (", italic(R[estab.]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.2,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=2, adj=c(0.5, 0.5), xpd=NA)

}


PrBalSelNewMutations_lower_n_Fig  <-  function(ns = c(25, 10, 5), z = 1, h = 1/2, reps=10^5) {

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate data to plot
    x.series    <- c(0.05, seq(0.25,5,0.25))
    Fvals       <-  c(0, 0.25, 0.5)
    PrBalSelData  <-  list()
    for(k in 1:length(ns)) {
        O = rep(0, ns[k]) #optimal phenotype
        A = c(-z, rep(0, ns[k] - 1)) #wild-type phenotype
        Pr.bal.sim  <-  vector()
        for(f in 1:length(Fvals)) {
            s.het = vector()
            s.hom = vector()
            for(j in 1:length(x.series)){
            r = x.series[j]*(2*z)/sqrt(ns[k])
                for(i in 1:reps){
                    mut = rnorm(ns[k])
                    A.het = A + r*h*mut/sqrt(sum(mut^2))
                    z.het = sqrt(sum((A.het - O)^2))
                    A.hom = A + r*mut/sqrt(sum(mut^2))
                    z.hom = sqrt(sum((A.hom - O)^2))
                    s.het[i] = exp(-z.het^2/2)/exp(-z^2/2) - 1
                    s.hom[i] = exp(-z.hom^2/2)/exp(-z^2/2) - 1
                    }
                Pr.bal.sim  <-  c(Pr.bal.sim, sum(s.het*(1 - Fvals[f]) > s.hom & s.het*(1 - Fvals[f]) > -Fvals[f]*s.hom)/reps)
                }
            }
        plotDat  <-  as.data.frame(cbind(Pr.bal.sim, rep(x.series, times=3), rep(Fvals, each=length(x.series))))
        names(plotDat)  <-  c("Pr.bal.sim", "xSeries", "F")
        PrBalSelData[[k]]  <-  plotDat
    }
    # Generate predicted lines
    pred.x  <-  c(0:100)/20
    Pred1  <-  PrBalPredNewMut(x=pred.x, a=funa(F=Fvals[1], h=1/2), b=funb(F=Fvals[1], h=1/2))
    Pred2  <-  PrBalPredNewMut(x=pred.x, a=funa(F=Fvals[2], h=1/2), b=funb(F=Fvals[2], h=1/2))
    Pred3  <-  PrBalPredNewMut(x=pred.x, a=funa(F=Fvals[3], h=1/2), b=funb(F=Fvals[3], h=1/2))

    # set plot layout
    layout.mat  <-  matrix(c(1), nrow=1, ncol=1, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Create L=Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,2), bty='o', xaxt='s', yaxt='s')
     # Panel (A) Polymorphic space
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,5), ylim = c(0,0.3), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Predicted Curves
        lines(Pred1 ~ pred.x, lwd=2, col = COL8[1])
        lines(Pred2 ~ pred.x, lwd=2, col = COL8[2])
        lines(Pred3 ~ pred.x, lwd=2, col = COL8[3])
        # Simulation Points
        pts  <-  c(21, 23, 24)
        for(i in 1:length(PrBalSelData)){
            points(Pr.bal.sim[F==0] ~ xSeries[F==0], pch=pts[i], col = transparentColor(COL8[1], opacity=0.8), bg = transparentColor(COL8[1], opacity=0.5), data=PrBalSelData[[i]])
            points(Pr.bal.sim[F==0.25] ~ xSeries[F==0.25], pch=pts[i], col = transparentColor(COL8[2], opacity=0.8), bg = transparentColor(COL8[2], opacity=0.5), data=PrBalSelData[[i]])
            points(Pr.bal.sim[F==0.5] ~ xSeries[F==0.5], pch=pts[i], col = transparentColor(COL8[3], opacity=0.8), bg = transparentColor(COL8[3], opacity=0.5), data=PrBalSelData[[i]])
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel( -0.25,  0.5, expression(paste("Pr(bal. | ", italic(x), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)
        proportionalLabel( 0.5,  -0.25, expression(paste("Scaled mutation size (", italic(x), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)        
        # Legend
        legend(
               x       =  usr[2]*0.99,
               y       =  usr[4]*0.99,
               legend  =  c(expression(italic(F)==0.0),
                            expression(italic(F)==0.25),
                            expression(italic(F)==0.5)),
               lty     =  1,
               lwd     =  2,
               col     =  c(COL8[1], COL8[2], COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        legend(
               x       =  usr[2]*0.955,
               y       =  usr[4]*0.8,
               legend  =  c(expression(italic(n)==5),
                            expression(italic(n)==10),
                            expression(italic(n)==25)),
               pch     =  rev(pts),
               col     =  c(transparentColor(COL8[1], opacity=1),
                            transparentColor(COL8[1], opacity=1),
                            transparentColor(COL8[1], opacity=1)),
               pt.bg   =  NA,
               pt.cex  =  1,
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

}



######################################
# Kimura-Ohta strong-selection fig
KimuraOhta_InvPlot  <- function() {

    # Colors
    COL8  <-  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1), nrow=1, ncol=1, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(1, 1, 0.75, 1), mar = c(5,5,1,1), bty='o', xaxt='s', yaxt='s')
     # Panel (A) F = 0
     plot(NA, axes=FALSE, type='n', main='', xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot lower invasion boundaries
        s1.vals  <-  seq(0,1,0.01)
        lines(KO_InvCond_F_low(s1 = s1.vals, F=0.1) ~ s1.vals, ylim=c(0, 1), lwd=2, col=COL8[1])
        lines(KO_InvCond_F_low(s1 = s1.vals, F=0.3) ~ s1.vals, ylim=c(0, 1), lwd=2, col=COL8[2])
        lines(KO_InvCond_F_low(s1 = s1.vals, F=0.5) ~ s1.vals, ylim=c(0, 1), lwd=2, col=COL8[3])
        lines(KO_InvCond_F_low(s1 = s1.vals, F=0.7) ~ s1.vals, ylim=c(0, 1), lwd=2, col=COL8[4])
        lines(KO_InvCond_F_low(s1 = s1.vals, F=0.98) ~ s1.vals, ylim=c(0, 1), lwd=2, col=COL8[6])
        # Polygon showing always polymorphic space
        segments(0.0,0.0,1,1, lwd=2, lty=3)
        polygon(c(0.5,0.5,1,1), c(0.5,1,1,0.5), col=adjustcolor(COL8[1], alpha=0.25))
        # Plot lower invasion boundaries
        s1.vals  <-  seq(0,0.5,0.001)
        lines(KO_InvCond_F_up(s1 = s1.vals, F=0.1)[KO_InvCond_F_up(s1 = s1.vals, F=0.1) <=1] ~ s1.vals[KO_InvCond_F_up(s1 = s1.vals, F=0.1) <=1], ylim=c(0, 1), lwd=2, col=COL8[1])
        lines(KO_InvCond_F_up(s1 = s1.vals, F=0.3)[KO_InvCond_F_up(s1 = s1.vals, F=0.3) <=1] ~ s1.vals[KO_InvCond_F_up(s1 = s1.vals, F=0.3) <=1], ylim=c(0, 1), lwd=2, col=COL8[2])
        lines(KO_InvCond_F_up(s1 = s1.vals, F=0.5)[KO_InvCond_F_up(s1 = s1.vals, F=0.5) <=1] ~ s1.vals[KO_InvCond_F_up(s1 = s1.vals, F=0.5) <=1], ylim=c(0, 1), lwd=2, col=COL8[3])
        lines(KO_InvCond_F_up(s1 = s1.vals, F=0.7)[KO_InvCond_F_up(s1 = s1.vals, F=0.7) <=1] ~ s1.vals[KO_InvCond_F_up(s1 = s1.vals, F=0.7) <=1], ylim=c(0, 1), lwd=2, col=COL8[4])
        s1.vals  <-  seq(0,0.5,0.0001)
        lines(KO_InvCond_F_up(s1 = s1.vals, F=0.98)[KO_InvCond_F_up(s1 = s1.vals, F=0.98) <=1] ~ s1.vals[KO_InvCond_F_up(s1 = s1.vals, F=0.98) <=1], ylim=c(0, 1), lwd=2, col=COL8[6])
        # Inv Cond. Annotations
        proportionalLabel( 0.875,  0.15, expression(paste(italic(F), " = ", 0.1)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=6, col=COL8[1])
        proportionalLabel( 0.875,  0.27,  expression(paste(italic(F), " = ", 0.3)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=12, col=COL8[2])
        proportionalLabel( 0.875,  0.36,  expression(paste(italic(F), " = ", 0.5)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=14, col=COL8[3])
        proportionalLabel( 0.875,  0.44,  expression(paste(italic(F), " = ", 0.7)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=12, col=COL8[4])
        proportionalLabel( 0.65,  0.45,  expression(paste(italic(F), " = ", 0.98)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=3, col=COL8[6])
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste("Kimura-Ohta Result")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.75,  0.78,   expression(paste(italic("Always"))), cex=1.2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.75,  0.72,   expression(paste(italic("Polymorphic"))), cex=1.2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( 0.5, -0.2, expression(paste(italic(s[1]))), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.2, 0.5,   expression(paste(italic(s[2]))), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)

}


######################################
# Ratio of balancing selection ((F > 0) vs. (F = 0) in black); 
# 1 - F in red

# Ratio of inbred/outcross balancing selection
RelBalancingFig  <-  function(F.I = 1/2, h=1/2, favouredMuts = FALSE) {
    
    # mutation size
    sizes  <-  1:1000/200

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,1), bty='o', xaxt='s', yaxt='s')
     # Panel (A) Ratio of Balancing selection for inbreeding to outcrossing
     if(favouredMuts) { yMax  <-  1.2} 
        else (yMax  <-  0.7)
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,5), ylim = c(0,yMax), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        if(favouredMuts) {
                Rbal     <-  (Pr.bal.F(x=sizes, F.I=F.I, h=h) / Pr.inv.F(x=sizes, F.I=F.I, h=h)) / (Pr.bal.F(x=sizes, F.I=0, h=h) / Pr.inv.F(x=sizes, F.I=0, h=h))
        } else{ Rbal     <-  Pr.bal.F(x=sizes, F.I=F.I, h=h)/Pr.bal(x=sizes, h=h) }
        RbalLim  <-  Rbal_smallMutLimit(F=F.I, h=h)
        RbalLim  <-  Rbal_smallMutLimit(F=F.I, h=h)
        lines(Rbal ~ sizes,  col = "grey60", lwd = 3)
        lines(rep(1 - F.I, length(sizes)) ~ sizes, lwd = 3)
        lines(rep(RbalLim, length(sizes)) ~ sizes, lty = 3, lwd = 3)
        # Simulations
            RbalSim  <-  relBalancingMutSize_Sims()
        if(favouredMuts) {
            points(rBal.fav ~ x, pch=21, col=transparentColor(COL8[1], opacity=0.6), bg=transparentColor(COL8[1], opacity=0.4), data=RbalSim)
            } else {
            points(rBal ~ x, pch=21, col=transparentColor(COL8[1], opacity=0.6), bg=transparentColor(COL8[1], opacity=0.4), data=RbalSim)
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.5,  1.1,   expression(paste(italic(F), " = ", italic(nu), " = ", 1/2)), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel(-0.25,  0.5,  expression(paste("Relative probability (", italic(R[new]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25, expression(paste("Mutation size (", italic(x), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)        
        if(!favouredMuts) {
            # Legend
            legend(
                   x       =  usr[2]*0.4,
                   y       =  usr[4]*0.99,
                   legend  =  c(expression(paste(1-italic(F))),
                                expression(paste(Eq(7,B6))),
                                expression(paste(Eq(8,B10)))),
                   lty     =  c(1, 3, 1),
                   lwd     =  3,
                   col     =  c(COL8[1], COL8[1], "grey60"),
                   cex     =  1,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )
            # Legend
            legend(
                   x       =  usr[2]*0.7,
                   y       =  usr[4]*0.99,
                   legend  =  c(expression(paste("Simulations"))),
                   pch     =  21,
                   pt.bg   =  transparentColor(COL8[1], opacity=0.4),
                   col     =  transparentColor(COL8[1], opacity=0.6),
                   cex     =  1,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )
        }
        if(favouredMuts) {
            # Legend
            legend(
                   x       =  usr[2]*0.4,
                   y       =  usr[4]*0.99,
                   legend  =  c(expression(paste(1-italic(F))),
                                expression(paste(Eq(10,B15))),
                                expression(paste(Eq(11,B17)))),
                   lty     =  c(1, 3, 1),
                   lwd     =  3,
                   col     =  c(COL8[1], COL8[1], "grey60"),
                   cex     =  1,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )
            # Legend
            legend(
                   x       =  usr[2]*0.7,
                   y       =  usr[4]*0.99,
                   legend  =  c(expression(paste("Simulations"))),
                   pch     =  21,
                   pt.bg   =  transparentColor(COL8[1], opacity=0.4),
                   col     =  transparentColor(COL8[1], opacity=0.6),
                   cex     =  1,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )
        }

    # Panel (B) Proportion of parameter space where balancing selection occurs
    F.values = 0:100/100
    h.i = c(0.5, 0.25, 0.1)
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        lines(Rbal_smallMutLimit(F = F.values, h = h.i[1]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[2], lwd = 3)
        lines(Rbal_smallMutLimit(F = F.values, h = h.i[2]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[3], lwd = 3)
        lines(Rbal_smallMutLimit(F = F.values, h = h.i[3]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = COL8[4], lwd = 3)
        lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 3)
                # Simulations
        RbalSim_h1  <-  relBalancing_F_Sims(h=1/2)
        RbalSim_h2  <-  relBalancing_F_Sims(h=1/4)
        RbalSim_h3  <-  relBalancing_F_Sims(h=1/10)
        if(favouredMuts) {
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim_h1)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim_h2)
            points(rBal.fav ~ F, pch=21, col=transparentColor(COL8[4], opacity=0.6), bg=transparentColor(COL8[4], opacity=0.4), data=RbalSim_h3)
            } else {
            points(rBal ~ F, pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=RbalSim_h1)
            points(rBal ~ F, pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=RbalSim_h2)
            points(rBal ~ F, pch=21, col=transparentColor(COL8[4], opacity=0.6), bg=transparentColor(COL8[4], opacity=0.4), data=RbalSim_h3)
        }
        # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.5,  1.1,   expression(paste("Small-mutation limit (", italic(x)%->%0, ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        if(!favouredMuts) {
            legend(
                   x       =  usr[2]*0.99,
                   y       =  usr[4]*0.99,
                   legend  =  c(expression(paste(1-italic(F))),
                                expression(paste(Eq(7,B6), "; ", italic(nu)==1/2)),
                                expression(paste(Eq(7,B6), "; ", italic(nu)==1/4)),
                                expression(paste(Eq(7,B6), "; ", italic(nu)==1/10))),
                   lty     =  1,
                   lwd     =  3,
                   col     =  c(COL8[1], COL8[2], COL8[3], COL8[4]),
                   cex     =  1,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )
        }
        if(favouredMuts) {
            legend(
                   x       =  usr[2]*0.99,
                   y       =  usr[4]*0.99,
                   legend  =  c(expression(paste(1-italic(F))),
                                expression(paste(Eq(10,B15), "; ", italic(nu)==1/2)),
                                expression(paste(Eq(10,B15), "; ", italic(nu)==1/4)),
                                expression(paste(Eq(10,B15), "; ", italic(nu)==1/10))),
                   lty     =  1,
                   lwd     =  3,
                   col     =  c(COL8[1], COL8[2], COL8[3], COL8[4]),
                   cex     =  1,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )
        }
}


######################################
# Fig showing the ratio of mutation size
# with maximal probability of balancing selection
# for inbred relative to outcrossing populations

# Ancillary functions for this figure
xMaxF  <- function(a,b){
  sqrt(2*log(a/b)/(a^2 - b^2))
}
xMax  <-  function(h){
  sqrt(2*log((1 + h)/h)/(1 + 2*h))
}


# ratio of mutation size with maximal probability of balancing selection
# for inbred relative to outcrossing populations
relMutSizeMaxRBal  <-  function() {
    F.values = 0:99/100
    h.1 = 0.5
    h.2 = 0.25
    h.3 = 0.1

    xMaxFs.1  <-  xMaxF(a = funa(F=F.values, h=h.1), b=funb(F = F.values, h = h.1))
    xMaxs.1   <-  xMax(h=h.1)
    xMaxFs.2  <-  xMaxF(a = funa(F=F.values, h=h.2), b=funb(F = F.values, h = h.2))
    xMaxs.2   <-  xMax(h=h.2)
    xMaxFs.3  <-  xMaxF(a = funa(F=F.values, h=h.3), b=funb(F = F.values, h = h.3))
    xMaxs.3   <-  xMax(h=h.3)

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # 
    par(omi=c(0.5, 0.25, 0.25, 0.5), mar = c(5,5,1,1), bty='o', xaxt='s', yaxt='s')
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        lines((xMaxFs.1/xMaxs.1) ~ F.values, lwd=2, col=COL8[1])
        lines((xMaxFs.2/xMaxs.2) ~ F.values, lwd=2, col=COL8[2])
        lines((xMaxFs.3/xMaxs.3) ~ F.values, lwd=2, col=COL8[3])
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
#        proportionalLabel(0.5,  1.1,   expression(paste("Small-mutation limit (", italic(x)%->%0, ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel(-0.25,  0.5, expression(hat(italic(x))/hat(italic(x))[F==0]), cex=1.3, adj=c(0.5, 0.5), srt=90, xpd=NA)
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)

        # Legend
        legend(
               x       =  usr[2]*0.99,
               y       =  usr[4]*0.3,
               legend  =  c(expression(italic(h)==1/2),
                            expression(italic(h)==1/4),
                            expression(italic(h)==1/10)),
               lty     =  c(1, 1, 1),
               lwd     =  2,
               col     =  c(COL8[1], COL8[2], COL8[3]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
}





################################################################
# Balancing selection in mutations that become established in
# the population

# Ancillary functions for this figure
Pr.est.F = function(x, F.I, z = 1, n = 50){
  z^2/n*x*(sqrt(2)*(1 + F.I)/sqrt(pi)*exp(-x^2/2*(1 + 3*F.I)^2/(2*(1 + F.I))^2) - x*(1 + 3*F.I)/2*(1 - erf(x*(1 + 3*F.I)/(2*sqrt(2)*(1 + F.I)))))
}

Pr.fix.F = function(x, F.I, z = 1, n = 50){
  z^2/n*x*(sqrt(2)*(1 + F.I)/sqrt(pi)*exp(-x^2/2*(3 + 1*F.I)^2/(2*(1 + F.I))^2) - x*(1 + 3*F.I)/2*(1 - erf(x*(3 + 1*F.I)/(2*sqrt(2)*(1 + F.I)))))
}

Pr.bal.est.F = function(x, F.I, z = 1, n = 50){
  z^2/n*x*(sqrt(2)*(1 + F.I)/sqrt(pi)*exp(-x^2/2*(1 + 3*F.I)^2/(2*(1 + F.I))^2) - x*(1 + 3*F.I)/2*(1 - erf(x*(1 + 3*F.I)/(2*sqrt(2)*(1 + F.I))))) - z^2/n*x*(sqrt(2)*(1 + F.I)/sqrt(pi)*exp(-x^2/2*(3 + 1*F.I)^2/(2*(1 + F.I))^2) - x*(1 + 3*F.I)/2*(1 - erf(x*(3 + 1*F.I)/(2*sqrt(2)*(1 + F.I)))))
}

Pr.est = function(x, z = 1, n = 50){
  z^2/n*x*(sqrt(2)/sqrt(pi)*exp(-x^2/8) - x/2*(1 - erf(x/(2*sqrt(2)))))
}

Pr.fix = function(x, z = 1, n = 50){
  z^2/n*x*(sqrt(2)/sqrt(pi)*exp(-9*x^2/8) - x/2*(1 - erf(3*x/(2*sqrt(2)))))
}

Pr.bal.est = function(x, z = 1, n = 50){
  z^2/n*x*(sqrt(2)/sqrt(pi)*exp(-x^2/8) - x/2*(1 - erf(x/(2*sqrt(2))))) - z^2/n*x*(sqrt(2)/sqrt(pi)*exp(-9*x^2/8) - x/2*(1 - erf(3*x/(2*sqrt(2)))))
}



# Ratio of inbred/outcross balancing selection
RelBalancingEstablishedFig  <-  function(F.I = 1/2, h=1/2, Ne = 10^4) {
    
    # mutation size
    sizes  <-  1:1000/200

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,1), bty='o', xaxt='s', yaxt='s')
     # Panel (A) Ratio of Balancing selection for inbreeding to outcrossing
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,5), ylim = c(0,1.2), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
            Rbal     <-  (Pr.bal.est.F(x = sizes, F.I = F.I)/Pr.est.F(x = sizes, F.I = F.I))/(Pr.bal.est(sizes)/Pr.est(sizes))
            lines(Rbal ~ sizes,  col = "grey60", lwd = 3)
            lines(rep(1 - F.I, length(sizes)) ~ sizes, lwd = 3)
            lines(rep(((1 - F.I)/(1 + F.I)), length(sizes)) ~ sizes, lty = 3, lwd = 3)
            lines(rep((1 - F.I)^2/(1 + F.I)^2, length(sizes))~ sizes, lty = 3, , lwd = 3, col = "grey60")
        # Simulations
        RbalSim  <-  read.csv("./out/relBal_smallMut_EstabMuts_Ne1e+05_F0.5_n50_z1_h0.5_reps10000.csv")
            points(rBal.est ~ x, pch=21, col=transparentColor(COL8[1], opacity=0.6), bg=transparentColor(COL8[1], opacity=0.4), data=RbalSim)
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.5,  1.1,   expression(paste(italic(F), " = ", italic(h), " = ", 1/2)), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel(-0.25,  0.5,  expression(paste("Relative fraction (", italic(R[f]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25, expression(paste("Mutation size (", italic(x), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)        
        # Legend
        legend(
               x       =  usr[2]*0.35,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste(Eq(4))),
                            expression(paste(Eq(5))),
                            expression(paste(Eq(6)))),
               lty     =  c(1, 3, 1, 3),
               lwd     =  3,
               col     =  c(COL8[1], COL8[1], "grey60", "grey60"),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Legend
        legend(
               x       =  usr[2]*0.7,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste("Simulations"))),
               pch     =  21,
               pt.bg   =  transparentColor(COL8[1], opacity=0.4),
               col     =  transparentColor(COL8[1], opacity=0.6),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

    # Panel (B) Proportion of parameter space where balancing selection occurs
    F.values = 0:100/100
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
            lines((1 - F.values) ~ F.values, col = COL8[1], lwd = 3)
            lines(F.values, (1 - F.values)/(1 + F.values), lty = 3, lwd = 2, col = COL8[1])
            lines(F.values, (1 - F.values)^2/(1 + F.values)^2, lty = 3, lwd = 2, col = "grey60")
        # Simulations
            RbalSim.F  <-  read.csv("./out/relBal_smallMut_F_EstabMuts_Ne1e+06_n50_z1_h0.5_reps10000.csv", header=TRUE)
#            RbalSim.F  <-  relBalancingSmallx_EstabMuts_F_Sims(h=1/2, Ne=Ne, reps=10^5)
            points(rBal.est ~ F, pch=21, col=transparentColor(COL8[1], opacity=0.6), bg=transparentColor(COL8[1], opacity=0.4), data=RbalSim.F)
        # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.5,  1.1,   expression(paste("Small-mutation limit (", italic(x)%->%0, ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
#        legend(
#               x       =  usr[2]*0.99,
#               y       =  usr[4]*0.99,
#               legend  =  c(expression(paste(1-italic(F))),
#                            expression(paste(Eq(17), "; ", italic(h)==1/2)),
#                            expression(paste(Eq(17), "; ", italic(h)==1/4)),
#                            expression(paste(Eq(17), "; ", italic(h)==1/10))),
#               lty     =  1,
#               lwd     =  3,
#               col     =  c(COL8[1], COL8[2], COL8[3], COL8[4]),
#               cex     =  1,
#               xjust   =  1,
#               yjust   =  1,
#               bty     =  'n',
#               border  =  NA
#               )

}





################################################################

###################################
###################################
# Visualize 2-d FGM for Inbreeding

InvCircle_F  <-  function(h, F, z) {
    r  <-  z*(F + (1 - F)*h)/(F + (1 - F)*h^2)
    c  <-  -z+r

    c(c,r)
}

PosCircle_F  <-  function(h, F, z) {
    r  <-  z*(1 - (1 - F)*h)/(1 - (1 - F)*h^2)
    c  <-  -z+r

    c(c,r)
}

invCircle_asin_nsolve  <-  function(z, h, F) {
    rs  <-  seq(0,4*z, length=500)[-1]
    test  <-  na.exclude(0 < asin(z)^2 - asin(rs*h - z)^2 + F*(asin(rs*h - z)^2 - asin(rs - z)^2))
    r  <-  rs[sum(test)]/2
    c  <-  -z + r
    c(c,r)
}
posCircle_asin_nsolve  <-  function(z, h, F) {
    rs  <-  seq(0,4*z, length=500)[-1]
    test  <-   na.exclude(asin(abs(z - rs*h))^2 - asin(abs(rs - z))^2 >   F*(asin(abs(z - rs*h))^2 - asin(z)^2))
    r  <-  rs[sum(test)]/2
    c  <-  -z + r
    c(c,r)
}


Fisher_2D_ExploreFig <-  function(z = 0.5, h = 1/2, reps=10^4) {
    
    # Create data for plotting
    dat <-  Fisher_2D_ExploreFigSims(z = z, h = h, reps = reps)

    # Colors
    COL8  <-  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1:6), nrow=2, ncol=3, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(1, 1, 0.75, 1), mar = c(5,5,1,1), bty='o', xaxt='s', yaxt='s')

     # Panel (A) F = 0
     plot(NA, axes=FALSE, type='n', main='', xlim = c(min(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1])), 
                                             ylim = c(min(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1])), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        points(z.hom.raw.1[F == 0.2 & BalSel.out == 1] ~ 
               z.hom.raw.2[F == 0.2 & BalSel.out == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        points(z.hom.raw.1[F == 0.2 & PosSel.out == 1] ~ 
               z.hom.raw.2[F == 0.2 & PosSel.out == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        # Analytic ranges of pos. & bal. sel.
        # draw.circle(0,0,1, lwd=1.5, border=COL8[1]) # pos. sel.
        InvCircle  <-  InvCircle_F(h=h, F=0.0, z=z)
        PosCircle  <-  PosCircle_F(h=h, F=0.0, z=z)
#        InvCircle  <-  invCircle_asin_nsolve(h=h, F=0.0, z=z)
#        PosCircle  <-  posCircle_asin_nsolve(h=h, F=0.0, z=z)
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
        draw.circle(0,PosCircle[1],PosCircle[2], lty=1, lwd=1.5, border=COL8[5]) # bal. sel.
        draw.circle(0,0,z, lty=2, lwd=1.5, border=COL8[1]) # bal. sel.
        points(0,0, pch=21, col=1, bg=1, cex=1.5) # Phenotypic Optimum
        points(0,-z, pch=21, col=2, bg=2, cex=1.5) # Wild-type population phenotype
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste("Outcrossing")), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.2, 0.5,   expression(paste("Dimension 2")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)

        proportionalLabel(0.525,  0.25, expression(paste(italic(O))), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(0.525,  0.09,    expression(paste(italic(A))), cex=1.5, adj=c(0.5, 0.5), xpd=NA)


     # Panel (B) F = 0.2
     plot(NA, axes=FALSE, type='n', main='', xlim = c(min(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1])), 
                                             ylim = c(min(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1])), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        points(z.hom.raw.1[F == 0.2 & BalSel.F == 1] ~ 
               z.hom.raw.2[F == 0.2 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        points(z.hom.raw.1[F == 0.2 & PosSel.F == 1] ~ 
               z.hom.raw.2[F == 0.2 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        InvCircle  <-  InvCircle_F(h=h, F=0.2, z=z)
        PosCircle  <-  PosCircle_F(h=h, F=0.2, z=z)
#        InvCircle  <-  invCircle_asin_nsolve(h=h, F=0.2, z=z)
#        PosCircle  <-  posCircle_asin_nsolve(h=h, F=0.2, z=z)
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
        draw.circle(0,PosCircle[1],PosCircle[2], lty=1, lwd=1.5, border=COL8[5]) # bal. sel.
        draw.circle(0,0,z, lty=2, lwd=1.5, border=COL8[1]) # bal. sel.
        points(0,0, pch=21, col=1, bg=1, cex=1.5) # Phenotypic Optimum
        points(0,-z, pch=21, col=2, bg=2, cex=1.5) # Wild-type population phenotype
        # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste(italic(F)," = ", 0.2)), cex=2, adj=c(0.5, 0.5), xpd=NA)

     # Panel (C) F = 0.4
     plot(NA, axes=FALSE, type='n', main='', xlim = c(min(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1])), 
                                             ylim = c(min(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1])), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        # Balancing sel
        points(z.hom.raw.1[F == 0.4 & BalSel.F == 1] ~ 
               z.hom.raw.2[F == 0.4 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        # Positive sel
        points(z.hom.raw.1[F == 0.4 & PosSel.F == 1] ~ 
               z.hom.raw.2[F == 0.4 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        InvCircle  <-  InvCircle_F(h=h, F=0.4, z=z)
        PosCircle  <-  PosCircle_F(h=h, F=0.4, z=z)
#        InvCircle  <-  invCircle_asin_nsolve(h=h, F=0.4, z=z)
#        PosCircle  <-  posCircle_asin_nsolve(h=h, F=0.4, z=z)
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
        draw.circle(0,PosCircle[1],PosCircle[2], lty=1, lwd=1.5, border=COL8[5]) # bal. sel.
        draw.circle(0,0,z, lty=2, lwd=1.5, border=COL8[1]) # bal. sel.
        points(0,0, pch=21, col=1, bg=1, cex=1.5) # Phenotypic Optimum
        points(0,-z, pch=21, col=2, bg=2, cex=1.5) # Wild-type population phenotype
       # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste(italic(F)," = ", 0.4)), cex=2, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.9,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste("Balancing sel.")),
                            expression(paste("Directional sel."))),
               pch     =  21,
               pt.bg   =  c(transparentColor(COL8[3], opacity=0.4),
                            transparentColor(COL8[2], opacity=0.4)),
               col     =  c(transparentColor(COL8[3], opacity=0.6),
                            transparentColor(COL8[2], opacity=0.6)),
               cex     =  1.5,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

     # Panel (D) F = 0.6
     plot(NA, axes=FALSE, type='n', main='', xlim = c(min(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1])), 
                                             ylim = c(min(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1])), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        # Balancing sel
        points(z.hom.raw.1[F == 0.6 & BalSel.F == 1] ~ 
               z.hom.raw.2[F == 0.6 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        # Positive sel
        points(z.hom.raw.1[F == 0.6 & PosSel.F == 1] ~ 
               z.hom.raw.2[F == 0.6 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        InvCircle  <-  InvCircle_F(h=h, F=0.6, z=z)
        PosCircle  <-  PosCircle_F(h=h, F=0.6, z=z)
#        InvCircle  <-  invCircle_asin_nsolve(h=h, F=0.6, z=z)
#        PosCircle  <-  posCircle_asin_nsolve(h=h, F=0.6, z=z)
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
        draw.circle(0,PosCircle[1],PosCircle[2], lty=1, lwd=1.5, border=COL8[5]) # bal. sel.
        draw.circle(0,0,z, lty=2, lwd=1.5, border=COL8[1]) # bal. sel.
        points(0,0, pch=21, col=1, bg=1, cex=1.5) # Phenotypic Optimum
        points(0,-z, pch=21, col=2, bg=2, cex=1.5) # Wild-type population phenotype
       # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste(italic(F)," = ", 0.6)), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( 0.5, -0.2, expression(paste("Dimension 1")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.2, 0.5,   expression(paste("Dimension 2")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)

     # Panel (E) F = 0.8
     plot(NA, axes=FALSE, type='n', main='', xlim = c(min(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1])), 
                                             ylim = c(min(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1])), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        # Balancing sel
        points(z.hom.raw.1[F == 0.8 & BalSel.F == 1] ~ 
               z.hom.raw.2[F == 0.8 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        # Positive sel
        points(z.hom.raw.1[F == 0.8 & PosSel.F == 1] ~ 
               z.hom.raw.2[F == 0.8 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        InvCircle  <-  InvCircle_F(h=h, F=0.8, z=z)
        PosCircle  <-  PosCircle_F(h=h, F=0.8, z=z)
#        InvCircle  <-  invCircle_asin_nsolve(h=h, F=0.8, z=z)
#        PosCircle  <-  posCircle_asin_nsolve(h=h, F=0.8, z=z)
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
        draw.circle(0,PosCircle[1],PosCircle[2], lty=1, lwd=1.5, border=COL8[5]) # bal. sel.
        draw.circle(0,0,z, lty=2, lwd=1.5, border=COL8[1]) # bal. sel.
        points(0,0, pch=21, col=1, bg=1, cex=1.5) # Phenotypic Optimum
        points(0,-z, pch=21, col=2, bg=2, cex=1.5) # Wild-type population phenotype
       # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste(italic(F)," = ", 0.8)), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( 0.5, -0.2, expression(paste("Dimension 1")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)

     # Panel (F) F = 0.98
     plot(NA, axes=FALSE, type='n', main='', xlim = c(min(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.2[dat$F == 0.2 & dat$BalSel.out == 1])), 
                                             ylim = c(min(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1]),max(dat$z.hom.raw.1[dat$F == 0.2 & dat$BalSel.out == 1])), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        # Balancing sel
        points(z.hom.raw.1[F == 0.98 & BalSel.F == 1] ~ 
               z.hom.raw.2[F == 0.98 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        # Positive sel
        points(z.hom.raw.1[F == 0.98 & PosSel.F == 1] ~ 
               z.hom.raw.2[F == 0.98 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        # Analytic ranges of pos. & bal. sel.
        InvCircle  <-  InvCircle_F(h=h, F=0.98, z=z)
        PosCircle  <-  PosCircle_F(h=h, F=0.98, z=z)
#        InvCircle  <-  invCircle_asin_nsolve(h=h, F=0.98, z=z)
#        PosCircle  <-  posCircle_asin_nsolve(h=h, F=0.98, z=z)
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
        draw.circle(0,PosCircle[1],PosCircle[2], lty=1, lwd=1.5, border=COL8[5]) # bal. sel.
        draw.circle(0,0,z, lty=1, lwd=1.5, border=COL8[1]) # pos. sel.
        points(0,0, pch=21, col=1, bg=1, cex=1.5) # Phenotypic Optimum
        points(0,-z, pch=21, col=2, bg=2, cex=1.5) # Wild-type population phenotype
       # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste(italic(F)," = ", 0.98)), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( 0.5, -0.2, expression(paste("Dimension 1")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
}




# Corresponding bivariate plot of selection coefficients for 2-D 
# figure of FGM
Fisher_2D_Explore_KimuraPlotFig <-  function(z = 0.05, h = 1/2, reps=10^4) {
    
    # Create data for plotting
    dat <-  Fisher_2D_ExploreFigSims(z = z, h = h, reps = reps)

    # back-calculate selection coefficients for classic heterozygote advantage model (s.1 and s.2)
    dat$s1  <-  -dat$t.wt
    dat$s2  <-  -dat$t.hom


    # set plot axes limits
        yLim  <-  c(min(dat$s2),max(dat$s2))
        xLim  <-  c(0,max(dat$s1,yLim))
    if(z == 0.05) {
        yLim  <-  c(min(dat$s2),0.01)
        xLim  <-  c(0,0.01)
    }
    # Colors
    COL8  <-  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1:6), nrow=2, ncol=3, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(1, 1, 0.75, 1), mar = c(5,5,1,1), bty='o', xaxt='s', yaxt='s')

     # Panel (A) F = 0
    plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        points(s2[F == 0.2 & PosSel.out == 0 & BalSel.out == 0] ~ 
               s1[F == 0.2 & PosSel.out == 0 & BalSel.out == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
        points(s2[F == 0.2 & PosSel.out == 1] ~ 
               s1[F == 0.2 & PosSel.out == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        points(s2[F == 0.2 & BalSel.out == 1] ~ 
               s1[F == 0.2 & BalSel.out == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        lines(x=c(0,1), y=c(0,0), lwd=2, col=COL8[1])
        lines(x=c(0,0), y=c(0,1), lwd=2, col=COL8[1])
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste("Outcrossing")), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( -0.2,  0.5, expression(paste("Selection against ", italic(aa), " ",(italic(s[2])))), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=90)

     # Panel (B) F = 0.2
    plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        points(s2[F == 0.2 & PosSel.F == 0 & BalSel.F == 0] ~ 
               s1[F == 0.2 & PosSel.F == 0 & BalSel.F == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
        points(s2[F == 0.2 & PosSel.F == 1] ~ 
               s1[F == 0.2 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        points(s2[F == 0.2 & BalSel.F == 1& s1 > 0 & s2 > 0] ~ 
               s1[F == 0.2 & BalSel.F == 1& s1 > 0 & s2 > 0], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        # Plot invasion boundaries
        s2.vals  <-  0:1000/1000
        upper  <-  upper.classic(F=0.2, s.2 = s2.vals)
        lower  <-  lower.classic(F=0.2, s.2 = s2.vals)
        lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1])
        lines(lower ~ s2.vals, lwd=2, col=COL8[1])
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste(italic(F)," = ", 0.2)), cex=2, adj=c(0.5, 0.5), xpd=NA)

     # Panel (C) F = 0.4
    plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        points(s2[F == 0.4 & PosSel.F == 0 & BalSel.F == 0 ] ~ 
               s1[F == 0.4 & PosSel.F == 0 & BalSel.F == 0 ], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
        points(s2[F == 0.4 & PosSel.F == 1 ] ~ 
               s1[F == 0.4 & PosSel.F == 1 ], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        points(s2[F == 0.4 & BalSel.F == 1] ~ 
               s1[F == 0.4 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        # Plot invasion boundaries
        upper  <-  upper.classic(F=0.4, s.2 = s2.vals)
        lower  <-  lower.classic(F=0.4, s.2 = s2.vals)
        lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1])
        lines(lower ~ s2.vals, lwd=2, col=COL8[1])
       # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste(italic(F)," = ", 0.4)), cex=2, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*1,
               y       =  usr[4]*1,
                   legend  =  c(expression(paste("Directional sel. against ", italic(a))),
                                expression(paste("Balancing sel.")),
                                expression(paste("Directional sel. for ", italic(a)))),
               pch     =  21,
               pt.bg   =  c(transparentColor('tomato', opacity=0.4),
                            transparentColor(COL8[3], opacity=0.4),
                            transparentColor(COL8[2], opacity=0.4)),
               col     =  c(transparentColor('tomato', opacity=0.6),
                            transparentColor(COL8[3], opacity=0.6),
                            transparentColor(COL8[2], opacity=0.6)),
               cex     =  1.5,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

     # Panel (D) F = 0.6
    plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        points(s2[F == 0.6 & PosSel.F == 0 & BalSel.F == 0 ] ~ 
               s1[F == 0.6 & PosSel.F == 0 & BalSel.F == 0 ], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
        points(s2[F == 0.6 & PosSel.F == 1 ] ~ 
               s1[F == 0.6 & PosSel.F == 1 ], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        points(s2[F == 0.6 & BalSel.F == 1 ] ~ 
               s1[F == 0.6 & BalSel.F == 1 ], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        # Plot invasion boundaries
        upper  <-  upper.classic(F=0.6, s.2 = s2.vals)
        lower  <-  lower.classic(F=0.6, s.2 = s2.vals)
        lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1])
        lines(lower ~ s2.vals, lwd=2, col=COL8[1])
       # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste(italic(F)," = ", 0.6)), cex=2, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel( -0.2,  0.5, expression(paste("Selection against ", italic(aa), " ",(italic(s[2])))), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=90)
        proportionalLabel( 0.5,  -0.2, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        

     # Panel (E) F = 0.8
    plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        points(s2[F == 0.8 & PosSel.F == 0 & BalSel.F == 0] ~ 
               s1[F == 0.8 & PosSel.F == 0 & BalSel.F == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
        points(s2[F == 0.8 & PosSel.F == 1] ~ 
               s1[F == 0.8 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        points(s2[F == 0.8 & BalSel.F == 1] ~ 
               s1[F == 0.8 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        # Plot invasion boundaries
        upper  <-  upper.classic(F=0.8, s.2 = s2.vals)
        lower  <-  lower.classic(F=0.8, s.2 = s2.vals)
        lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1])
        lines(lower ~ s2.vals, lwd=2, col=COL8[1])
       # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste(italic(F)," = ", 0.8)), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( 0.5,  -0.2, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        

     # Panel (F) F = 0.98
    plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
           # Plot Points
        points(s2[F == 0.98 & PosSel.F == 0 & BalSel.F == 0] ~ 
               s1[F == 0.98 & PosSel.F == 0 & BalSel.F == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
        points(s2[F == 0.98 & PosSel.F == 1] ~ 
               s1[F == 0.98 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
        points(s2[F == 0.98 & BalSel.F == 1] ~ 
               s1[F == 0.98 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
        # Plot invasion boundaries
        s2.vals  <-  0:1000/1000
        for(i in 1:4){
            upper  <-  upper.classic(F=0.98, s.2 = s2.vals)
            lower  <-  lower.classic(F=0.98, s.2 = s2.vals)
            lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1])
            lines(lower ~ s2.vals, lwd=2, col=COL8[1])
        }
       # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste(italic(F)," = ", 0.98)), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel( 0.5,  -0.2, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
}
    




#########################
# Generalized plot of Bivariate distribution of selection coefficients

# Bivariate plot of selection coefficients for any number of traits
# arg:  coeffs = "CLASSIC" or "FGM" toggles whether plot shows
# the classic s1, s2 paramerization of het. adv. or FGM s.het, s.hom
BivariateSelFig <-  function(xAvg = 5, n = 50, z = 1, h = 1/2, reps=10^4, coeffs = "CLASSIC", smallMuts = FALSE, yLim = NA, xLim = NA) {
    
    # Create data for plotting
    dat <-  BivariateSelFigSims(xAvg=xAvg, n = n, z = z, h = h, reps = reps, smallMuts = smallMuts)

    # back-calculate selection coefficients for classic heterozygote advantage model (s.1 and s.2)
    dat$s1  <-  -dat$t.wt
    dat$s2  <-  -dat$t.hom

    # Colors
    COL8  <-  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1:6), nrow=2, ncol=3, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(1, 1, 0.75, 1), mar = c(5,5,1,1), mgp = c(3,1.5,0), bty='o', xaxt='s', yaxt='s')

    if(coeffs == "CLASSIC"){
        
        # set plot axes limits
        if(is.na(yLim)[1]) {
            yLim  <-  c(min(dat$s2),max(dat$s2))
        }
        if(is.na(xLim)[1]) {
            xLim  <-  c(min(dat$s1),max(dat$s1))
        }
        
         # Panel (A) F = 0
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s2[F == 0 & PosSel.out == 0 & BalSel.out == 0] ~ 
                   s1[F == 0 & PosSel.out == 0 & BalSel.out == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s2[F == 0 & PosSel.out == 1] ~ 
                   s1[F == 0 & PosSel.out == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0 & BalSel.out == 1] ~ 
                   s1[F == 0 & BalSel.out == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            lines(x=c(0,1), y=c(0,0), lwd=2, col=COL8[1], lty=2)
            lines(x=c(0,0), y=c(0,1), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lty=3, lwd=2)
            abline(v=0, lty=3, lwd=2)            
            # axes
            axis(1, labels=NA)
            axis(2, las=1, cex.axis=2)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste("Outcrossing")), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( -0.25,  0.5, expression(paste("Selection against ", italic(aa), " ",(italic(s[2])))), cex=2, adj=c(0.5, 0.5), xpd=NA, srt=90)

    
         # Panel (B) F = 0.2
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s2[F == 0.2 & PosSel.F == 0 & BalSel.F == 0] ~ 
                   s1[F == 0.2 & PosSel.F == 0 & BalSel.F == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s2[F == 0.2 & PosSel.F == 1] ~ 
                   s1[F == 0.2 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0.2 & BalSel.F == 1] ~ 
                   s1[F == 0.2 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
            s2.vals  <-  0:1000/1000
                upper  <-  upper.classic(F=0.2, s.2 = s2.vals)
                lower  <-  lower.classic(F=0.2, s.2 = s2.vals)
                lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1], lty=2)
                lines(lower ~ s2.vals, lwd=2, col=COL8[1], lty=2)
            abline(h=0, lty=3, lwd=2)
            abline(v=0, lty=3, lwd=2)
            # axes
            axis(1, labels=NA)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste(italic(F)," = ", 0.2)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
    
         # Panel (C) F = 0.4
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s2[F == 0.4 & PosSel.F == 0 & BalSel.F == 0 ] ~ 
                   s1[F == 0.4 & PosSel.F == 0 & BalSel.F == 0 ], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s2[F == 0.4 & PosSel.F == 1 ] ~ 
                   s1[F == 0.4 & PosSel.F == 1 ], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0.4 & BalSel.F == 1] ~ 
                   s1[F == 0.4 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
            s2.vals  <-  0:1000/1000
                upper  <-  upper.classic(F=0.4, s.2 = s2.vals)
                lower  <-  lower.classic(F=0.4, s.2 = s2.vals)
                lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1], lty=2)
                lines(lower ~ s2.vals, lwd=2, col=COL8[1], lty=2)
            abline(h=0, lty=3, lwd=2)
            abline(v=0, lty=3, lwd=2)
           # axes
            axis(1, labels=NA)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste(italic(F)," = ", 0.4)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
                # Legend
            legend(
                   x       =  usr[2]*1,
                   y       =  usr[4]*1,
                   legend  =  c(expression(paste("Directional sel. against ", italic(a))),
                                expression(paste("Balancing sel.")),
                                expression(paste("Directional sel. for ", italic(a)))),
                   pch     =  21,
                   pt.bg   =  c(transparentColor('tomato', opacity=0.4),
                                transparentColor(COL8[3], opacity=0.4),
                                transparentColor(COL8[2], opacity=0.4)),
                   col     =  c(transparentColor('tomato', opacity=0.6),
                                transparentColor(COL8[3], opacity=0.6),
                                transparentColor(COL8[2], opacity=0.6)),
                   cex     =  1.5,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )

         # Panel (D) F = 0.6
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s2[F == 0.6 & PosSel.F == 0 & BalSel.F == 0 ] ~ 
                   s1[F == 0.6 & PosSel.F == 0 & BalSel.F == 0 ], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s2[F == 0.6 & PosSel.F == 1 ] ~ 
                   s1[F == 0.6 & PosSel.F == 1 ], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0.6 & BalSel.F == 1 ] ~ 
                   s1[F == 0.6 & BalSel.F == 1 ], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
            s2.vals  <-  0:1000/1000
                upper  <-  upper.classic(F=0.6, s.2 = s2.vals)
                lower  <-  lower.classic(F=0.6, s.2 = s2.vals)
                lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1], lty=2)
                lines(lower ~ s2.vals, lwd=2, col=COL8[1], lty=2)
            abline(h=0, lty=3, lwd=2)
            abline(v=0, lty=3, lwd=2)
           # axes
            axis(1, las=1, cex.axis=2)
            axis(2, las=1, cex.axis=2)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste(italic(F)," = ", 0.6)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)        
            proportionalLabel( -0.25,  0.5, expression(paste("Selection against ", italic(aa), " ",(italic(s[2])))), cex=2, adj=c(0.5, 0.5), xpd=NA, srt=90)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=2, adj=c(0.5, 0.5), xpd=NA)        
    
         # Panel (E) F = 0.8
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s2[F == 0.8 & PosSel.F == 0 & BalSel.F == 0] ~ 
                   s1[F == 0.8 & PosSel.F == 0 & BalSel.F == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s2[F == 0.8 & PosSel.F == 1] ~ 
                   s1[F == 0.8 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0.8 & BalSel.F == 1] ~ 
                   s1[F == 0.8 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
            s2.vals  <-  0:1000/1000
                upper  <-  upper.classic(F=0.8, s.2 = s2.vals)
                lower  <-  lower.classic(F=0.8, s.2 = s2.vals)
                lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1], lty=2)
                lines(lower ~ s2.vals, lwd=2, col=COL8[1], lty=2)
            abline(h=0, lty=3, lwd=2)
            abline(v=0, lty=3, lwd=2)
           # axes
            axis(1, las=1, cex.axis=2)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste(italic(F)," = ", 0.8)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=2, adj=c(0.5, 0.5), xpd=NA)        
    
         # Panel (F) F = 0.9
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
               # Plot Points
            points(s2[F == 0.9 & PosSel.F == 0 & BalSel.F == 0] ~ 
                   s1[F == 0.9 & PosSel.F == 0 & BalSel.F == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s2[F == 0.9 & PosSel.F == 1] ~ 
                   s1[F == 0.9 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0.9 & BalSel.F == 1] ~ 
                   s1[F == 0.9 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
            s2.vals  <-  0:1000/1000
                upper  <-  upper.classic(F=0.9, s.2 = s2.vals)
                lower  <-  lower.classic(F=0.9, s.2 = s2.vals)
                lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1], lty=2)
                lines(lower ~ s2.vals, lwd=2, col=COL8[1], lty=2)
            abline(h=0, lty=3, lwd=2)
            abline(v=0, lty=3, lwd=2)
           # axes
            axis(1, las=1, cex.axis=2)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste(italic(F)," = ", 0.9)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=2, adj=c(0.5, 0.5), xpd=NA)        
    }

    # Using standard FGM s.het, s.hom coefficients
    if(coeffs == "FGM"){
        
        if(is.na(xLim)[1]) {
            xLim  <-  c(min(dat$s.hom),max(dat$s.hom))
        }
        if(is.na(yLim)[1]) {
            yLim  <-  c(min(dat$s.het),max(dat$s.het))
        }

         # Panel (A) F = 0
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s.het[F == 0 & PosSel.out == 0 & BalSel.out == 0] ~ 
                   s.hom[F == 0 & PosSel.out == 0 & BalSel.out == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s.het[F == 0 & PosSel.out == 1] ~ 
                   s.hom[F == 0 & PosSel.out == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0 & BalSel.out == 1] ~ 
                   s.hom[F == 0 & BalSel.out == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # invasion conditions
#            lines(x=c(0,1), y=c(0,1), lwd=2, col=COL8[1], lty=2)
#            lines(x=c(0,0), y=c(0,-1), lwd=2, col=COL8[1], lty=2)
            lines(x=c(0,1), y=c(0,1), lwd=2, col=COL8[1], lty=2)
            lines(x=c(-1,0), y=c(0,0), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
            # axes
            axis(1, labels=NA)
            axis(2, las=1, cex.axis=2)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste("Outcrossing")), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( -0.3,  0.5, expression(paste("Selection on ", italic(Aa), " ",(italic(s[het])))), cex=2.5, adj=c(0.5, 0.5), xpd=NA, srt=90)
            # Legend
            legend(
                   x       =  usr[1] + 0.8*(usr[2]-usr[1]),
                   y       =  usr[4]*0.99,# - 0.15*(usr[4]-usr[3]),
                   legend  =  c(expression(paste("Directional sel. for ", italic(a))),
                                expression(paste("Balancing sel.")),
                                expression(paste("Directional sel. against ", italic(a)))),
                   pch     =  21,
                   pt.bg   =  c(transparentColor(COL8[2], opacity=0.4),
                                transparentColor(COL8[3], opacity=0.4),
                                transparentColor('tomato', opacity=0.4)),
                   col     =  c(transparentColor(COL8[2], opacity=0.6),
                                transparentColor(COL8[3], opacity=0.6),
                                transparentColor('tomato', opacity=0.6)),
                   cex     =  2,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )
    
         # Panel (B) F = 0.2
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s.het[F == 0.2 & PosSel.F == 0 & BalSel.F == 0] ~ 
                   s.hom[F == 0.2 & PosSel.F == 0 & BalSel.F == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s.het[F == 0.2 & PosSel.F == 1] ~ 
                   s.hom[F == 0.2 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0.2 & BalSel.F == 1] ~ 
                   s.hom[F == 0.2 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
#            s.het.vals  <-  0:1000/1000
#                upper  <-  upper.FGM(F=0.2, s.het = s.het.vals)
#                lower  <-  lower.FGM(F=0.2, s.het = s.het.vals)
#                lines(upper[upper < 1 ] ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
#                lines(lower ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
            s.hom.vals  <-  0:1000/1000
                upper  <-  upper.FGM2(F=0.2, s.hom = s.hom.vals)
                lower  <-  lower.FGM2(F=0.2, s.hom = rev(-s.hom.vals))
                lines(upper ~ s.hom.vals, lwd=2, col=COL8[1], lty=2)
                lines(lower ~ rev(-s.hom.vals), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
            # axes
            axis(1, labels=NA)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste(italic(F)," = ", 0.2)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
    
         # Panel (C) F = 0.4
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s.het[F == 0.4 & PosSel.F == 0 & BalSel.F == 0 ] ~ 
                   s.hom[F == 0.4 & PosSel.F == 0 & BalSel.F == 0 ], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s.het[F == 0.4 & PosSel.F == 1 ] ~ 
                   s.hom[F == 0.4 & PosSel.F == 1 ], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0.4 & BalSel.F == 1] ~ 
                   s.hom[F == 0.4 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
#            upper  <-  upper.FGM(F=0.4, s.het = s.het.vals)
#            lower  <-  lower.FGM(F=0.4, s.het = s.het.vals)
#            lines(upper[upper < 1 ] ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
#            lines(lower ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
                upper  <-  upper.FGM2(F=0.4, s.hom = s.hom.vals)
                lower  <-  lower.FGM2(F=0.4, s.hom = rev(-s.hom.vals))
                lines(upper ~ s.hom.vals, lwd=2, col=COL8[1], lty=2)
                lines(lower ~ rev(-s.hom.vals), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
           # axes
            axis(1, labels=NA)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste(italic(F)," = ", 0.4)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
    
         # Panel (D) F = 0.6
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s.het[F == 0.6 & PosSel.F == 0 & BalSel.F == 0 ] ~ 
                   s.hom[F == 0.6 & PosSel.F == 0 & BalSel.F == 0 ], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s.het[F == 0.6 & PosSel.F == 1 ] ~ 
                   s.hom[F == 0.6 & PosSel.F == 1 ], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0.6 & BalSel.F == 1 ] ~ 
                   s.hom[F == 0.6 & BalSel.F == 1 ], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
#            upper  <-  upper.FGM(F=0.6, s.het = s.het.vals)
#            lower  <-  lower.FGM(F=0.6, s.het = s.het.vals)
#            lines(upper[upper < 1 ] ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
#            lines(lower ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
            upper  <-  upper.FGM2(F=0.6, s.hom = s.hom.vals)
            lower  <-  lower.FGM2(F=0.6, s.hom = rev(-s.hom.vals))
            lines(upper ~ s.hom.vals, lwd=2, col=COL8[1], lty=2)
            lines(lower ~ rev(-s.hom.vals), lwd=2, col=COL8[1], lty=2)
            # benchmark shet=0, shom=0 axes
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
           # axes
            axis(1, las=1, cex.axis=2)
            axis(2, las=1, cex.axis=2)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste(italic(F)," = ", 0.6)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)        
            proportionalLabel( -0.3,  0.5, expression(paste("Selection on ", italic(Aa), " ",(italic(s[het])))), cex=2.5, adj=c(0.5, 0.5), xpd=NA, srt=90)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection on ", italic(aa), " ",(italic(s[hom])))), cex=2.5, adj=c(0.5, 0.5), xpd=NA)        
    
         # Panel (E) F = 0.8
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s.het[F == 0.8 & PosSel.F == 0 & BalSel.F == 0] ~ 
                   s.hom[F == 0.8 & PosSel.F == 0 & BalSel.F == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s.het[F == 0.8 & PosSel.F == 1] ~ 
                   s.hom[F == 0.8 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0.8 & BalSel.F == 1] ~ 
                   s.hom[F == 0.8 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
#            upper  <-  upper.FGM(F=0.8, s.het = s.het.vals)
#            lower  <-  lower.FGM(F=0.8, s.het = s.het.vals)
#            lines(upper[upper < 1 ] ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
#            lines(lower ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
            upper  <-  upper.FGM2(F=0.8, s.hom = s.hom.vals)
            lower  <-  lower.FGM2(F=0.8, s.hom = rev(-s.hom.vals))
            lines(upper ~ s.hom.vals, lwd=2, col=COL8[1], lty=2)
            lines(lower ~ rev(-s.hom.vals), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
           # axes
            axis(1, las=1, cex.axis=2)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste(italic(F)," = ", 0.8)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection on ", italic(aa), " ",(italic(s[hom])))), cex=2.5, adj=c(0.5, 0.5), xpd=NA)        
    
         # Panel (F) F = 0.9
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
               # Plot Points
            points(s.het[F == 0.9 & PosSel.F == 0 & BalSel.F == 0] ~ 
                   s.hom[F == 0.9 & PosSel.F == 0 & BalSel.F == 0], pch=21, col=transparentColor('tomato', opacity=0.6), bg=transparentColor('tomato', opacity=0.4), data=dat)
            points(s.het[F == 0.9 & PosSel.F == 1] ~ 
                   s.hom[F == 0.9 & PosSel.F == 1], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0.9 & BalSel.F == 1] ~ 
                   s.hom[F == 0.9 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
#            upper  <-  upper.FGM(F=0.9, s.het = s.het.vals)
#            lower  <-  lower.FGM(F=0.9, s.het = s.het.vals)
#            lines(upper[upper < 1 ] ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
#            lines(lower ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
            upper  <-  upper.FGM2(F=0.9, s.hom = s.hom.vals)
            lower  <-  lower.FGM2(F=0.9, s.hom = rev(-s.hom.vals))
            lines(upper ~ s.hom.vals, lwd=2, col=COL8[1], lty=2)
            lines(lower ~ rev(-s.hom.vals), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
           # axes
            axis(1, las=1, cex.axis=2)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.075,   expression(paste(italic(F)," = ", 0.9)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection on ", italic(aa), " ",(italic(s[hom])))), cex=2.5, adj=c(0.5, 0.5), xpd=NA)        
    }


}





#########################
# Generalized plot of Bivariate distribution of selection coefficients
# FOR ESTABLISHED MUTATIONS
# Bivariate plot of selection coefficients for any number of traits
# arg:  coeffs = "CLASSIC" or "FGM" toggles whether plot shows
# the classic s1, s2 paramerization of het. adv. or FGM s.het, s.hom
BivariateSelFig_EstabMuts <-  function(datafile = "./out/BivariateSelFigSims_EstabMuts_Ne1e+05_n50_z1_h0.5_reps10000.csv", coeffs = "CLASSIC", xLim=NA, yLim=NA) {
    
    # IMPORT data for plotting
    dat     <-  read.csv(file=datafile, header=TRUE)

    # back-calculate selection coefficients for classic heterozygote advantage model (s.1 and s.2)
    dat$s1     <-  -dat$t.wt
    dat$s2     <-  -dat$t.hom

    # Colors
    COL8  <-  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1:6), nrow=2, ncol=3, byrow=TRUE)
    layout      <-  layout(layout.mat, respect=TRUE)

    # Generate Plot
    par(omi=c(1, 1, 0.75, 1), mar = c(5,5,1,1), mgp = c(3,1.5,0), bty='o', xaxt='s', yaxt='s')

    if(coeffs == "CLASSIC"){
        
        # set plot axes limits
        if(is.na(yLim)[1]){
                yLim  <-  c(min(dat$s2),max(dat$s2))
            }
        if(is.na(xLim)[1]){
            xLim  <-  c(min(c(dat$s1), -0.01),max(dat$s1))
        }
        
         # Panel (A) F = 0
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(dat$s2[dat$F == 0 & dat$BalSel.F == 0] ~ 
                   s1[F == 0 & BalSel.F == 0], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0 & BalSel.F == 1] ~ 
                   s1[F == 0 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            lines(x=c(0,1), y=c(0,0), lwd=2, col=COL8[1], lty=2)
            lines(x=c(0,0), y=c(0,1), lwd=2, col=COL8[1], lty=2)
            # axes
            axis(1, labels=NA)
            axis(2, las=1, cex.axis=1.5)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste("Outcrossing")), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( -0.3,  0.5, expression(paste("Selection against ", italic(aa), " ",(italic(s[2])))), cex=2, adj=c(0.5, 0.5), xpd=NA, srt=90)

    
         # Panel (B) F = 0.2
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s2[F == 0.2 & BalSel.F == 0] ~ 
                   s1[F == 0.2 & BalSel.F == 0], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0.2 & BalSel.F == 1] ~ 
                   s1[F == 0.2 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
            s2.vals  <-  0:1000/1000
                upper  <-  upper.classic(F=0.2, s.2 = s2.vals)
                lower  <-  lower.classic(F=0.2, s.2 = s2.vals)
                lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1], lty=2)
                lines(lower ~ s2.vals, lwd=2, col=COL8[1], lty=2)
            # axes
            axis(1, labels=NA)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste(italic(F)," = ", 0.2)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
    
         # Panel (C) F = 0.4
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s2[F == 0.4 & BalSel.F == 0 ] ~ 
                   s1[F == 0.4 & BalSel.F == 0 ], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0.4 & BalSel.F == 1] ~ 
                   s1[F == 0.4 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
            s2.vals  <-  0:1000/1000
                upper  <-  upper.classic(F=0.4, s.2 = s2.vals)
                lower  <-  lower.classic(F=0.4, s.2 = s2.vals)
                lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1], lty=2)
                lines(lower ~ s2.vals, lwd=2, col=COL8[1], lty=2)
           # axes
            axis(1, labels=NA)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste(italic(F)," = ", 0.4)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
                # Legend
            legend(
                   x       =  usr[2]*0.75,
                   y       =  usr[4]*1,
                   legend  =  c(expression(paste("Balancing sel.")),
                                expression(paste("Directional sel. for ", italic(a)))),
                   pch     =  21,
                   pt.bg   =  c(transparentColor(COL8[3], opacity=0.4),
                                transparentColor(COL8[2], opacity=0.4)),
                   col     =  c(transparentColor(COL8[3], opacity=0.6),
                                transparentColor(COL8[2], opacity=0.6)),
                   cex     =  1.5,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )

         # Panel (D) F = 0.6
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s2[F == 0.6 & BalSel.F == 0 ] ~ 
                   s1[F == 0.6 & BalSel.F == 0 ], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0.6 & BalSel.F == 1 ] ~ 
                   s1[F == 0.6 & BalSel.F == 1 ], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
                upper  <-  upper.classic(F=0.6, s.2 = s2.vals)
                lower  <-  lower.classic(F=0.6, s.2 = s2.vals)
                lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1], lty=2)
                lines(lower ~ s2.vals, lwd=2, col=COL8[1], lty=2)
           # axes
            axis(1, las=1, cex.axis=1.5)
            axis(2, las=1, cex.axis=1.5)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste(italic(F)," = ", 0.6)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)        
            proportionalLabel( -0.3,  0.5, expression(paste("Selection against ", italic(aa), " ",(italic(s[2])))), cex=2, adj=c(0.5, 0.5), xpd=NA, srt=90)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=2, adj=c(0.5, 0.5), xpd=NA)        
    
         # Panel (E) F = 0.8
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s2[F == 0.8 & BalSel.F == 0] ~ 
                   s1[F == 0.8 & BalSel.F == 0], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0.8 & BalSel.F == 1] ~ 
                   s1[F == 0.8 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
                upper  <-  upper.classic(F=0.8, s.2 = s2.vals)
                lower  <-  lower.classic(F=0.8, s.2 = s2.vals)
                lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1], lty=2)
                lines(lower ~ s2.vals, lwd=2, col=COL8[1], lty=2)
           # axes
            axis(1, las=1, cex.axis=1.5)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste(italic(F)," = ", 0.8)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=2, adj=c(0.5, 0.5), xpd=NA)        
    
         # Panel (F) F = 0.9
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
               # Plot Points
            points(s2[F == 0.9 & BalSel.F == 0] ~ 
                   s1[F == 0.9 & BalSel.F == 0], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s2[F == 0.9 & BalSel.F == 1] ~ 
                   s1[F == 0.9 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
                upper  <-  upper.classic(F=0.9, s.2 = s2.vals)
                lower  <-  lower.classic(F=0.9, s.2 = s2.vals)
                lines(upper[upper < 1 ] ~ s2.vals[upper < 1 ], lwd=2, col=COL8[1], lty=2)
                lines(lower ~ s2.vals, lwd=2, col=COL8[1], lty=2)
           # axes
            axis(1, las=1, cex.axis=1.5)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste(italic(F)," = ", 0.9)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection against ", italic(AA), " ",(italic(s[1])))), cex=2, adj=c(0.5, 0.5), xpd=NA)        
    }

    # Using standard FGM s.het, s.hom coefficients
    if(coeffs == "FGM"){

        # Set axes limits
        if(is.na(xLim)[1]){
            xLim  <-  c(min(dat$s.hom),max(dat$s.hom))
        }
        if(is.na(yLim)[1]){
            yLim  <-  c(min(dat$s.het),max(dat$s.het))
        }

         # Panel (A) F = 0
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s.het[F == 0 & BalSel.F == 0] ~ 
                   s.hom[F == 0 & BalSel.F == 0], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0 & BalSel.F == 1] ~ 
                   s.hom[F == 0 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # invasion conditions
            lines(x=c(0,1), y=c(0,1), lwd=2, col=COL8[1], lty=2)
            lines(x=c(-1,0), y=c(0,0), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
            # axes
            axis(1, labels=NA)
            axis(2, las=1, cex.axis=1.5)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste("Outcrossing")), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( -0.3,  0.5, expression(paste("Selection on ", italic(Aa), " ",(italic(s[het])))), cex=2, adj=c(0.5, 0.5), xpd=NA, srt=90)
            # Legend
            legend(
                   x       =  (usr[1] + 0.75*(usr[2]-usr[1])),
                   y       =  usr[4],# - 0.15*(usr[4]-usr[3]),
                   legend  =  c(expression(paste("Directional sel. for ", italic(a))),
                                expression(paste("Balancing sel."))),
                   pch     =  21,
                   pt.bg   =  c(transparentColor(COL8[2], opacity=0.4),
                                transparentColor(COL8[3], opacity=0.4)),
                   col     =  c(transparentColor(COL8[2], opacity=0.6),
                                transparentColor(COL8[3], opacity=0.6)),
                   cex     =  1.5,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )


         # Panel (B) F = 0.2
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s.het[F == 0.2 & BalSel.F == 0] ~ 
                   s.hom[F == 0.2 & BalSel.F == 0], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0.2 & BalSel.F == 1] ~ 
                   s.hom[F == 0.2 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
#            s.het.vals  <-  0:1000/1000
#                upper  <-  upper.FGM(F=0.2, s.het = s.het.vals)
#                lower  <-  lower.FGM(F=0.2, s.het = s.het.vals)
#                lines(upper[upper < 1 ] ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
#                lines(lower ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
            s.hom.vals  <-  0:1000/1000
                upper  <-  upper.FGM2(F=0.2, s.hom = s.hom.vals)
                lower  <-  lower.FGM2(F=0.2, s.hom = rev(-s.hom.vals))
                lines(upper ~ s.hom.vals, lwd=2, col=COL8[1], lty=2)
                lines(lower ~ rev(-s.hom.vals), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
            # axes
            axis(1, labels=NA)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste(italic(F)," = ", 0.2)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
    
         # Panel (C) F = 0.4
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s.het[F == 0.4 & BalSel.F == 0] ~ 
                   s.hom[F == 0.4 & BalSel.F == 0], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0.4 & BalSel.F == 1] ~ 
                   s.hom[F == 0.4 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
#            upper  <-  upper.FGM(F=0.4, s.het = s.het.vals)
#            lower  <-  lower.FGM(F=0.4, s.het = s.het.vals)
#            lines(upper[upper < 1 ] ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
#            lines(lower ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
                upper  <-  upper.FGM2(F=0.4, s.hom = s.hom.vals)
                lower  <-  lower.FGM2(F=0.4, s.hom = rev(-s.hom.vals))
                lines(upper ~ s.hom.vals, lwd=2, col=COL8[1], lty=2)
                lines(lower ~ rev(-s.hom.vals), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
           # axes
            axis(1, labels=NA)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste(italic(F)," = ", 0.4)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
    
         # Panel (D) F = 0.6
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s.het[F == 0.6 & BalSel.F == 0] ~ 
                   s.hom[F == 0.6 & BalSel.F == 0], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0.6 & BalSel.F == 1] ~ 
                   s.hom[F == 0.6 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
#            upper  <-  upper.FGM(F=0.6, s.het = s.het.vals)
#            lower  <-  lower.FGM(F=0.6, s.het = s.het.vals)
#            lines(upper[upper < 1 ] ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
#            lines(lower ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
                upper  <-  upper.FGM2(F=0.6, s.hom = s.hom.vals)
                lower  <-  lower.FGM2(F=0.6, s.hom = rev(-s.hom.vals))
                lines(upper ~ s.hom.vals, lwd=2, col=COL8[1], lty=2)
                lines(lower ~ rev(-s.hom.vals), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
           # axes
            axis(1, las=1, cex.axis=1.5)
            axis(2, las=1, cex.axis=1.5)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste(italic(F)," = ", 0.6)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)        
            proportionalLabel( -0.3,  0.5, expression(paste("Selection on ", italic(Aa), " ",(italic(s[het])))), cex=2, adj=c(0.5, 0.5), xpd=NA, srt=90)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection on ", italic(aa), " ",(italic(s[hom])))), cex=2, adj=c(0.5, 0.5), xpd=NA)        
    
         # Panel (E) F = 0.8
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(s.het[F == 0.8 & BalSel.F == 0] ~ 
                   s.hom[F == 0.8 & BalSel.F == 0], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0.8 & BalSel.F == 1] ~ 
                   s.hom[F == 0.8 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
#            upper  <-  upper.FGM(F=0.8, s.het = s.het.vals)
#            lower  <-  lower.FGM(F=0.8, s.het = s.het.vals)
#            lines(upper[upper < 1 ] ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
#            lines(lower ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
                upper  <-  upper.FGM2(F=0.8, s.hom = s.hom.vals)
                lower  <-  lower.FGM2(F=0.8, s.hom = rev(-s.hom.vals))
                lines(upper ~ s.hom.vals, lwd=2, col=COL8[1], lty=2)
                lines(lower ~ rev(-s.hom.vals), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
           # axes
            axis(1, las=1, cex.axis=1.5)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste(italic(F)," = ", 0.8)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection on ", italic(aa), " ",(italic(s[hom])))), cex=2, adj=c(0.5, 0.5), xpd=NA)        
    
         # Panel (F) F = 0.9
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.5)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
               # Plot Points
            points(s.het[F == 0.9 & BalSel.F == 0] ~ 
                   s.hom[F == 0.9 & BalSel.F == 0], pch=21, col=transparentColor(COL8[2], opacity=0.6), bg=transparentColor(COL8[2], opacity=0.4), data=dat)
            points(s.het[F == 0.9 & BalSel.F == 1] ~ 
                   s.hom[F == 0.9 & BalSel.F == 1], pch=21, col=transparentColor(COL8[3], opacity=0.6), bg=transparentColor(COL8[3], opacity=0.4), data=dat)
            # Plot invasion boundaries
#            upper  <-  upper.FGM(F=0.9, s.het = s.het.vals)
#            lower  <-  lower.FGM(F=0.9, s.het = s.het.vals)
#            lines(upper[upper < 1 ] ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
#            lines(lower ~ s.het.vals, lwd=2, col=COL8[1], lty=2)
                upper  <-  upper.FGM2(F=0.9, s.hom = s.hom.vals)
                lower  <-  lower.FGM2(F=0.9, s.hom = rev(-s.hom.vals))
                lines(upper ~ s.hom.vals, lwd=2, col=COL8[1], lty=2)
                lines(lower ~ rev(-s.hom.vals), lwd=2, col=COL8[1], lty=2)
            abline(h=0, lwd=1, lty=3)
            abline(v=0, lwd=1, lty=3)
           # axes
            axis(1, las=1, cex.axis=1.5)
            axis(2, labels=NA)
            # Plot labels etc.
            proportionalLabel(0.5,  1.1,  expression(paste(italic(F)," = ", 0.9)), cex=2.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( 0.5,  -0.25, expression(paste("Selection on ", italic(aa), " ",(italic(s[hom])))), cex=2, adj=c(0.5, 0.5), xpd=NA)        
    }


}

####################
Enrichment_EstabMutsFig <-  function(xAvg=2) {
    
    # IMPORT data for plotting
    dat       <-  read.csv(file="./out/BivariateSelFigSims_EstabMuts_xAvg5_Ne1e+05_n50_z1_h0.5_reps10000.csv", header=TRUE)

    # Run simulations for new adaptive mutations
    datNew       <-  BivariateSelFigSims_FavMuts(xAvg=xAvg, n = 50, z = 1, h = 1/2, reps=10^4)

    # back-calculate selection coefficients for classic heterozygote advantage model (s.1 and s.2)
    dat$s1          <-  -dat$t.wt
    dat$s2          <-  -dat$t.hom
    datNew$s1       <-  -datNew$t.wt
    datNew$s2       <-  -datNew$t.hom

    # Calculate relative enrichment of alleles under balancing and positive selection
    # in established vs. new adaptive mutations
    Fs               <-  unique(dat$F)
    RelBal.est.fav   <-  c()
    RelPos.est.fav   <-  c()
    RelBal.fav       <-  c()
    RelBal.est       <-  c()
    meanSel.Bal.fav  <-  c()
    meanSel.Bal.est  <-  c()
    meanSel.Pos.fav  <-  c()
    meanSel.Pos.est  <-  c()

    for(i in 1:length(Fs)) {
        RelBal.est.fav[i]  <-  (sum(dat$BalSel[dat$F==Fs[i]])/sum(dat$F==Fs[i])) / (sum(datNew$BalSel[datNew$F==Fs[i]])/sum(datNew$F==Fs[i]))
        RelPos.est.fav[i]  <-  (sum(dat$BalSel[dat$F==Fs[i]] == 0) / sum(dat$F==Fs[i])) / (sum(datNew$BalSel[datNew$F==Fs[i]] == 0) / sum(datNew$F==Fs[i]))
        
        RelBal.fav[i]  <-  (sum(datNew$BalSel[datNew$F==Fs[i]] == 1)/sum(datNew$F==Fs[i])) / (sum(datNew$BalSel[datNew$F==0])/sum(datNew$F==Fs[1]))
        RelBal.est[i]  <-  (sum(dat$BalSel[dat$F==Fs[i]] == 1)/sum(dat$F==Fs[i])) / (sum(dat$BalSel[dat$F==0])/sum(dat$F==Fs[1]))

        meanSel.Bal.fav[i]  <-  mean( datNew$s.het[datNew$BalSel == 1 & datNew$F==Fs[i]]*(1-Fs[i]) + datNew$s.hom[datNew$BalSel == 1 & datNew$F==Fs[i]]*Fs[i])
        meanSel.Pos.fav[i]  <-  mean( datNew$s.het[datNew$BalSel == 0 & datNew$F==Fs[i]]*(1-Fs[i]) + datNew$s.hom[datNew$BalSel == 0 & datNew$F==Fs[i]]*Fs[i])
        
        meanSel.Bal.est[i]  <-  mean( dat$s.het[dat$BalSel == 1 & dat$F==Fs[i]]*(1-Fs[i]) + dat$s.hom[dat$BalSel == 1 & dat$F==Fs[i]]*Fs[i])
        meanSel.Pos.est[i]  <-  mean( dat$s.het[dat$BalSel == 0 & dat$F==Fs[i]]*(1-Fs[i]) + dat$s.hom[dat$BalSel == 0 & dat$F==Fs[i]]*Fs[i])

    }

    # Colors
    COL8  <-  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(1:2, nrow=2, ncol=1, byrow=TRUE)
    layout      <-  layout(layout.mat, respect=TRUE)

    # Generate Plot
    par(omi=c(0.75, 0.75, 0.75, 0.75), mar = c(3,3,1,1), bty='o', xaxt='s', yaxt='s')


         # Panel (A) Relative enrichment in established vs. new adaptive mutations
        plot(NA, axes=FALSE, type='n', main='', xlim = c(0,1), ylim = c(0, 1.4), ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(RelPos.est.fav ~ Fs, pch=21, col=transparentColor(COL8[2], opacity=1), bg=transparentColor(COL8[2], opacity=0.8), data=dat)
            points(RelBal.est.fav ~ Fs, pch=21, col=transparentColor(COL8[3], opacity=1), bg=transparentColor(COL8[3], opacity=0.8), data=dat)
            # Benchmark ~ 1
            abline(h=1, lwd=1, lty=3)
#            lines((1 - Fs) ~ Fs, lty=2, lwd=1)
            # axes
            axis(1, labels=NA)
            axis(2, las=1)
            # Plot labels etc.
#            proportionalLabel(0.5,  1.05,  expression(paste("Outcrossing")), cex=2, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel(0.05,  1.075,   expression(paste("A")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( -0.25,  0.5, expression(paste("Relative proportion (estab./adapt.)")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)
#            proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
            # Legend
            legend(
                   x       =  usr[2],
                   y       =  usr[4],
                   legend  =  c(expression(paste("Balancing sel.")),
                                expression(paste("Directional sel."))),
                   pch     =  21,
                   pt.bg   =  c(transparentColor(COL8[3], opacity=0.6),
                                transparentColor(COL8[2], opacity=0.6)),
                   col     =  c(transparentColor(COL8[3], opacity=0.8),
                                transparentColor(COL8[2], opacity=0.8)),
                   cex     =  1,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )

        # Panel (B) Average strength of selection on established & new adaptive mutations under
        #           either balancing or positive selection
         plot(NA, axes=FALSE, type='n', main='', xlim = c(0,1), ylim = c(0, 0.04), ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot Points
            points(meanSel.Bal.fav ~ Fs, pch=21, col=transparentColor(COL8[3], opacity=1), bg=transparentColor(COL8[3], opacity=0.8), data=dat)
            points(meanSel.Pos.fav ~ Fs, pch=21, col=transparentColor(COL8[2], opacity=1), bg=transparentColor(COL8[2], opacity=0.8), data=dat)

            points(meanSel.Bal.est ~ Fs, pch=23, col=transparentColor(COL8[3], opacity=1), bg=transparentColor(COL8[3], opacity=0.8), data=dat)
            points(meanSel.Pos.est ~ Fs, pch=23, col=transparentColor(COL8[2], opacity=1), bg=transparentColor(COL8[2], opacity=0.8), data=dat)
            # axes
            axis(1, las=1)
            axis(2, las=1)
            proportionalLabel(0.05,  1.075,   expression(paste("B")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( -0.25,  0.5, expression(paste("Mean selective advantage")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)
            proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
            legend(
                   x       =  usr[2],
                   y       =  usr[4],
                   legend  =  c(expression(paste("Established")),
                                expression(paste("New adaptive"))),
                   pch     =  c(23,21),
                   pt.bg   =  NA,
                   col     =  1,
                   cex     =  1,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )


}


#########################
# Marginal distributions of selection coefficients
# Distribution of dominance
# arg:  coeffs = "CLASSIC" or "FGM" toggles whether plot shows
# the classic s1, s2 paramerization of het. adv. or FGM s.het, s.hom
SelectionAndDominanceFig <-  function(xAvg=2, n = 50, z = 1, h = 1/2, reps = 10^4, estMuts = TRUE, datafile = "./out/BivariateSelFigSims_EstabMuts_xAvg2_Ne1e+05_n50_z1_h0.5_reps10000.csv") {

    # Create data for new mutations
    if(estMuts) {
        # IMPORT data for plotting
        dat <-  read.csv(file=datafile, header=TRUE)

    } else {dat <-  BivariateSelFigSims(xAvg=xAvg, n = n, z = z, h = h, reps = reps)}
    

    # Colors
    COL8  <-  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1:6), nrow=2, ncol=3, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(1, 1, 0.75, 1), mar = c(6,5,1,1), bty='o', xaxt='s', yaxt='s')

        # Calculate densities of new mutations under balancing seleciton
        s.hetdens0     <-  density(dat$s.het[dat$F == 0    & dat$BalSel.F == 1], kernel='gaussian') # dist of s.het when F = 0
        s.hetdens0.2   <-  density(dat$s.het[dat$F == 0.2  & dat$BalSel.F == 1], kernel='gaussian') # dist of s.het when F = 0.2
        s.hetdens0.4   <-  density(dat$s.het[dat$F == 0.4  & dat$BalSel.F == 1], kernel='gaussian') # dist of s.het when F = 0.4
        s.hetdens0.6   <-  density(dat$s.het[dat$F == 0.6  & dat$BalSel.F == 1], kernel='gaussian') # dist of s.het when F = 0.6
        s.hetdens0.8   <-  density(dat$s.het[dat$F == 0.8  & dat$BalSel.F == 1], kernel='gaussian') # dist of s.het when F = 0.8
        s.hetdens0.98  <-  density(dat$s.het[dat$F == 0.98 & dat$BalSel.F == 1], kernel='gaussian') # dist of s.het when F = 0.98

        s.homdens0     <-  density(dat$s.hom[dat$F == 0    & dat$BalSel.F == 1], kernel='gaussian') # dist of s.hom when F = 0
        s.homdens0.2   <-  density(dat$s.hom[dat$F == 0.2  & dat$BalSel.F == 1], kernel='gaussian') # dist of s.hom when F = 0.2
        s.homdens0.4   <-  density(dat$s.hom[dat$F == 0.4  & dat$BalSel.F == 1], kernel='gaussian') # dist of s.hom when F = 0.4
        s.homdens0.6   <-  density(dat$s.hom[dat$F == 0.6  & dat$BalSel.F == 1], kernel='gaussian') # dist of s.hom when F = 0.6
        s.homdens0.8   <-  density(dat$s.hom[dat$F == 0.8  & dat$BalSel.F == 1], kernel='gaussian') # dist of s.hom when F = 0.8
        s.homdens0.98  <-  density(dat$s.hom[dat$F == 0.98 & dat$BalSel.F == 1], kernel='gaussian') # dist of s.hom when F = 0.98

        # set plot axes limits
        if(estMuts) {
            xLim  <-  c(min(c(dat$s.het[dat$BalSel.F == 1]), -0.01),max(dat$s.het[dat$BalSel.F == 1]))    
        } else{xLim  <-  c(min(dat$s.het[dat$BalSel.F == 1]), max(dat$s.het[dat$BalSel.F == 1]))}
        yLim  <-  c(0, max(c((s.hetdens0$y/sum(s.hetdens0$y)),(s.hetdens0.2$y/sum(s.hetdens0.2$y)),(s.hetdens0.4$y/sum(s.hetdens0.4$y)),(s.hetdens0.6$y/sum(s.hetdens0.6$y)),(s.hetdens0.8$y/sum(s.hetdens0.8$y)),(s.hetdens0.98$y/sum(s.hetdens0.98$y)))))
        
         # Panel (A) s.hom Densities
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot distribution of s1
            lines((s.hetdens0$y/sum(s.hetdens0$y)) ~ s.hetdens0$x, lwd=2, col=transparentColor(COL8[1], opacity=0.8))
            lines((s.hetdens0.2$y/sum(s.hetdens0.2$y)) ~ s.hetdens0.2$x, lwd=2, col=transparentColor(COL8[2], opacity=0.8))
            lines((s.hetdens0.4$y/sum(s.hetdens0.4$y)) ~ s.hetdens0.4$x, lwd=2, col=transparentColor(COL8[3], opacity=0.8))
            lines((s.hetdens0.6$y/sum(s.hetdens0.6$y)) ~ s.hetdens0.6$x, lwd=2, col=transparentColor(COL8[4], opacity=0.8))
            lines((s.hetdens0.8$y/sum(s.hetdens0.8$y)) ~ s.hetdens0.8$x, lwd=2, col=transparentColor(COL8[6], opacity=0.8))
            # axes
            axis(1, las=1)
            axis(2, las=1)
            # Plot labels etc.
            proportionalLabel(0.05,  1.05,   expression(paste("A")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( -0.25,  0.5, expression(paste("Probability density")), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=90)
#            proportionalLabel( 0.5,  -0.2, expression(paste("Selection on ", italic(Aa), " ",(italic(s[het])))), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=0)
#            proportionalLabel(1.18,  1.175,  expression(paste("Distributions of selection coefficients")), cex=2, adj=c(0.5, 0.5), xpd=NA)
            # Legend
            legend(
                   x       =  usr[2],
                   y       =  usr[4],
                   legend  =  c(expression(paste(italic(F)==0)),
                                expression(paste(italic(F)==0.2)),
                                expression(paste(italic(F)==0.4)),
                                expression(paste(italic(F)==0.6)),
                                expression(paste(italic(F)==0.8))),
                   lwd     =  2,
                   col     =  c(transparentColor(COL8[1], opacity=0.9),
                                transparentColor(COL8[2], opacity=0.9),
                                transparentColor(COL8[3], opacity=0.9),
                                transparentColor(COL8[4], opacity=0.9),
                                transparentColor(COL8[6], opacity=0.9)),
                   cex     =  1.25,
                   xjust   =  1,
                   yjust   =  1,
                   bty     =  'n',
                   border  =  NA
                   )
        
         # Panel (B) s.het Densities
        # set plot axes limits
        if(estMuts) {
                    xLim  <-  c(-0.05, max(dat$s.hom[dat$BalSel.F == 1]))
        } else{xLim  <-       c(-0.15, max(dat$s.hom[dat$BalSel.F == 1]))}
        yLim  <-  c(0, max(c((s.homdens0$y/sum(s.homdens0$y)),(s.homdens0.2$y/sum(s.homdens0.2$y)),(s.homdens0.4$y/sum(s.homdens0.4$y)),(s.homdens0.6$y/sum(s.homdens0.6$y)),(s.homdens0.8$y/sum(s.homdens0.8$y)),(s.homdens0.98$y/sum(s.homdens0.98$y)))))

        plot(NA, axes=FALSE, type='n', main='', xlim = c(-0.15,0.15), ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot distribution of s1
            lines((s.homdens0$y/sum(s.homdens0$y)) ~ s.homdens0$x, lwd=2, col=transparentColor(COL8[1], opacity=0.8))
            lines((s.homdens0.2$y/sum(s.homdens0.2$y)) ~ s.homdens0.2$x, lwd=2, col=transparentColor(COL8[2], opacity=0.8))
            lines((s.homdens0.4$y/sum(s.homdens0.4$y)) ~ s.homdens0.4$x, lwd=2, col=transparentColor(COL8[3], opacity=0.8))
            lines((s.homdens0.6$y/sum(s.homdens0.6$y)) ~ s.homdens0.6$x, lwd=2, col=transparentColor(COL8[4], opacity=0.8))
            lines((s.homdens0.8$y/sum(s.homdens0.8$y)) ~ s.homdens0.8$x, lwd=2, col=transparentColor(COL8[6], opacity=0.8))
            # axes
            axis(1, las=1)
            axis(2, las=1)
            # Plot labels etc.
            proportionalLabel(0.05,  1.05,   expression(paste("B")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
#            proportionalLabel( 0.5,  -0.2, expression(paste("Selection on ", italic(aa), " ",(italic(s[hom])))), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=0)
            proportionalLabel(0.5,  1.175,  expression(paste("Overdominant mutations")), cex=2, adj=c(0.5, 0.5), xpd=NA)


    
         # Panel (C) Distribution of dominance for fitness (h)
        dat$h   <-  dat$s.hom - 2*dat$s.het
        hdens0     <-  density(dat$h[dat$F == 0    & dat$BalSel.F == 1], kernel='gaussian') # dist of h when F = 0
        hdens0.2   <-  density(dat$h[dat$F == 0.2  & dat$BalSel.F == 1], kernel='gaussian') # dist of h when F = 0.2
        hdens0.4   <-  density(dat$h[dat$F == 0.4  & dat$BalSel.F == 1], kernel='gaussian') # dist of h when F = 0.4
        hdens0.6   <-  density(dat$h[dat$F == 0.6  & dat$BalSel.F == 1], kernel='gaussian') # dist of h when F = 0.6
        hdens0.8   <-  density(dat$h[dat$F == 0.8  & dat$BalSel.F == 1], kernel='gaussian') # dist of h when F = 0.8


        xLim  <-  c(min(c(dat$h[dat$BalSel.F == 1])), max(dat$h[dat$BalSel.F == 1]))
        yLim  <-  c(0, max(c((hdens0$y/sum(hdens0$y)),(hdens0.2$y/sum(hdens0.2$y)),(hdens0.4$y/sum(hdens0.4$y)),(hdens0.6$y/sum(hdens0.6$y)),(hdens0.8$y/sum(hdens0.8$y)))))

        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot distribution of s1
            lines((hdens0$y/sum(hdens0$y)) ~ hdens0$x, lwd=2, col=transparentColor(COL8[1], opacity=0.8))
            lines((hdens0.2$y/sum(hdens0.2$y)) ~ hdens0.2$x, lwd=2, col=transparentColor(COL8[2], opacity=0.8))
            lines((hdens0.4$y/sum(hdens0.4$y)) ~ hdens0.4$x, lwd=2, col=transparentColor(COL8[3], opacity=0.8))
            lines((hdens0.6$y/sum(hdens0.6$y)) ~ hdens0.6$x, lwd=2, col=transparentColor(COL8[4], opacity=0.8))
            lines((hdens0.8$y/sum(hdens0.8$y)) ~ hdens0.8$x, lwd=2, col=transparentColor(COL8[6], opacity=0.8))
            # axes
            axis(1, las=1)
            axis(2, las=1)
            # Plot labels etc.
            proportionalLabel(0.05,  1.05,   expression(paste("C")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
#            proportionalLabel( 0.5,  -0.2, expression(paste("Fitness dominance (", italic(h), ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=0)
#            proportionalLabel(0.5,  1.175,  expression(paste("Distribution of dominance")), cex=2, adj=c(0.5, 0.5), xpd=NA)


    ## Beneficial mutations

        # Calculate densities of new mutations under positive seleciton
        s.hetdens0     <-  density(dat$s.het[dat$F == 0    & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.het when F = 0
        s.hetdens0.2   <-  density(dat$s.het[dat$F == 0.2  & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.het when F = 0.2
        s.hetdens0.4   <-  density(dat$s.het[dat$F == 0.4  & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.het when F = 0.4
        s.hetdens0.6   <-  density(dat$s.het[dat$F == 0.6  & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.het when F = 0.6
        s.hetdens0.8   <-  density(dat$s.het[dat$F == 0.8  & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.het when F = 0.8
        s.hetdens0.98  <-  density(dat$s.het[dat$F == 0.98 & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.het when F = 0.98

        s.homdens0     <-  density(dat$s.hom[dat$F == 0    & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.hom when F = 0
        s.homdens0.2   <-  density(dat$s.hom[dat$F == 0.2  & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.hom when F = 0.2
        s.homdens0.4   <-  density(dat$s.hom[dat$F == 0.4  & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.hom when F = 0.4
        s.homdens0.6   <-  density(dat$s.hom[dat$F == 0.6  & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.hom when F = 0.6
        s.homdens0.8   <-  density(dat$s.hom[dat$F == 0.8  & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.hom when F = 0.8
        s.homdens0.98  <-  density(dat$s.hom[dat$F == 0.98 & dat$BalSel.F == 0 & dat$s.hom > 0], kernel='gaussian') # dist of s.hom when F = 0.98

        xLim  <-  c(min(c(dat$s.het[dat$BalSel.F == 0 & dat$s.hom > 0]), -0.01),max(dat$s.het[dat$BalSel.F == 0 & dat$s.hom > 0]))
        yLim  <-  c(0, max(c((s.hetdens0$y/sum(s.hetdens0$y)),(s.hetdens0.2$y/sum(s.hetdens0.2$y)),(s.hetdens0.4$y/sum(s.hetdens0.4$y)),(s.hetdens0.6$y/sum(s.hetdens0.6$y)),(s.hetdens0.8$y/sum(s.hetdens0.8$y)),(s.hetdens0.98$y/sum(s.hetdens0.98$y)))))

        # Panel (D) het Densities
        # set plot axes limits
        if(estMuts) {
            xLim  <-  c(min(c(dat$s.het[dat$BalSel.F == 0]), -0.01),max(dat$s.het[dat$BalSel.F == 0]))    
        } else{xLim  <-  c(min(dat$s.het[dat$BalSel.F == 0 & dat$s.hom > 0]), max(dat$s.het[dat$BalSel.F == 0 & dat$s.hom > 0]))}
        yLim  <-  c(0, max(c((s.hetdens0$y/sum(s.hetdens0$y)),(s.hetdens0.2$y/sum(s.hetdens0.2$y)),(s.hetdens0.4$y/sum(s.hetdens0.4$y)),(s.hetdens0.6$y/sum(s.hetdens0.6$y)),(s.hetdens0.8$y/sum(s.hetdens0.8$y)),(s.hetdens0.98$y/sum(s.hetdens0.98$y)))))

        plot(NA, axes=FALSE, type='n', main='', xlim = c(0,xLim[2]), ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot distribution of s1
            lines((s.hetdens0$y/sum(s.hetdens0$y)) ~ s.hetdens0$x, lwd=2, col=transparentColor(COL8[1], opacity=0.8))
            lines((s.hetdens0.2$y/sum(s.hetdens0.2$y)) ~ s.hetdens0.2$x, lwd=2, col=transparentColor(COL8[2], opacity=0.8))
            lines((s.hetdens0.4$y/sum(s.hetdens0.4$y)) ~ s.hetdens0.4$x, lwd=2, col=transparentColor(COL8[3], opacity=0.8))
            lines((s.hetdens0.6$y/sum(s.hetdens0.6$y)) ~ s.hetdens0.6$x, lwd=2, col=transparentColor(COL8[4], opacity=0.8))
            lines((s.hetdens0.8$y/sum(s.hetdens0.8$y)) ~ s.hetdens0.8$x, lwd=2, col=transparentColor(COL8[6], opacity=0.8))
            # axes
            axis(1, las=1)
            axis(2, las=1)
            # Plot labels etc.
            proportionalLabel(0.05,  1.05,   expression(paste("D")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( -0.25,  0.5, expression(paste("Probability density")), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=90)
            proportionalLabel( 0.5,  -0.2, expression(paste("Selection on ", italic(Aa), " ",(italic(s[het])))), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=0)

        
         # Panel (E) s.hom Densities
        # set plot axes limits
        if(estMuts) {
                    xLim  <-  c(-0.05, max(dat$s.hom[dat$BalSel.F == 0]))
        } else{xLim  <-       c(-0.15, max(dat$s.hom[dat$BalSel.F == 0 & dat$s.hom > 0]))}
        yLim  <-  c(0, max(c((s.homdens0$y/sum(s.homdens0$y)),(s.homdens0.2$y/sum(s.homdens0.2$y)),(s.homdens0.4$y/sum(s.homdens0.4$y)),(s.homdens0.6$y/sum(s.homdens0.6$y)),(s.homdens0.8$y/sum(s.homdens0.8$y)),(s.homdens0.98$y/sum(s.homdens0.98$y)))))
        xLim  <-  c(0,max(dat$s.hom[dat$BalSel.F == 1]))
        yLim  <-  c(0, max(c((s.homdens0$y/sum(s.homdens0$y)),(s.homdens0.2$y/sum(s.homdens0.2$y)),(s.homdens0.4$y/sum(s.homdens0.4$y)),(s.homdens0.6$y/sum(s.homdens0.6$y)),(s.homdens0.8$y/sum(s.homdens0.8$y)),(s.homdens0.98$y/sum(s.homdens0.98$y)))))
        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot distribution of s1
            lines((s.homdens0$y/sum(s.homdens0$y)) ~ s.homdens0$x, lwd=2, col=transparentColor(COL8[1], opacity=0.8))
            lines((s.homdens0.2$y/sum(s.homdens0.2$y)) ~ s.homdens0.2$x, lwd=2, col=transparentColor(COL8[2], opacity=0.8))
            lines((s.homdens0.4$y/sum(s.homdens0.4$y)) ~ s.homdens0.4$x, lwd=2, col=transparentColor(COL8[3], opacity=0.8))
            lines((s.homdens0.6$y/sum(s.homdens0.6$y)) ~ s.homdens0.6$x, lwd=2, col=transparentColor(COL8[4], opacity=0.8))
            lines((s.homdens0.8$y/sum(s.homdens0.8$y)) ~ s.homdens0.8$x, lwd=2, col=transparentColor(COL8[6], opacity=0.8))
            # axes
            axis(1, las=1)
            axis(2, las=1)
            # Plot labels etc.
            proportionalLabel(0.05,  1.05,   expression(paste("E")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( 0.5,  -0.2, expression(paste("Selection on ", italic(aa), " ",(italic(s[hom])))), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=0)
            proportionalLabel(0.5,  1.175,  expression(paste("Beneficial mutations")), cex=2, adj=c(0.5, 0.5), xpd=NA)

    
         # Panel (F) Distribution of dominance for fitness (h)
        hdens0     <-  density(dat$h[dat$F == 0    & dat$BalSel.F == 0], kernel='gaussian') # dist of h when F = 0
        hdens0.2   <-  density(dat$h[dat$F == 0.2  & dat$BalSel.F == 0], kernel='gaussian') # dist of h when F = 0.2
        hdens0.4   <-  density(dat$h[dat$F == 0.4  & dat$BalSel.F == 0], kernel='gaussian') # dist of h when F = 0.4
        hdens0.6   <-  density(dat$h[dat$F == 0.6  & dat$BalSel.F == 0], kernel='gaussian') # dist of h when F = 0.6
        hdens0.8   <-  density(dat$h[dat$F == 0.8  & dat$BalSel.F == 0], kernel='gaussian') # dist of h when F = 0.8

        xLim  <-  c(min(c(dat$h[dat$BalSel.F == 0 & dat$s.hom > 0])),max(dat$h[dat$BalSel.F == 0 & dat$s.hom > 0]))
        yLim  <-  c(0, max(c((hdens0$y/sum(hdens0$y)),(hdens0.2$y/sum(hdens0.2$y)),(hdens0.4$y/sum(hdens0.4$y)),(hdens0.6$y/sum(hdens0.6$y)),(hdens0.8$y/sum(hdens0.8$y)))))

        plot(NA, axes=FALSE, type='n', main='', xlim = xLim, ylim = yLim, ylab='', xlab='', cex.lab=1.2)
            usr  <-  par('usr')
            rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
            plotGrid(lineCol='grey80')
            box()
            # Plot distribution of s1
            lines((hdens0$y/sum(hdens0$y)) ~ hdens0$x, lwd=2, col=transparentColor(COL8[1], opacity=0.8))
            lines((hdens0.2$y/sum(hdens0.2$y)) ~ hdens0.2$x, lwd=2, col=transparentColor(COL8[2], opacity=0.8))
            lines((hdens0.4$y/sum(hdens0.4$y)) ~ hdens0.4$x, lwd=2, col=transparentColor(COL8[3], opacity=0.8))
            lines((hdens0.6$y/sum(hdens0.6$y)) ~ hdens0.6$x, lwd=2, col=transparentColor(COL8[4], opacity=0.8))
            lines((hdens0.8$y/sum(hdens0.8$y)) ~ hdens0.8$x, lwd=2, col=transparentColor(COL8[6], opacity=0.8))
            # axes
            axis(1, las=1)
            axis(2, las=1)
            # Plot labels etc.
            proportionalLabel(0.05,  1.05,   expression(paste("F")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)
            proportionalLabel( 0.5,  -0.2, expression(paste("Fitness dominance (", italic(h), ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA, srt=0)


}




#####################




fBal_low_n_integral  <-  function(rInv, rPos){
    d  <-  2*rInv
    A  <-  2*rPos
#    1 - (d * acos((A/d)) - A*sqrt(1 - (A/d)^2)))/d
#    1 - (A/d)
    (d - A)#/d
}

Fisher_2D_CompareFig  <-  function(z = 0.05, h = 1/2, reps=5*10^4) {

    # Create data for plotting
    dat <-  Fisher_2D_ExploreFigSims(z = z, h = h, reps = reps)

    # Calculate Rbal for simulated data
    Fs    <-  unique(dat$F)
    Pbal  <-  c()
    Ppos  <-  c()

    # for outcrossers
    sub       <-  subset(dat, F == Fs[1])
    Pbal.out  <-  sum(sub$BalSel.out)/(sum(sub$BalSel.out) + sum(sub$PosSel.out))
    Ppos.out  <-  sum(sub$PosSel.out)/(sum(sub$BalSel.out) + sum(sub$PosSel.out))
    
    for(i in 1:length(Fs)) {
            sub      <-  subset(dat, F == Fs[i])
            Pbal[i]  <-  sum(sub$BalSel.F) /  (sum(sub$BalSel.F) + sum(sub$PosSel.F))
            Ppos[i]  <-  sum(sub$PosSel.F) /  (sum(sub$BalSel.F) + sum(sub$PosSel.F))
    }
    Pbal  <-  c(Pbal.out,Pbal)
    Rbal.sim  <-  Pbal/Pbal.out
    Fs  <-  c(0,Fs)

    # Calculate Rbal for geometric approx.
    Fs.app  <-  seq(0,1, by=0.01)
    invC  <-  c()
    balC  <-  c()
    fBal.approx  <-  c()
    for(i in 1:length(Fs.app)) {
        invC[i]  <-  InvCircle_F(h=h, F=Fs.app[i], z=z)[2]
        balC[i]  <-  PosCircle_F(h=h, F=Fs.app[i], z=z)[2]
        radInv   <-  InvCircle_F(h=h, F=Fs.app[i], z=z)[2]
        radPos   <-  PosCircle_F(h=h, F=Fs.app[i], z=z)[2]
        fBal.approx[i]  <-  fBal_low_n_integral(rInv=radInv, rPos=radPos)
        print(fBal_low_n_integral(rInv=radInv, rPos=radPos))
    }
    invArea  <-  pi*invC^2
    posArea  <-  pi*balC^2
    Rbal.approx  <-  ((invArea - posArea)) / ((invArea[1] - posArea[1]))
#    Rbal.approx  <-  fBal.approx/fBal.approx[1]

    # Colors
    COL8  <-  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # Generate Plot
    par(omi=c(1, 1, 0.75, 1), mar = c(5,5,1,1), bty='o', xaxt='s', yaxt='s')

     # Panel (A) F = 0
     plot(NA, axes=FALSE, type='n', main='', xlim = c(0,1), ylim = c(0,max(Rbal.sim, Rbal.approx)), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Plot Points
        smallMut_large_n  <-  Rbal_smallMutLimit(F.I=Fs.app, h=1/2)
        points(Rbal.sim ~ Fs, pch=21, col=transparentColor(COL8[3], opacity=1), bg=transparentColor(COL8[3], opacity=0.4))
        lines(Rbal.approx ~ Fs.app, lwd=2, lty=2, col=transparentColor(COL8[1], opacity=0.6))
        lines(smallMut_large_n ~ Fs.app, lwd=2, lty=3, col=transparentColor(COL8[1], opacity=0.6))
        lines((1-Fs.app) ~ Fs.app, lwd=2, col=transparentColor(COL8[1], opacity=0.6))
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.5,  1.05,   expression(paste("Low-", italic(n))), cex=2, adj=c(0.5, 0.5), xpd=NA)
        proportionalLabel(-0.2, 0.5,   expression(paste(italic(R)[italic(bal)])), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)
        proportionalLabel( 0.5,  -0.2,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.97,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste("Low-", italic(n), " approx.")),
                            expression(paste("High-", italic(n), ", small mut."))),
               lty     =  c(1,2,3),
               lwd     =  2,
               col     =  COL8[1],
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        legend(
               x       =  usr[2]*0.77,
               y       =  usr[4]*0.87,
               legend  =  c(expression(paste("   Sims."))),
               pch     =  21,
               col     =  c(transparentColor(COL8[3], opacity=0.8)),
               pt.bg   =  transparentColor(COL8[3], opacity=0.4),
               pt.cex  =  1,
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
}



#####################################
# Functions for \hat{V}_G supp figs


ExpVGHatFig  <-  function(smax=0.1) {

    # Set inbreeding values
    Fvals = 0:100/100

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1), nrow=1, ncol=1, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,2), bty='o', xaxt='s', yaxt='s')
     # Panel (A) Polymorphic space
     plot(NA, axes=FALSE, type='n', main='', xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        expVG  <-  ExpVGHat(smax=smax, F=Fvals)
        expVG.out  <-  ExpVGHat(smax=smax, F=0)
        points(expVG/expVG.out ~ Fvals, type='l', lwd=2, col=COL8[1])
        #  Calculate E[\hat{V}_G] from simulated selection coefficients
        F.sim      <-  0:10/10
        expVG.sim  <- c()
        for(i in 1:length(F.sim)) {
            s1vals        <-  runif(min=0, max=smax, n=10^6)
            s2vals        <-  runif(min=0, max=smax, n=10^6)
            VGvals        <-  VGHat(s1=s1vals, s2=s2vals, F=F.sim[i])
            expVG.sim[i]  <-  mean(VGvals)
        }
        points((expVG.sim/expVG.sim[1]) ~ F.sim, pch=21, col=COL8[1], bg=transparentColor(COL8[1], opacity=0.6))
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(-0.2,  0.5,   expression(paste("Relative genetic variance at equilibrium")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
}



relative_VG <-  function(F) {
            xterm = 2/(1+F)
            ( (1 + F)^2 * (3 - 2*F - F^2)/(4*(1 - F)) - (1 + F)^3/(1 - F) * log(x=xterm) ) / (3/4 - log(2))
        }


VGHat.fisher  <-  function(sHet, sHom, F) {
                pHat  <-  (((sHet - sHom)/(1 + sHet)) - F*(sHet/(1+sHet)))/((1 - F)*(((sHet - sHom)/(1 + sHet)) + (sHet/(1+sHet))))
                pHat[pHat > 1]  <-  1
                pHat[pHat < 0]  <-  0
                qHat  <-  1-pHat
                pHat*qHat*(sHet - sHom)*(1 - F^2)
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
VG_variable_x_EstabMuts_Sims  <-  function(xBar=0.2, Ne = 1000, n = 50, z = 1, h = 1/2, nMuts=10^2) {

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
                mutEstablishes       <-  WF_sim_estab_F(F = F, s.het = s.het, s.hom = s.hom, Ne = Ne)
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
    # return dataframe
    return(res.df)

}





ExpVGHatFisherFig  <-  function() {

    # Import data
    dat1     <-  read.csv(file="./out/EstabMut_VG_xBar2_Ne1000_n50_z1_h0.5_nMuts1e+07.csv", header=TRUE)    
    dat2     <-  read.csv(file="./out/EstabMut_VG_xBar10_Ne1000_n50_z1_h0.5_nMuts1e+07.csv", header=TRUE)    

    # Set inbreeding values
    Fvals = 0:100/100

    # Colors
    COL8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1), nrow=1, ncol=1, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,2), bty='o', xaxt='s', yaxt='s')
     # Panel (A) Polymorphic space
     plot(NA, axes=FALSE, type='n', main='', xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        relVG  <-  relative_VG(F=Fvals)
        points(relVG ~ Fvals, type='l', lwd=2, col=COL8[1])
        #  Calculate E[\hat{V}_G] from simulated selection coefficients
        lines((ExpVG/ExpVG[1]) ~ F, lty=3, col=transparentColor(COL8[1], opacity=0.6), data=dat1)
        points((ExpVG/ExpVG[1]) ~ F, pch=21, col=COL8[1], bg=transparentColor(COL8[1], opacity=0.6), data=dat1)
        lines((ExpVG/ExpVG[1]) ~ F, lty=3, col=transparentColor(COL8[1], opacity=0.3), data=dat2)
        points((ExpVG/ExpVG[1]) ~ F, pch=21, col=transparentColor(COL8[1], opacity=0.6), bg=transparentColor(COL8[1], opacity=0.3), data=dat2)
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(-0.2,  0.5,   expression(paste("E[", hat(V)[G], "] / E[", hat(V)[G],"; ",F==0, "]")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.97,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(bar(italic(x))==2)),
                            expression(paste(bar(italic(x))==10))),
               lty     =  c(3,3),
               lwd     =  2,
               col     =  c(transparentColor(COL8[1], opacity=0.6),
                            transparentColor(COL8[1], opacity=0.3)),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        legend(
               x       =  usr[2]*0.82,
               y       =  usr[4]*0.99,
               legend  =  c(" ",
                            " "),
               pch     =  21,
               col     =  c(transparentColor(COL8[1], opacity=0.6),
                            transparentColor(COL8[1], opacity=0.3)),
               pt.bg     =  c(transparentColor(COL8[1], opacity=0.6),
                            transparentColor(COL8[1], opacity=0.3)),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
}

