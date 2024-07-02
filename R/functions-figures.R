###############
# DEPENDENCIES
###############
library(extrafont)
library(fontcm)
loadfonts(quiet = TRUE)
library(wesanderson)
library(plotrix)

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

######################
# Preliminary Figures
######################


# Illustration of invasion conditions and proportion of parameter
# space permitting balancing selection under classic weak-selection
# approximation
classicParamSpaceFig  <-  function() {

    # Set inbreeding values
    inbreeding.states = c(0.1, 0.3, 0.5, 0.7)
    F.I = inbreeding.states[1]

    # define invasion condition functions
    upper.classic = function(t.2){
      t.2/F.I
    }
    lower.classic = function(t.2){
      F.I*t.2
    }

    # Colors
    colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,1), bty='o', xaxt='s', yaxt='s')
     # Panel (A) Polymorphic space
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        curve(upper.classic, 0, F.I, xlim = c(0, 1), lwd = 3, xlab = "t.2", ylab = "t.1", add=TRUE, col=colorBlindBlack8[1])
        curve(lower.classic, 0, 1, add = TRUE, lwd = 3, col=colorBlindBlack8[1])
        for(i in 2:4){
            F.I = inbreeding.states[i]
            curve(upper.classic, 0, F.I, add = TRUE, lwd = 3, col = colorBlindBlack8[i])
            curve(lower.classic, 0, 1, add = TRUE, lwd = 3, col = colorBlindBlack8[i])
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(-0.2,  0.5,   expression(paste(italic(t[2]))), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25, expression(paste(italic(t[1]))), cex=1.3, adj=c(0.5, 0.5), xpd=NA)        

        proportionalLabel( 0.875,  0.17, expression(paste(italic(F), " = ", 0.1)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=8, col=colorBlindBlack8[1])
        proportionalLabel( 0.875,  0.33,  expression(paste(italic(F), " = ", 0.3)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=13, col=colorBlindBlack8[2])
        proportionalLabel( 0.875,  0.5,  expression(paste(italic(F), " = ", 0.5)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=25, col=colorBlindBlack8[3])
        proportionalLabel( 0.875,  0.67,  expression(paste(italic(F), " = ", 0.7)), cex=1.2, adj=c(0.5, 0.5), xpd=NA, srt=30, col=colorBlindBlack8[4])

     # Panel (B) Proportion of parameter space where balancing selection occurs
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        inbreeding.states = 0.05 + 0:9/10
        Pr.balancing = rep(0, length(inbreeding.states))
        for(i in 1:length(inbreeding.states)){
          F.I = inbreeding.states[i]
          t.1 = runif(10^6)
          t.2 = runif(10^6)
          condition = t.1 > t.2*F.I & t.1 < t.2/F.I
          Pr.balancing[i] = length(condition[!condition == FALSE])/10^6
        }
        points(0:1000/1000, 1 - 0:1000/1000, type = "l", lwd = 3, xlab = "inbreeding coefficient (F)", ylab = "Pr.balancing")
        points(inbreeding.states, Pr.balancing, pch = 16, col = "RED")
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(-0.2,  0.5,   expression(paste("Pr(balancing)")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.88,
               y       =  usr[4]*0.99,
               legend  =  expression(paste(1-italic(F))),
               lty     =  1,
               col     =  colorBlindBlack8[1],
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        legend(
               x       =  usr[2]*0.975,
               y       =  usr[4]*0.91,
               legend  =  expression(paste("Simulations")),
               pch     =  21,
               col     =  'red',
               pt.bg   =  'red',
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

}



######################################
# Kimura-Ohta strong-selection fig

KO_InvCond_F_up  <-  function(s1, F) {
  S  <-  (2*F)/(F + 1)
  (s1*(2 - S - 2*s1))/(S*(1 - 2*s1))
}
KO_InvCond_F_low  <-  function(s1, F) {
  S  <-  (2*F)/(F + 1)
  (1/2)*(1 - sqrt(1 - S + (S^2)*(1/2 - s1)^2) - S*(1/2 - s1))
}

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
rBal_smallMutLimit  <-  function(F.I, h){
    (1 - F.I)*h*(1 - h)*(1/(1 - (1 - F.I)*h) + 1/(F.I + (1 - F.I)*h))
}


# Ratio of inbred/outcross balancing selection
RelBalancingFig  <-  function(F.I = 1/2, h=1/2, favouredMuts = FALSE) {
    
    # mutation size
    sizes  <-  1:1000/200

    # Colors
    colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
        RbalLim  <-  rBal_smallMutLimit(F.I=F.I, h=h)
        RbalLim  <-  rBal_smallMutLimit(F.I=F.I, h=h)
        lines(Rbal ~ sizes,  col = "grey60", lwd = 3)
        lines(rep(1 - F.I, length(sizes)) ~ sizes, lwd = 3)
        lines(rep(RbalLim, length(sizes)) ~ sizes, lty = 3, lwd = 3)
        # Simulations
            RbalSim  <-  relBalancingMutSize_Sims()
        if(favouredMuts) {
            points(rBal.fav ~ x, pch=21, col=transparentColor(colorBlindBlack8[1], opacity=0.6), bg=transparentColor(colorBlindBlack8[1], opacity=0.4), data=RbalSim)
            } else {
            points(rBal ~ x, pch=21, col=transparentColor(colorBlindBlack8[1], opacity=0.6), bg=transparentColor(colorBlindBlack8[1], opacity=0.4), data=RbalSim)
        }
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(0.5,  1.1,   expression(paste(italic(F), " = ", italic(h), " = ", 1/2)), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel(-0.25,  0.5,  expression(paste("Relative probability (", italic(R[bal]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.25, expression(paste("Mutation size (", italic(x), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)        
        # Legend
        legend(
               x       =  usr[2]*0.35,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste(Eq(18))),
                            expression(paste(Eq(15)))),
               lty     =  c(1, 3, 1),
               lwd     =  3,
               col     =  c(colorBlindBlack8[1], colorBlindBlack8[1], "grey60"),
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
               pt.bg   =  transparentColor(colorBlindBlack8[1], opacity=0.4),
               col     =  transparentColor(colorBlindBlack8[1], opacity=0.6),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

    # Panel (B) Proportion of parameter space where balancing selection occurs
    F.values = 0:100/100
    h.i = c(0.5, 0.25, 0.1)
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Lines + points
        lines(rBal_smallMutLimit(F.I = F.values, h = h.i[1]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = colorBlindBlack8[2], lwd = 3)
        lines(rBal_smallMutLimit(F.I = F.values, h = h.i[2]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = colorBlindBlack8[3], lwd = 3)
        lines(rBal_smallMutLimit(F.I = F.values, h = h.i[3]) ~ F.values, type = "l", xlab = "inbreeding coefficient (F)", ylab = "reduction in Pr(bal.)", col = colorBlindBlack8[4], lwd = 3)
        lines((1 - F.values) ~ F.values, col = colorBlindBlack8[1], lwd = 3)
                # Simulations
        RbalSim_h1  <-  relBalancingSmallx_F_Sims(h=1/2)
        RbalSim_h2  <-  relBalancingSmallx_F_Sims(h=1/4)
        RbalSim_h3  <-  relBalancingSmallx_F_Sims(h=1/10)
        if(favouredMuts) {
            points(rBal.fav ~ F, pch=21, col=transparentColor(colorBlindBlack8[2], opacity=0.6), bg=transparentColor(colorBlindBlack8[2], opacity=0.4), data=RbalSim_h1)
            points(rBal.fav ~ F, pch=21, col=transparentColor(colorBlindBlack8[3], opacity=0.6), bg=transparentColor(colorBlindBlack8[3], opacity=0.4), data=RbalSim_h2)
            points(rBal.fav ~ F, pch=21, col=transparentColor(colorBlindBlack8[4], opacity=0.6), bg=transparentColor(colorBlindBlack8[4], opacity=0.4), data=RbalSim_h3)
            } else {
            points(rBal ~ F, pch=21, col=transparentColor(colorBlindBlack8[2], opacity=0.6), bg=transparentColor(colorBlindBlack8[2], opacity=0.4), data=RbalSim_h1)
            points(rBal ~ F, pch=21, col=transparentColor(colorBlindBlack8[3], opacity=0.6), bg=transparentColor(colorBlindBlack8[3], opacity=0.4), data=RbalSim_h2)
            points(rBal ~ F, pch=21, col=transparentColor(colorBlindBlack8[4], opacity=0.6), bg=transparentColor(colorBlindBlack8[4], opacity=0.4), data=RbalSim_h3)
        }
        # axes
        axis(1, las=1)
        axis(2, labels=NA)
        # Plot labels etc.
        proportionalLabel(0.5,  1.1,   expression(paste("Small-mutation limit (", italic(x)%->%0, ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel( 0.5,  -0.25,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legend
        legend(
               x       =  usr[2]*0.99,
               y       =  usr[4]*0.99,
               legend  =  c(expression(paste(1-italic(F))),
                            expression(paste(Eq(17), "; ", italic(h)==1/2)),
                            expression(paste(Eq(17), "; ", italic(h)==1/4)),
                            expression(paste(Eq(17), "; ", italic(h)==1/10))),
               lty     =  1,
               lwd     =  3,
               col     =  c(colorBlindBlack8[1], colorBlindBlack8[2], colorBlindBlack8[3], colorBlindBlack8[4]),
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )

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
funa  <-  function(F,h) {
  (1 - (1 - F)*h^2)/(1 - (1 - F)*h)
}
funb  <-  function(F,h) {
  (F + (1 - F)*h^2)/(F + (1 - F)*h)
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
    colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # 
    par(omi=c(0.5, 0.25, 0.25, 0.5), mar = c(5,5,1,1), bty='o', xaxt='s', yaxt='s')
    plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
        lines((xMaxFs.1/xMaxs.1) ~ F.values, lwd=2, col=colorBlindBlack8[1])
        lines((xMaxFs.2/xMaxs.2) ~ F.values, lwd=2, col=colorBlindBlack8[2])
        lines((xMaxFs.3/xMaxs.3) ~ F.values, lwd=2, col=colorBlindBlack8[3])
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
#        proportionalLabel(0.5,  1.1,   expression(paste("Small-mutation limit (", italic(x)%->%0, ")")), cex=1.5, adj=c(0.5, 0.5), xpd=NA)        
        proportionalLabel(-0.25,  0.5, expression(hat(italic(x))[F]/hat(italic(x))[out]), cex=1.3, adj=c(0.5, 0.5), srt=90, xpd=NA)
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
               col     =  c(colorBlindBlack8[1], colorBlindBlack8[2], colorBlindBlack8[3]),
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

Pr.bal.F = function(x, F.I, z = 1, n = 50){
  z^2/n*x*(sqrt(2)*(1 + F.I)/sqrt(pi)*exp(-x^2/2*(1 + 3*F.I)^2/(2*(1 + F.I))^2) - x*(1 + 3*F.I)/2*(1 - erf(x*(1 + 3*F.I)/(2*sqrt(2)*(1 + F.I))))) - z^2/n*x*(sqrt(2)*(1 + F.I)/sqrt(pi)*exp(-x^2/2*(3 + 1*F.I)^2/(2*(1 + F.I))^2) - x*(1 + 3*F.I)/2*(1 - erf(x*(3 + 1*F.I)/(2*sqrt(2)*(1 + F.I)))))
}

Pr.est = function(x, z = 1, n = 50){
  z^2/n*x*(sqrt(2)/sqrt(pi)*exp(-x^2/8) - x/2*(1 - erf(x/(2*sqrt(2)))))
}

Pr.fix = function(x, z = 1, n = 50){
  z^2/n*x*(sqrt(2)/sqrt(pi)*exp(-9*x^2/8) - x/2*(1 - erf(3*x/(2*sqrt(2)))))
}

Pr.bal = function(x, z = 1, n = 50){
  z^2/n*x*(sqrt(2)/sqrt(pi)*exp(-x^2/8) - x/2*(1 - erf(x/(2*sqrt(2))))) - z^2/n*x*(sqrt(2)/sqrt(pi)*exp(-9*x^2/8) - x/2*(1 - erf(3*x/(2*sqrt(2)))))
}



# Ratio of inbred/outcross balancing selection
RelBalancingEstablishedFig  <-  function(F.I = 1/2, h=1/2, Ne = 10^4) {
    
    # mutation size
    sizes  <-  1:1000/200

    # Colors
    colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
            Rbal     <-  (Pr.bal.F(x = sizes, F.I = F.I)/Pr.est.F(x = sizes, F.I = F.I))/(Pr.bal(sizes)/Pr.est(sizes))
            lines(Rbal ~ sizes,  col = "grey60", lwd = 3)
            lines(rep(1 - F.I, length(sizes)) ~ sizes, lwd = 3)
            lines(rep(((1 - F.I)/(1 + F.I)), length(sizes)) ~ sizes, lty = 3, lwd = 3)
            lines(rep((1 - F.I)^2/(1 + F.I)^2, length(sizes))~ sizes, lty = 3, , lwd = 3, col = "grey60")
        # Simulations
        RbalSim  <-  read.csv("./out/relBal_smallMut_EstabMuts_Ne10e5_F0.5_n50_z1_h0.5_reps10e5.csv")
#            RbalSim  <-  relBalancingMutSize_EstabMuts_Sims(F = F.I, Ne=Ne, reps=5*10^3)
            points(rBal.est ~ x, pch=21, col=transparentColor(colorBlindBlack8[1], opacity=0.6), bg=transparentColor(colorBlindBlack8[1], opacity=0.4), data=RbalSim)
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
               col     =  c(colorBlindBlack8[1], colorBlindBlack8[1], "grey60", "grey60"),
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
               pt.bg   =  transparentColor(colorBlindBlack8[1], opacity=0.4),
               col     =  transparentColor(colorBlindBlack8[1], opacity=0.6),
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
            lines((1 - F.values) ~ F.values, col = colorBlindBlack8[1], lwd = 3)
            lines(F.values, (1 - F.values)/(1 + F.values), lty = 3, lwd = 2, col = colorBlindBlack8[1])
            lines(F.values, (1 - F.values)^2/(1 + F.values)^2, lty = 3, lwd = 2, col = "grey60")
        # Simulations
            RbalSim.F  <-  read.csv("./out/relBal_smallMut_F_EstabMuts_Ne10e5_n50_z1_h0.5_reps10e5.csv", header=TRUE)
#            RbalSim.F  <-  relBalancingSmallx_EstabMuts_F_Sims(h=1/2, Ne=Ne, reps=10^5)
            points(rBal.est ~ F, pch=21, col=transparentColor(colorBlindBlack8[1], opacity=0.6), bg=transparentColor(colorBlindBlack8[1], opacity=0.4), data=RbalSim.F)
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
#               col     =  c(colorBlindBlack8[1], colorBlindBlack8[2], colorBlindBlack8[3], colorBlindBlack8[4]),
#               cex     =  1,
#               xjust   =  1,
#               yjust   =  1,
#               bty     =  'n',
#               border  =  NA
#               )

}





################################################################
# Summary Figure showing small & large mutation size results
# for the relative fraction of favored and established mutations
# under balancing selection

# Ancillary functions for this figure

relBal_largex_fav  <-  function(F) {
    (3*(1 - F^2)) / ((1 + F)*(3 + F))
}

relBal_largex_est  <-  function(F) {
    (27* (1 - F)^2 * (7*F + 5)) / (5*(3 + F)^3 )
}


# Summary Figure for ratio of inbred/outcross balancing selection
RelBalancing_SummaryFig  <-  function(h=1/2, Ne = 10^4, sim.reps=10^3) {
    
    # Inbreeding values
    F.values = 0:100/100

    # Colors
    colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # set plot layout
    layout.mat  <-  matrix(1, nrow=1, ncol=1, byrow=TRUE)
    layout      <-  layout(layout.mat,respect=TRUE)

    # Generate Plot
    par(omi=c(0.5, 0.5, 0.5, 0.5), mar = c(4,4,4,1), bty='o', xaxt='s', yaxt='s')
     # Ratio of Balancing selection for inbreeding to outcrossing
     plot(NA, axes=FALSE, type='n', main='',xlim = c(0,1), ylim = c(0,1.1), ylab='', xlab='', cex.lab=1.2)
        usr  <-  par('usr')
        rect(usr[1], usr[3], usr[2], usr[4], col='white', border=NA)
        plotGrid(lineCol='grey80')
        box()
        # Generate Curves
            lines((1 - F.values) ~ F.values, col = colorBlindBlack8[1], lwd = 3)
            # Favoured mutations
            lines(rBal_smallMutLimit(F.I=F.values, h=1/2) ~ F.values, lty = 3, lwd = 2, col = colorBlindBlack8[2])
            lines(relBal_largex_fav(F=F.values) ~ F.values, lty = 2, lwd = 2, col = colorBlindBlack8[2])
            # Established mutations
            lines(((1 - F.values)^2/(1 + F.values)^2) ~ F.values, lty = 3, lwd = 2, col = colorBlindBlack8[3])
            lines(relBal_largex_est(F=F.values) ~ F.values, lty = 2, lwd = 2, col = colorBlindBlack8[3])
        # Simulations
            RbalSim.F  <-  read.csv("./out/relBal_variable_x_EstabMuts_Ne10e5_n50_z1_h0.5_reps10e5.csv", header=TRUE)
#            RbalSim.F  <-  relBalancingMutSize_variable_x_EstabMuts_Sims(h=1/2, sim.reps=sim.reps)
#            points(rBal.new ~ F, pch=21, col=transparentColor(colorBlindBlack8[2], opacity=0.9), bg=transparentColor(colorBlindBlack8[2], opacity=0.4), data=RbalSim.F)
            points(rBal.fav ~ F, pch=21, col=transparentColor(colorBlindBlack8[2], opacity=0.9), bg=transparentColor(colorBlindBlack8[2], opacity=0.4), data=RbalSim.F)
            points(rBal.est ~ F, pch=21, col=transparentColor(colorBlindBlack8[3], opacity=0.9), bg=transparentColor(colorBlindBlack8[3], opacity=0.4), data=RbalSim.F)
        # axes
        axis(1, las=1)
        axis(2, las=1)
        # Plot labels etc.
        proportionalLabel(-0.15,  0.5,  expression(paste("Relative fraction (", italic(R[f]), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA, srt=90)        
        proportionalLabel( 0.5,  -0.15,  expression(paste("Inbreeding coefficient (", italic(F), ")")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        # Legends
        # Naive predictions
        proportionalLabel( 0.73,  0.95,  expression(paste("Naive Prediction")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        legend(
               x       =  usr[2]*0.78,
               y       =  usr[4]*0.925,
               legend  =  c(expression(paste(1-italic(F)))),
               lty     =  1,
               lwd     =  3,
               col     =  colorBlindBlack8[1],
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Favored mutations
        proportionalLabel( 0.75,  0.82,  expression(paste("Favored Mutations")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        legend(
               x       =  usr[2]*0.93,
               y       =  usr[4]*0.79,
               legend  =  c(expression(paste("Large-", italic(x), " (Eq.7)")),
                            expression(paste("Small-", italic(x), " (Eq.4)"))),
               lty     =  c(2,3),
               lwd     =  3,
               col     =  colorBlindBlack8[2],
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # Established mutations
        proportionalLabel( 0.78,  0.64,  expression(paste("Established Mutations")), cex=1.3, adj=c(0.5, 0.5), xpd=NA)
        legend(
               x       =  usr[2]*0.93,
               y       =  usr[4]*0.6,
               legend  =  c(expression(paste("Large-", italic(x), " (Eq.8)")),
                            expression(paste("Small-", italic(x), " (Eq.6)"))),
               lty     =  c(2,3),
               lwd     =  3,
               col     =  colorBlindBlack8[3],
               cex     =  1,
               xjust   =  1,
               yjust   =  1,
               bty     =  'n',
               border  =  NA
               )
        # simulations
#        legend(
#               x       =  usr[2]*0.865,
#               y       =  usr[4]*0.47,
#               legend  =  c(expression(paste("Simulations"))),
#               pch     =  21,
#               pt.bg   =  transparentColor(colorBlindBlack8[1], opacity=0.4),
#               col     =  transparentColor(colorBlindBlack8[1], opacity=0.6),
#               cex     =  1,
#               xjust   =  1,
#               yjust   =  1,
#               bty     =  'n',
#               border  =  NA
#               )
}

###################################
###################################
# Visualize 2-d FGM for Inbreeding

InvCircle_F  <-  function(h, F, z) {
    c  <-  ((1 - F)/(1 + F))*((1 - h)/h)*z
#    r  <-  ((1 - F)/(1 + F))*(z/h) + (1 - (1 - F)/(1+F))*z

    c(c,r)
}


Fisher_2D_ExploreFig <-  function(z = 1, h = 1/2, reps=10^4) {
    
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
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
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
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
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
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
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
                            expression(paste("Positive sel."))),
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
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
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
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
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
        draw.circle(0,InvCircle[1],InvCircle[2], lty=1, lwd=1.5, border=COL8[1]) # bal. sel.
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