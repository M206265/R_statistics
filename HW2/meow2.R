
# Е1 НОРМАЛЬНОЕ РАСПРЕДЕЛЕНИЕ  -------------------------------------------------------------------------------------------------------------------------------
{e1 = seq(-2, 2, 0.001)
pdf( file="meowV2.pdf")
layout (matrix (c (1,2,3,4), 2, 2))
dsp = 0.2
sd = sqrt(dsp)
mean = 0
{dvalues=dnorm(e1,mean,sd)
plot(e1, dvalues,
     type="l")
 lines(c(mean, mean), c(0,dnorm(mean,mean,sd)), lty="dashed", col= "red")
 lines(c(mean+sd, mean+sd), c(0, dnorm(mean+sd,mean,sd)), lty="dashed", col="blue")
 lines(c(mean-sd, mean-sd), c(0, dnorm(mean-sd,mean,sd)), lty="dashed", col="blue")
 location = "topright"
 lns = c(1, 1)
 colors=c("red","blue")
 labels=c("E=0","D=")
 means=c(round(mean,2), (round(dsp, 2)))
 legend(location, NULL, means, col=colors, lwd=lns )
}
sd(e1)
{pvalues=pnorm(e1,mean,sd)
 plot(e1, pvalues,
      type="l")
 lines(c(mean,mean), c(0,pnorm(mean,mean,sd)), lty="dashed", col="red")
 lines(c(mean+sd, mean+sd), c(0, pnorm(mean+sd,mean,sd)), lty="dashed", col="blue")
 lines(c(mean-sd, mean-sd), c(0, pnorm(mean-sd,mean,sd)), lty="dashed", col="blue")
 location = "topleft"
 lns = c(1, 1)
 colors=c("red","blue")
 labels=c("E=","D=")
 means=c(round(median(e1),2),(round(dsp,2)))
 legend(location, NULL,means, col=colors, lwd=lns)}

 {e11=rnorm(300,mean,sd)
 hist(e11, breaks=10, freq=FALSE)
lines(e1,dnorm(e1,mean,sd),type="l", col="red")}
 
   plot(e1,pnorm(e1,mean,sd), type="l", col="red")
 lines(ecdf(e11))
}
 # Е2 И ЛОГАРИФМ -------------------------------------------------------------------------------------------------------------------------------
 {e2 = seq(-0.1, 10, 0.01)
 
 dspNorm = 1.2
 sdNorm = sqrt(dsp)
 meanNorm = 0.1
 
 dspLog = exp(dspNorm - 1)*exp(2*meanNorm + dspNorm)
 meanLog = exp(meanNorm + dspNorm/2)
 sdLog = sqrt(dspLog)
 
 {dlvalues=dlnorm(e2,0.1,1.2)
   plot(e2, dlvalues,
        type="l")
   lines(c(meanLog,meanLog), c(0, dlnorm(meanLog,meanNorm,sdNorm)), lty="dashed", col= "red")
   lines(c(meanLog+sdLog, meanLog+sdLog), c(0, dlnorm(meanLog+sdLog,meanNorm, sdNorm)), lty="dashed", col="blue")
   lines(c(meanLog-sdLog, meanLog-sdLog), c(0, dlnorm(meanLog-sdLog,meanNorm, sdNorm)), lty="dashed", col="blue")
   location = "topright"
   lns = c(1, 1)
   colors=c("red","blue")
   labels=c("E=","D=")
   meanLogs=c(round(meanLog,2), (round(dspLog, 2)))
   legend(location, NULL, meanLogs, col=colors, lwd=lns )
 }
 {plvalues=plnorm(e2,meanNorm,sdNorm)
   plot(e2, plvalues,
        type="l")
   lines(c(meanLog,meanLog), c(0,plnorm(meanLog,meanNorm,sdNorm)), lty="dashed", col="red")
   lines(c(meanLog+sdLog, meanLog+sdLog), c(0, plnorm(meanLog+sdLog,meanNorm,sdNorm)), lty="dashed", col="blue")
   lines(c(meanLog-sdLog, meanLog-sdLog), c(0, plnorm(meanLog-sdLog,meanNorm,sdNorm)), lty="dashed", col="blue")
   location = "topright"
   lns = c(1, 1)
   colors=c("red","blue")
   labels=c("E=","D=")
   meanLogs=c(round(meanLog,2),(round(dspLog,2)))
   legend(location, NULL, meanLogs, col=colors, lwd=lns)}
 
   e22=rlnorm(300,meanNorm,sdNorm)
   hist(e22, breaks=10, freq = FALSE)
   lines(e2, dlnorm(e2,meanNorm,sdNorm), col="red")
 
   plot(e2,plnorm(e2,meanNorm,sdNorm), type="l", col="red")
   lines(ecdf(e22))
 }
   
# E3 ДИСКРЕТНОЕ РАСПРЕДЕЛЕНИЕ  ----------------------------------------------------------------------------------

{pdisc = function(e3) {
   y = rep(0, length(e3))
   for (i in 1:length(e3))
   {
      if (e3[i] < -4.1) {
         y[i] = 0
      }
      else if (e3[i] < -4.0) {
         y[i] = 0.2
      }
      else if (e3[i] < 0.3) {
         y[i] = 0.2+0.2
      }
      else if (e3[i] < 3.2) {
         y[i] = 0.2+0.2+0.1
      }
      else if (e3[i] < 3.9) {
         y[i] = 0.2+0.2+0.1+0.2
      }
      else {
         y[i] = 1
      }
   }
   return(y)
}
#x = c(-5, -4.1, -4.1,-4.0, -4.0,0.3, 0.3, 3.2, 3.2, 3.9, 3.9, 5)
#y = c(0, 0, 0.2,0.2, 0.4,0.4, 0.5, 0.5, 0.7, 0.7, 1, 1)
#lines(x,y, type ='l', color = 'blue')
e3 = seq(-5,5,0.01)
mean = (-4.1*0.2-4*0.2+0.3*0.1+3.2*0.2+3.9*0.3)
d = (-4.1-mean)**2*0.2+(-4-mean)**2*0.2+(0.3-mean)**2*0.1+(3.2-mean)**2*0.2+(3.9-mean)**2*0.3
sd = sqrt(d)

   {plot(e3, pdisc(e3),
        type="l")
   lines(c(mean,mean), c(0,pdisc(mean)), lty="dashed", col="red")
   lines(c(mean+sd, mean+sd), c(0, pdisc(mean+sd)), lty="dashed", col="blue")
   lines(c(mean-sd, mean-sd), c(0, pdisc(mean-sd)), lty="dashed", col="blue")
   location = "topleft"
   lns = c(1, 1)
   colors=c("red","blue")
   labels=c("E=","D=")
   means=c(round(mean,2),(round(d,2)))
   legend(location, NULL, means, col=colors, lwd=lns)}



  {X=c(-4.1,-4.0,0.3,3.2,3.9)
  P=c(0.2,0.2,0.1,0.2,0.3)
  n=300
  i=1:n
  
  rdisc=sample(X,n,replace=T,prob=P)
 
  
   plot(sort(rdisc[i]), i / n, xlim=c(-5,5), type = "s")
   lines(e3, pdisc(e3), type = "s", col="red") 
   }

   hist(rdisc, breaks=10, freq=FALSE)}

## E12 = E1 + E2 НОРМАЛЬНОЕ + ЛОГАРИФМИЧЕСКОЕ  ----------------------------------------------------------------------------------
{dspnorm = 0.2
sdnorm = sqrt(dspnorm)
meannorm = 0

dsplnorm = 1.2
sdlnorm = sqrt(dsplnorm)
meanlnorm = 0.1

mean = meannorm+meanlnorm
dsp = dspnorm+dsplnorm
sd = sqrt(dsp)

e12 = seq(-5,10,0.01)

dfun = function(t) {
   y = rep(0, length(t))
   for (i in 1:length(t)) {
      mult = function(e12) {
         res = dnorm(e12, meannorm, sdnorm) * dlnorm(t[i] - e12, meanlnorm, sdlnorm)
         return(res)
      }
      y[i] = integrate(mult, -Inf, Inf)
   }
   return(y)
}

df = dfun(e12)
{plot(e12, dfun(e12),
      type="l")
   lines(c(mean,mean), c(0,dfun(mean)), lty="dashed", col="red")
   lines(c(mean+sd, mean+sd), c(0, dfun(mean+sd)), lty="dashed", col="blue")
   lines(c(mean-sd, mean-sd), c(0, dfun(mean-sd)), lty="dashed", col="blue")
   location = "topleft"
   lns = c(1, 1)
   colors=c("red","blue")
   labels=c("E=","D=")
   means=c(round(mean,2),(round(dsp,2)))
   legend(location, NULL, means, col=colors, lwd=lns)}

pfun = e12
pfun[1] = 0
stp = 0.01
for (i in 2:length(e12)) {
   j = i - 1
   pfun[i] = pfun[j] + (df[[i]] * stp)
}

{plot(e12, pfun,
      type="l")
   lines(c(mean,mean), c(0,1), lty="dashed", col="red")
   lines(c(mean+sd, mean+sd), c(0,1), lty="dashed", col="blue")
   lines(c(mean-sd, mean-sd), c(0,1), lty="dashed", col="blue")
   location = "topleft"
   lns = c(1, 1)
   colors=c("red","blue")
   labels=c("E=","D=")
   means=c(round(mean,2),(round(dsp,2)))
   legend(location, NULL, means, col=colors, lwd=lns)}

n = 300
i = 1:n
ran = rnorm(n, meannorm, dspnorm) + rlnorm(n, meanlnorm, dsplnorm)
plot(sort(ran[i]), i / n, type = "p", xlim=c(-5,5))
lines(e12, pfun, type = "l", col = "red")

hist(ran, freq=FALSE, breaks = 10,xlim=c(-5, 10))
lines(e12, dfun(e12), type = "l", col = "red") }

## E13 = E1 + E3 НОРМАЛЬНОЕ + ДИСКРЕТНОЕ  !!!!!!!----------------------------------------------------------------------------------

{dspnorm = 0.2
sdnorm = sqrt(dspnorm)
meannorm = 0

meandisc = (-4.1*0.2-4*0.2+0.3*0.1+3.2*0.2+3.9*0.3)
dspdisc = (-4.1-meandisc)**2*0.2+(-4-meandisc)**2*0.2+(0.3-meandisc)**2*0.1+(3.2-meandisc)**2*0.2+(3.9-meandisc)**2*0.3

y=0
X=c(-4.1,-4,0.3,3.2,3.9)
P=c(0.2,0.2,0.1,0.2,0.3)

mean = meannorm + meandisc
dsp = dspnorm + dspdisc
sd = sqrt(dsp)

dfun =  Vectorize( function(x){
            for( i in 1:length(X) ) {
            y=y+P[i]*dnorm(x-X[i], meannorm, sdnorm) }
         return(y)} )

pfun =  Vectorize( function(x){
            for( i in 1:length(X) ) {
            y=y+P[i]*pnorm(x-X[i], meannorm, sdnorm) }
         return(y)} )

e13 = seq(-5,10,0.01)

{plot(e13, dfun(e13), 
      type="l")
   lines(c(mean,mean), c(0,dfun(mean)), lty="dashed", col="red")
   lines(c(mean+sd, mean+sd), c(0, dfun(mean+sd)), lty="dashed", col="blue")
   lines(c(mean-sd, mean-sd), c(0, dfun(mean-sd)), lty="dashed", col="blue")
   location = "topleft"
   lns = c(1, 1)
   colors=c("red","blue")
   labels=c("E=","D=")
   means=c(round(mean,2),(round(dsp,2)))
   legend(location, NULL, means, col=colors, lwd=lns)}

{plot(e13, pfun(e13),
      type="l")
   lines(c(mean,mean), c(0,pfun(mean)), lty="dashed", col="red")
   lines(c(mean+sd, mean+sd), c(0,pfun(mean+sd)), lty="dashed", col="blue")
   lines(c(mean-sd, mean-sd), c(0,pfun(mean-sd)), lty="dashed", col="blue")
   location = "topleft"
   lns = c(1, 1)
   colors=c("red","blue")
   labels=c("E=","D=")
   means=c(round(mean,2),(round(dsp,2)))
   legend(location, NULL, means, col=colors, lwd=lns)}

n = 300
i = 1:n
rdisc=sample(X,n,replace=T,prob=P)
rane13 = rdisc + rnorm(n,meannorm,sdnorm)

plot(sort(rane13[i]), i / n, type = "p", xlim=c(-5,5))
lines(e13, pfun(e13), type = "l", col = "red")

hist(rane13, freq=FALSE, breaks = 10,xlim=c(-5, 10))
lines(e13, dfun(e13), type = "l", col = "red")
}

## E23 = E2 + E3 ЛОГАРИФМИЧЕСКОЕ + ДИСКРЕТНОЕ !!!!!! ----------------------------------------------------------------------------------
{

dsplnorm = 1.2
sdlnorm = sqrt(dsplnorm)
meanlnorm = 0.1

meandisc = (-4.1*0.2-4*0.2+0.3*0.1+3.2*0.2+3.9*0.3)
dspdisc = (-4.1-meandisc)**2*0.2+(-4-meandisc)**2*0.2+(0.3-meandisc)**2*0.1+(3.2-meandisc)**2*0.2+(3.9-meandisc)**2*0.3

y=0
X=c(-4.1,-4,0.3,3.2,3.9)
P=c(0.2,0.2,0.1,0.2,0.3)

mean = meanlnorm + meandisc
dsp = dsplnorm + dspdisc
sd = sqrt(dsp)

dfun =  Vectorize( function(x){
   for( i in 1:length(X) ) {
      y=y+P[i]*dlnorm(x-X[i], meanlnorm, sdlnorm) }
   return(y)} )

pfun =  Vectorize( function(x){
   for( i in 1:length(X) ) {
      y=y+P[i]*plnorm(x-X[i], meanlnorm, sdlnorm) }
   return(y)} )

e23 = seq(-4.1,10,0.01)

{plot(e23, dfun(e23), 
      type="l")
   lines(c(mean,mean), c(0,dfun(mean)), lty="dashed", col="red")
   lines(c(mean+sd, mean+sd), c(0, dfun(mean+sd)), lty="dashed", col="blue")
   lines(c(mean-sd, mean-sd), c(0, dfun(mean-sd)), lty="dashed", col="blue")
   location = "topleft"
   lns = c(1, 1)
   colors=c("red","blue")
   labels=c("E=","D=")
   means=c(round(mean,2),(round(dsp,2)))
   legend(location, NULL, means, col=colors, lwd=lns)}

{plot(e23, pfun(e23),
      type="l")
   lines(c(mean,mean), c(0,pfun(mean)), lty="dashed", col="red")
   lines(c(mean+sd, mean+sd), c(0,pfun(mean+sd)), lty="dashed", col="blue")
   lines(c(mean-sd, mean-sd), c(0,pfun(mean-sd)), lty="dashed", col="blue")
   location = "topleft"
   lns = c(1, 1)
   colors=c("red","blue")
   labels=c("E=","D=")
   means=c(round(mean,2),(round(dsp,2)))
   legend(location, NULL, means, col=colors, lwd=lns)}

n = 300
i = 1:n
rdisc=sample(X,n,replace=T,prob=P)
rane23 = rdisc + rlnorm(n,meanlnorm,sdlnorm)

plot(sort(rane23[i]), i / n, type = "p", xlim=c(-5,5))
lines(e23, pfun(e23), type = "l", col = "red")

hist(rane23, freq=FALSE, breaks = 10,xlim=c(-5, 10))
lines(e23, dfun(e23), type = "l", col = "red")

}
dev.off()
