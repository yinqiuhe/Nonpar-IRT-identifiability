#This file reproduces plots in the manuscript

#Plot derivatives
subfolder=""

################################
#1. normal orgive
################################
a=1 #>1 lowr bound issue, <1 upper bound issue
b=1
n=500

#1.1 approximate 0
figtext = paste0(subfolder, "ogive_0",".pdf" )
pdf(file = figtext, width=3.5, height=3) #for adding box in legend
par(mar=c(4.3, 4.5, 0.7, 1.8))
exponum = seq(15,20,length.out = 51)
candidate_N = exp(-exponum)
knum = length(candidate_N)
allratio = rep(NA, knum)
i = 1
for(thetav in candidate_N){
  phiinvtheta=qnorm(thetav)
  nume = a*dnorm ( a* (phiinvtheta - b)  )
  deom = dnorm(phiinvtheta)
  ratio = nume/deom
  allratio[i] = ratio
  i =  i+1
}
plot( candidate_N,  (allratio), type = "l",
      xlab=expression(theta), 
      ylab=expression( paste("P'(", theta, ")" ) ))
dev.off()


#1.2 approximate 1
figtext = paste0(subfolder, "ogive_1",".pdf" )
pdf(file = figtext, width=3.5, height=3) #for adding box in legend
par(mar=c(4.3, 4.5, 0.7, 1.8))
exponum = seq(15,20,length.out = 51)
candidate_N = 1-exp(-exponum)
knum = length(candidate_N)
allratio = rep(NA, knum)
i = 1
for(thetav in candidate_N){
  phiinvtheta=qnorm(thetav)
  nume = a*dnorm ( a* (phiinvtheta - b)  )
  deom = dnorm(phiinvtheta)
  ratio = nume/deom
  allratio[i] = ratio
  i =  i+1
}
plot( candidate_N,  (allratio), type = "l",
      xlab=expression(theta), 
      ylab=expression( paste("P'(", theta, ")" ) ))
dev.off()

#lower bound approximates 0
 
#1.3 IRF functions
figtext = paste0(subfolder, "ogive_IRF",".pdf" )
pdf(file = figtext, width=3.5, height=3) #for adding box in legend
par(mar=c(4.3, 4.5, 0.7, 1.8))
candidate_N = seq(0,1,length.out = 201)
knum = length(candidate_N)
allratio = rep(NA, knum)
i = 1
for(thetav in candidate_N){
  phiinvtheta= qnorm(thetav)
  nume =  a* (phiinvtheta - b)  
  ratio = pnorm( nume )
  allratio[i] = ratio
  i =  i+1
}
plot( candidate_N,  (allratio), type = "l",
      xlab=expression(theta), 
      ylab=expression( paste("P(", theta, ")" ) ))
abline(h=0, lty = 2)
abline(h=1, lty = 2)
dev.off()



################################ 
#2PL
################################

myf <- function(x){
  exp(x)/(1+exp(x))^2
}
 
d = 0.8; c = 0.2

#2.1 approximate 0
figtext = paste0(subfolder, "logistic_0",".pdf" )
pdf(file = figtext, width=3.5, height=3) #for adding box in legend
par(mar=c(4.3, 4.5, 0.5, 1.4))

exponum = seq(10,15,length.out = 51)
candidate_N = exp(-exponum)
knum = length(candidate_N)
allratio = rep(NA, knum)
i = 1
for(thetav in candidate_N){
  phiinvtheta=qnorm(thetav)
  nume = a*myf( a* (phiinvtheta - b) )
  deom = dnorm(phiinvtheta)
  ratio =  (d-c) * nume/deom
  allratio[i] = ratio
  i =  i+1
}
plot( candidate_N,  (allratio), type = "l",
      xlab=expression(theta), 
      ylab=expression( paste("P'(", theta, ")" ) ))
dev.off()

#2.2 approximate 1
figtext = paste0(subfolder, "logistic_1",".pdf" )
pdf(file = figtext, width=3.5, height=3) #for adding box in legend
par(mar=c(4.3, 4.5, 0.5, 1.4))

exponum = seq(10,15,length.out = 51)
candidate_N = 1-exp(-exponum)
knum = length(candidate_N)
allratio = rep(NA, knum)
i = 1
for(thetav in candidate_N){
  phiinvtheta=qnorm(thetav)
  nume = a*myf( a* (phiinvtheta - b) )
  deom =  dnorm(phiinvtheta)
  ratio = (d-c)   * nume/deom
  allratio[i] = ratio
  i =  i+1
}
plot( candidate_N,  (allratio), type = "l",
      xlab=expression(theta), 
      ylab=expression( paste(" P'(", theta, ")" ) ))
dev.off()


#2.3 IRF functions
figtext = paste0(subfolder, "logistic_IRF",".pdf" )
pdf(file = figtext, width=3.5, height=3) #for adding box in legend
par(mar=c(4.3, 4.5, 0.5, 1.4))
candidate_N = seq(0.0000001,0.99999999,length.out = 301)
knum = length(candidate_N)
allratio = rep(NA, knum)
i = 1
for(thetav in candidate_N){
  phiinvtheta= qnorm(thetav)
  nume =  a* (phiinvtheta - b)  
  ratio = c + (d-c) * exp( nume )/(1+ exp( nume ))
  allratio[i] = ratio
  i =  i+1
}
plot( candidate_N,  (allratio), type = "l",
      xlab=expression(theta), 
      ylab=expression( paste("P(", theta, ")" ) ),
      ylim = c(0,1))
abline(h=d, lty = 2)
abline(h=c, lty = 2)
dev.off()


