#A 

# Setting the values

SG = 1997
SW = 906
SuG = 904
SuW = 32


TOT = SG + SW + SuG + SuW
TOT

# Computing Theta with Quadratic Equation

a=-3839
b=-1655
c=64

discr <- (b^2) - (4*a*c)
TH1<-(-b + sqrt(discr)) / (2*a)
TH2<-(-b - sqrt(discr)) / (2*a)

TH1
TH2


THE<-(uniroot(function(THe) (SG + SW + SuG + SuW)*THe^2 - (SG-2*SW-2*SuG-SuW)*THe - 2*SuW, lower = 0, upper = 1)$root)
THE

# Common probability

CP<-c(0.25*(2+THE), 0.25*(1-THE), 0.25*(1-THE), 0.25*THE)
CP
# Asymptotic variance


VarTHE=1/(1997/((THE+2)^2)+(1810/((1-THE)^2))+32/THE^2)
VarTHE

# B

# Standard Error

SETHE=sqrt(VarTHE)
SETHE

# CI of theta

CITHElo=(THE-1.96*SETHE)
CITHEhi=(THE+1.96*SETHE)
CITHElo
CITHEhi


# C


VEC<-c(1,2,3,4) # Creating the vector
THSE<-numeric(0) # Setting the standard error for Theta

REP<-1000 # Number of repeated samplings

# Repeated sampling

for(i in 1:REP){
  s<-sample(VEC, TOT, prob = CP, replace = T)
  SGs<-sum(s==1)
  SWs<-sum(s==2)
  SuGs<-sum(s==3)
  SuWs<-sum(s==4)
  
  # Setting up the MLE for Theta for samples
  
  MLE<-function(SGs, SWs, SuGs, SuWs){
    F<-(uniroot(function(THs) (SGs + SWs + SuGs + SuWs)*THs^2 - (SGs-2*SWs-2*SuGs-SuWs)*THs - 2*SuWs, lower = 0, upper = 1)$root)
    return(F)
  } 
  
  
  THSE[i]<-MLE(SGs, SWs, SuGs, SuWs)
}

THSD<-sd(THSE)
THSD


# D

VEC<-c(1,2,3,4) # Creating the vector
DELTA<-numeric(0) # Setting the standard error for Theta

REP<-1000 # Number of repeated samplings

# Repeated sampling

for(i in 1:REP){
  s<-sample(VEC, TOT, prob = CP, replace = T)
  SGs<-sum(s==1)
  SWs<-sum(s==2)
  SuGs<-sum(s==3)
  SuWs<-sum(s==4)
  
  # Setting up the MLE for Theta for samples
  
  MLE<-function(SGs, SWs, SuGs, SuWs){
    F<-(uniroot(function(THs) (SGs + SWs + SuGs + SuWs)*THs^2 - (SGs-2*SWs-2*SuGs-SuWs)*THs - 2*SuWs, lower = 0, upper = 1)$root)
    return(F)
  } 

  DELTA[i]<-MLE(SGs, SWs, SuGs, SuWs)-THE

}


DELTA<sort(DELTA)


D975<-(DELTA[975])
D25<-(DELTA[25])

print(THE-D975)

print(THE-D25)





