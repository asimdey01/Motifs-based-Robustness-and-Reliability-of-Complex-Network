
##################################################################################################
#Reliability analysis of individual motifs and also entire network assuming parallel system
###################################################################################################

library(survival) 
library(OIsurv)
library(KMsurv)
  
# Reliability analysis for German Power grid  
# Similarly we can study other power grids

dm01<-read.csv("Motif_Germany_Degree_FRACTION.csv")


######################################################################################################
#------------- Survival curves of V1 ------------------------------------------
D1_V1<- abs(diff(dm01$V1, lag = 1)) # Death
Time1_V1<-dm01$Time[-1]     # delete first one
group1_V1<-dm01$Network


 
df1<-data.frame(D1_V1,Time1_V1)

dm1<- df1[rep(rownames(df1), df1$D1_V1), ]
colnames(dm1) <- c("Death", "Time") #Making column name same
dm1$event1=1


#----------Non-parametric Survival Model-----------------------
NPsV1 <- survfit(Surv(Time, event1) ~ 1,  type="kaplan-meier", conf.type="log", data=dm1) 
#summary(NPsV1)
#plot(NPsV1)
#WPsV1<- w1[1:length(NPsV1$surv)]*(NPsV1$surv)

#-----------Parametric Survival Model-------------------------
PsV1 <- survreg(Surv(Time, event1) ~ 1,  data=dm1,dist="exponential")
#summary(PsV1) 
#lemda=exp(-Intercept)
lamda1<-exp(-PsV1$coefficients[[1]])

t<-c(Time1_V1)
S_V1<-exp(-lamda1*t) 

################## Plot nonparametric vs parametreic ######################

plot(NPsV1,conf.int=FALSE,lwd=2,lty=1,xlab="Time",ylab="Survival Probability",main="V1")
lines(S_V1,col="red",lwd=2,lty=1)

legend('topright',c( "KME","MLE"),
       lty=c(1,1),  lwd=c(2,2), 
       col=c("black","red"))    


#Kaplan-Meier estimate (KME)
# MLE - Max likelihood estimate (Exponential model)
#----------------------------------------------------------------------------------------------------------------------
#------------- Survival curves of V2 -----------------------------------------------------------------------------------
D1_V2<- abs(diff(dm01$V2, lag = 1)) # Death
Time1_V2<-dm01$Time[-1]              
group1_V2<-dm01$Network

#w2<-Cn_Germany_deg$C_V2[-1]
df2<-data.frame(D1_V2,Time1_V2)


dm2<- df2[rep(rownames(df2), df2$D1_V2), ]
colnames(dm2) <- c("Death", "Time") #Making column name same
dm2$event1=1

#----------Non-parametric Survival Model----------------------------------
NPsV2 <- survfit(Surv(Time, event1) ~ 1,  type="kaplan-meier", conf.type="log", data=dm2)

#-----------Parametric Survival Model--------------------------------------
PsV2 <- survreg(Surv(Time, event1) ~ 1,  data=dm2, dist="exponential")
lamda2<-exp(-PsV2$coefficients[[1]])# 0.03107279


S_V2<-exp(-lamda2*t) 
plot(NPsV2,conf.int=FALSE,lwd=2,lty=1,xlab="Time",ylab="Survival Probability")
lines(S_V2,col="red",lwd=2,lty=1)

#----------------------------------------------------------------------------------------------------------------------
#------------- Survival curves of V3 ----------------------------------------------------------------------------------
D1_V3<- abs(diff(dm01$V3, lag = 1)) # Death
Time1_V3<-dm01$Time[-1]      
group1_V3<-dm01$Network


#w3<-Cn_Germany_deg$C_V3[-1]
df3<-data.frame(D1_V3,Time1_V3)

dm3<- df3[rep(rownames(df3), df3$D1_V3), ]
colnames(dm3) <- c("Death", "Time") #Making column name same
dm3$event1=1


#----------Non-parametric Survival Model----------------------------------
NPsV3 <- survfit(Surv(Time, event1) ~ 1,  type="kaplan-meier", conf.type="log", data=dm3)

#-----------Parametric Survival Model--------------------------------------
PsV3 <- survreg(Surv(Time, event1) ~ 1,  data=dm3, dist="exponential")
lamda3<-exp(-PsV3$coefficients[[1]]) #lemda=exp(-Intercept)


S_V3<-exp(-lamda3*t) 
plot(NPsV3,conf.int=FALSE,lwd=2,lty=1,xlab="Time",ylab="Survival Probability")
lines(S_V3,col="red",lwd=2,lty=1)

#----------------------------------------------------------------------------------------------------------------------
#------------- Survival curves of V4 ----------------------------------------------------------------------------------
D1_V4<- abs(diff(dm01$V4, lag = 1)) # Death
Time1_V4<-dm01$Time[-1]          
group1_V4<-dm01$Network


#w4<-Cn_Germany_deg$C_V4[-1]
df4<-data.frame(D1_V4,Time1_V4)

dm4<- df4[rep(rownames(df4), df4$D1_V4), ]
colnames(dm4) <- c("Death", "Time") # Making column name same
dm4$event1=1

#----------Non-parametric Survival Model----------------------------------
NPsV4 <- survfit(Surv(Time, event1) ~ 1,  type="kaplan-meier", conf.type="log", data=dm4)

#-----------Parametric Survival Model--------------------------------------
PsV4 <- survreg(Surv(Time, event1) ~ 1,  data=dm4, dist="exponential")
lamda4<-exp(-PsV4$coefficients[[1]]) #lemda=exp(-Intercept)


S_V4<-exp(-lamda4*t) 
plot(NPsV4,conf.int=FALSE,lwd=2,lty=1,xlab="Time",ylab="Survival Probability")
lines(S_V4,col="red",lwd=2,lty=1)
#----------------------------------------------------------------------------------------------------------------------
#------------- Survival curves of V5 ----------------------------------------------------------------------------------
D1_V5<- abs(diff(dm01$V5, lag = 1)) # Death
Time1_V5<-dm01$Time[-1]  
group1_V5<-dm01$Network

#w5<-Cn_Germany_deg$C_V5[-1]
df5<-data.frame(D1_V5,Time1_V5)


dm5<- df5[rep(rownames(df5), df5$D1_V5), ]
colnames(dm5) <- c("Death", "Time") #Making column name same
dm5$event1=1



#----------Non-parametric Survival Model----------------------------------
NPsV5 <- survfit(Surv(Time, event1) ~ 1,  type="kaplan-meier", conf.type="log", data=dm5)

#-----------Parametric Survival Model--------------------------------------
PsV5 <- survreg(Surv(Time, event1) ~ 1,  data=dm5, dist="exponential")
lamda5<-exp(-PsV5$coefficients[[1]]) #lemda=exp(-Intercept)



S_V5<-exp(-lamda5*t) 
plot(NPsV5,conf.int=FALSE,lwd=2,lty=1,xlab="Time",ylab="Survival Probability")
lines(S_V5,col="red",lwd=2,lty=1)


print(rbind(lamda1,lamda2,lamda3,lamda4,lamda5))




###################### Dependent Comoponent ########################
#####################################################################################
t<-Time1_V1      #Time1_V1<-dm01$Time[-1]
DRt<-numeric(length(t))

for (i in (1:length(t))) { #i=1
  
  PAc<- ((1-exp(-lamda1*t[i]))*(1-exp(-lamda2*t[i]))*(1-exp(-lamda3*t[i]))* (1-exp(-lamda4*t[i]))*(1-exp(-lamda5*t[i])))
  Mn<-min((1-exp(-lamda1*t[i])),(1-exp(-lamda2*t[i])),(1-exp(-lamda3*t[i])), (1-exp(-lamda4*t[i])),(1-exp(-lamda5*t[i])))
  
  DRt[i]<- 1-sqrt(Mn*PAc)
  
}
 

S_V1<-exp(-lamda1*t) 
S_V2<-exp(-lamda2*t) 
S_V3<-exp(-lamda3*t) 
S_V4<-exp(-lamda4*t) 
S_V5<-exp(-lamda5*t) 




###############################Figures################################################
#---------------------- Black-white-------------------------
plot(t,DRt,  type="l", lwd=2,  col='black' , xlab="Time",ylab="Survival Probability", main="German power grid",xlim=c(0,60))

lines(t,S_V1,type='l', lty=4,lwd=2,col='black')   
lines(t,S_V2, type='o', pch = 1,cex = .5, col='black')   
lines(t,S_V3, type='l', lty=3,lwd=2,col='black')

lines(t,S_V4, type='l', lty=5,lwd=2,col='black')

lines(t,S_V5, type='o', pch = 25,cex = .4, col='black')   


legend('topright',cex=1.1,c( "Network System","V1 ", "V2"," V3","V4","V5"),
       lty=c(1,4,1,3,5,1),  lwd=c(2,2,1,2,2,1),  pch = c(NA, NA,1,NA,NA,25))
