##################################
# Set-up (Packages)
#################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rio,tidyverse,rsq,xtable)

#Read me
# If you are working with the MainData.csv file you can skip the first blocks of code, which show how that 
# data file is created from the raw data collected.


#############################################
# Load in raw data - Experiment
#############################################

#set working directory to folder with raw experiment data
dir="Data/Experiment data"

#Create one datafile from 182 csvs
filenames <- list.files(dir, pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)
res <- lapply(ldf, summary)
names(res) <- substr(filenames, 6, 30)
expd=plyr::rbind.fill(ldf)


#Calculate new variables for subj-ID and BMI
expd$Subj=expd$Subjnr
expd$BMI=expd$Weight/((expd$Height/100)*(expd$Height/100))

#######################
#Raw survey data
#######################

surveyd=import("Data/Survey data/Survey data.xlsx")


#Recoding - TSC - items 2, 3, 4, 5, 7, 9 ,10 ,12 and 13 need to be reverse coded
surveyd$TSCs=c()
TSCm=cbind(6-select(surveyd, TSC2,TSC3,TSC4,TSC5,TSC7,TSC9,TSC10,TSC12,TSC13),surveyd$TSC1,surveyd$TSC6,surveyd$TSC8)
surveyd$TSCs=apply(TSCm,1,mean,na.rm=T)

#Scoring - CRT - the correct answers are 5, 5, and 47 respectively.
surveyd$CRTs=c()
CRTm=cbind(surveyd$CRT1=="5",surveyd$CRT2=="5",surveyd$CRT3=="47")
surveyd$CRTs=apply(CRTm,1,sum,na.rm=T)
#Assign NA to all NA 
for (i in 1:nrow(surveyd)){
  if(is.na(surveyd$CRT1[i])&is.na(surveyd$CRT1[i])&is.na(surveyd$CRT1[i])){surveyd$CRTs[i]=NA}
}

#Recoding + scoring - EPQR-A
PQNm=cbind(2-surveyd$PQ1,2-surveyd$PQ9,2-surveyd$PQ11,2-surveyd$PQ14,2-surveyd$PQ18,2-surveyd$PQ21)
PQEm=cbind(2-surveyd$PQ2,2-surveyd$PQ4,2-surveyd$PQ13,surveyd$PQ15-1,surveyd$PQ20-1,2-surveyd$PQ23)
PQPm=cbind(surveyd$PQ3-1,2-surveyd$PQ6,2-surveyd$PQ8,2-surveyd$PQ12,surveyd$PQ16-1,surveyd$PQ22-1)
PQLm=cbind(surveyd$PQ5-1,surveyd$PQ7-1,surveyd$PQ10-1,surveyd$PQ17-1,surveyd$PQ19-1,2-surveyd$PQ24)
surveyd$PQNs=c();surveyd$PQEs=c();surveyd$PQPs=c();surveyd$PQLs=c()

surveyd$PQNs=apply(PQNm,1,mean,na.rm=T)
surveyd$PQEs=apply(PQEm,1,mean,na.rm=T)
surveyd$PQPs=apply(PQPm,1,mean,na.rm=T)
surveyd$PQLs=apply(PQLm,1,mean,na.rm=T)



###################################
# Merge data
####################################
surveyds=select(surveyd,Subj,TSCs,CRTs,PQNs,PQEs,PQPs,PQLs)

fulld=merge(expd,surveyds,by="Subj", all.x=T)

#Some values were set to NAN (i.e. negative NAs, which cause bugs, set to NA)
for (j in 1:ncol(fulld)){
  for (i in 1:nrow(fulld)){
    if(is.nan(fulld[i,j])){
      fulld[i,j]=NA
    }
  }
}
########################################
# Generate full data-set 
########################################

#store data
export(fulld,"Data/MainData.csv")

##########################################
# Functions for further analyses
############################################

#Summary statistics
# create a descriptive statistics table with median, quantiles, mix and max
StatDes<-function(X){
  Tab=matrix(NA,dim(X)[2],5)
  Tab[,1]=apply(X,2,median,na.rm=T)
  Tab[,2]=apply(X,2,quantile,.25,na.rm=T)
  Tab[,3]=apply(X,2,quantile,.75,na.rm=T)
  Tab[,4]=apply(X,2,min,na.rm=T)
  Tab[,5]=apply(X,2,max,na.rm=T)
  colnames(Tab)=c("median","first quartile","third quartile","min","max")
  return(Tab)
}


#####################################################
# Table 2 (descriptives)
#####################################################

#Simple descriptives for Table 2 (column 2)
Tb=StatDes(fulld[,c(17,20:23,57,58,60,61)])
row.names(Tb)=colnames(fulld[,c(17,20:23,57,58,60,61)])
print.xtable(xtable(Tb),type="html",file='Results/T2a.html')

#Classifications for Table 2 (column 3 to 6)
T2n=rbind(
  c(sum(fulld$LAKW<1),sum(fulld$LAKW==1),sum(fulld$LAKW>1)),
  c(sum(fulld$Alpha.G<1),sum(fulld$Alpha.G==1),sum(fulld$Alpha.G>1)),
  c(sum(fulld$Alpha.L<1),sum(fulld$Alpha.L==1),sum(fulld$Alpha.L>1)),
  c(sum(fulld$Gamma.G<1),sum(fulld$Gamma.G==1),sum(fulld$Gamma.G>1)),
  c(sum(fulld$Gamma.L<1),sum(fulld$Gamma.L==1),sum(fulld$Gamma.L>1)),
  c(sum(fulld$BetaG<1,na.rm=T),sum(fulld$BetaG==1,na.rm=T),sum(fulld$BetaG>1,na.rm=T)),
  c(sum(fulld$BetaL<1),sum(fulld$BetaL==1),sum(fulld$BetaL>1)),
  c(sum(fulld$RG<0),sum(fulld$RG==0),sum(fulld$RG>0)),
  c(sum(fulld$RL<0),sum(fulld$RL==0),sum(fulld$RL>0))
)
print.xtable(xtable(T2n),type="html",file='Results/T2n.html')

T2p=round(rbind(
  c(sum(fulld$LAKW<1),sum(fulld$LAKW==1),sum(fulld$LAKW>1)),
  c(sum(fulld$Alpha.G<1),sum(fulld$Alpha.G==1),sum(fulld$Alpha.G>1)),
  c(sum(fulld$Alpha.L<1),sum(fulld$Alpha.L==1),sum(fulld$Alpha.L>1)),
  c(sum(fulld$Gamma.G<1),sum(fulld$Gamma.G==1),sum(fulld$Gamma.G>1)),
  c(sum(fulld$Gamma.L<1),sum(fulld$Gamma.L==1),sum(fulld$Gamma.L>1)),
  c(sum(fulld$BetaG<1,na.rm=T),sum(fulld$BetaG==1,na.rm=T),sum(fulld$BetaG>1,na.rm=T)),
  c(sum(fulld$BetaL<1),sum(fulld$BetaL==1),sum(fulld$BetaL>1)),
  c(sum(fulld$RG<0),sum(fulld$RG==0),sum(fulld$RG>0)),
  c(sum(fulld$RL<0),sum(fulld$RL==0),sum(fulld$RL>0))
)/182*100,0)
print.xtable(xtable(T2p),type="html",file='Results/T2p.html')

##################################
# Chi-squared reported in Table 2 
##################################

chisq.test(c(22,11,149)) #LA
chisq.test(c(114,7,61)) #UC-G
chisq.test(c(104,7,71)) #UC-L
chisq.test(c(123,4,55)) #PBW - G
chisq.test(c(88,4,90)) #PBW-L
chisq.test(c(135,5,41)) #Beta -G
chisq.test(c(101,17,64)) #Beta -L
chisq.test(c(21,6,155)) #Disc- G
chisq.test(c(66,30,86)) #Disc- L


##############################
# Table 3 Descriptives
###############################

#note that after identifying one double-counted participants, means and standard deviations are slightly different
#than reported in the table in the manuscript. I only identified this when  developing this repository a year later
# Given that it only involves some secondary demographics I decided it was not worth it to publish an erratum.

t3dat=select(fulld,Age,Cigarettes,BMI,Alcohol,Exercise,TSCs,CRTs,PQNs,PQEs,PQPs,PQLs)
StatDes2<-function(X){
  Tab=matrix(NA,dim(X)[2],3)
  Tab[,1]=apply(X,2,function(x) sum(!is.na(x)))
  Tab[,2]=apply(X,2,mean,na.rm=T)
  Tab[,3]=apply(X,2,sd,na.rm=T)
  colnames(Tab)=c("n","mean","sd")
  return(Tab)
}
T3=StatDes2(t3dat)
row.names(T3)=colnames(t3dat)
print.xtable(xtable(T3),type='html',file='Results/t3.html')

###########################################################################
#Association between selected tailored incentives and economic preferences#
###########################################################################
cordat=select(fulld,Prec,Weekly,Mode,P,LAKW,AUC.G,AUC.L,Alpha.G,Alpha.L,Gamma.G,Gamma.L,RG,BetaG,RL,BetaL,Age,Cigarettes,BMI,Alcohol,Exercise,TSCs,CRTs,PQNs,PQEs,PQPs,PQLs,MDTH1,MDTH2,MDTH3A,MDTH3B,MDTH3C,MDTH3D,MDTH3E,MDTH3F,MDTH3G,MDTI4,MDTI5,MDTI6A,MDTI6B,MDTI6C,MDTI6D,MDTI6E,MDTI6F,MDTI6G)

#The code below, for each of the 4 dimensions, performs a t-test or correlation analysis that test whether
#the economic preferences measured differed between those that selected these incentive dimensions
# if it is significant, the rest result is printed. For correlation analysis, also the highest correlation is indexed.

#PRECOMMIT - t.test
prec.t.pval=c()
#Economic preferences
for (i in 1:length(5:26)){
  prec.t.pval[i]=t.test(cordat[,4+i]~cordat$Prec)$p.value
  if(prec.t.pval[i]<0.05){
    print(colnames(cordat)[4+i])
    print(t.test(cordat[,4+i]~cordat$Prec))
  }
}
min(prec.t.pval)

#Weekly - t.test
weekly.t.pval=c()
#Economic preferences
for (i in 1:length(5:26)){
  weekly.t.pval[i]=t.test(cordat[,4+i]~cordat$Weekly)$p.value
  if(weekly.t.pval[i]<0.05){
    print(colnames(cordat)[4+i])
    print(t.test(cordat[,4+i]~cordat$Weekly))
  }
}
min(weekly.t.pval)


#Structure - cor.test
structure.rval=c()
structure.t.pval=c()

for (i in 1:length(5:26)){
  structure.t.pval[i]=cor.test(cordat[,4+i],cordat$Mode)$p.value
  structure.rval[i]=cor.test(cordat[,4+i],cordat$Mode)$estimate
  if(structure.t.pval[i]<0.05){
    print(colnames(cordat)[4+i])
    print(cor.test(cordat[,4+i],cordat$Mode,type="Spearma"))
  }
}
min(structure.t.pval[structure.t.pval>0.05])
max(structure.rval[structure.t.pval>0.05])
#Risk - cor.test
risk.rval=c()
risk.t.pval=c()

for (i in 1:length(5:26)){
  risk.t.pval[i]=cor.test(cordat[,4+i],cordat$P)$p.value
  risk.rval[i]=cor.test(cordat[,4+i],cordat$P)$estimate
  if(risk.t.pval[i]<0.05){
    print(colnames(cordat)[4+i])
    print(cor.test(cordat[,4+i],cordat$P,type="Spearman"))
  }
}
min(risk.t.pval);max(risk.rval)


########################################
# Patterns + simple comparisons
########################################
cordat$combined=apply(cordat[ , 1:4 ] , 1 , paste , collapse = "-" )
cordat$Comb1=cordat$combined=="2-1-3-100"
cordat$Comb2=cordat$combined=="2-2-3-100" 
cordat$Comb3=cordat$combined=="2-2-2-100"
cordat$CombT=cordat$combined=="2-2-2-100"|cordat$combined=="2-1-3-100"|cordat$combined=="2-2-3-100" 

#similar strategy as above to report if differences existed depending on selected patterns

comb1.t.pval=c()
#Most popular
for (i in 1:length(5:26)){
  comb1.t.pval[i]=t.test(cordat[,4+i]~cordat$Comb1)$p.value
  if(comb1.t.pval[i]<0.05){
    print(colnames(cordat)[4+i])
    print(t.test(cordat[,4+i]~cordat$Comb1))
  }
}
min(comb1.t.pval)

comb2.t.pval=c()
#Most popular
for (i in 1:length(5:26)){
  comb2.t.pval[i]=t.test(cordat[,4+i]~cordat$Comb2)$p.value
  if(comb2.t.pval[i]<0.05){
    print(colnames(cordat)[4+i])
    print(t.test(cordat[,4+i]~cordat$Comb2))
  }
}
min(comb2.t.pval[comb2.t.pval>0.05])

comb3.t.pval=c()
#Most popular
for (i in 1:length(5:26)){
  comb3.t.pval[i]=t.test(cordat[,4+i]~cordat$Comb3)$p.value
  if(comb3.t.pval[i]<0.05){
    print(colnames(cordat)[4+i])
    print(t.test(cordat[,4+i]~cordat$Comb3))
  }
}
min(comb3.t.pval[comb3.t.pval>0.05])

combt.t.pval=c()
#Most popular
for (i in 1:length(5:26)){
  combt.t.pval[i]=t.test(cordat[,4+i]~cordat$CombT)$p.value
  if(combt.t.pval[i]<0.05){
    print(colnames(cordat)[4+i])
    print(t.test(cordat[,4+i]~cordat$CombT))
  }
}


##################################
# Appendix models - Precommit
##################################
#Note
#The code below reprints all the models summarized in the Online Supplements
#Given the large amount of models involved I have not prepared a script to produce the 3-page table.

library(rsq)

fulld$Prec=fulld$Prec-1

#Economic preferences - no interactions or control
m1=glm(Prec~LAKW,data=fulld, family=binomial(link = "logit"));summary(m1)
a=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~Alpha.L,data=fulld, family=binomial(link = "logit"));summary(m1)
b=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~Gamma.L,data=fulld, family=binomial(link = "logit"));summary(m1)
c=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~BetaL,data=fulld, family=binomial(link = "logit"));summary(m1)
d=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~RL,data=fulld, family=binomial(link = "logit"));summary(m1)
e=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~LAKW+Alpha.L+Gamma.L+BetaL+RL,data=fulld, family=binomial(link = "logit"));summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)



#Economic preferences, including control
m1=glm(Prec~LAKW+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
a=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~Alpha.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
b=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~Gamma.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
c=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~BetaL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
d=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~RL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
e=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~LAKW+Alpha.L+Gamma.L+BetaL+RL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)

#Economic preferences, including psychological measures

m1=glm(Prec~LAKW+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
a=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~Alpha.L+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
b=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~Gamma.L+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
c=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~BetaL+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
d=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~RL+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
e=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~LAKW+Alpha.L+Gamma.L+BetaL+RL+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)

#Economic preferences, including psychological measures and demographics
m1=glm(Prec~LAKW+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
a=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~Alpha.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
b=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~Gamma.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
c=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~BetaL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
d=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~RL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
e=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Prec~LAKW+Alpha.L+Gamma.L+BetaL+RL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)

#Demographics
m1=glm(Prec~BMI,data=fulld, family=binomial(link = "logit"));summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Prec~Age,data=fulld, family=binomial(link = "logit"));summary(m1)
b=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Prec~Gender,data=fulld, family=binomial(link = "logit"));summary(m1)
c=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Prec~Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
d=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

#Psychological measures
m1=glm(Prec~CRTs,data=fulld, family=binomial(link = "logit"));summary(m1)
e=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Prec~TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
f=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Prec~PQNs+PQEs+PQLs+PQPs,data=fulld, family=binomial(link = "logit"));summary(m1)
g=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Prec~BMI+Age+Gender+Exercise+Cigarettes+Alcohol+PQNs+PQEs+PQLs+PQPs,data=fulld, family=binomial(link = "logit"));summary(m1)
h=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f,g,h)

##################################
# Appendix models - Weekly
##################################

fulld$Weekly=fulld$Weekly-1



#Economic preferences - no interactions or control
m1=glm(Weekly~LAKW,data=fulld, family=binomial(link = "logit"));summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~Alpha.G,data=fulld, family=binomial(link = "logit"));summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~Alpha.L,data=fulld, family=binomial(link = "logit"));summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~BetaG,data=fulld, family=binomial(link = "logit"));summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~BetaL,data=fulld, family=binomial(link = "logit"));summary(m1)
e=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~LAKW+Alpha.G+Alpha.L+BetaG+BetaL,data=fulld, family=binomial(link = "logit"));summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))
        
rbind(a,b,c,d,e,f)



#Economic preferences, including control
m1=glm(Weekly~LAKW+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
a=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~Alpha.G+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
b=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~Alpha.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
c=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~BetaG+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
d=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~BetaL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
e=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~LAKW+Alpha.G+Alpha.L+BetaG+BetaL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)

#Economic preferences, including psychological measures

m1=glm(Weekly~LAKW+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
a=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~Alpha.G+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
b=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~Alpha.L+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
c=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~BetaG+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
d=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~BetaL+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
e=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~LAKW+Alpha.G+Alpha.L+BetaG+BetaL+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)

#Economic preferences, including psychological measures and demographics
m1=glm(Weekly~LAKW+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
a=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~Alpha.G+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
b=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~Alpha.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
c=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~BetaG+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
d=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~BetaL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
e=cbind(rsq(m1),AIC(m1),BIC(m1))

m1=glm(Weekly~LAKW+Alpha.G+Alpha.L+BetaG+BetaL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)

#Demographics
m1=glm(Weekly~BMI,data=fulld, family=binomial(link = "logit"));summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~Age,data=fulld, family=binomial(link = "logit"));summary(m1)
b=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~Gender,data=fulld, family=binomial(link = "logit"));summary(m1)
c=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~Exercise+Cigarettes+Alcohol,data=fulld, family=binomial(link = "logit"));summary(m1)
d=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

#Psychological measures
m1=glm(Weekly~CRTs,data=fulld, family=binomial(link = "logit"));summary(m1)
e=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~TSCs,data=fulld, family=binomial(link = "logit"));summary(m1)
f=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~PQNs+PQEs+PQLs+PQPs,data=fulld, family=binomial(link = "logit"));summary(m1)
g=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=glm(Weekly~BMI+Age+Gender+Exercise+Cigarettes+Alcohol+PQNs+PQEs+PQLs+PQPs,data=fulld, family=binomial(link = "logit"));summary(m1)
h=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f,g,h)

######################
# Sequence models
#######################

#Economic preferences - no interactions or control
m1=lm(Mode~LAKW,data=fulld);summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Alpha.G,data=fulld);summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Alpha.L,data=fulld);summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~RG,data=fulld);summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~RL,data=fulld);summary(m1)
e=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~LAKW+Alpha.G+Alpha.L+RG+RL,data=fulld);summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)

#Economic preferences - control demographics
m1=lm(Mode~LAKW+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld);summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Alpha.G+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld);summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Alpha.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld);summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~RG+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld);summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~RL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld);summary(m1)
e=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~LAKW+Alpha.G+Alpha.L+RG+RL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld);summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)

#Economic preferences - control psychological measures
m1=lm(Mode~LAKW+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Alpha.G+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Alpha.L+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~RG+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~RL+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
e=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~LAKW+Alpha.G+Alpha.L+RG+RL+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)

#Economic preferences - control demographics + Psychological measures
m1=lm(Mode~LAKW+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Alpha.G+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Alpha.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~RG+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~RL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
e=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~LAKW+Alpha.G+Alpha.L+RG+RL+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
f=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f)

#Demographics
m1=lm(Mode~BMI, data=fulld);summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Age, data=fulld);summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Gender, data=fulld);summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~Exercise+Alcohol+Cigarettes, data=fulld);summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

#Psychological measures
m1=lm(Mode~CRTs,data=fulld)
e=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~TSCs,data=fulld);summary(m1)
f=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~PQNs+PQEs+PQLs+PQPs,data=fulld);summary(m1)
g=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(Mode~BMI+Age+Gender+Exercise+Cigarettes+Alcohol+PQNs+PQEs+PQLs+PQPs,data=fulld);summary(m1)
h=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f,g,h)

######################
# Risk mode ls
#######################

#Economic preferences - no interactions or control
m1=lm(P~LAKW,data=fulld);summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Gamma.G,data=fulld);summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Gamma.L,data=fulld);summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~LAKW+Gamma.G+Gamma.L,data=fulld);summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d)

#Economic preferences - control demographics
m1=lm(P~LAKW+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld);summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Gamma.G+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld);summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Gamma.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld);summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~LAKW+Gamma.G+Gamma.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol,data=fulld);summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d)

#Economic preferences - control psychological measures
m1=lm(P~LAKW+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Gamma.G+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Gamma.L+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~LAKW+Gamma.G+Gamma.L+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d)

#Economic preferences - control demographics + Psychological measures
m1=lm(P~LAKW+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Gamma.G+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Gamma.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~LAKW+Gamma.G+Gamma.L+BMI+Age+Gender+Exercise+Cigarettes+Alcohol+CRTs+PQNs+PQEs+PQLs+PQPs+TSCs,data=fulld);summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d)

#Demographics
m1=lm(P~BMI, data=fulld);summary(m1)
a=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Age, data=fulld);summary(m1)
b=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Gender, data=fulld);summary(m1)
c=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~Exercise+Alcohol+Cigarettes, data=fulld);summary(m1)
d=cbind(round(rsq(m1),3),round(AIC(m1),2),round(BIC(m1),2))

#Psychological measures
m1=lm(P~CRTs,data=fulld)
e=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~TSCs,data=fulld);summary(m1)
f=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~PQNs+PQEs+PQLs+PQPs,data=fulld);summary(m1)
g=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

m1=lm(P~BMI+Age+Gender+Exercise+Cigarettes+Alcohol+PQNs+PQEs+PQLs+PQPs,data=fulld);summary(m1)
h=cbind(round(rsq(m1),2),round(AIC(m1),2),round(BIC(m1),2))

rbind(a,b,c,d,e,f,g,h)


######################
# Best models
#######################

#timing - PQN, CRT, BMI, PQP
mbest=glm(Weekly~BetaL+PQNs+PQPs+CRTs+BMI,data=fulld,family=binomial(link = "logit"))
summary(mbest)
#Mode - PQP, Age, Exercise
mbest2=lm(Mode~PQPs+Age+Exercise,data=fulld)
summary(mbest2)
#Risk - Exercise
