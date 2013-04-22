################################################################################

#This is the data set up and discriptive program for Differential diag#

###############################################################################

setwd("Z:/Yi/Differential_Diagnostic/data")
library(lattice)
library(reshape2)

wit_fscore <- read.csv("Z:/Yi/Differential_Diagnostic/report/witness_CFA.csv", header=F)
#This is the dataset contains diagnosis raw score##
pow_comp<-read.csv("pow_comp.csv")
wit_fscore<-cbind(pow_comp$Diagnosis,wit_fscore)
colnames(wit_fscore)<-c("diag",paste("wit",seq(1:27)),"loss", "automitisms","Drama_motor","pale")

##Sort by diag##
wit_fscore<-wit_fscore[order(wit_fscore$diag),]

#Descriptive as mean and SD
des<-function(var=loss)
{
  with(wit_fscore, do.call(rbind, tapply(var, diag, function(x) c(M = mean(x), SD = sd(x)))))
}

des(wit_fscore$loss)
des(wit_fscore$automitisms)
des(wit_fscore$Drama_motor)
des(wit_fscore$pale)

############################################################
setwd("Z:/Yi/Differential_Diagnostic/data")


pat_fscore <- read.csv("Z:/Yi/Differential_Diagnostic/report/participant_CFA.csv", header=F)
#This is the dataset contains diagnosis raw score##
# pow_comp<-read.csv("pow_comp.csv")
# wit_fscore<-cbind(pow_comp$Diagnosis,wit_fscore)
colnames(pat_fscore)<-c("diag",paste("part",seq(1:74)),"knowSth","outside_body","lighthead","Pain","Amenestia")

##Sort by diag##
pat_fscore<-pat_fscore[order(pat_fscore$diag),]

#Descriptive as mean and SD
des<-function(var=loss)
{
  with(wit_fscore, do.call(rbind, tapply(var, diag, function(x) c(M = mean(x), SD = sd(x)))))
}

des(wit_fscore$loss)
des(wit_fscore$automitisms)
des(wit_fscore$Drama_motor)
des(wit_fscore$pale)
##Package reshape2##
fscore_long<-melt(wit_fscore[,c("diag","loss","automitisms","Drama_motor","pale")])

summary(aov(loss~diag,data=wit_fscore))
summary(aov(automitisms~diag,data=wit_fscore))
summary(aov(Drama_motor~diag,data=wit_fscore))
summary(aov(pale~diag,data=wit_fscore))

#####################################################################################################

#Use another data_set to calculate sum,pow_comp#
#Set up sign of loadings
loss_l<-rep(0,30)
loss_l[c(4,5,6,7)]<-c(1,1,1,1)

automitisms_l<-rep(0,30)
automitisms_l[c(3,5,8,9,11,19,24)]<-c(1,-1,1,1,1,-1,-1)

Drama_motor_l<-rep(0,30)
Drama_motor_l[c(10,12,13,14,15,16,17,18,22,25,26)]<-rep(1,11)

pale_l<-rep(0,30)
pale_l[c(20,23,27,28,29,30)]<-c(1,1,-1,1,1,-1)

#Dataframe that composes  loading signs#
factor_sign<-cbind(loss_l,automitisms_l,Drama_motor_l,pale_l)
factor_list<-as.list(as.data.frame(factor_sign))


dat<-subset(pow_comp,select=c(Diagnosis,w1:w30))
#Calculate the mean after take out NA.
score_mean<-sapply(factor_list,function(z){colMeans(subset(t(dat[,2:31])*z,z!=0),na.rm=T)})
# colMeans(
#   subset(t(dat[,2:31])*factor_list[[2]],factor_list[[2]]!=0)
#   ,na.rm=T)
score_mean<-data.frame(dat$Diagnosis,score_mean)
#This works just so amazing......
mean_long<-melt(score_mean)


#Calculate mean for complete(no NA in response)
dat1<-dat[complete.cases(dat[,2:31]),]
score_cmean<-sapply(factor_list,function(z){colMeans(subset(t(dat1[,2:31])*z,z!=0),na.rm=T)})
score_cmean<-data.frame(dat1$Diagnosis,score_cmean)
cmean_long<-melt(score_cmean)


