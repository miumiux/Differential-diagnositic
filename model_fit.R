#############################################################################

##This file contains model building and illustration from fscore/Raw score: Logistics regression / LDA

#############################################################################

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
require(MASS)
library(klaR)
require(stats)
setwd("Z:/Yi/Differential_Diagnostic/data")

wit_fscore$diag<-relevel(wit_fscore$diag,ref="E")
#multinomial logistic regression model.
test<-multinom(diag~loss+automitisms+Drama_motor+pale+Drama_motor*pale+Drama_motor*automitisms,data=wit_fscore)
summary(test)
exp(coef(test))
pp<-data.frame(fitted(test))
colnames(pp)<-c("E","N","S")

aa<-names(pp)[apply(pp,1,which.max)]
ct1<-table(aa,wit_fscore$diag)
diag(prop.table(ct1, 2))
# total percent correct
sum(diag(prop.table(ct1)))


######################################################
#Participant##
######################################################
pat_fscore$diag<-relevel(pat_fscore$diag,ref="E")
#multinomial logistic regression model.
test<-multinom(diag~knowSth+outside_body+lighthead+Pain+Amenestia+lighthead*Pain+lighthead*Amenestia,data=pat_fscore)
summary(test)
exp(coef(test))
pp<-data.frame(fitted(test))
colnames(pp)<-c("E","N","S")

aa<-names(pp)[apply(pp,1,which.max)]
ct1<-table(aa,pat_fscore$diag)

diag(prop.table(ct1, 2))
# total percent correct
sum(diag(prop.table(ct1)))

?prop.table

##LDA
fit <- lda(diag~knowSth+outside_body+lighthead+Pain+Amenestia+outside_body*lighthead+Amenestia*lighthead,data=pat_fscore, 
           na.action="na.omit", CV=TRUE)
summary(fit)
# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(fit$class,pat_fscore$diag)
diag(prop.table(ct, 2))
# total percent correct
sum(diag(prop.table(ct)))
la<-pat_fscore$lighthead*pat_fscore$Amenestia
lp<-pat_fscore$lighthead*pat_fscore$Pain
partimat(diag ~ knowSth+outside_body+lighthead+Pain+Amenestia,data=pat_fscore,method="lda")

super.sym <- trellis.par.get("superpose.symbol")
splom(~pat_fscore[,c("knowSth","outside_body","lighthead","Pain","Amenestia")],groups=diag,data=pat_fscore,
      panel = panel.superpose,
      key=list(title="Patient fscore mattrix plot with different diagnosis",
               columns=3,
               points=list(pch=super.sym$pch[1:3],
                           col=super.sym$col[1:3]),
               text=list(c("Epilepsy","PNES","Syncope"))))


pairs(~ knowSth+outside_body+lighthead+Pain+Amenestia,col=as.factor(pat_fscore$diag),data=pat_fscore)

score_mean$dat.Diagnosis<-relevel(score_mean$dat.Diagnosis,ref="E")
test_sum<-multinom(dat.Diagnosis~loss_l+automitisms_l+Drama_motor_l+pale_l,data=score_mean)
summary(test_sum)
exp(coef(test_sum))
pp_sum<-data.frame(fitted(test_sum))

colnames(pp)<-c("E","N","S")
str(test_sum)
aa<-names(pp_sum)[apply(pp,1,which.max)]
ct_sum<-table(aa,score_mean$dat.Diagnosis)
diag(prop.table(ct_sum, 1))
# total percent correct
sum(diag(prop.table(ct_sum)))
summary(test_sum)
exp(coef(test_sum))


####Build prediction data####
automitisms=rep(mean(wit_fscore$automitisms),nrow(wit_fscore))
Drama_motor=rep(mean(wit_fscore$Drama_motor),nrow(wit_fscore))
pale=rep(mean(wit_fscore$pale),nrow(wit_fscore))
loss=rep(mean(wit_fscore$loss),nrow(wit_fscore))
dloss=seq(from=min(wit_fscore$loss),to=max(wit_fscore$loss),by=(max(wit_fscore$loss)-min(wit_fscore$loss))/(nrow(wit_fscore)-1))
dautomitisms=seq(from=min(wit_fscore$automitisms),to=max(wit_fscore$automitisms),by=(max(wit_fscore$automitisms)-min(wit_fscore$automitisms))/(nrow(wit_fscore)-1))
dmotor=seq(from=min(wit_fscore$Drama_motor),to=max(wit_fscore$Drama_motor),by=(max(wit_fscore$Drama_motor)-min(wit_fscore$Drama_motor))/(nrow(wit_fscore)-1))
dpale=seq(from=min(wit_fscore$pale),to=max(wit_fscore$pale),by=(max(wit_fscore$pale)-min(wit_fscore$pale))/(nrow(wit_fscore)-1))

d.loss<-data.frame(dloss,automitisms,Drama_motor,pale)
names(d.loss) <- sub("dloss", "loss", names(d.loss))
d.automitisms<-data.frame(dautomitisms,loss,Drama_motor,pale)
names(d.automitisms) <- sub("dautomitisms", "automitisms", names(d.automitisms))
d.Drama_motor<-data.frame(dmotor,loss,automitisms,pale)
names(d.Drama_motor) <- sub("dmotor", "Drama_motor", names(d.Drama_motor))
d.pale<-data.frame(dpale,automitisms,Drama_motor,loss)
names(d.pale) <- sub("dpale", "pale", names(d.pale))

pp.loss<-data.frame(dloss,predict(test,newdata=d.loss,type="probs",se=T))
pp.automitisms<-data.frame(dautomitisms,predict(test,newdata=d.automitisms,type="probs",se=T))
pp.motor<-data.frame(dmotor,predict(test,newdata=d.Drama_motor,type="probs",se=T))
pp.pale<-data.frame(dpale,predict(test,newdata=d.pale,type="probs",se=T))

#########################################################
## melt data set to long for ggplot2
pp.loss_long <- melt(pp.loss, id.vars = "dloss", value.name = "probability")
pp.automitisms_long <- melt(pp.automitisms, id.vars = "dautomitisms", value.name = "probability")
pp.motor_long <- melt(pp.motor, id.vars = "dmotor", value.name = "probability")
pp.pale_long <- melt(pp.pale, id.vars = "dpale", value.name = "probability")


p1<-ggplot(pp.loss_long, aes(x = dloss, y = probability, colour = variable)) + geom_line()+
  ggtitle("Predicted probablity:Loss")

p2<-ggplot(pp.automitisms_long, aes(x = dautomitisms, y = probability, colour = variable)) + geom_line() +
  ggtitle("Predicted probablity:Automitisms")

p3<-ggplot(pp.motor_long, aes(x = dmotor, y = probability, colour = variable)) + geom_line() +
  ggtitle("Predicted probablity:Dramatic_motor")

p4<-ggplot(pp.pale_long, aes(x = dpale, y = probability, colour = variable)) + geom_line() +
  ggtitle("Predicted probablity:Pale")

#This is an useful function
source("Z:/Yi/Differential_Diagnostic/R program/multiplot_ggplot.R")
multiplot(p1, p2, p3, p4, cols=2)

######################################################################################################

##LDA
fit <- lda(diag ~ loss+automitisms+Drama_motor+pale+automitisms*Drama_motor+Drama_motor+pale, data=wit_fscore, 
           na.action="na.omit", CV=TRUE)
summary(fit)
# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table( fit$class,wit_fscore$diag)
diag(prop.table(ct, 2))
# total percent correct
sum(diag(prop.table(ct)))

##Pairwised partition C(2 4)=6
partimat(diag ~ loss+automitisms+Drama_motor+pale,data=wit_fscore,method="lda")

#Check assumptions for LDA
d1<-ggplot(wit_fscore, aes(x = loss)) + geom_histogram(aes(y = ..density..),col=3) + geom_density()+  ggtitle("Distribution of factor:Loss")
d2<-ggplot(wit_fscore, aes(x = automitisms)) + geom_histogram(aes(y = ..density..),col=3) + geom_density()+  ggtitle("Distribution of factor:Automitisms")
d3<-ggplot(wit_fscore, aes(x = Drama_motor)) + geom_histogram(aes(y = ..density..),col=3) + geom_density()+  ggtitle("Distribution of factor:Drama_motor")
d4<-ggplot(wit_fscore, aes(x = pale)) + geom_histogram(aes(y = ..density..),col=3) + geom_density()+  ggtitle("Distribution of factor:pale")
multiplot(d1, d2, d3, d4, cols=2)

