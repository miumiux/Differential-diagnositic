################################################################################

#This is a program contains all ploting commands for Differential diag#

###############################################################################


#######Scatterplot matrix for FSCORE###############
super.sym <- trellis.par.get("superpose.symbol")
splom(~wit_fscore[,c("loss","automitisms","Drama_motor","pale")],groups=diag,data=wit_fscore,
      panel = panel.superpose,
      key=list(title="witness,fscore mattrix plot with different diagnosis,N=249",
               columns=3,
               points=list(pch=super.sym$pch[1:3],
                           col=super.sym$col[1:3]),
               text=list(c("Epilepsy","PNES","Syncope"))))

#######Panel fscore boxplot#######
bwplot(value~diag|variable,data=fscore_long, horizontal=F,col=c(4,2,3),main="plot 1.1, fscore by diagnosis--Complete cases, N=249")

boxplot(value~diag*variable,data=fscore_long,col=rep(c(4,2,3),3),notch=T,
        main="plot 1.2, fscore by diagnosis--Complete cases, N=249",
        names=paste(rep(c("Epilepsy","PNES","Syncope"),3),
                    rep(c("loss","automitisms","Drama_motor","pale"),3)))

  temp <- legend("bottomright", legend = c(" ", " "," "),
                 text.width = strwidth("Diagnosis"),
                 col=c(4,2,3), pch=19,xjust = 1, yjust = 1,
                 title = "Diagnosis")
  text(temp$rect$left + temp$rect$w, temp$text$y,
       c("Epilepsy", "PNES","Syncope"), pos=2) 
  



##############Below is the look at raw summation#############
###########Could write a function

#Mean score without NAs####
pairs(~loss_l+automitisms_l+Drama_motor_l+pale_l,data=score_mean,col=as.factor(dat$Diagnosis),
      main="Mean score without NAs, N=249")


boxplot(value~dat.Diagnosis*variable,data=mean_long,col=rep(c(4,2,3),3),notch=T,
        main="mean raw score by diagnosis,N=249")

  temp <- legend("bottomright", legend = c(" ", " "," "),
               text.width = strwidth("Diagnosis"),
               col=c(4,2,3), pch=19,xjust = 1, yjust = 1,
               title = "Diagnosis")
  text(temp$rect$left + temp$rect$w, temp$text$y,
       c("Epilepsy", "PNES","Syncope"), pos=2)

##################################################################

#Mean score for complete dataset, N=146#

pairs(~loss_l+automitisms_l+Drama_motor_l+pale_l,data=score_cmean,col=as.factor(dat1$Diagnosis),
      main="Mean score for complete cases. N=146")

bwplot(value~dat1.Diagnosis|variable,data=cmean_long, horizontal=F)

boxplot(value~dat1.Diagnosis*variable,data=cmean_long,col=rep(c(4,2,3),3),notch=T,
        main="mean raw score by diagnosis--Complete cases, N=146")
  temp <- legend("bottomright", legend = c(" ", " "," "),
                 text.width = strwidth("Diagnosis"),
                 col=c(4,2,3), pch=19,xjust = 1, yjust = 1,
                 title = "Diagnosis")
  text(temp$rect$left + temp$rect$w, temp$text$y,
       c("Epilepsy", "PNES","Syncope"), pos=2)
