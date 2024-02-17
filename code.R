library(readxl)
library(tidyverse)
library(lubridate)
library(moonBook)
library(survival)
library(survminer)
library(xlsx)

Cx0 <- read_excel("Cx0.xlsx", sheet = "Cx0") %>% select(-`...1`)


#### 
Cx0 <-  Cx0 %>% mutate( scc_c1=log(scc1/scc0), scc_c2=log(scc2/scc0),
                                 scc_v1=scc_c1/diff1*100, scc_v2=scc_c2/diff2*100,
                                 scc_s1=(scc1-scc0)/scc0, scc_s2=(scc2-scc0)/scc0,
                                 scc_sv1=(scc1-scc0)/diff1, scc_sv2=(scc2-scc0)/diff2,
                                 cyfra_c1=log(cyfra1/cyfra0), cyfra_c2=log(cyfra2/cyfra0),
                                 cyfra_v1=cyfra_c1/diff1*100, cyfra_v2=cyfra_c2/diff2*100,
                                 cyfra_s1=(cyfra1-cyfra0)/cyfra0, cyfra_s2=(cyfra2-cyfra0)/cyfra0,
                                 cyfra_sv1=(cyfra1-cyfra0)/diff1, cyfra_sv2=(cyfra2-cyfra0)/diff2)

Cx0 <- Cx0 %>% mutate(scc0_m = ifelse(scc0 >= median(scc0), 1, 0), scc1_m = ifelse(scc1 >= median(scc1), 1, 0), scc2_m = ifelse(scc2 >= median(scc2), 1, 0), 
                      scc_v1_m = ifelse(scc_v1 >= median(scc_v1), 1, 0), scc_v2_m = ifelse(scc_v2 >= median(scc_v2), 1, 0),
                      cyfra0_m = ifelse(cyfra0 >= median(cyfra0), 1, 0), cyfra1_m = ifelse(cyfra1 >= median(cyfra1), 1, 0), cyfra2_m = ifelse(cyfra2 >= median(cyfra2), 1, 0), 
                      cyfra_v1_m = ifelse(cyfra_v1 >= median(cyfra_v1), 1, 0), cyfra_v2_m = ifelse(cyfra_v2 >= median(cyfra_v2), 1, 0),
                      Age_m=ifelse(Age >=65, 1, 0), Hb0_m = ifelse(Hb0 < median(Hb0), 1, 0), NLR_m = ifelse(NLR >= median(NLR), 1, 0), PLT_m = ifelse(PLT >= median(PLT), 1, 0),
                      duration_m= ifelse(duration >= 56, 1, 0),
                      EQD2_m=ifelse(EQD2 < 70, 1, 0),
                      recur_m = ifelse(recur1>0,1,0),
                      recur2 = ifelse(recur1==1, 1,0),
                      recur3 = ifelse(recur1==2|recur1==3, 1,0))


####
library(multipleROC)
library(pROC)

Cx_r <- Cx0 %>% select(css, scc0, scc1, scc2, scc_v1, scc_v2, scc_sv1, scc_sv2, scc_s1, scc_s2, cyfra0, cyfra1, cyfra2, cyfra_v1, cyfra_v2, cyfra_sv1, cyfra_sv2, cyfra_s1, cyfra_s2) %>% 
    mutate(survival=css, surv=ifelse(css==1,"death","survival"), pre_scc=scc0, log_change_mid_scc=scc_v1, log_change_post_scc= scc_v2,
           pre_cyfra=cyfra0, log_change_mid_cyfra=cyfra_v1, log_change_post_cyfra= cyfra_v2)

#### scc, log, time 
z0 = multipleROC(survival~pre_scc, data=Cx_r)
zz0= multipleROC2roc(z0)
z1 = multipleROC(form=survival~pre_scc+log_change_mid_scc, data=Cx_r)
zz1= multipleROC2roc(z1)
z2 = multipleROC(form=survival~pre_scc+log_change_post_scc, data=Cx_r)
zz2= multipleROC2roc(z2)
plot_ROC(list(z0,z1))
plot_ROC(list(z0,z2))

plot_ROC(list(z0,z1,z2))

plot.roc(smooth(zz0), col="black", lty=2)
par(new=T)
plot.roc(smooth(zz1), col="blue", lty=1)
par(new=T)
plot.roc(smooth(zz2), col="red", lty=1)
text(0.5, 0.35, labels=expression(paste("pre SCC-Ag"+frac(Delta*log("SCC-Ag"),Delta*time)(post))), col="red", cex=1)
text(0.3, 0.27, labels="AUC:0.708 (0.581-0.836), p<0.001", col="red", cex=1, font=2)
text(0.5, 0.19, labels=expression(paste("pre SCC-Ag"+frac(Delta*log("SCC-Ag"),Delta*time)(mid))), col="blue", cex=1)
text(0.3, 0.12, labels="AUC:0.663 (0.529-0.797), p<0.001", col="blue", cex=1)
text(0.5, 0.07, labels="pre SCC-Ag", col="black", cex=1)
text(0.3, 0.02, labels="AUC:0.666 (0.528-0.804), p<0.001", col="black", cex=1)


## cyfra, scc
z0 = multipleROC(survival~pre_scc, data=Cx_r)
zz0= multipleROC2roc(z0)
z1 = multipleROC(survival~pre_cyfra, data=Cx_r)
zz1= multipleROC2roc(z1)
z2 =multipleROC(form=survival~pre_scc+pre_cyfra, data=Cx_r)
zz2= multipleROC2roc(z2)

plot_ROC(list(z1,z2))
plot_ROC(list(z0,z2))

plot_ROC(list(z0,z1,z2))
plot.roc(smooth(zz0), col="black", lty=1)
par(new=T)
plot.roc(smooth(zz1), col="blue", lty=1)
par(new=T)
plot.roc(smooth(zz2), col="red", lty=1)
text(0.5, 0.35, labels="pre SCC-Ag + pre Cyfra", col="red")
text(0.3, 0.3, labels="AUC:0.695 (0.562-0.827), p<0.001", col="red", cex=1)
text(0.5, 0.25, labels="pre Cyfra", col="blue")
text(0.3, 0.2, labels="AUC:0.642 (0.511-0.773), p<0.001", col="blue", cex=1)
text(0.5, 0.15, labels="pre SCC-Ag", col="black")
text(0.3, 0.1, labels="AUC:0.666 (0.528-0.804), p<0.001", col="black", cex=1)

##### cyfra, log, time
z0 = multipleROC(survival~pre_cyfra, data=Cx_r)
zz0= multipleROC2roc(z0)
z1 = multipleROC(form=survival~pre_cyfra+log_change_mid_cyfra, data=Cx_r)
zz1= multipleROC2roc(z1)
z2 = multipleROC(form=survival~pre_cyfra+log_change_post_cyfra, data=Cx_r)
zz2= multipleROC2roc(z2)
plot_ROC(list(z0,z1))
plot_ROC(list(z0,z2))

plot_ROC(list(z0,z1,z2))
plot.roc(smooth(zz0), col="black", lty=2)
par(new=T)
plot.roc(smooth(zz1), col="blue", lty=1)
par(new=T)
plot.roc(smooth(zz2), col="red", lty=1)
text(0.5, 0.35, labels=expression(paste("pre Cyfra"+frac(Delta*log(Cyfra),Delta*time)(post))), col="red", cex=1)
text(0.3, 0.27, labels="AUC:0.639 (0.5-0.777), p<0.001", col="red", cex=1.1)
text(0.5, 0.19, labels=expression(paste("pre Cyfra"+frac(Delta*log(Cyfra),Delta*time)(mid))), col="blue", cex=1)
text(0.3, 0.12, labels="AUC:0.626 (0.497-0.754), p<0.001", col="blue", cex=1)
text(0.5, 0.07, labels="pre Cyfra", col="black", cex=1)
text(0.3, 0.02, labels="AUC:0.642 (0.511-0.773), p<0.001", col="black", cex=1)


########################
#### scc, rel 
z0 = multipleROC(survival~pre_scc, data=Cx_r)
zz0= multipleROC2roc(z0)
z1 = multipleROC(form=survival~pre_scc+scc_s1, data=Cx_r)
zz1= multipleROC2roc(z1)
z2 = multipleROC(form=survival~pre_scc+scc_s2, data=Cx_r)
zz2= multipleROC2roc(z2)
plot_ROC(list(z0,z1))
plot_ROC(list(z0,z2))

plot_ROC(list(z0,z1,z2))
plot.roc(smooth(zz0), col="black", lty=2)
par(new=T)
plot.roc(smooth(zz1), col="blue", lty=1)
par(new=T)
plot.roc(smooth(zz2), col="red", lty=1)
text(0.5, 0.35, labels=expression(paste("pre SCC-Ag"+frac(Delta*"SCC-Ag","pre SCC-Ag")(post))), col="red", cex=1)
text(0.3, 0.27, labels="AUC:0.668 (0.535-0.802), p<0.001", col="red", cex=1)
text(0.5, 0.19, labels=expression(paste("pre SCC-Ag"+frac(Delta*"SCC-Ag","pre SCC-Ag")(mid))), col="blue", cex=1)
text(0.3, 0.12, labels="AUC:0.66 (0.527-0.794), p<0.001", col="blue", cex=1)
text(0.5, 0.07, labels="pre SCC-Ag", col="black", cex=1)
text(0.3, 0.02, labels="AUC:0.666 (0.528-0.804), p<0.001", col="black", cex=1)


#### cyfra, rel
z0 = multipleROC(survival~pre_cyfra, data=Cx_r)
zz0= multipleROC2roc(z0)
z1 = multipleROC(form=survival~pre_cyfra+cyfra_s1, data=Cx_r)
zz1= multipleROC2roc(z1)
z2 = multipleROC(form=survival~pre_cyfra+cyfra_s2, data=Cx_r)
zz2= multipleROC2roc(z2)
plot_ROC(list(z0,z1))
plot_ROC(list(z0,z2))

plot_ROC(list(z0,z1,z2))
plot.roc(smooth(zz0), col="black", lty=2)
par(new=T)
plot.roc(smooth(zz1), col="blue", lty=1)
par(new=T)
plot.roc(smooth(zz2), col="red", lty=1)
text(0.5, 0.35, labels=expression(paste("pre Cyfra"+frac(Delta*Cyfra,"pre Cyfra")(post))), col="red", cex=1)
text(0.3, 0.27, labels="AUC:0.612 (0.486-0.739), p<0.001", col="red", cex=1)
text(0.5, 0.19, labels=expression(paste("pre Cyfra"+frac(Delta*Cyfra,"pre Cyfra")(mid))), col="blue", cex=1)
text(0.3, 0.12, labels="AUC:0.631 (0.501-0.76), p<0.001", col="blue", cex=1)
text(0.5, 0.07, labels="pre Cyfra", col="black", cex=1)
text(0.3, 0.02, labels="AUC:0.642 (0.511-0.773), p<0.001", col="black", cex=1)

#### scc, rel, time
z0 = multipleROC(survival~pre_scc, data=Cx_r)
zz0= multipleROC2roc(z0)
z1 = multipleROC(form=survival~pre_scc+scc_sv1, data=Cx_r)
zz1= multipleROC2roc(z1)
z2 = multipleROC(form=survival~pre_scc+scc_sv2, data=Cx_r)
zz2= multipleROC2roc(z2)
plot_ROC(list(z0,z1))
plot_ROC(list(z0,z2))

plot_ROC(list(z0,z1,z2))
plot.roc(smooth(zz0), col="black", lty=2)
par(new=T)
plot.roc(smooth(zz1), col="blue", lty=1)
par(new=T)
plot.roc(smooth(zz2), col="red", lty=1)
text(0.5, 0.35, labels=expression(paste("pre SCC-Ag"+frac(Delta*"SCC-Ag",Delta*time)(post))), col="red", cex=1)
text(0.3, 0.27, labels="AUC:0.644 (0.491-0.797), p<0.001", col="red", cex=1)
text(0.5, 0.19, labels=expression(paste("pre SCC-Ag"+frac(Delta*"SCC-Ag",Delta*time)(mid))), col="blue", cex=1)
text(0.3, 0.12, labels="AUC:0.666 (0.531-0.802), p<0.001", col="blue", cex=1)
text(0.5, 0.07, labels="pre SCC-Ag", col="black", cex=1)
text(0.3, 0.02, labels="AUC:0.666 (0.528-0.804), p<0.001", col="black", cex=1)



#### cyfra, rel, time
z0 = multipleROC(survival~pre_cyfra, data=Cx_r)
zz0= multipleROC2roc(z0)
z1 = multipleROC(form=survival~pre_cyfra+cyfra_sv1, data=Cx_r)
zz1= multipleROC2roc(z1)
z2 = multipleROC(form=survival~pre_cyfra+cyfra_sv2, data=Cx_r)
zz2= multipleROC2roc(z2)
plot_ROC(list(z0,z1))
plot_ROC(list(z0,z2))

plot_ROC(list(z0,z1,z2))
plot.roc(smooth(zz0), col="black", lty=2)
par(new=T)
plot.roc(smooth(zz1), col="blue", lty=1)
par(new=T)
plot.roc(smooth(zz2), col="red", lty=1)
text(0.5, 0.35, labels=expression(paste("pre Cyfra"+frac(Delta*Cyfra,Delta*time))(post)), col="red", cex=1)
text(0.3, 0.27, labels="AUC:0.648 (0.517-0.779), p<0.001", col="red", cex=1)
text(0.5, 0.19, labels=expression(paste("pre Cyfra"+frac(Delta*Cyfra,Delta*time))(mid)), col="blue", cex=1)
text(0.3, 0.12, labels="AUC:0.583 (0.437-0.73), p<0.001", col="blue", cex=1)
text(0.5, 0.07, labels="pre Cyfra", col="black", cex=1)
text(0.3, 0.02, labels="AUC:0.642 (0.511-0.773), p<0.001", col="black", cex=1)

##
Cx2 <- Cx0 %>% mutate(TS = Surv(fu_date, css==1)) 
Cx_m <-  Cx2  %>% select(TS, Age_m, EQD2_m, duration_m, le, Hb0_m, NLR_m, PLT_m, scc0_m, scc1_m, scc2_m, cyfra0_m, cyfra1_m, cyfra2_m, 
                         scc_v1_m,  scc_v2_m, cyfra_v1_m, cyfra_v2_m) %>% as.data.frame()
out=mycph(TS~., data=Cx_m)
write.csv(out, file="uv_css.csv")
HRplot(out, type=3, show.CI=T, pch=5, cex=6)
result<-coxph(TS~.-cyfra_v1_m, data=Cx_m)
finalmodel<-step(result, direction="backward")
summary(finalmodel)
ggforest(finalmodel, data=Cx_m, fontsize = 1.7)

####
Cx2 <- Cx0 %>% mutate(TS = Surv(recur_date, recur_m==1)) 
Cx_m <-  Cx2  %>% select(TS, Age_m, EQD2_m, duration_m, le, Hb0_m, NLR_m, PLT_m, scc0_m, scc1_m, scc2_m, cyfra0_m, cyfra1_m, cyfra2_m, 
                         scc_v1_m,  scc_v2_m, cyfra_v1_m, cyfra_v2_m) %>% as.data.frame()

out=mycph(TS~., data=Cx_m)
write.csv(out, file="uv_pfs.csv")
#row.names(out) <- c("pre SCC Ag ≥ 5.55","pre Cyfra 21-1 ≥ 3.1","SCC Ag \nduring CCRT ≥ 1.3", "Cyfra 21-1 \nduring CCRT ≥ 1.7",
#                   "pre Hb < 11.7","pre ALC < 1894", "NLR ≥ 2.35", "Treatment time ≥ 53", "Total dose ≥ 72.25", "Age < 53", "PMI", "PW/LVI")
HRplot(out, type=3, show.CI=T, pch=5, cex=6)
result<-coxph(TS~., data=Cx_m)
finalmodel<-step(result, direction="backward")
summary(finalmodel)
ggforest(finalmodel, data=Cx_m, fontsize = 1.7)



####
library(survival)
library(survminer)
library(ggpubr)

Cx3 <- Cx0 %>%   mutate(treatment=ifelse(treatment!="TPA"& treatment!="TP", "others", treatment), 
                        group=ifelse(scc0_m==1 & scc_v2_m==1,2, ifelse(scc0_m==0 & scc_v2_m==0,0,1)), group=factor(group, levels=c(0,1,2)))

fit=survfit(Surv(fu_date, css==1)~group, data=Cx3)
ggsurvplot(fit, xlab="Months", ylab="Disease specific survival rate", pval=T, pval.size=8,fun="pct", conf.int=F, risk.table = T, size=1, linetype = c("dashed","solid","dashed"),
           palette = c("blue","black","red"), legend= "none", legend.title=expression(paste("SCC-Ag,", frac(Delta*log("SCC-Ag"),Delta*time))), 
           legend.lab=c("low","intermediate","high"),
           break.time.by=12, xlim=c(0,84), risk.table.height=0.3, ylim=c(0,100), ggtheme = theme_classic2(base_size=17, base_family = "Arial"),
           font.family = "Arial" )
summary(fit, time=60)
fit=survfit(Surv(fu_date, css==1)~1, data=Cx3)
summary(fit, time=60)


fit=survfit(Surv(recur_date, recur_m==1)~group, data=Cx3)
ggsurvplot(fit, xlab="Months", ylab="Progression free survival rate", pval=T, pval.size=8,fun="pct", conf.int=F, risk.table = T, size=1, linetype = c("dashed","solid","dashed"),
           palette = c("blue","black","red"), legend= "none", legend.title=expression(paste("SCC-Ag,", frac(Delta*log("SCC-Ag"),Delta*time))),  
           legend.lab=c("low","intermediate","high"),
           break.time.by=12, xlim=c(0,84), risk.table.height=0.3, ylim=c(0,100), ggtheme = theme_classic2(base_size=17, base_family = "Arial"),
           font.family = "Arial" )
summary(fit, time=60)
fit=survfit(Surv(recur_date, recur_m==1)~1, data=Cx3)
summary(fit, time=60)


### 

library(moonBook)
mytable(group~css+fu_date+recur1+recur_date+Age_m+le+EQD2_m+duration_m+Hb0_m+NLR_m+PLT_m+scc1_m+scc2_m+scc_v1_m+cyfra0_m+cyfra1_m+cyfra2_m+cyfra_v1_m+cyfra_v2_m+treatment, data=Cx3, method=3)
mytable(~css+fu_date+recur1+recur_date+Age_m+le+EQD2_m+duration_m+Hb0_m+NLR_m+PLT_m+scc1_m+scc2_m+scc_v1_m+cyfra0_m+cyfra1_m+cyfra2_m+cyfra_v1_m+cyfra_v2_m+treatment, data=Cx3, method=3)

summary(Cx3)
