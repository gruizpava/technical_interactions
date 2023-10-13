library(MASS)
library(sna)
##### Rowles models with QAP interdependence.#####:

#Prediction of DSM

model2la<- netlogit(DSM_R, list(mod_R),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlogit(DSM_R, list(mod_R, m.cpc.freq.cos),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlogit(DSM_R, list(mod_R, EN_Ad1),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlogit(DSM_R, list(mod_R, Rz_all),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)


model2la<- netlogit(DSM_R, list(mod_R, m.cpc.freq.cos, Rz_all),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlogit(DSM_R, list(mod_R, m.cpc.freq.cos, EN_Ad1),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlogit(DSM_R, list(EN_Ad1, Rz_all, mod_R),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

####Explanation of citation intensity######

library("Hmisc")
x<- data.frame(as.vector(Rz_all), as.vector(EN_Ad1), as.vector(DSM_R), as.vector(m.cpc.freq.cos),as.vector(m.cpc),as.vector(m.cit),as.vector(m.exm), as.vector(m.iname), as.vector(m.inet.den),as.vector(m.inet.bet),as.vector(m.inet.deg),as.vector(m.enet.den),as.vector(m.enet.bet),as.vector(m.enet.deg))
x1 <- subset(x, (subset = !is.na(Rz_all))& (diag_R!=1))

corrgram(x, order=NULL, cex.labels=0.5, upper.panel=panel.pie, text.panel=panel.txt, main="Correlogram",col.regions =colorRampPalette(c("white", "blue")))
x.rcorr = rcorr(as.matrix(x1))
write.csv(x.rcorr$r, file = "rcorr_table1.csv")


model2la<- netlm(Rz_all, list(DSM_R),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"), diag=FALSE)
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.cit*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"), diag=FALSE)
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.cpc*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"), diag=FALSE)
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.cpc.freq.cos*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"), diag=FALSE)
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.inet.den*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.inet.bet*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.inet.deg*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.inet.bet*100 , m.inet.deg*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.enet.den*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.enet.bet*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.enet.deg*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

#all
model2la<- netlm(Rz_all, list(m.cpc.freq.cos*100, m.inet.den*100, m.inet.bet*100 , m.inet.deg*100, m.enet.den*100, m.enet.bet*100 , m.enet.deg*100 ),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)

model2la<- netlm(Rz_all, list(DSM_R, m.inet.den*100, m.inet.bet*100 , m.inet.deg*100, m.enet.den*100, m.enet.bet*100 , m.enet.deg*100 ),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2la)


#####ordinal logit

cat_Rz_all = as.vector(Rz_all)
table(cat_Rz_all)
ordinal <- as.data.frame(cat_Rz_all)
ordinal$cat <- cut(ordinal$cat_Rz_all, breaks = c(-Inf,-2,2,Inf), labels = c("Low","Medium","High"))
ordinal$cos <- as.vector(m.cpc.freq.cos)
ordinal$o.cpc <- as.vector(m.cpc)
ordinal$o.cit <- as.vector(m.cit)
ordinal$o.exm <- as.vector(m.exm)
ordinal$o.iname <- as.vector(m.iname)
ordinal$o.soa <- as.vector(m.soa)
ordinal$dsm <- as.vector(DSM_R)
ordinal$dsm = factor(ordinal$dsm, levels = c("0", "1"), ordered = TRUE) 
ordinal$meth <- 0
ordinal$diag <- as.vector(diag_R)
model= polr(cat ~ dsm , data = ordinal, Hess = TRUE)
summary(model)


library(effects)
Effect(focal.predictors = "dsm",model)
plot(Effect(focal.predictors = "dsm",model))

model2= polr(cat ~ dsm + o.cpc + o.exm , data = ordinal, Hess = TRUE)
summary(model2)
plot(Effect(focal.predictors = "dsm",model2))

model2= polr(cat ~ dsm + o.cit, data = ordinal, Hess = TRUE)
summary(model2)
plot(Effect(focal.predictors = "dsm",model2))

model2= polr(cat ~ dsm + cos, data = ordinal, Hess = TRUE)
summary(model2)
plot(Effect(focal.predictors = "dsm",model2))
plot(Effect(focal.predictors = "cos",model2))

model3= polr(cat ~ dsm + cos + o.exm, data = ordinal, Hess = TRUE)
summary(model3)
plot(Effect(focal.predictors = "dsm",model3))
plot(Effect(focal.predictors = "cos",model3))
plot(Effect(focal.predictors = "o.exm",model3))

model3= polr(cat ~ dsm + o.exm, data = ordinal, Hess = TRUE)
summary(model3)
plot(Effect(focal.predictors = "dsm",model3))
plot(Effect(focal.predictors = "o.exm",model3))

model3= polr(cat ~ dsm + o.iname, data = ordinal, Hess = TRUE)
summary(model3)
plot(Effect(focal.predictors = "dsm",model3))
plot(Effect(focal.predictors = "o.iname",model3))

model3= polr(cat ~ cos + o.iname, data = ordinal, Hess = TRUE)
summary(model3)
plot(Effect(focal.predictors = "cos",model3))
plot(Effect(focal.predictors = "o.iname",model3))

##### James models with QAP interdependence.#####

#Prediction

model2g<- netlogit(DSM_J, list(mod_J),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2g)

model2g<- netlogit(DSM_J, list(mod_J, m.cpc.freq.cos.j),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2g)

model2g<- netlogit(DSM_J, list(mod_J, EN_Ajd1),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2g)

model2g<- netlogit(DSM_J, list(mod_J, Jz_all),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2g)

model2g<- netlogit(DSM_J, list(mod_J, EN_Ajd1, Jz_all),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2g)

model2g<- netlogit(DSM_J, list(mod_J, EN_Ajd1, m.cpc.freq.cos.j),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2g)

model2g<- netlogit(DSM_J, list(mod_J, Jz_all, m.cpc.freq.cos.j),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model2g)


####Explanation of citation intensity######

library("Hmisc")
x<- data.frame(as.vector(Jz_all), as.vector(EN_Ajd1), as.vector(DSM_J), as.vector(m.cpc.freq.cos.j),as.vector(m.cpc.j),as.vector(m.cit.j),as.vector(m.exm.j), as.vector(m.iname.j), as.vector(m.soa.j), as.vector(diag_J),as.vector(m.inet.den.j),as.vector(m.inet.bet.j),as.vector(m.inet.deg.j),as.vector(m.enet.den.j),as.vector(m.enet.bet.j),as.vector(m.enet.deg.j))
x1 <- subset(x, (subset = !is.na(Jz_all))& (diag_J!=1))
x.rcorr = rcorr(as.matrix(x1))
write.csv(x.rcorr$r, file = "rcorr_tableJ.csv")

table(is.na(Jz_all))
model4la<- netlm(Jz_all, list(DSM_J),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(DSM_J, m.cpc.freq.cos.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(DSM_J, m.cpc.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(DSM_J, m.cit.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(DSM_J, m.inet.den.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(DSM_J, m.inet.bet.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(DSM_J, m.inet.deg.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(DSM_J, m.enet.den.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(DSM_J, m.enet.bet.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(DSM_J, m.enet.deg.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(m.cpc.freq.cos.j*100, m.inet.den.j*100, m.inet.bet.j*100, m.inet.deg.j*100, m.enet.den.j*100, m.enet.bet.j*100, m.enet.deg.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)

model4la<- netlm(Jz_all, list(DSM_J, m.inet.den.j*100, m.inet.bet.j*100, m.inet.deg.j*100, m.enet.den.j*100, m.enet.bet.j*100, m.enet.deg.j*100),  intercept = TRUE, mode = "digraph", nullhyp = c("qap"))
summary(model4la)


###### Ordered logit James

cat_Jz_all = as.vector(Jz_all)
table(cat_Jz_all)
ordinal_J <- as.data.frame(cat_Jz_all)
ordinal_J$cat <- cut(ordinal_J$cat_Jz_all, breaks = c(-Inf,-2,2,Inf), labels = c("Low","Medium","High"))
ordinal_J$cos <- as.vector(m.cpc.freq.cos.j)
ordinal_J$o.cpc <- as.vector(m.cpc.j)
ordinal_J$o.cit <- as.vector(m.cit.j)
ordinal_J$o.exm <- as.vector(m.exm.j)
ordinal_J$o.iname <- as.vector(m.iname.j)
ordinal_J$o.soa <- as.vector(m.soa.j)
ordinal_J$dsm <- as.vector(DSM_J)
ordinal_J$dsm = factor(ordinal_J$dsm, levels = c("0", "1"), ordered = TRUE) 
ordinal_J$meth <- 1
ordinal_J$diag <- as.vector(diag_J)
model.j= polr(cat ~ dsm , data = ordinal_J, Hess = TRUE)
summary(model.j)


Effect(focal.predictors = "dsm",model.j)
plot(Effect(focal.predictors = "dsm",model.j))

model2= polr(cat ~ dsm + o.cpc + o.exm , data = ordinal_J, Hess = TRUE)
summary(model2.j)
plot(Effect(focal.predictors = "dsm",model2.j))

model2= polr(cat ~ dsm + o.cit, data = ordinal, Hess = TRUE)
summary(model2)
plot(Effect(focal.predictors = "dsm",model2))

model2= polr(cat ~ dsm + cos, data = ordinal, Hess = TRUE)
summary(model2)
plot(Effect(focal.predictors = "dsm",model2))
plot(Effect(focal.predictors = "cos",model2))

model3= polr(cat ~ dsm + cos + o.exm, data = ordinal, Hess = TRUE)
summary(model3)
plot(Effect(focal.predictors = "dsm",model3))
plot(Effect(focal.predictors = "cos",model3))
plot(Effect(focal.predictors = "o.exm",model3))

model3.j= polr(cat ~ dsm + o.exm, data = ordinal_J, Hess = TRUE)
summary(model3.j)
plot(Effect(focal.predictors = "dsm",model3))
plot(Effect(focal.predictors = "o.exm",model3))

model3.j= polr(cat ~ dsm + o.iname, data = ordinal_J, Hess = TRUE)
summary(model3)
plot(Effect(focal.predictors = "dsm",model3.j))
plot(Effect(focal.predictors = "o.iname",model3.j))

model3.j= polr(cat ~ cos + o.iname, data = ordinal_J, Hess = TRUE)
summary(model3.j)
plot(Effect(focal.predictors = "cos",model3.j))
plot(Effect(focal.predictors = "o.iname",model3.j))

table(ordinal_J$cat, ordinal_J$dsm)
