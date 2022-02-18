
library(sna)
library(network)
library(dplyr)
library(reshape)
library(lsa)
library(AMR)
library(cleaner)
library(readxl)


#######Set the working directory of components' similarity indexes#####

setwd("~/Data/Alternative")
#### Rowles components ####

#comp_cpc_JI

comp1a = read_excel("comp1a_cpc.xlsx")
comp_cpc <- function(comp1a) {
  t<- as.numeric(ncol(comp1a))
  cpc <- data.frame(comp1a [c(1,3:t)])
  md <- melt.data.frame(cpc,id.vars = "pn", na.rm = TRUE)
  e<- distinct(md)
  f<- as.data.frame(e[c(1,3)])
  
  cpc_sub <- as.data.frame(as.character(f$value), optional= TRUE)
  pn <- as.data.frame(as.character(f$pn), optional= TRUE)
  g<- cbind.data.frame(pn, cpc_sub)
  v.p3 <- comp1a [c(1,2)]
  v.cpc_comp1a <- merge.data.frame(v.p3, g, by="pn")
  
  rm(t,cpc,md,e,f, cpc_sub, pn,g,v.p3)
  return(v.cpc_comp1a)
}
v.cpc_comp1a <- comp_cpc(comp1a)

comp2a = read_excel("comp2a_cpc.xlsx")
v.cpc_comp2a <- comp_cpc(comp2a)

comp3a = read_excel("comp3a_cpc.xlsx")
v.cpc_comp3a <- comp_cpc(comp3a)

comp4a = read_excel("comp4a_cpc.xlsx")
v.cpc_comp4a <- comp_cpc(comp4a)

comp5a = read_excel("comp5a_cpc.xlsx")
v.cpc_comp5a <- comp_cpc(comp5a)

comp6a = read_excel("comp6a_cpc.xlsx")
v.cpc_comp6a <- comp_cpc(comp6a)

comp7a = read_excel("comp7a_cpc.xlsx")
v.cpc_comp7a <- comp_cpc(comp7a)

comp8a = read_excel("comp8a_cpc.xlsx")
v.cpc_comp8a <- comp_cpc(comp8a)

comp9a = read_excel("comp9a_cpc.xlsx")
v.cpc_comp9a <- comp_cpc(comp9a)

comp10a = read_excel("comp10a_cpc.xlsx")
v.cpc_comp10a <- comp_cpc(comp10a)

comp11a = read_excel("comp11a_cpc.xlsx")
v.cpc_comp11a <- comp_cpc(comp11a)

comp12a = read_excel("comp12a_cpc.xlsx")
v.cpc_comp12a <- comp_cpc(comp12a)

comp13a = read_excel("comp13a_cpc.xlsx")
v.cpc_comp13a <- comp_cpc(comp13a)

comp14a = read_excel("comp14a_cpc.xlsx")
v.cpc_comp14a <- comp_cpc(comp14a)

comp15a = read_excel("comp15a_cpc.xlsx")
v.cpc_comp15a <- comp_cpc(comp15a)

comp16a = read_excel("comp16a_cpc.xlsx")
v.cpc_comp16a <- comp_cpc(comp16a)

#comp_cpc_Frequency

comp_cpc_freq <- function(comp1a) {
  t<- as.numeric(ncol(comp1a))
  cpc <- data.frame(comp1a [c(1,3:t)])
  md <- melt.data.frame(cpc,id.vars = "pn", na.rm = TRUE)
  e<- distinct(md)
  f<- as.data.frame(e[c(1,3)])
  
  cpc_freq_comp1a <- as.matrix (freq(f$value,nmax = 15, na.rm= TRUE))
  return(cpc_freq_comp1a)
}

cpc_freq_comp1a <- comp_cpc_freq(comp1a)
cpc_freq_comp2a <- comp_cpc_freq(comp2a)
cpc_freq_comp3a <- comp_cpc_freq(comp3a)
cpc_freq_comp4a <- comp_cpc_freq(comp4a)
cpc_freq_comp5a <- comp_cpc_freq(comp5a)
cpc_freq_comp6a <- comp_cpc_freq(comp6a)
cpc_freq_comp7a <- comp_cpc_freq(comp7a)
cpc_freq_comp8a <- comp_cpc_freq(comp8a)
cpc_freq_comp9a <- comp_cpc_freq(comp9a)
cpc_freq_comp10a <- comp_cpc_freq(comp10a)
cpc_freq_comp11a <- comp_cpc_freq(comp11a)
cpc_freq_comp12a <- comp_cpc_freq(comp12a)
cpc_freq_comp13a <- comp_cpc_freq(comp13a)
cpc_freq_comp14a <- comp_cpc_freq(comp14a)
cpc_freq_comp15a <- comp_cpc_freq(comp15a)
cpc_freq_comp16a <- comp_cpc_freq(comp16a)


#comp_iname

comp1a = read_excel("comp1a_iname.xlsx")
comp_iname <- function(comp1a){
  t<- as.numeric(ncol(comp1a))
  iname <- data.frame(comp1a [c(1,3:t)])
  md <- melt.data.frame(iname,id.vars = "pn", na.rm = TRUE)
  e1<- distinct(as.data.frame(md$value))
  iname_sub1 <- as.data.frame(as.character(e1$`md$value`), optional=FALSE, col.names= c("var0"))
  iname_sub1 <- rename(iname_sub1,c("as.character(e1$`md$value`)"="var0"))
  v.iname_comp1a <- iname_sub1
  return(v.iname_comp1a)
  rm(t,iname,md,e1,iname_sub1)
}
v.iname_comp1a <- comp_iname(comp1a)

comp2a = read_excel("comp2a_iname.xlsx")
v.iname_comp2a <- comp_iname(comp2a)

comp3a = read_excel("comp3a_iname.xlsx")
v.iname_comp3a <- comp_iname(comp3a)

comp4a = read_excel("comp4a_iname.xlsx")
v.iname_comp4a <- comp_iname(comp4a)

comp5a = read_excel("comp5a_iname.xlsx")
v.iname_comp5a <- comp_iname(comp5a)

comp6a = read_excel("comp6a_iname.xlsx")
v.iname_comp6a <- comp_iname(comp6a)

comp7a = read_excel("comp7a_iname.xlsx")
v.iname_comp7a <- comp_iname(comp7a)

comp8a = read_excel("comp8a_iname.xlsx")
v.iname_comp8a <- comp_iname(comp8a)

comp9a = read_excel("comp9a_iname.xlsx")
v.iname_comp9a <- comp_iname(comp9a)

comp10a = read_excel("comp10a_iname.xlsx")
v.iname_comp10a <- comp_iname(comp10a)

comp11a = read_excel("comp11a_iname.xlsx")
v.iname_comp11a <- comp_iname(comp11a)

comp12a = read_excel("comp12a_iname.xlsx")
v.iname_comp12a <- comp_iname(comp12a)

comp13a = read_excel("comp13a_iname.xlsx")
v.iname_comp13a <- comp_iname(comp13a)

comp14a = read_excel("comp14a_iname.xlsx")
v.iname_comp14a <- comp_iname(comp14a)

comp15a = read_excel("comp15a_iname.xlsx")
v.iname_comp15a <- comp_iname(comp15a)

comp16a = read_excel("comp16a_iname.xlsx")
v.iname_comp16a <- comp_iname(comp16a)

#comp_exm

comp1a = read_excel("comp1a_exm.xlsx")
comp_exm <- function(comp1a){
  t<- as.numeric(ncol(comp1a))
  exm <- data.frame(comp1a [c(1,3:t)])
  md <- melt.data.frame(exm,id.vars = "pn", na.rm = TRUE)
  e1<- distinct(as.data.frame(md$value))
  exm_sub1 <- as.data.frame(as.character(e1$`md$value`), optional=FALSE, col.names= c("var0"))
  exm_sub1 <- rename(exm_sub1,c("as.character(e1$`md$value`)"="var0"))
  v.exm_comp1a <- exm_sub1
  return(v.exm_comp1a)
  rm(t,exm,md,e1,exm_sub1)
}
v.exm_comp1a <- comp_exm(comp1a)

comp2a = read_excel("comp2a_exm.xlsx")
v.exm_comp2a <- comp_exm(comp2a)

comp3a = read_excel("comp3a_exm.xlsx")
v.exm_comp3a <- comp_exm(comp3a)

comp4a = read_excel("comp4a_exm.xlsx")
v.exm_comp4a <- comp_exm(comp4a)

comp5a = read_excel("comp5a_exm.xlsx")
v.exm_comp5a <- comp_exm(comp5a)

comp6a = read_excel("comp6a_exm.xlsx")
v.exm_comp6a <- comp_exm(comp6a)

comp7a = read_excel("comp7a_exm.xlsx")
v.exm_comp7a <- comp_exm(comp7a)

comp8a = read_excel("comp8a_exm.xlsx")
v.exm_comp8a <- comp_exm(comp8a)

comp9a = read_excel("comp9a_exm.xlsx")
v.exm_comp9a <- comp_exm(comp9a)

comp10a = read_excel("comp10a_exm.xlsx")
v.exm_comp10a <- comp_exm(comp10a)

comp11a = read_excel("comp11a_exm.xlsx")
v.exm_comp11a <- comp_exm(comp11a)

comp12a = read_excel("comp12a_exm.xlsx")
v.exm_comp12a <- comp_exm(comp12a)

comp13a = read_excel("comp13a_exm.xlsx")
v.exm_comp13a <- comp_exm(comp13a)

comp14a = read_excel("comp14a_exm.xlsx")
v.exm_comp14a <- comp_exm(comp14a)

comp15a = read_excel("comp15a_exm.xlsx")
v.exm_comp15a <- comp_exm(comp15a)

comp16a = read_excel("comp16a_exm.xlsx")
v.exm_comp16a <- comp_exm(comp16a)




#### James components ####

#comp_cpc_JI

compJ1a = read_excel("compJ1a_cpc.xlsx")
v.cpc_compJ1a <- comp_cpc(compJ1a)

compJ2a = read_excel("compJ2a_cpc.xlsx")
v.cpc_compJ2a <- comp_cpc(compJ2a)

compJ4a = read_excel("compJ4a_cpc.xlsx")
v.cpc_compJ4a <- comp_cpc(compJ4a)

compJ5a = read_excel("compJ5a_cpc.xlsx")
v.cpc_compJ5a <- comp_cpc(compJ5a)

compJ6a = read_excel("compJ6a_cpc.xlsx")
v.cpc_compJ6a <- comp_cpc(compJ6a)

compJ7a = read_excel("compJ7a_cpc.xlsx")
v.cpc_compJ7a <- comp_cpc(compJ7a)

compJ8a = read_excel("compJ8a_cpc.xlsx")
v.cpc_compJ8a <- comp_cpc(compJ8a)

compJ9a = read_excel("compJ9a_cpc.xlsx")
v.cpc_compJ9a <- comp_cpc(compJ9a)

compJ10a = read_excel("compJ10a_cpc.xlsx")
v.cpc_compJ10a <- comp_cpc(compJ10a)

compJ11a = read_excel("compJ11a_cpc.xlsx")
v.cpc_compJ11a <- comp_cpc(compJ11a)

compJ12a = read_excel("compJ12a_cpc.xlsx")
v.cpc_compJ12a <- comp_cpc(compJ12a)

compJ13a = read_excel("compJ13a_cpc.xlsx")
v.cpc_compJ13a <- comp_cpc(compJ13a)

#comp_cpc_Frequency

cpc_freq_compJ1a <- comp_cpc_freq(compJ1a)
cpc_freq_compJ2a <- comp_cpc_freq(compJ2a)
cpc_freq_compJ4a <- comp_cpc_freq(compJ4a)
cpc_freq_compJ5a <- comp_cpc_freq(compJ5a)
cpc_freq_compJ6a <- comp_cpc_freq(compJ6a)
cpc_freq_compJ7a <- comp_cpc_freq(compJ7a)
cpc_freq_compJ8a <- comp_cpc_freq(compJ8a)
cpc_freq_compJ9a <- comp_cpc_freq(compJ9a)
cpc_freq_compJ10a <- comp_cpc_freq(compJ10a)
cpc_freq_compJ11a <- comp_cpc_freq(compJ11a)
cpc_freq_compJ12a <- comp_cpc_freq(compJ12a)
cpc_freq_compJ13a <- comp_cpc_freq(compJ13a)



#comp_iname

compJ1a = read_excel("compJ1a_iname.xlsx")
v.iname_compJ1a <- comp_iname(compJ1a)

compJ2a = read_excel("compJ2a_iname.xlsx")
v.iname_compJ2a <- comp_iname(compJ2a)

compJ4a = read_excel("compJ4a_iname.xlsx")
v.iname_compJ4a <- comp_iname(compJ4a)

compJ5a = read_excel("compJ5a_iname.xlsx")
v.iname_compJ5a <- comp_iname(compJ5a)

compJ6a = read_excel("compJ6a_iname.xlsx")
v.iname_compJ6a <- comp_iname(compJ6a)

compJ7a = read_excel("compJ7a_iname.xlsx")
v.iname_compJ7a <- comp_iname(compJ7a)

compJ8a = read_excel("compJ8a_iname.xlsx")
v.iname_compJ8a <- comp_iname(compJ8a)

compJ9a = read_excel("compJ9a_iname.xlsx")
v.iname_compJ9a <- comp_iname(compJ9a)

compJ10a = read_excel("compJ10a_iname.xlsx")
v.iname_compJ10a <- comp_iname(compJ10a)

compJ11a = read_excel("compJ11a_iname.xlsx")
v.iname_compJ11a <- comp_iname(compJ11a)

compJ12a = read_excel("compJ12a_iname.xlsx")
v.iname_compJ12a <- comp_iname(compJ12a)

compJ13a = read_excel("compJ13a_iname.xlsx")
v.iname_compJ13a <- comp_iname(compJ13a)

#comp_exm

compJ1a = read_excel("compJ1a_exm.xlsx")
v.exm_compJ1a <- comp_exm(compJ1a)

compJ2a = read_excel("compJ2a_exm.xlsx")
v.exm_compJ2a <- comp_exm(compJ2a)

compJ4a = read_excel("compJ4a_exm.xlsx")
v.exm_compJ4a <- comp_exm(compJ4a)

compJ5a = read_excel("compJ5a_exm.xlsx")
v.exm_compJ5a <- comp_exm(compJ5a)

compJ6a = read_excel("compJ6a_exm.xlsx")
v.exm_compJ6a <- comp_exm(compJ6a)

compJ7a = read_excel("compJ7a_exm.xlsx")
v.exm_compJ7a <- comp_exm(compJ7a)

compJ8a = read_excel("compJ8a_exm.xlsx")
v.exm_compJ8a <- comp_exm(compJ8a)

compJ9a = read_excel("compJ9a_exm.xlsx")
v.exm_compJ9a <- comp_exm(compJ9a)

compJ10a = read_excel("compJ10a_exm.xlsx")
v.exm_compJ10a <- comp_exm(compJ10a)

compJ11a = read_excel("compJ11a_exm.xlsx")
v.exm_compJ11a <- comp_exm(compJ11a)

compJ12a = read_excel("compJ12a_exm.xlsx")
v.exm_compJ12a <- comp_exm(compJ12a)

compJ13a = read_excel("compJ13a_exm.xlsx")
v.exm_compJ13a <- comp_exm(compJ13a)



######define Jaccard Similarity function in citations#####
#setwd("C:/~/Public repository final study/Data/citations")
jaccard_cit <- function(v.comp1a,v.comp2a) {
  a<- distinct(as.data.frame(v.comp1a$out_cit))
  b<- distinct(as.data.frame(v.comp2a$out_cit))
  
  intersection = length(intersect(a$`v.comp1a$out_cit`, b$`v.comp2a$out_cit`))
  union = length(a$`v.comp1a$out_cit`) + length(b$`v.comp2a$out_cit`) - intersection
  return (intersection/union)
}
jaccard_cit(v.comp1a,v.comp2a)

######define Jaccard Similarity function in cpc#####
y<-2015
jaccard_cpc <- function(v.cpc_comp1a,v.cpc_comp2a,y) {
  new_a <- subset.data.frame(v.cpc_comp1a, year<=y)
  new_b <- subset.data.frame(v.cpc_comp2a, year<=y)
  a<- distinct(as.data.frame(new_a$cpc_sub))
  b<- distinct(as.data.frame(new_b$cpc_sub))
  
  intersection = length(intersect(a$`new_a$cpc_sub`, b$`new_b$cpc_sub`))
  union = length(a$`new_a$cpc_sub`) + length(b$`new_b$cpc_sub`) - intersection
  return (intersection/union)
}

jaccard_cpc(v.cpc_comp1a,v.cpc_comp2a, 2015)


######define cosine Similarity function in cpc#####

comp1a = read_excel("comp1a_cpc.xlsx")
comp2a = read_excel("comp2a_cpc.xlsx")
comp3a = read_excel("comp3a_cpc.xlsx")
comp4a = read_excel("comp4a_cpc.xlsx")
comp5a = read_excel("comp5a_cpc.xlsx")
comp6a = read_excel("comp6a_cpc.xlsx")
comp7a = read_excel("comp7a_cpc.xlsx")
comp8a = read_excel("comp8a_cpc.xlsx")
comp9a = read_excel("comp9a_cpc.xlsx")
comp10a = read_excel("comp10a_cpc.xlsx")
comp11a = read_excel("comp11a_cpc.xlsx")
comp12a = read_excel("comp12a_cpc.xlsx")
comp13a = read_excel("comp13a_cpc.xlsx")
comp14a = read_excel("comp14a_cpc.xlsx")
comp15a = read_excel("comp15a_cpc.xlsx")
comp16a = read_excel("comp16a_cpc.xlsx")

v.comp <- c(comp1a,comp2a,comp3a,comp4a,comp5a,comp6a,comp7a,comp8a,comp9a,comp10a,comp11a,comp12a,comp13a,comp14a,comp15a,comp16a)

jaccard_cpc_cos <- function(cpc_freq_comp1a,cpc_freq_comp2a, comp1a, comp2a) {
  
  cpc_freq_comp1a <- as.data.frame(cpc_freq_comp1a)
  cpc_freq_comp1a$cpc1 <-  as.numeric(cpc_freq_comp1a$count)/(nrow(comp1a))
  cpc_freq_comp2a <- as.data.frame(cpc_freq_comp2a)
  cpc_freq_comp2a$cpc2 <-  as.numeric(cpc_freq_comp2a$count)/(nrow(comp2a))
  cpc <- merge(cpc_freq_comp1a, cpc_freq_comp2a, by= "item", all=TRUE)
  cpc[is.na(cpc)] <- 0
  return (cosine(cpc$cpc1, cpc$cpc2))
}
jaccard_cpc_cos (cpc_freq_comp1a,cpc_freq_comp2a, comp1a, comp2a)

######define Jaccard Similarity function in exm#####


jaccard_exm <- function(v.exm_comp1a,v.exm_comp2a) {
  intersection = length(intersect(v.exm_comp1a$var0, v.exm_comp2a$var0))
  union = length(v.exm_comp1a$var0) + length(v.exm_comp2a$var0) - intersection
  return (intersection/union)
}
jaccard_exm(v.exm_comp1a,v.exm_comp2a)

######define Jaccard Similarity function in iname#####

jaccard_iname <- function(v.iname_comp1a,v.iname_comp2a) {
  intersection = length(intersect(v.iname_comp1a$var0,v.iname_comp2a$var0))
  union = length(v.iname_comp1a$var0) + length(v.iname_comp2a$var0) - intersection
  return (intersection/union)
}

jaccard_iname(v.iname_comp1a,v.iname_comp2a)


#####Similarity matrices####

#citations
##Rowles
v.cit <- c(v.comp1a,v.comp2a,v.comp3a,v.comp4a,v.comp5a,v.comp6a,v.comp7a,v.comp8a,v.comp9a,v.comp10a,v.comp11a,v.comp12a,v.comp13a,v.comp14a,v.comp15a,v.comp16a)
matrix_dist <- function (v.cit) {
  A11 <- jaccard_cit(v.comp1a,v.comp1a)
  A12 <- jaccard_cit(v.comp1a,v.comp2a)
  A13 <- jaccard_cit(v.comp1a,v.comp3a)
  A14 <- jaccard_cit(v.comp1a,v.comp4a)
  A15 <- jaccard_cit(v.comp1a,v.comp5a)
  A16 <- jaccard_cit(v.comp1a,v.comp6a)
  A17 <- jaccard_cit(v.comp1a,v.comp7a)
  A18 <- jaccard_cit(v.comp1a,v.comp8a)
  A19 <- jaccard_cit(v.comp1a,v.comp9a)
  A110 <- jaccard_cit(v.comp1a,v.comp10a)
  A111 <- jaccard_cit(v.comp1a,v.comp11a)
  A112 <- jaccard_cit(v.comp1a,v.comp12a)
  A113 <- jaccard_cit(v.comp1a,v.comp13a)
  A114 <- jaccard_cit(v.comp1a,v.comp14a)
  A115 <- jaccard_cit(v.comp1a,v.comp15a)
  A116 <- jaccard_cit(v.comp1a,v.comp16a)
  
  #row2
  A21 <- jaccard_cit(v.comp2a,v.comp1a)
  A22 <- jaccard_cit(v.comp2a,v.comp2a)
  A23 <- jaccard_cit(v.comp2a,v.comp3a)
  A24 <- jaccard_cit(v.comp2a,v.comp4a)
  A25 <- jaccard_cit(v.comp2a,v.comp5a)
  A26 <- jaccard_cit(v.comp2a,v.comp6a)
  A27 <- jaccard_cit(v.comp2a,v.comp7a)
  A28 <- jaccard_cit(v.comp2a,v.comp8a)
  A29 <- jaccard_cit(v.comp2a,v.comp9a)
  A210 <- jaccard_cit(v.comp2a,v.comp10a)
  A211 <- jaccard_cit(v.comp2a,v.comp11a)
  A212 <- jaccard_cit(v.comp2a,v.comp12a)
  A213 <- jaccard_cit(v.comp2a,v.comp13a)
  A214 <- jaccard_cit(v.comp2a,v.comp14a)
  A215 <- jaccard_cit(v.comp2a,v.comp15a)
  A216 <- jaccard_cit(v.comp2a,v.comp16a)
  
  #row3
  A31 <- jaccard_cit(v.comp3a,v.comp1a)
  A32 <- jaccard_cit(v.comp3a,v.comp2a)
  A33 <- jaccard_cit(v.comp3a,v.comp3a)
  A34 <- jaccard_cit(v.comp3a,v.comp4a)
  A35 <- jaccard_cit(v.comp3a,v.comp5a)
  A36 <- jaccard_cit(v.comp3a,v.comp6a)
  A37 <- jaccard_cit(v.comp3a,v.comp7a)
  A38 <- jaccard_cit(v.comp3a,v.comp8a)
  A39 <- jaccard_cit(v.comp3a,v.comp9a)
  A310 <- jaccard_cit(v.comp3a,v.comp10a)
  A311 <- jaccard_cit(v.comp3a,v.comp11a)
  A312 <- jaccard_cit(v.comp3a,v.comp12a)
  A313 <- jaccard_cit(v.comp3a,v.comp13a)
  A314 <- jaccard_cit(v.comp3a,v.comp14a)
  A315 <- jaccard_cit(v.comp3a,v.comp15a)
  A316 <- jaccard_cit(v.comp3a,v.comp16a)
  
  #row4
  A41 <- jaccard_cit(v.comp4a,v.comp1a)
  A42 <- jaccard_cit(v.comp4a,v.comp2a)
  A43 <- jaccard_cit(v.comp4a,v.comp3a)
  A44 <- jaccard_cit(v.comp4a,v.comp4a)
  A45 <- jaccard_cit(v.comp4a,v.comp5a)
  A46 <- jaccard_cit(v.comp4a,v.comp6a)
  A47 <- jaccard_cit(v.comp4a,v.comp7a)
  A48 <- jaccard_cit(v.comp4a,v.comp8a)
  A49 <- jaccard_cit(v.comp4a,v.comp9a)
  A410 <- jaccard_cit(v.comp4a,v.comp10a)
  A411 <- jaccard_cit(v.comp4a,v.comp11a)
  A412 <- jaccard_cit(v.comp4a,v.comp12a)
  A413 <- jaccard_cit(v.comp4a,v.comp13a)
  A414 <- jaccard_cit(v.comp4a,v.comp14a)
  A415 <- jaccard_cit(v.comp4a,v.comp15a)
  A416 <- jaccard_cit(v.comp4a,v.comp16a)
  
  #row5
  A51 <- jaccard_cit(v.comp5a,v.comp1a)
  A52 <- jaccard_cit(v.comp5a,v.comp2a)
  A53 <- jaccard_cit(v.comp5a,v.comp3a)
  A54 <- jaccard_cit(v.comp5a,v.comp4a)
  A55 <- jaccard_cit(v.comp5a,v.comp5a)
  A56 <- jaccard_cit(v.comp5a,v.comp6a)
  A57 <- jaccard_cit(v.comp5a,v.comp7a)
  A58 <- jaccard_cit(v.comp5a,v.comp8a)
  A59 <- jaccard_cit(v.comp5a,v.comp9a)
  A510 <- jaccard_cit(v.comp5a,v.comp10a)
  A511 <- jaccard_cit(v.comp5a,v.comp11a)
  A512 <- jaccard_cit(v.comp5a,v.comp12a)
  A513 <- jaccard_cit(v.comp5a,v.comp13a)
  A514 <- jaccard_cit(v.comp5a,v.comp14a)
  A515 <- jaccard_cit(v.comp5a,v.comp15a)
  A516 <- jaccard_cit(v.comp5a,v.comp16a)
  
  #row6
  A61 <- jaccard_cit(v.comp6a,v.comp1a)
  A62 <- jaccard_cit(v.comp6a,v.comp2a)
  A63 <- jaccard_cit(v.comp6a,v.comp3a)
  A64 <- jaccard_cit(v.comp6a,v.comp4a)
  A65 <- jaccard_cit(v.comp6a,v.comp5a)
  A66 <- jaccard_cit(v.comp6a,v.comp6a)
  A67 <- jaccard_cit(v.comp6a,v.comp7a)
  A68 <- jaccard_cit(v.comp6a,v.comp8a)
  A69 <- jaccard_cit(v.comp6a,v.comp9a)
  A610 <- jaccard_cit(v.comp6a,v.comp10a)
  A611 <- jaccard_cit(v.comp6a,v.comp11a)
  A612 <- jaccard_cit(v.comp6a,v.comp12a)
  A613 <- jaccard_cit(v.comp6a,v.comp13a)
  A614 <- jaccard_cit(v.comp6a,v.comp14a)
  A615 <- jaccard_cit(v.comp6a,v.comp15a)
  A616 <- jaccard_cit(v.comp6a,v.comp16a)
  
  #row7
  A71 <- jaccard_cit(v.comp7a,v.comp1a)
  A72 <- jaccard_cit(v.comp7a,v.comp2a)
  A73 <- jaccard_cit(v.comp7a,v.comp3a)
  A74 <- jaccard_cit(v.comp7a,v.comp4a)
  A75 <- jaccard_cit(v.comp7a,v.comp5a)
  A76 <- jaccard_cit(v.comp7a,v.comp6a)
  A77 <- jaccard_cit(v.comp7a,v.comp7a)
  A78 <- jaccard_cit(v.comp7a,v.comp8a)
  A79 <- jaccard_cit(v.comp7a,v.comp9a)
  A710 <- jaccard_cit(v.comp7a,v.comp10a)
  A711 <- jaccard_cit(v.comp7a,v.comp11a)
  A712 <- jaccard_cit(v.comp7a,v.comp12a)
  A713 <- jaccard_cit(v.comp7a,v.comp13a)
  A714 <- jaccard_cit(v.comp7a,v.comp14a)
  A715 <- jaccard_cit(v.comp7a,v.comp15a)
  A716 <- jaccard_cit(v.comp7a,v.comp16a)
  
  #row8
  A81 <- jaccard_cit(v.comp8a,v.comp1a)
  A82 <- jaccard_cit(v.comp8a,v.comp2a)
  A83 <- jaccard_cit(v.comp8a,v.comp3a)
  A84 <- jaccard_cit(v.comp8a,v.comp4a)
  A85 <- jaccard_cit(v.comp8a,v.comp5a)
  A86 <- jaccard_cit(v.comp8a,v.comp6a)
  A87 <- jaccard_cit(v.comp8a,v.comp7a)
  A88 <- jaccard_cit(v.comp8a,v.comp8a)
  A89 <- jaccard_cit(v.comp8a,v.comp9a)
  A810 <- jaccard_cit(v.comp8a,v.comp10a)
  A811 <- jaccard_cit(v.comp8a,v.comp11a)
  A812 <- jaccard_cit(v.comp8a,v.comp12a)
  A813 <- jaccard_cit(v.comp8a,v.comp13a)
  A814 <- jaccard_cit(v.comp8a,v.comp14a)
  A815 <- jaccard_cit(v.comp8a,v.comp15a)
  A816 <- jaccard_cit(v.comp8a,v.comp16a)
  
  #row9
  A91 <- jaccard_cit(v.comp9a,v.comp1a)
  A92 <- jaccard_cit(v.comp9a,v.comp2a)
  A93 <- jaccard_cit(v.comp9a,v.comp3a)
  A94 <- jaccard_cit(v.comp9a,v.comp4a)
  A95 <- jaccard_cit(v.comp9a,v.comp5a)
  A96 <- jaccard_cit(v.comp9a,v.comp6a)
  A97 <- jaccard_cit(v.comp9a,v.comp7a)
  A98 <- jaccard_cit(v.comp9a,v.comp8a)
  A99 <- jaccard_cit(v.comp9a,v.comp9a)
  A910 <- jaccard_cit(v.comp9a,v.comp10a)
  A911 <- jaccard_cit(v.comp9a,v.comp11a)
  A912 <- jaccard_cit(v.comp9a,v.comp12a)
  A913 <- jaccard_cit(v.comp9a,v.comp13a)
  A914 <- jaccard_cit(v.comp9a,v.comp14a)
  A915 <- jaccard_cit(v.comp9a,v.comp15a)
  A916 <- jaccard_cit(v.comp9a,v.comp16a)
  
  #row10
  A101 <- jaccard_cit(v.comp10a,v.comp1a)
  A102 <- jaccard_cit(v.comp10a,v.comp2a)
  A103 <- jaccard_cit(v.comp10a,v.comp3a)
  A104 <- jaccard_cit(v.comp10a,v.comp4a)
  A105 <- jaccard_cit(v.comp10a,v.comp5a)
  A106 <- jaccard_cit(v.comp10a,v.comp6a)
  A107 <- jaccard_cit(v.comp10a,v.comp7a)
  A108 <- jaccard_cit(v.comp10a,v.comp8a)
  A109 <- jaccard_cit(v.comp10a,v.comp9a)
  A1010 <- jaccard_cit(v.comp10a,v.comp10a)
  A1011 <- jaccard_cit(v.comp10a,v.comp11a)
  A1012 <- jaccard_cit(v.comp10a,v.comp12a)
  A1013 <- jaccard_cit(v.comp10a,v.comp13a)
  A1014 <- jaccard_cit(v.comp10a,v.comp14a)
  A1015 <- jaccard_cit(v.comp10a,v.comp15a)
  A1016 <- jaccard_cit(v.comp10a,v.comp16a)
  
  #row11
  A111 <- jaccard_cit(v.comp11a,v.comp1a)
  A112 <- jaccard_cit(v.comp11a,v.comp2a)
  A113 <- jaccard_cit(v.comp11a,v.comp3a)
  A114 <- jaccard_cit(v.comp11a,v.comp4a)
  A115 <- jaccard_cit(v.comp11a,v.comp5a)
  A116 <- jaccard_cit(v.comp11a,v.comp6a)
  A117 <- jaccard_cit(v.comp11a,v.comp7a)
  A118 <- jaccard_cit(v.comp11a,v.comp8a)
  A119 <- jaccard_cit(v.comp11a,v.comp9a)
  A1110 <- jaccard_cit(v.comp11a,v.comp10a)
  A1111 <- jaccard_cit(v.comp11a,v.comp11a)
  A1112 <- jaccard_cit(v.comp11a,v.comp12a)
  A1113 <- jaccard_cit(v.comp11a,v.comp13a)
  A1114 <- jaccard_cit(v.comp11a,v.comp14a)
  A1115 <- jaccard_cit(v.comp11a,v.comp15a)
  A1116 <- jaccard_cit(v.comp11a,v.comp16a)
  
  #row12
  A121 <- jaccard_cit(v.comp12a,v.comp1a)
  A122 <- jaccard_cit(v.comp12a,v.comp2a)
  A123 <- jaccard_cit(v.comp12a,v.comp3a)
  A124 <- jaccard_cit(v.comp12a,v.comp4a)
  A125 <- jaccard_cit(v.comp12a,v.comp5a)
  A126 <- jaccard_cit(v.comp12a,v.comp6a)
  A127 <- jaccard_cit(v.comp12a,v.comp7a)
  A128 <- jaccard_cit(v.comp12a,v.comp8a)
  A129 <- jaccard_cit(v.comp12a,v.comp9a)
  A1210 <- jaccard_cit(v.comp12a,v.comp10a)
  A1211 <- jaccard_cit(v.comp12a,v.comp11a)
  A1212 <- jaccard_cit(v.comp12a,v.comp12a)
  A1213 <- jaccard_cit(v.comp12a,v.comp13a)
  A1214 <- jaccard_cit(v.comp12a,v.comp14a)
  A1215 <- jaccard_cit(v.comp12a,v.comp15a)
  A1216 <- jaccard_cit(v.comp12a,v.comp16a)
  
  #row13
  A131 <- jaccard_cit(v.comp13a,v.comp1a)
  A132 <- jaccard_cit(v.comp13a,v.comp2a)
  A133 <- jaccard_cit(v.comp13a,v.comp3a)
  A134 <- jaccard_cit(v.comp13a,v.comp4a)
  A135 <- jaccard_cit(v.comp13a,v.comp5a)
  A136 <- jaccard_cit(v.comp13a,v.comp6a)
  A137 <- jaccard_cit(v.comp13a,v.comp7a)
  A138 <- jaccard_cit(v.comp13a,v.comp8a)
  A139 <- jaccard_cit(v.comp13a,v.comp9a)
  A1310 <- jaccard_cit(v.comp13a,v.comp10a)
  A1311 <- jaccard_cit(v.comp13a,v.comp11a)
  A1312 <- jaccard_cit(v.comp13a,v.comp12a)
  A1313 <- jaccard_cit(v.comp13a,v.comp13a)
  A1314 <- jaccard_cit(v.comp13a,v.comp14a)
  A1315 <- jaccard_cit(v.comp13a,v.comp15a)
  A1316 <- jaccard_cit(v.comp13a,v.comp16a)
  
  #row14
  A141 <- jaccard_cit(v.comp14a,v.comp1a)
  A142 <- jaccard_cit(v.comp14a,v.comp2a)
  A143 <- jaccard_cit(v.comp14a,v.comp3a)
  A144 <- jaccard_cit(v.comp14a,v.comp4a)
  A145 <- jaccard_cit(v.comp14a,v.comp5a)
  A146 <- jaccard_cit(v.comp14a,v.comp6a)
  A147 <- jaccard_cit(v.comp14a,v.comp7a)
  A148 <- jaccard_cit(v.comp14a,v.comp8a)
  A149 <- jaccard_cit(v.comp14a,v.comp9a)
  A1410 <- jaccard_cit(v.comp14a,v.comp10a)
  A1411 <- jaccard_cit(v.comp14a,v.comp11a)
  A1412 <- jaccard_cit(v.comp14a,v.comp12a)
  A1413 <- jaccard_cit(v.comp14a,v.comp13a)
  A1414 <- jaccard_cit(v.comp14a,v.comp14a)
  A1415 <- jaccard_cit(v.comp14a,v.comp15a)
  A1416 <- jaccard_cit(v.comp14a,v.comp16a)
  
  #row15
  A151 <- jaccard_cit(v.comp15a,v.comp1a)
  A152 <- jaccard_cit(v.comp15a,v.comp2a)
  A153 <- jaccard_cit(v.comp15a,v.comp3a)
  A154 <- jaccard_cit(v.comp15a,v.comp4a)
  A155 <- jaccard_cit(v.comp15a,v.comp5a)
  A156 <- jaccard_cit(v.comp15a,v.comp6a)
  A157 <- jaccard_cit(v.comp15a,v.comp7a)
  A158 <- jaccard_cit(v.comp15a,v.comp8a)
  A159 <- jaccard_cit(v.comp15a,v.comp9a)
  A1510 <- jaccard_cit(v.comp15a,v.comp10a)
  A1511 <- jaccard_cit(v.comp15a,v.comp11a)
  A1512 <- jaccard_cit(v.comp15a,v.comp12a)
  A1513 <- jaccard_cit(v.comp15a,v.comp13a)
  A1514 <- jaccard_cit(v.comp15a,v.comp14a)
  A1515 <- jaccard_cit(v.comp15a,v.comp15a)
  A1516 <- jaccard_cit(v.comp15a,v.comp16a)
  
  #row16
  A161 <- jaccard_cit(v.comp16a,v.comp1a)
  A162 <- jaccard_cit(v.comp16a,v.comp2a)
  A163 <- jaccard_cit(v.comp16a,v.comp3a)
  A164 <- jaccard_cit(v.comp16a,v.comp4a)
  A165 <- jaccard_cit(v.comp16a,v.comp5a)
  A166 <- jaccard_cit(v.comp16a,v.comp6a)
  A167 <- jaccard_cit(v.comp16a,v.comp7a)
  A168 <- jaccard_cit(v.comp16a,v.comp8a)
  A169 <- jaccard_cit(v.comp16a,v.comp9a)
  A1610 <- jaccard_cit(v.comp16a,v.comp10a)
  A1611 <- jaccard_cit(v.comp16a,v.comp11a)
  A1612 <- jaccard_cit(v.comp16a,v.comp12a)
  A1613 <- jaccard_cit(v.comp16a,v.comp13a)
  A1614 <- jaccard_cit(v.comp16a,v.comp14a)
  A1615 <- jaccard_cit(v.comp16a,v.comp15a)
  A1616 <- jaccard_cit(v.comp16a,v.comp16a)
  
  EN_A <- t(matrix(c(A11  ,A12  ,A13  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,
                     A21  ,A22  ,A23  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,A214 ,A215 ,A216 ,
                     A31  ,A32  ,A33  ,A34  ,A35  ,A36  ,A37  ,A38  ,A39  ,A310 ,A311 ,A312 ,A313 ,A314 ,A315 ,A316 ,
                     A41  ,A42  ,A43  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,A414 ,A415 ,A416 ,
                     A51  ,A52  ,A53  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,A514 ,A515 ,A516 ,
                     A61  ,A62  ,A63  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,A614 ,A615 ,A616 ,
                     A71  ,A72  ,A73  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,A714 ,A715 ,A716 ,
                     A81  ,A82  ,A83  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,A814 ,A815 ,A816 ,
                     A91  ,A92  ,A93  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,A914 ,A915 ,A916 ,
                     A101 ,A102 ,A103 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,A1014,A1015,A1016,
                     A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,A1114,A1115,A1116,
                     A121 ,A122 ,A123 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,A1214,A1215,A1216,
                     A131 ,A132 ,A133 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313,A1314,A1315,A1316,
                     A141 ,A142 ,A143 ,A144 ,A145 ,A146 ,A147 ,A148 ,A149 ,A1410,A1411,A1412,A1413,A1414,A1415,A1416,
                     A151 ,A152 ,A153 ,A154 ,A155 ,A156 ,A157 ,A158 ,A159 ,A1510,A1511,A1512,A1513,A1514,A1515,A1516,
                     A161 ,A162 ,A163 ,A164 ,A165 ,A166 ,A167 ,A168 ,A169 ,A1610,A1611,A1612,A1613,A1614,A1615,A1616), nrow=16, ncol=16))
  
  return(EN_A)
  
}
m.cit <- matrix_dist(v.cit)

##James
v.cit.j <- c(v.compJ1a,v.compJ2a,v.compJ4a,v.compJ5a,v.compJ6a,v.compJ7a,v.compJ8a,v.compJ9a,v.compJ10a,v.compJ11a,v.compJ12a,v.compJ13a)
matrix_dist_j <- function (v.cit.j) {
  A11 <- jaccard_cit(v.compJ1a,v.compJ1a)
  A12 <- jaccard_cit(v.compJ1a,v.compJ2a)
  A14 <- jaccard_cit(v.compJ1a,v.compJ4a)
  A15 <- jaccard_cit(v.compJ1a,v.compJ5a)
  A16 <- jaccard_cit(v.compJ1a,v.compJ6a)
  A17 <- jaccard_cit(v.compJ1a,v.compJ7a)
  A18 <- jaccard_cit(v.compJ1a,v.compJ8a)
  A19 <- jaccard_cit(v.compJ1a,v.compJ9a)
  A110 <- jaccard_cit(v.compJ1a,v.compJ10a)
  A111 <- jaccard_cit(v.compJ1a,v.compJ11a)
  A112 <- jaccard_cit(v.compJ1a,v.compJ12a)
  A113 <- jaccard_cit(v.compJ1a,v.compJ13a)
  
  
  #row2
  A21 <- jaccard_cit(v.compJ2a,v.compJ1a)
  A22 <- jaccard_cit(v.compJ2a,v.compJ2a)
  A24 <- jaccard_cit(v.compJ2a,v.compJ4a)
  A25 <- jaccard_cit(v.compJ2a,v.compJ5a)
  A26 <- jaccard_cit(v.compJ2a,v.compJ6a)
  A27 <- jaccard_cit(v.compJ2a,v.compJ7a)
  A28 <- jaccard_cit(v.compJ2a,v.compJ8a)
  A29 <- jaccard_cit(v.compJ2a,v.compJ9a)
  A210 <- jaccard_cit(v.compJ2a,v.compJ10a)
  A211 <- jaccard_cit(v.compJ2a,v.compJ11a)
  A212 <- jaccard_cit(v.compJ2a,v.compJ12a)
  A213 <- jaccard_cit(v.compJ2a,v.compJ13a)
  
  #row4
  A41 <- jaccard_cit(v.compJ4a,v.compJ1a)
  A42 <- jaccard_cit(v.compJ4a,v.compJ2a)
  A44 <- jaccard_cit(v.compJ4a,v.compJ4a)
  A45 <- jaccard_cit(v.compJ4a,v.compJ5a)
  A46 <- jaccard_cit(v.compJ4a,v.compJ6a)
  A47 <- jaccard_cit(v.compJ4a,v.compJ7a)
  A48 <- jaccard_cit(v.compJ4a,v.compJ8a)
  A49 <- jaccard_cit(v.compJ4a,v.compJ9a)
  A410 <- jaccard_cit(v.compJ4a,v.compJ10a)
  A411 <- jaccard_cit(v.compJ4a,v.compJ11a)
  A412 <- jaccard_cit(v.compJ4a,v.compJ12a)
  A413 <- jaccard_cit(v.compJ4a,v.compJ13a)
  
  #row5
  A51 <- jaccard_cit(v.compJ5a,v.compJ1a)
  A52 <- jaccard_cit(v.compJ5a,v.compJ2a)
  A54 <- jaccard_cit(v.compJ5a,v.compJ4a)
  A55 <- jaccard_cit(v.compJ5a,v.compJ5a)
  A56 <- jaccard_cit(v.compJ5a,v.compJ6a)
  A57 <- jaccard_cit(v.compJ5a,v.compJ7a)
  A58 <- jaccard_cit(v.compJ5a,v.compJ8a)
  A59 <- jaccard_cit(v.compJ5a,v.compJ9a)
  A510 <- jaccard_cit(v.compJ5a,v.compJ10a)
  A511 <- jaccard_cit(v.compJ5a,v.compJ11a)
  A512 <- jaccard_cit(v.compJ5a,v.compJ12a)
  A513 <- jaccard_cit(v.compJ5a,v.compJ13a)
  
  #row6
  A61 <- jaccard_cit(v.compJ6a,v.compJ1a)
  A62 <- jaccard_cit(v.compJ6a,v.compJ2a)
  A64 <- jaccard_cit(v.compJ6a,v.compJ4a)
  A65 <- jaccard_cit(v.compJ6a,v.compJ5a)
  A66 <- jaccard_cit(v.compJ6a,v.compJ6a)
  A67 <- jaccard_cit(v.compJ6a,v.compJ7a)
  A68 <- jaccard_cit(v.compJ6a,v.compJ8a)
  A69 <- jaccard_cit(v.compJ6a,v.compJ9a)
  A610 <- jaccard_cit(v.compJ6a,v.compJ10a)
  A611 <- jaccard_cit(v.compJ6a,v.compJ11a)
  A612 <- jaccard_cit(v.compJ6a,v.compJ12a)
  A613 <- jaccard_cit(v.compJ6a,v.compJ13a)
  
  #row7
  A71 <- jaccard_cit(v.compJ7a,v.compJ1a)
  A72 <- jaccard_cit(v.compJ7a,v.compJ2a)
  A74 <- jaccard_cit(v.compJ7a,v.compJ4a)
  A75 <- jaccard_cit(v.compJ7a,v.compJ5a)
  A76 <- jaccard_cit(v.compJ7a,v.compJ6a)
  A77 <- jaccard_cit(v.compJ7a,v.compJ7a)
  A78 <- jaccard_cit(v.compJ7a,v.compJ8a)
  A79 <- jaccard_cit(v.compJ7a,v.compJ9a)
  A710 <- jaccard_cit(v.compJ7a,v.compJ10a)
  A711 <- jaccard_cit(v.compJ7a,v.compJ11a)
  A712 <- jaccard_cit(v.compJ7a,v.compJ12a)
  A713 <- jaccard_cit(v.compJ7a,v.compJ13a)
  
  #row8
  A81 <- jaccard_cit(v.compJ8a,v.compJ1a)
  A82 <- jaccard_cit(v.compJ8a,v.compJ2a)
  A84 <- jaccard_cit(v.compJ8a,v.compJ4a)
  A85 <- jaccard_cit(v.compJ8a,v.compJ5a)
  A86 <- jaccard_cit(v.compJ8a,v.compJ6a)
  A87 <- jaccard_cit(v.compJ8a,v.compJ7a)
  A88 <- jaccard_cit(v.compJ8a,v.compJ8a)
  A89 <- jaccard_cit(v.compJ8a,v.compJ9a)
  A810 <- jaccard_cit(v.compJ8a,v.compJ10a)
  A811 <- jaccard_cit(v.compJ8a,v.compJ11a)
  A812 <- jaccard_cit(v.compJ8a,v.compJ12a)
  A813 <- jaccard_cit(v.compJ8a,v.compJ13a)
  
  #row9
  A91 <- jaccard_cit(v.compJ9a,v.compJ1a)
  A92 <- jaccard_cit(v.compJ9a,v.compJ2a)
  A94 <- jaccard_cit(v.compJ9a,v.compJ4a)
  A95 <- jaccard_cit(v.compJ9a,v.compJ5a)
  A96 <- jaccard_cit(v.compJ9a,v.compJ6a)
  A97 <- jaccard_cit(v.compJ9a,v.compJ7a)
  A98 <- jaccard_cit(v.compJ9a,v.compJ8a)
  A99 <- jaccard_cit(v.compJ9a,v.compJ9a)
  A910 <- jaccard_cit(v.compJ9a,v.compJ10a)
  A911 <- jaccard_cit(v.compJ9a,v.compJ11a)
  A912 <- jaccard_cit(v.compJ9a,v.compJ12a)
  A913 <- jaccard_cit(v.compJ9a,v.compJ13a)
  
  #row10
  A101 <- jaccard_cit(v.compJ10a,v.compJ1a)
  A102 <- jaccard_cit(v.compJ10a,v.compJ2a)
  A104 <- jaccard_cit(v.compJ10a,v.compJ4a)
  A105 <- jaccard_cit(v.compJ10a,v.compJ5a)
  A106 <- jaccard_cit(v.compJ10a,v.compJ6a)
  A107 <- jaccard_cit(v.compJ10a,v.compJ7a)
  A108 <- jaccard_cit(v.compJ10a,v.compJ8a)
  A109 <- jaccard_cit(v.compJ10a,v.compJ9a)
  A1010 <- jaccard_cit(v.compJ10a,v.compJ10a)
  A1011 <- jaccard_cit(v.compJ10a,v.compJ11a)
  A1012 <- jaccard_cit(v.compJ10a,v.compJ12a)
  A1013 <- jaccard_cit(v.compJ10a,v.compJ13a)
  
  #row11
  A111 <- jaccard_cit(v.compJ11a,v.compJ1a)
  A112 <- jaccard_cit(v.compJ11a,v.compJ2a)
  A114 <- jaccard_cit(v.compJ11a,v.compJ4a)
  A115 <- jaccard_cit(v.compJ11a,v.compJ5a)
  A116 <- jaccard_cit(v.compJ11a,v.compJ6a)
  A117 <- jaccard_cit(v.compJ11a,v.compJ7a)
  A118 <- jaccard_cit(v.compJ11a,v.compJ8a)
  A119 <- jaccard_cit(v.compJ11a,v.compJ9a)
  A1110 <- jaccard_cit(v.compJ11a,v.compJ10a)
  A1111 <- jaccard_cit(v.compJ11a,v.compJ11a)
  A1112 <- jaccard_cit(v.compJ11a,v.compJ12a)
  A1113 <- jaccard_cit(v.compJ11a,v.compJ13a)
  
  #row12
  A121 <- jaccard_cit(v.compJ12a,v.compJ1a)
  A122 <- jaccard_cit(v.compJ12a,v.compJ2a)
  A124 <- jaccard_cit(v.compJ12a,v.compJ4a)
  A125 <- jaccard_cit(v.compJ12a,v.compJ5a)
  A126 <- jaccard_cit(v.compJ12a,v.compJ6a)
  A127 <- jaccard_cit(v.compJ12a,v.compJ7a)
  A128 <- jaccard_cit(v.compJ12a,v.compJ8a)
  A129 <- jaccard_cit(v.compJ12a,v.compJ9a)
  A1210 <- jaccard_cit(v.compJ12a,v.compJ10a)
  A1211 <- jaccard_cit(v.compJ12a,v.compJ11a)
  A1212 <- jaccard_cit(v.compJ12a,v.compJ12a)
  A1213 <- jaccard_cit(v.compJ12a,v.compJ13a)
  
  #row13
  A131 <- jaccard_cit(v.compJ13a,v.compJ1a)
  A132 <- jaccard_cit(v.compJ13a,v.compJ2a)
  A134 <- jaccard_cit(v.compJ13a,v.compJ4a)
  A135 <- jaccard_cit(v.compJ13a,v.compJ5a)
  A136 <- jaccard_cit(v.compJ13a,v.compJ6a)
  A137 <- jaccard_cit(v.compJ13a,v.compJ7a)
  A138 <- jaccard_cit(v.compJ13a,v.compJ8a)
  A139 <- jaccard_cit(v.compJ13a,v.compJ9a)
  A1310 <- jaccard_cit(v.compJ13a,v.compJ10a)
  A1311 <- jaccard_cit(v.compJ13a,v.compJ11a)
  A1312 <- jaccard_cit(v.compJ13a,v.compJ12a)
  A1313 <- jaccard_cit(v.compJ13a,v.compJ13a)
  
  EN_J <- t(matrix(c(A11  ,A12  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,
                     A21  ,A22  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,
                     A41  ,A42  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,
                     A51  ,A52  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,
                     A61  ,A62  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,
                     A71  ,A72  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,
                     A81  ,A82  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,
                     A91  ,A92  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,
                     A101 ,A102 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,
                     A111 ,A112 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,
                     A121 ,A122 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,
                     A131 ,A132 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313), nrow=12, ncol=12))
  
  return(EN_J)
  
}
m.cit.j <- matrix_dist_j(v.cit.j)

#cpc
##Rowles
v.cpc <- c(v.cpc_comp1a,v.cpc_comp2a,v.cpc_comp3a,v.cpc_comp4a,v.cpc_comp5a,v.cpc_comp6a,v.cpc_comp7a,v.cpc_comp8a,v.cpc_comp9a,v.cpc_comp10a,v.cpc_comp11a,v.cpc_comp12a,v.cpc_comp13a,v.cpc_comp14a,v.cpc_comp15a,v.cpc_comp16a)
matrix_dist_y <- function (v.cpc, y) {
  A11 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp1a, y)
  A12 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp2a, y)
  A13 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp3a, y)
  A14 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp4a, y)
  A15 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp5a, y)
  A16 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp6a, y)
  A17 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp7a, y)
  A18 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp8a, y)
  A19 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp9a, y)
  A110 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp10a, y)
  A111 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp11a, y)
  A112 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp12a, y)
  A113 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp13a, y)
  A114 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp14a, y)
  A115 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp15a, y)
  A116 <- jaccard_cpc(v.cpc_comp1a,v.cpc_comp16a, y)
  
  #row2
  A21 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp1a, y)
  A22 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp2a, y)
  A23 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp3a, y)
  A24 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp4a, y)
  A25 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp5a, y)
  A26 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp6a, y)
  A27 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp7a, y)
  A28 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp8a, y)
  A29 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp9a, y)
  A210 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp10a, y)
  A211 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp11a, y)
  A212 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp12a, y)
  A213 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp13a, y)
  A214 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp14a, y)
  A215 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp15a, y)
  A216 <- jaccard_cpc(v.cpc_comp2a,v.cpc_comp16a, y)
  
  #row3
  A31 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp1a, y)
  A32 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp2a, y)
  A33 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp3a, y)
  A34 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp4a, y)
  A35 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp5a, y)
  A36 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp6a, y)
  A37 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp7a, y)
  A38 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp8a, y)
  A39 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp9a, y)
  A310 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp10a, y)
  A311 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp11a, y)
  A312 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp12a, y)
  A313 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp13a, y)
  A314 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp14a, y)
  A315 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp15a, y)
  A316 <- jaccard_cpc(v.cpc_comp3a,v.cpc_comp16a, y)
  
  #row4
  A41 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp1a, y)
  A42 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp2a, y)
  A43 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp3a, y)
  A44 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp4a, y)
  A45 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp5a, y)
  A46 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp6a, y)
  A47 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp7a, y)
  A48 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp8a, y)
  A49 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp9a, y)
  A410 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp10a, y)
  A411 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp11a, y)
  A412 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp12a, y)
  A413 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp13a, y)
  A414 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp14a, y)
  A415 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp15a, y)
  A416 <- jaccard_cpc(v.cpc_comp4a,v.cpc_comp16a, y)
  
  #row5
  A51 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp1a, y)
  A52 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp2a, y)
  A53 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp3a, y)
  A54 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp4a, y)
  A55 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp5a, y)
  A56 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp6a, y)
  A57 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp7a, y)
  A58 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp8a, y)
  A59 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp9a, y)
  A510 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp10a, y)
  A511 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp11a, y)
  A512 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp12a, y)
  A513 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp13a, y)
  A514 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp14a, y)
  A515 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp15a, y)
  A516 <- jaccard_cpc(v.cpc_comp5a,v.cpc_comp16a, y)
  
  #row6
  A61 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp1a, y)
  A62 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp2a, y)
  A63 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp3a, y)
  A64 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp4a, y)
  A65 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp5a, y)
  A66 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp6a, y)
  A67 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp7a, y)
  A68 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp8a, y)
  A69 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp9a, y)
  A610 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp10a, y)
  A611 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp11a, y)
  A612 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp12a, y)
  A613 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp13a, y)
  A614 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp14a, y)
  A615 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp15a, y)
  A616 <- jaccard_cpc(v.cpc_comp6a,v.cpc_comp16a, y)
  
  #row7
  A71 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp1a, y)
  A72 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp2a, y)
  A73 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp3a, y)
  A74 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp4a, y)
  A75 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp5a, y)
  A76 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp6a, y)
  A77 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp7a, y)
  A78 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp8a, y)
  A79 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp9a, y)
  A710 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp10a, y)
  A711 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp11a, y)
  A712 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp12a, y)
  A713 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp13a, y)
  A714 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp14a, y)
  A715 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp15a, y)
  A716 <- jaccard_cpc(v.cpc_comp7a,v.cpc_comp16a, y)
  
  #row8
  A81 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp1a, y)
  A82 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp2a, y)
  A83 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp3a, y)
  A84 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp4a, y)
  A85 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp5a, y)
  A86 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp6a, y)
  A87 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp7a, y)
  A88 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp8a, y)
  A89 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp9a, y)
  A810 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp10a, y)
  A811 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp11a, y)
  A812 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp12a, y)
  A813 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp13a, y)
  A814 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp14a, y)
  A815 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp15a, y)
  A816 <- jaccard_cpc(v.cpc_comp8a,v.cpc_comp16a, y)
  
  #row9
  A91 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp1a, y)
  A92 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp2a, y)
  A93 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp3a, y)
  A94 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp4a, y)
  A95 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp5a, y)
  A96 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp6a, y)
  A97 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp7a, y)
  A98 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp8a, y)
  A99 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp9a, y)
  A910 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp10a, y)
  A911 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp11a, y)
  A912 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp12a, y)
  A913 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp13a, y)
  A914 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp14a, y)
  A915 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp15a, y)
  A916 <- jaccard_cpc(v.cpc_comp9a,v.cpc_comp16a, y)
  
  #row10
  A101 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp1a, y)
  A102 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp2a, y)
  A103 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp3a, y)
  A104 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp4a, y)
  A105 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp5a, y)
  A106 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp6a, y)
  A107 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp7a, y)
  A108 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp8a, y)
  A109 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp9a, y)
  A1010 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp10a, y)
  A1011 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp11a, y)
  A1012 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp12a, y)
  A1013 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp13a, y)
  A1014 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp14a, y)
  A1015 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp15a, y)
  A1016 <- jaccard_cpc(v.cpc_comp10a,v.cpc_comp16a, y)
  
  #row11
  A111 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp1a, y)
  A112 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp2a, y)
  A113 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp3a, y)
  A114 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp4a, y)
  A115 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp5a, y)
  A116 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp6a, y)
  A117 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp7a, y)
  A118 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp8a, y)
  A119 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp9a, y)
  A1110 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp10a, y)
  A1111 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp11a, y)
  A1112 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp12a, y)
  A1113 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp13a, y)
  A1114 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp14a, y)
  A1115 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp15a, y)
  A1116 <- jaccard_cpc(v.cpc_comp11a,v.cpc_comp16a, y)
  
  #row12
  A121 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp1a, y)
  A122 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp2a, y)
  A123 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp3a, y)
  A124 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp4a, y)
  A125 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp5a, y)
  A126 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp6a, y)
  A127 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp7a, y)
  A128 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp8a, y)
  A129 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp9a, y)
  A1210 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp10a, y)
  A1211 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp11a, y)
  A1212 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp12a, y)
  A1213 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp13a, y)
  A1214 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp14a, y)
  A1215 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp15a, y)
  A1216 <- jaccard_cpc(v.cpc_comp12a,v.cpc_comp16a, y)
  
  #row13
  A131 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp1a, y)
  A132 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp2a, y)
  A133 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp3a, y)
  A134 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp4a, y)
  A135 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp5a, y)
  A136 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp6a, y)
  A137 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp7a, y)
  A138 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp8a, y)
  A139 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp9a, y)
  A1310 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp10a, y)
  A1311 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp11a, y)
  A1312 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp12a, y)
  A1313 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp13a, y)
  A1314 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp14a, y)
  A1315 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp15a, y)
  A1316 <- jaccard_cpc(v.cpc_comp13a,v.cpc_comp16a, y)
  
  #row14
  A141 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp1a, y)
  A142 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp2a, y)
  A143 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp3a, y)
  A144 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp4a, y)
  A145 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp5a, y)
  A146 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp6a, y)
  A147 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp7a, y)
  A148 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp8a, y)
  A149 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp9a, y)
  A1410 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp10a, y)
  A1411 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp11a, y)
  A1412 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp12a, y)
  A1413 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp13a, y)
  A1414 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp14a, y)
  A1415 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp15a, y)
  A1416 <- jaccard_cpc(v.cpc_comp14a,v.cpc_comp16a, y)
  
  #row15
  A151 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp1a, y)
  A152 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp2a, y)
  A153 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp3a, y)
  A154 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp4a, y)
  A155 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp5a, y)
  A156 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp6a, y)
  A157 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp7a, y)
  A158 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp8a, y)
  A159 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp9a, y)
  A1510 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp10a, y)
  A1511 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp11a, y)
  A1512 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp12a, y)
  A1513 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp13a, y)
  A1514 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp14a, y)
  A1515 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp15a, y)
  A1516 <- jaccard_cpc(v.cpc_comp15a,v.cpc_comp16a, y)
  
  #row16
  A161 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp1a, y)
  A162 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp2a, y)
  A163 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp3a, y)
  A164 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp4a, y)
  A165 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp5a, y)
  A166 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp6a, y)
  A167 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp7a, y)
  A168 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp8a, y)
  A169 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp9a, y)
  A1610 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp10a, y)
  A1611 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp11a, y)
  A1612 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp12a, y)
  A1613 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp13a, y)
  A1614 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp14a, y)
  A1615 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp15a, y)
  A1616 <- jaccard_cpc(v.cpc_comp16a,v.cpc_comp16a, y)
  
  EN_A <- t(matrix(c(A11  ,A12  ,A13  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,
                     A21  ,A22  ,A23  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,A214 ,A215 ,A216 ,
                     A31  ,A32  ,A33  ,A34  ,A35  ,A36  ,A37  ,A38  ,A39  ,A310 ,A311 ,A312 ,A313 ,A314 ,A315 ,A316 ,
                     A41  ,A42  ,A43  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,A414 ,A415 ,A416 ,
                     A51  ,A52  ,A53  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,A514 ,A515 ,A516 ,
                     A61  ,A62  ,A63  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,A614 ,A615 ,A616 ,
                     A71  ,A72  ,A73  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,A714 ,A715 ,A716 ,
                     A81  ,A82  ,A83  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,A814 ,A815 ,A816 ,
                     A91  ,A92  ,A93  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,A914 ,A915 ,A916 ,
                     A101 ,A102 ,A103 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,A1014,A1015,A1016,
                     A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,A1114,A1115,A1116,
                     A121 ,A122 ,A123 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,A1214,A1215,A1216,
                     A131 ,A132 ,A133 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313,A1314,A1315,A1316,
                     A141 ,A142 ,A143 ,A144 ,A145 ,A146 ,A147 ,A148 ,A149 ,A1410,A1411,A1412,A1413,A1414,A1415,A1416,
                     A151 ,A152 ,A153 ,A154 ,A155 ,A156 ,A157 ,A158 ,A159 ,A1510,A1511,A1512,A1513,A1514,A1515,A1516,
                     A161 ,A162 ,A163 ,A164 ,A165 ,A166 ,A167 ,A168 ,A169 ,A1610,A1611,A1612,A1613,A1614,A1615,A1616), nrow=16, ncol=16))
  
  return(EN_A)
  
}
m.cpc <- matrix_dist_y(v.cpc, 2015)

##James
v.cpc.j <- c(v.cpc_compJ1a,v.cpc_compJ2a,v.cpc_compJ4a,v.cpc_compJ5a,v.cpc_compJ6a,v.cpc_compJ7a,v.cpc_compJ8a,v.cpc_compJ9a,v.cpc_compJ10a,v.cpc_compJ11a,v.cpc_compJ12a,v.cpc_compJ13a)
matrix_dist_y_j <- function (v.cpc.j, y) {
  A11 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ1a, y)
  A12 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ2a, y)
  A14 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ4a, y)
  A15 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ5a, y)
  A16 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ6a, y)
  A17 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ7a, y)
  A18 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ8a, y)
  A19 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ9a, y)
  A110 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ10a, y)
  A111 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ11a, y)
  A112 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ12a, y)
  A113 <- jaccard_cpc(v.cpc_compJ1a,v.cpc_compJ13a, y)
  
  #row2
  A21 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ1a, y)
  A22 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ2a, y)
  A24 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ4a, y)
  A25 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ5a, y)
  A26 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ6a, y)
  A27 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ7a, y)
  A28 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ8a, y)
  A29 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ9a, y)
  A210 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ10a, y)
  A211 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ11a, y)
  A212 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ12a, y)
  A213 <- jaccard_cpc(v.cpc_compJ2a,v.cpc_compJ13a, y)
  
  #row4
  A41 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ1a, y)
  A42 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ2a, y)
  A44 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ4a, y)
  A45 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ5a, y)
  A46 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ6a, y)
  A47 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ7a, y)
  A48 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ8a, y)
  A49 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ9a, y)
  A410 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ10a, y)
  A411 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ11a, y)
  A412 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ12a, y)
  A413 <- jaccard_cpc(v.cpc_compJ4a,v.cpc_compJ13a, y)
  
  #row5
  A51 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ1a, y)
  A52 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ2a, y)
  
  A54 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ4a, y)
  A55 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ5a, y)
  A56 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ6a, y)
  A57 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ7a, y)
  A58 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ8a, y)
  A59 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ9a, y)
  A510 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ10a, y)
  A511 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ11a, y)
  A512 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ12a, y)
  A513 <- jaccard_cpc(v.cpc_compJ5a,v.cpc_compJ13a, y)
  
  #row6
  A61 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ1a, y)
  A62 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ2a, y)
  
  A64 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ4a, y)
  A65 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ5a, y)
  A66 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ6a, y)
  A67 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ7a, y)
  A68 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ8a, y)
  A69 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ9a, y)
  A610 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ10a, y)
  A611 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ11a, y)
  A612 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ12a, y)
  A613 <- jaccard_cpc(v.cpc_compJ6a,v.cpc_compJ13a, y)
  
  #row7
  A71 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ1a, y)
  A72 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ2a, y)
  
  A74 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ4a, y)
  A75 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ5a, y)
  A76 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ6a, y)
  A77 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ7a, y)
  A78 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ8a, y)
  A79 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ9a, y)
  A710 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ10a, y)
  A711 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ11a, y)
  A712 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ12a, y)
  A713 <- jaccard_cpc(v.cpc_compJ7a,v.cpc_compJ13a, y)
  
  #row8
  A81 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ1a, y)
  A82 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ2a, y)
  
  A84 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ4a, y)
  A85 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ5a, y)
  A86 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ6a, y)
  A87 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ7a, y)
  A88 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ8a, y)
  A89 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ9a, y)
  A810 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ10a, y)
  A811 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ11a, y)
  A812 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ12a, y)
  A813 <- jaccard_cpc(v.cpc_compJ8a,v.cpc_compJ13a, y)
  
  #row9
  A91 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ1a, y)
  A92 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ2a, y)
  
  A94 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ4a, y)
  A95 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ5a, y)
  A96 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ6a, y)
  A97 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ7a, y)
  A98 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ8a, y)
  A99 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ9a, y)
  A910 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ10a, y)
  A911 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ11a, y)
  A912 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ12a, y)
  A913 <- jaccard_cpc(v.cpc_compJ9a,v.cpc_compJ13a, y)
  
  #row10
  A101 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ1a, y)
  A102 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ2a, y)
  
  A104 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ4a, y)
  A105 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ5a, y)
  A106 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ6a, y)
  A107 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ7a, y)
  A108 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ8a, y)
  A109 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ9a, y)
  A1010 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ10a, y)
  A1011 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ11a, y)
  A1012 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ12a, y)
  A1013 <- jaccard_cpc(v.cpc_compJ10a,v.cpc_compJ13a, y)
  
  #row11
  A111 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ1a, y)
  A112 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ2a, y)
  
  A114 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ4a, y)
  A115 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ5a, y)
  A116 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ6a, y)
  A117 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ7a, y)
  A118 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ8a, y)
  A119 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ9a, y)
  A1110 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ10a, y)
  A1111 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ11a, y)
  A1112 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ12a, y)
  A1113 <- jaccard_cpc(v.cpc_compJ11a,v.cpc_compJ13a, y)
  
  #row12
  A121 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ1a, y)
  A122 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ2a, y)
  
  A124 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ4a, y)
  A125 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ5a, y)
  A126 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ6a, y)
  A127 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ7a, y)
  A128 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ8a, y)
  A129 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ9a, y)
  A1210 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ10a, y)
  A1211 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ11a, y)
  A1212 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ12a, y)
  A1213 <- jaccard_cpc(v.cpc_compJ12a,v.cpc_compJ13a, y)
  
  #row13
  A131 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ1a, y)
  A132 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ2a, y)
  
  A134 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ4a, y)
  A135 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ5a, y)
  A136 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ6a, y)
  A137 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ7a, y)
  A138 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ8a, y)
  A139 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ9a, y)
  A1310 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ10a, y)
  A1311 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ11a, y)
  A1312 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ12a, y)
  A1313 <- jaccard_cpc(v.cpc_compJ13a,v.cpc_compJ13a, y)
  
  
  EN_J <- t(matrix(c(A11  ,A12  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,
                     A21  ,A22  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,
                     A41  ,A42  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,
                     A51  ,A52  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,
                     A61  ,A62  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,
                     A71  ,A72  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,
                     A81  ,A82  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,
                     A91  ,A92  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,
                     A101 ,A102 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,
                     A111 ,A112 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,
                     A121 ,A122 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,
                     A131 ,A132 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313), nrow=12, ncol=12))
  
  return(EN_J)
  
}
m.cpc.j <- matrix_dist_y_j(v.cpc.j, 2015)

#cpc frequencies
comp1a = read_excel("comp1a_cpc.xlsx")
comp2a = read_excel("comp2a_cpc.xlsx")
comp3a = read_excel("comp3a_cpc.xlsx")
comp4a = read_excel("comp4a_cpc.xlsx")
comp5a = read_excel("comp5a_cpc.xlsx")
comp6a = read_excel("comp6a_cpc.xlsx")
comp7a = read_excel("comp7a_cpc.xlsx")
comp8a = read_excel("comp8a_cpc.xlsx")
comp9a = read_excel("comp9a_cpc.xlsx")
comp10a = read_excel("comp10a_cpc.xlsx")
comp11a = read_excel("comp11a_cpc.xlsx")
comp12a = read_excel("comp12a_cpc.xlsx")
comp13a = read_excel("comp13a_cpc.xlsx")
comp14a = read_excel("comp14a_cpc.xlsx")
comp15a = read_excel("comp15a_cpc.xlsx")
comp16a = read_excel("comp16a_cpc.xlsx")


v.comp <- c(comp1a,comp2a,comp3a,comp4a,comp5a,comp6a,comp7a,comp8a,comp9a,comp10a,comp11a,comp12a,comp13a,comp14a,comp15a,comp16a)
v.cpc.freq <- c(cpc_freq_comp1a,cpc_freq_comp2a,cpc_freq_comp3a,cpc_freq_comp4a,cpc_freq_comp5a,cpc_freq_comp6a,cpc_freq_comp7a,cpc_freq_comp8a,cpc_freq_comp9a,cpc_freq_comp10a,cpc_freq_comp11a,cpc_freq_comp12a,cpc_freq_comp13a,cpc_freq_comp14a,cpc_freq_comp15a,cpc_freq_comp16a)
matrix_dist_cos <- function (v.cpc.freq, v.comp) {
  A11 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp1a, comp1a, comp1a)
  A12 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp2a, comp1a, comp2a)
  A13 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp3a, comp1a, comp3a)
  A14 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp4a, comp1a, comp4a)
  A15 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp5a, comp1a, comp5a)
  A16 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp6a, comp1a, comp6a)
  A17 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp7a, comp1a, comp7a)
  A18 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp8a, comp1a, comp8a)
  A19 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp9a, comp1a, comp9a)
  A110 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp10a, comp1a, comp10a)
  A111 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp11a, comp1a, comp11a)
  A112 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp12a, comp1a, comp12a)
  A113 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp13a, comp1a, comp13a)
  A114 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp14a, comp1a, comp14a)
  A115 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp15a, comp1a, comp15a)
  A116 <- jaccard_cpc_cos(cpc_freq_comp1a,cpc_freq_comp16a, comp1a, comp16a)
  
  #row2
  A21 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp1a, comp2a, comp1a)
  A22 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp2a, comp2a, comp2a)
  A23 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp3a, comp2a, comp3a)
  A24 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp4a, comp2a, comp4a)
  A25 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp5a, comp2a, comp5a)
  A26 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp6a, comp2a, comp6a)
  A27 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp7a, comp2a, comp7a)
  A28 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp8a, comp2a, comp8a)
  A29 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp9a, comp2a, comp9a)
  A210 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp10a, comp2a, comp10a)
  A211 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp11a, comp2a, comp11a)
  A212 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp12a, comp2a, comp12a)
  A213 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp13a, comp2a, comp13a)
  A214 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp14a, comp2a, comp14a)
  A215 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp15a, comp2a, comp15a)
  A216 <- jaccard_cpc_cos(cpc_freq_comp2a,cpc_freq_comp16a, comp2a, comp16a)
  
  #row3
  A31 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp1a, comp3a, comp1a)
  A32 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp2a, comp3a, comp2a)
  A33 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp3a, comp3a, comp3a)
  A34 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp4a, comp3a, comp4a)
  A35 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp5a, comp3a, comp5a)
  A36 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp6a, comp3a, comp6a)
  A37 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp7a, comp3a, comp7a)
  A38 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp8a, comp3a, comp8a)
  A39 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp9a, comp3a, comp9a)
  A310 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp10a, comp3a, comp10a)
  A311 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp11a, comp3a, comp11a)
  A312 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp12a, comp3a, comp12a)
  A313 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp13a, comp3a, comp13a)
  A314 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp14a, comp3a, comp14a)
  A315 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp15a, comp3a, comp15a)
  A316 <- jaccard_cpc_cos(cpc_freq_comp3a,cpc_freq_comp16a, comp3a, comp16a)
  
  #row4
  A41 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp1a, comp4a, comp1a)
  A42 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp2a, comp4a, comp2a)
  A43 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp3a, comp4a, comp3a)
  A44 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp4a, comp4a, comp4a)
  A45 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp5a, comp4a, comp5a)
  A46 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp6a, comp4a, comp6a)
  A47 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp7a, comp4a, comp7a)
  A48 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp8a, comp4a, comp8a)
  A49 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp9a, comp4a, comp9a)
  A410 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp10a, comp4a, comp10a)
  A411 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp11a, comp4a, comp11a)
  A412 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp12a, comp4a, comp12a)
  A413 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp13a, comp4a, comp13a)
  A414 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp14a, comp4a, comp14a)
  A415 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp15a, comp4a, comp15a)
  A416 <- jaccard_cpc_cos(cpc_freq_comp4a,cpc_freq_comp16a, comp4a, comp16a)
  
  #row5
  A51 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp1a, comp5a, comp1a)
  A52 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp2a, comp5a, comp2a)
  A53 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp3a, comp5a, comp3a)
  A54 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp4a, comp5a, comp4a)
  A55 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp5a, comp5a, comp5a)
  A56 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp6a, comp5a, comp6a)
  A57 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp7a, comp5a, comp7a)
  A58 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp8a, comp5a, comp8a)
  A59 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp9a, comp5a, comp9a)
  A510 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp10a, comp5a, comp10a)
  A511 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp11a, comp5a, comp11a)
  A512 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp12a, comp5a, comp12a)
  A513 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp13a, comp5a, comp13a)
  A514 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp14a, comp5a, comp14a)
  A515 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp15a, comp5a, comp15a)
  A516 <- jaccard_cpc_cos(cpc_freq_comp5a,cpc_freq_comp16a, comp5a, comp16a)
  
  #row6
  A61 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp1a, comp6a, comp1a)
  A62 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp2a, comp6a, comp2a)
  A63 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp3a, comp6a, comp3a)
  A64 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp4a, comp6a, comp4a)
  A65 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp5a, comp6a, comp5a)
  A66 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp6a, comp6a, comp6a)
  A67 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp7a, comp6a, comp7a)
  A68 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp8a, comp6a, comp8a)
  A69 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp9a, comp6a, comp9a)
  A610 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp10a, comp6a, comp10a)
  A611 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp11a, comp6a, comp11a)
  A612 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp12a, comp6a, comp12a)
  A613 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp13a, comp6a, comp13a)
  A614 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp14a, comp6a, comp14a)
  A615 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp15a, comp6a, comp15a)
  A616 <- jaccard_cpc_cos(cpc_freq_comp6a,cpc_freq_comp16a, comp6a, comp16a)
  
  #row7
  A71 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp1a, comp7a, comp1a)
  A72 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp2a, comp7a, comp2a)
  A73 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp3a, comp7a, comp3a)
  A74 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp4a, comp7a, comp4a)
  A75 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp5a, comp7a, comp5a)
  A76 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp6a, comp7a, comp6a)
  A77 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp7a, comp7a, comp7a)
  A78 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp8a, comp7a, comp8a)
  A79 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp9a, comp7a, comp9a)
  A710 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp10a, comp7a, comp10a)
  A711 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp11a, comp7a, comp11a)
  A712 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp12a, comp7a, comp12a)
  A713 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp13a, comp7a, comp13a)
  A714 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp14a, comp7a, comp14a)
  A715 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp15a, comp7a, comp15a)
  A716 <- jaccard_cpc_cos(cpc_freq_comp7a,cpc_freq_comp16a, comp7a, comp16a)
  
  #row8
  A81 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp1a, comp8a, comp1a)
  A82 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp2a, comp8a, comp2a)
  A83 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp3a, comp8a, comp3a)
  A84 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp4a, comp8a, comp4a)
  A85 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp5a, comp8a, comp5a)
  A86 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp6a, comp8a, comp6a)
  A87 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp7a, comp8a, comp7a)
  A88 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp8a, comp8a, comp8a)
  A89 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp9a, comp8a, comp9a)
  A810 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp10a, comp8a, comp10a)
  A811 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp11a, comp8a, comp11a)
  A812 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp12a, comp8a, comp12a)
  A813 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp13a, comp8a, comp13a)
  A814 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp14a, comp8a, comp14a)
  A815 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp15a, comp8a, comp15a)
  A816 <- jaccard_cpc_cos(cpc_freq_comp8a,cpc_freq_comp16a, comp8a, comp16a)
  
  #row9
  A91 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp1a, comp9a, comp1a)
  A92 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp2a, comp9a, comp2a)
  A93 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp3a, comp9a, comp3a)
  A94 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp4a, comp9a, comp4a)
  A95 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp5a, comp9a, comp5a)
  A96 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp6a, comp9a, comp6a)
  A97 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp7a, comp9a, comp7a)
  A98 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp8a, comp9a, comp8a)
  A99 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp9a, comp9a, comp9a)
  A910 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp10a, comp9a, comp10a)
  A911 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp11a, comp9a, comp11a)
  A912 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp12a, comp9a, comp12a)
  A913 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp13a, comp9a, comp13a)
  A914 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp14a, comp9a, comp14a)
  A915 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp15a, comp9a, comp15a)
  A916 <- jaccard_cpc_cos(cpc_freq_comp9a,cpc_freq_comp16a, comp9a, comp16a)
  
  #row10
  A101 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp1a, comp10a, comp1a)
  A102 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp2a, comp10a, comp2a)
  A103 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp3a, comp10a, comp3a)
  A104 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp4a, comp10a, comp4a)
  A105 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp5a, comp10a, comp5a)
  A106 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp6a, comp10a, comp6a)
  A107 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp7a, comp10a, comp7a)
  A108 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp8a, comp10a, comp8a)
  A109 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp9a, comp10a, comp9a)
  A1010 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp10a, comp10a, comp10a)
  A1011 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp11a, comp10a, comp11a)
  A1012 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp12a, comp10a, comp12a)
  A1013 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp13a, comp10a, comp13a)
  A1014 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp14a, comp10a, comp14a)
  A1015 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp15a, comp10a, comp15a)
  A1016 <- jaccard_cpc_cos(cpc_freq_comp10a,cpc_freq_comp16a, comp10a, comp16a)
  
  #row11
  A111 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp1a, comp11a, comp1a)
  A112 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp2a, comp11a, comp2a)
  A113 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp3a, comp11a, comp3a)
  A114 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp4a, comp11a, comp4a)
  A115 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp5a, comp11a, comp5a)
  A116 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp6a, comp11a, comp6a)
  A117 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp7a, comp11a, comp7a)
  A118 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp8a, comp11a, comp8a)
  A119 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp9a, comp11a, comp9a)
  A1110 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp10a, comp11a, comp10a)
  A1111 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp11a, comp11a, comp11a)
  A1112 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp12a, comp11a, comp12a)
  A1113 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp13a, comp11a, comp13a)
  A1114 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp14a, comp11a, comp14a)
  A1115 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp15a, comp11a, comp15a)
  A1116 <- jaccard_cpc_cos(cpc_freq_comp11a,cpc_freq_comp16a, comp11a, comp16a)
  
  #row12
  A121 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp1a, comp12a, comp1a)
  A122 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp2a, comp12a, comp2a)
  A123 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp3a, comp12a, comp3a)
  A124 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp4a, comp12a, comp4a)
  A125 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp5a, comp12a, comp5a)
  A126 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp6a, comp12a, comp6a)
  A127 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp7a, comp12a, comp7a)
  A128 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp8a, comp12a, comp8a)
  A129 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp9a, comp12a, comp9a)
  A1210 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp10a, comp12a, comp10a)
  A1211 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp11a, comp12a, comp11a)
  A1212 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp12a, comp12a, comp12a)
  A1213 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp13a, comp12a, comp13a)
  A1214 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp14a, comp12a, comp14a)
  A1215 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp15a, comp12a, comp15a)
  A1216 <- jaccard_cpc_cos(cpc_freq_comp12a,cpc_freq_comp16a, comp12a, comp16a)
  
  #row13
  A131 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp1a, comp13a, comp1a)
  A132 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp2a, comp13a, comp2a)
  A133 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp3a, comp13a, comp3a)
  A134 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp4a, comp13a, comp4a)
  A135 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp5a, comp13a, comp5a)
  A136 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp6a, comp13a, comp6a)
  A137 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp7a, comp13a, comp7a)
  A138 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp8a, comp13a, comp8a)
  A139 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp9a, comp13a, comp9a)
  A1310 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp10a, comp13a, comp10a)
  A1311 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp11a, comp13a, comp11a)
  A1312 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp12a, comp13a, comp12a)
  A1313 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp13a, comp13a, comp13a)
  A1314 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp14a, comp13a, comp14a)
  A1315 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp15a, comp13a, comp15a)
  A1316 <- jaccard_cpc_cos(cpc_freq_comp13a,cpc_freq_comp16a, comp13a, comp16a)
  
  #row14
  A141 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp1a, comp14a, comp1a)
  A142 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp2a, comp14a, comp2a)
  A143 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp3a, comp14a, comp3a)
  A144 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp4a, comp14a, comp4a)
  A145 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp5a, comp14a, comp5a)
  A146 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp6a, comp14a, comp6a)
  A147 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp7a, comp14a, comp7a)
  A148 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp8a, comp14a, comp8a)
  A149 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp9a, comp14a, comp9a)
  A1410 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp10a, comp14a, comp10a)
  A1411 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp11a, comp14a, comp11a)
  A1412 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp12a, comp14a, comp12a)
  A1413 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp13a, comp14a, comp13a)
  A1414 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp14a, comp14a, comp14a)
  A1415 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp15a, comp14a, comp15a)
  A1416 <- jaccard_cpc_cos(cpc_freq_comp14a,cpc_freq_comp16a, comp14a, comp16a)
  
  #row15
  A151 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp1a, comp15a, comp1a)
  A152 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp2a, comp15a, comp2a)
  A153 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp3a, comp15a, comp3a)
  A154 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp4a, comp15a, comp4a)
  A155 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp5a, comp15a, comp5a)
  A156 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp6a, comp15a, comp6a)
  A157 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp7a, comp15a, comp7a)
  A158 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp8a, comp15a, comp8a)
  A159 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp9a, comp15a, comp9a)
  A1510 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp10a, comp15a, comp10a)
  A1511 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp11a, comp15a, comp11a)
  A1512 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp12a, comp15a, comp12a)
  A1513 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp13a, comp15a, comp13a)
  A1514 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp14a, comp15a, comp14a)
  A1515 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp15a, comp15a, comp15a)
  A1516 <- jaccard_cpc_cos(cpc_freq_comp15a,cpc_freq_comp16a, comp15a, comp16a)
  
  #row16
  A161 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp1a, comp16a, comp1a)
  A162 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp2a, comp16a, comp2a)
  A163 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp3a, comp16a, comp3a)
  A164 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp4a, comp16a, comp4a)
  A165 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp5a, comp16a, comp5a)
  A166 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp6a, comp16a, comp6a)
  A167 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp7a, comp16a, comp7a)
  A168 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp8a, comp16a, comp8a)
  A169 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp9a, comp16a, comp9a)
  A1610 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp10a, comp16a, comp10a)
  A1611 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp11a, comp16a, comp11a)
  A1612 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp12a, comp16a, comp12a)
  A1613 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp13a, comp16a, comp13a)
  A1614 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp14a, comp16a, comp14a)
  A1615 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp15a, comp16a, comp15a)
  A1616 <- jaccard_cpc_cos(cpc_freq_comp16a,cpc_freq_comp16a, comp16a, comp16a)
  
  EN_A <- t(matrix(c(A11  ,A12  ,A13  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,
                     A21  ,A22  ,A23  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,A214 ,A215 ,A216 ,
                     A31  ,A32  ,A33  ,A34  ,A35  ,A36  ,A37  ,A38  ,A39  ,A310 ,A311 ,A312 ,A313 ,A314 ,A315 ,A316 ,
                     A41  ,A42  ,A43  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,A414 ,A415 ,A416 ,
                     A51  ,A52  ,A53  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,A514 ,A515 ,A516 ,
                     A61  ,A62  ,A63  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,A614 ,A615 ,A616 ,
                     A71  ,A72  ,A73  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,A714 ,A715 ,A716 ,
                     A81  ,A82  ,A83  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,A814 ,A815 ,A816 ,
                     A91  ,A92  ,A93  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,A914 ,A915 ,A916 ,
                     A101 ,A102 ,A103 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,A1014,A1015,A1016,
                     A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,A1114,A1115,A1116,
                     A121 ,A122 ,A123 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,A1214,A1215,A1216,
                     A131 ,A132 ,A133 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313,A1314,A1315,A1316,
                     A141 ,A142 ,A143 ,A144 ,A145 ,A146 ,A147 ,A148 ,A149 ,A1410,A1411,A1412,A1413,A1414,A1415,A1416,
                     A151 ,A152 ,A153 ,A154 ,A155 ,A156 ,A157 ,A158 ,A159 ,A1510,A1511,A1512,A1513,A1514,A1515,A1516,
                     A161 ,A162 ,A163 ,A164 ,A165 ,A166 ,A167 ,A168 ,A169 ,A1610,A1611,A1612,A1613,A1614,A1615,A1616), nrow=16, ncol=16))
  
  return(EN_A)
  
}
m.cpc.freq.cos <- matrix_dist_cos(v.cpc.freq, v.comp)

compJ1a = read_excel("compJ1a_cpc.xlsx")
compJ2a = read_excel("compJ2a_cpc.xlsx")
compJ4a = read_excel("compJ4a_cpc.xlsx")
compJ5a = read_excel("compJ5a_cpc.xlsx")
compJ6a = read_excel("compJ6a_cpc.xlsx")
compJ7a = read_excel("compJ7a_cpc.xlsx")
compJ8a = read_excel("compJ8a_cpc.xlsx")
compJ9a = read_excel("compJ9a_cpc.xlsx")
compJ10a = read_excel("compJ10a_cpc.xlsx")
compJ11a = read_excel("compJ11a_cpc.xlsx")
compJ12a = read_excel("compJ12a_cpc.xlsx")
compJ13a = read_excel("compJ13a_cpc.xlsx")


v.compJ <- c(compJ1a,compJ2a,compJ4a,compJ5a,compJ6a,compJ7a,compJ8a,compJ9a,compJ10a,compJ11a,compJ12a,compJ13a)
v.cpc.freq.j <- c(cpc_freq_compJ1a,cpc_freq_compJ2a,cpc_freq_compJ4a,cpc_freq_compJ5a,cpc_freq_compJ6a,cpc_freq_compJ7a,cpc_freq_compJ8a,cpc_freq_compJ9a,cpc_freq_compJ10a,cpc_freq_compJ11a,cpc_freq_compJ12a,cpc_freq_compJ13a)
matrix_dist_cos_j <- function (v.cpc.freq.j, v.compJ) {
  A11 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ1a, compJ1a, compJ1a)
  A12 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ2a, compJ1a, compJ2a)
  
  A14 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ4a, compJ1a, compJ4a)
  A15 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ5a, compJ1a, compJ5a)
  A16 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ6a, compJ1a, compJ6a)
  A17 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ7a, compJ1a, compJ7a)
  A18 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ8a, compJ1a, compJ8a)
  A19 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ9a, compJ1a, compJ9a)
  A110 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ10a, compJ1a, compJ10a)
  A111 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ11a, compJ1a, compJ11a)
  A112 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ12a, compJ1a, compJ12a)
  A113 <- jaccard_cpc_cos(cpc_freq_compJ1a,cpc_freq_compJ13a, compJ1a, compJ13a)
  
  #row2
  A21 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ1a, compJ2a, compJ1a)
  A22 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ2a, compJ2a, compJ2a)
  
  A24 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ4a, compJ2a, compJ4a)
  A25 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ5a, compJ2a, compJ5a)
  A26 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ6a, compJ2a, compJ6a)
  A27 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ7a, compJ2a, compJ7a)
  A28 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ8a, compJ2a, compJ8a)
  A29 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ9a, compJ2a, compJ9a)
  A210 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ10a, compJ2a, compJ10a)
  A211 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ11a, compJ2a, compJ11a)
  A212 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ12a, compJ2a, compJ12a)
  A213 <- jaccard_cpc_cos(cpc_freq_compJ2a,cpc_freq_compJ13a, compJ2a, compJ13a)
  
  #row4
  A41 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ1a, compJ4a, compJ1a)
  A42 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ2a, compJ4a, compJ2a)
  
  A44 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ4a, compJ4a, compJ4a)
  A45 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ5a, compJ4a, compJ5a)
  A46 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ6a, compJ4a, compJ6a)
  A47 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ7a, compJ4a, compJ7a)
  A48 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ8a, compJ4a, compJ8a)
  A49 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ9a, compJ4a, compJ9a)
  A410 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ10a, compJ4a, compJ10a)
  A411 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ11a, compJ4a, compJ11a)
  A412 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ12a, compJ4a, compJ12a)
  A413 <- jaccard_cpc_cos(cpc_freq_compJ4a,cpc_freq_compJ13a, compJ4a, compJ13a)
  
  #row5
  A51 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ1a, compJ5a, compJ1a)
  A52 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ2a, compJ5a, compJ2a)
  
  A54 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ4a, compJ5a, compJ4a)
  A55 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ5a, compJ5a, compJ5a)
  A56 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ6a, compJ5a, compJ6a)
  A57 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ7a, compJ5a, compJ7a)
  A58 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ8a, compJ5a, compJ8a)
  A59 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ9a, compJ5a, compJ9a)
  A510 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ10a, compJ5a, compJ10a)
  A511 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ11a, compJ5a, compJ11a)
  A512 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ12a, compJ5a, compJ12a)
  A513 <- jaccard_cpc_cos(cpc_freq_compJ5a,cpc_freq_compJ13a, compJ5a, compJ13a)
  
  #row6
  A61 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ1a, compJ6a, compJ1a)
  A62 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ2a, compJ6a, compJ2a)
  
  A64 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ4a, compJ6a, compJ4a)
  A65 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ5a, compJ6a, compJ5a)
  A66 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ6a, compJ6a, compJ6a)
  A67 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ7a, compJ6a, compJ7a)
  A68 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ8a, compJ6a, compJ8a)
  A69 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ9a, compJ6a, compJ9a)
  A610 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ10a, compJ6a, compJ10a)
  A611 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ11a, compJ6a, compJ11a)
  A612 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ12a, compJ6a, compJ12a)
  A613 <- jaccard_cpc_cos(cpc_freq_compJ6a,cpc_freq_compJ13a, compJ6a, compJ13a)
  
  #row7
  A71 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ1a, compJ7a, compJ1a)
  A72 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ2a, compJ7a, compJ2a)
  
  A74 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ4a, compJ7a, compJ4a)
  A75 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ5a, compJ7a, compJ5a)
  A76 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ6a, compJ7a, compJ6a)
  A77 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ7a, compJ7a, compJ7a)
  A78 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ8a, compJ7a, compJ8a)
  A79 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ9a, compJ7a, compJ9a)
  A710 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ10a, compJ7a, compJ10a)
  A711 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ11a, compJ7a, compJ11a)
  A712 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ12a, compJ7a, compJ12a)
  A713 <- jaccard_cpc_cos(cpc_freq_compJ7a,cpc_freq_compJ13a, compJ7a, compJ13a)
  
  #row8
  A81 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ1a, compJ8a, compJ1a)
  A82 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ2a, compJ8a, compJ2a)
  
  A84 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ4a, compJ8a, compJ4a)
  A85 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ5a, compJ8a, compJ5a)
  A86 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ6a, compJ8a, compJ6a)
  A87 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ7a, compJ8a, compJ7a)
  A88 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ8a, compJ8a, compJ8a)
  A89 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ9a, compJ8a, compJ9a)
  A810 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ10a, compJ8a, compJ10a)
  A811 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ11a, compJ8a, compJ11a)
  A812 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ12a, compJ8a, compJ12a)
  A813 <- jaccard_cpc_cos(cpc_freq_compJ8a,cpc_freq_compJ13a, compJ8a, compJ13a)
  
  #row9
  A91 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ1a, compJ9a, compJ1a)
  A92 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ2a, compJ9a, compJ2a)
  
  A94 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ4a, compJ9a, compJ4a)
  A95 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ5a, compJ9a, compJ5a)
  A96 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ6a, compJ9a, compJ6a)
  A97 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ7a, compJ9a, compJ7a)
  A98 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ8a, compJ9a, compJ8a)
  A99 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ9a, compJ9a, compJ9a)
  A910 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ10a, compJ9a, compJ10a)
  A911 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ11a, compJ9a, compJ11a)
  A912 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ12a, compJ9a, compJ12a)
  A913 <- jaccard_cpc_cos(cpc_freq_compJ9a,cpc_freq_compJ13a, compJ9a, compJ13a)
  
  #row10
  A101 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ1a, compJ10a, compJ1a)
  A102 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ2a, compJ10a, compJ2a)
  
  A104 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ4a, compJ10a, compJ4a)
  A105 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ5a, compJ10a, compJ5a)
  A106 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ6a, compJ10a, compJ6a)
  A107 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ7a, compJ10a, compJ7a)
  A108 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ8a, compJ10a, compJ8a)
  A109 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ9a, compJ10a, compJ9a)
  A1010 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ10a, compJ10a, compJ10a)
  A1011 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ11a, compJ10a, compJ11a)
  A1012 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ12a, compJ10a, compJ12a)
  A1013 <- jaccard_cpc_cos(cpc_freq_compJ10a,cpc_freq_compJ13a, compJ10a, compJ13a)
  
  #row11
  A111 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ1a, compJ11a, compJ1a)
  A112 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ2a, compJ11a, compJ2a)
  
  A114 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ4a, compJ11a, compJ4a)
  A115 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ5a, compJ11a, compJ5a)
  A116 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ6a, compJ11a, compJ6a)
  A117 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ7a, compJ11a, compJ7a)
  A118 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ8a, compJ11a, compJ8a)
  A119 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ9a, compJ11a, compJ9a)
  A1110 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ10a, compJ11a, compJ10a)
  A1111 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ11a, compJ11a, compJ11a)
  A1112 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ12a, compJ11a, compJ12a)
  A1113 <- jaccard_cpc_cos(cpc_freq_compJ11a,cpc_freq_compJ13a, compJ11a, compJ13a)
  
  #row12
  A121 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ1a, compJ12a, compJ1a)
  A122 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ2a, compJ12a, compJ2a)
  
  A124 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ4a, compJ12a, compJ4a)
  A125 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ5a, compJ12a, compJ5a)
  A126 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ6a, compJ12a, compJ6a)
  A127 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ7a, compJ12a, compJ7a)
  A128 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ8a, compJ12a, compJ8a)
  A129 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ9a, compJ12a, compJ9a)
  A1210 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ10a, compJ12a, compJ10a)
  A1211 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ11a, compJ12a, compJ11a)
  A1212 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ12a, compJ12a, compJ12a)
  A1213 <- jaccard_cpc_cos(cpc_freq_compJ12a,cpc_freq_compJ13a, compJ12a, compJ13a)
  
  #row13
  A131 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ1a, compJ13a, compJ1a)
  A132 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ2a, compJ13a, compJ2a)
  
  A134 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ4a, compJ13a, compJ4a)
  A135 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ5a, compJ13a, compJ5a)
  A136 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ6a, compJ13a, compJ6a)
  A137 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ7a, compJ13a, compJ7a)
  A138 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ8a, compJ13a, compJ8a)
  A139 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ9a, compJ13a, compJ9a)
  A1310 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ10a, compJ13a, compJ10a)
  A1311 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ11a, compJ13a, compJ11a)
  A1312 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ12a, compJ13a, compJ12a)
  A1313 <- jaccard_cpc_cos(cpc_freq_compJ13a,cpc_freq_compJ13a, compJ13a, compJ13a)
  
  
  
  EN_A <- t(matrix(c(A11  ,A12  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,
                     A21  ,A22  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,
                     A41  ,A42  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,
                     A51  ,A52  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,
                     A61  ,A62  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,
                     A71  ,A72  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,
                     A81  ,A82  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,
                     A91  ,A92  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,
                     A101 ,A102 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,
                     A111 ,A112 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,
                     A121 ,A122 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,
                     A131 ,A132 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313), nrow=12, ncol=12))
  
  return(EN_A)
  
}
m.cpc.freq.cos.j <- matrix_dist_cos_j(v.cpc.freq.j, v.compJ)

#examiners
v.exm <- c(v.exm_comp1a,v.exm_comp2a,v.exm_comp3a,v.exm_comp4a,v.exm_comp5a,v.exm_comp6a,v.exm_comp7a,v.exm_comp8a,v.exm_comp9a,v.exm_comp10a,v.exm_comp11a,v.exm_comp12a,v.exm_comp13a,v.exm_comp14a,v.exm_comp15a,v.exm_comp16a)
matrix_dist_exm <- function (v.exm) {
  A11 <- jaccard_exm(v.exm_comp1a,v.exm_comp1a)
  A12 <- jaccard_exm(v.exm_comp1a,v.exm_comp2a)
  A13 <- jaccard_exm(v.exm_comp1a,v.exm_comp3a)
  A14 <- jaccard_exm(v.exm_comp1a,v.exm_comp4a)
  A15 <- jaccard_exm(v.exm_comp1a,v.exm_comp5a)
  A16 <- jaccard_exm(v.exm_comp1a,v.exm_comp6a)
  A17 <- jaccard_exm(v.exm_comp1a,v.exm_comp7a)
  A18 <- jaccard_exm(v.exm_comp1a,v.exm_comp8a)
  A19 <- jaccard_exm(v.exm_comp1a,v.exm_comp9a)
  A110 <- jaccard_exm(v.exm_comp1a,v.exm_comp10a)
  A111 <- jaccard_exm(v.exm_comp1a,v.exm_comp11a)
  A112 <- jaccard_exm(v.exm_comp1a,v.exm_comp12a)
  A113 <- jaccard_exm(v.exm_comp1a,v.exm_comp13a)
  A114 <- jaccard_exm(v.exm_comp1a,v.exm_comp14a)
  A115 <- jaccard_exm(v.exm_comp1a,v.exm_comp15a)
  A116 <- jaccard_exm(v.exm_comp1a,v.exm_comp16a)
  
  #row2
  A21 <- jaccard_exm(v.exm_comp2a,v.exm_comp1a)
  A22 <- jaccard_exm(v.exm_comp2a,v.exm_comp2a)
  A23 <- jaccard_exm(v.exm_comp2a,v.exm_comp3a)
  A24 <- jaccard_exm(v.exm_comp2a,v.exm_comp4a)
  A25 <- jaccard_exm(v.exm_comp2a,v.exm_comp5a)
  A26 <- jaccard_exm(v.exm_comp2a,v.exm_comp6a)
  A27 <- jaccard_exm(v.exm_comp2a,v.exm_comp7a)
  A28 <- jaccard_exm(v.exm_comp2a,v.exm_comp8a)
  A29 <- jaccard_exm(v.exm_comp2a,v.exm_comp9a)
  A210 <- jaccard_exm(v.exm_comp2a,v.exm_comp10a)
  A211 <- jaccard_exm(v.exm_comp2a,v.exm_comp11a)
  A212 <- jaccard_exm(v.exm_comp2a,v.exm_comp12a)
  A213 <- jaccard_exm(v.exm_comp2a,v.exm_comp13a)
  A214 <- jaccard_exm(v.exm_comp2a,v.exm_comp14a)
  A215 <- jaccard_exm(v.exm_comp2a,v.exm_comp15a)
  A216 <- jaccard_exm(v.exm_comp2a,v.exm_comp16a)
  
  #row3
  A31 <- jaccard_exm(v.exm_comp3a,v.exm_comp1a)
  A32 <- jaccard_exm(v.exm_comp3a,v.exm_comp2a)
  A33 <- jaccard_exm(v.exm_comp3a,v.exm_comp3a)
  A34 <- jaccard_exm(v.exm_comp3a,v.exm_comp4a)
  A35 <- jaccard_exm(v.exm_comp3a,v.exm_comp5a)
  A36 <- jaccard_exm(v.exm_comp3a,v.exm_comp6a)
  A37 <- jaccard_exm(v.exm_comp3a,v.exm_comp7a)
  A38 <- jaccard_exm(v.exm_comp3a,v.exm_comp8a)
  A39 <- jaccard_exm(v.exm_comp3a,v.exm_comp9a)
  A310 <- jaccard_exm(v.exm_comp3a,v.exm_comp10a)
  A311 <- jaccard_exm(v.exm_comp3a,v.exm_comp11a)
  A312 <- jaccard_exm(v.exm_comp3a,v.exm_comp12a)
  A313 <- jaccard_exm(v.exm_comp3a,v.exm_comp13a)
  A314 <- jaccard_exm(v.exm_comp3a,v.exm_comp14a)
  A315 <- jaccard_exm(v.exm_comp3a,v.exm_comp15a)
  A316 <- jaccard_exm(v.exm_comp3a,v.exm_comp16a)
  
  #row4
  A41 <- jaccard_exm(v.exm_comp4a,v.exm_comp1a)
  A42 <- jaccard_exm(v.exm_comp4a,v.exm_comp2a)
  A43 <- jaccard_exm(v.exm_comp4a,v.exm_comp3a)
  A44 <- jaccard_exm(v.exm_comp4a,v.exm_comp4a)
  A45 <- jaccard_exm(v.exm_comp4a,v.exm_comp5a)
  A46 <- jaccard_exm(v.exm_comp4a,v.exm_comp6a)
  A47 <- jaccard_exm(v.exm_comp4a,v.exm_comp7a)
  A48 <- jaccard_exm(v.exm_comp4a,v.exm_comp8a)
  A49 <- jaccard_exm(v.exm_comp4a,v.exm_comp9a)
  A410 <- jaccard_exm(v.exm_comp4a,v.exm_comp10a)
  A411 <- jaccard_exm(v.exm_comp4a,v.exm_comp11a)
  A412 <- jaccard_exm(v.exm_comp4a,v.exm_comp12a)
  A413 <- jaccard_exm(v.exm_comp4a,v.exm_comp13a)
  A414 <- jaccard_exm(v.exm_comp4a,v.exm_comp14a)
  A415 <- jaccard_exm(v.exm_comp4a,v.exm_comp15a)
  A416 <- jaccard_exm(v.exm_comp4a,v.exm_comp16a)
  
  #row5
  A51 <- jaccard_exm(v.exm_comp5a,v.exm_comp1a)
  A52 <- jaccard_exm(v.exm_comp5a,v.exm_comp2a)
  A53 <- jaccard_exm(v.exm_comp5a,v.exm_comp3a)
  A54 <- jaccard_exm(v.exm_comp5a,v.exm_comp4a)
  A55 <- jaccard_exm(v.exm_comp5a,v.exm_comp5a)
  A56 <- jaccard_exm(v.exm_comp5a,v.exm_comp6a)
  A57 <- jaccard_exm(v.exm_comp5a,v.exm_comp7a)
  A58 <- jaccard_exm(v.exm_comp5a,v.exm_comp8a)
  A59 <- jaccard_exm(v.exm_comp5a,v.exm_comp9a)
  A510 <- jaccard_exm(v.exm_comp5a,v.exm_comp10a)
  A511 <- jaccard_exm(v.exm_comp5a,v.exm_comp11a)
  A512 <- jaccard_exm(v.exm_comp5a,v.exm_comp12a)
  A513 <- jaccard_exm(v.exm_comp5a,v.exm_comp13a)
  A514 <- jaccard_exm(v.exm_comp5a,v.exm_comp14a)
  A515 <- jaccard_exm(v.exm_comp5a,v.exm_comp15a)
  A516 <- jaccard_exm(v.exm_comp5a,v.exm_comp16a)
  
  #row6
  A61 <- jaccard_exm(v.exm_comp6a,v.exm_comp1a)
  A62 <- jaccard_exm(v.exm_comp6a,v.exm_comp2a)
  A63 <- jaccard_exm(v.exm_comp6a,v.exm_comp3a)
  A64 <- jaccard_exm(v.exm_comp6a,v.exm_comp4a)
  A65 <- jaccard_exm(v.exm_comp6a,v.exm_comp5a)
  A66 <- jaccard_exm(v.exm_comp6a,v.exm_comp6a)
  A67 <- jaccard_exm(v.exm_comp6a,v.exm_comp7a)
  A68 <- jaccard_exm(v.exm_comp6a,v.exm_comp8a)
  A69 <- jaccard_exm(v.exm_comp6a,v.exm_comp9a)
  A610 <- jaccard_exm(v.exm_comp6a,v.exm_comp10a)
  A611 <- jaccard_exm(v.exm_comp6a,v.exm_comp11a)
  A612 <- jaccard_exm(v.exm_comp6a,v.exm_comp12a)
  A613 <- jaccard_exm(v.exm_comp6a,v.exm_comp13a)
  A614 <- jaccard_exm(v.exm_comp6a,v.exm_comp14a)
  A615 <- jaccard_exm(v.exm_comp6a,v.exm_comp15a)
  A616 <- jaccard_exm(v.exm_comp6a,v.exm_comp16a)
  
  #row7
  A71 <- jaccard_exm(v.exm_comp7a,v.exm_comp1a)
  A72 <- jaccard_exm(v.exm_comp7a,v.exm_comp2a)
  A73 <- jaccard_exm(v.exm_comp7a,v.exm_comp3a)
  A74 <- jaccard_exm(v.exm_comp7a,v.exm_comp4a)
  A75 <- jaccard_exm(v.exm_comp7a,v.exm_comp5a)
  A76 <- jaccard_exm(v.exm_comp7a,v.exm_comp6a)
  A77 <- jaccard_exm(v.exm_comp7a,v.exm_comp7a)
  A78 <- jaccard_exm(v.exm_comp7a,v.exm_comp8a)
  A79 <- jaccard_exm(v.exm_comp7a,v.exm_comp9a)
  A710 <- jaccard_exm(v.exm_comp7a,v.exm_comp10a)
  A711 <- jaccard_exm(v.exm_comp7a,v.exm_comp11a)
  A712 <- jaccard_exm(v.exm_comp7a,v.exm_comp12a)
  A713 <- jaccard_exm(v.exm_comp7a,v.exm_comp13a)
  A714 <- jaccard_exm(v.exm_comp7a,v.exm_comp14a)
  A715 <- jaccard_exm(v.exm_comp7a,v.exm_comp15a)
  A716 <- jaccard_exm(v.exm_comp7a,v.exm_comp16a)
  
  #row8
  A81 <- jaccard_exm(v.exm_comp8a,v.exm_comp1a)
  A82 <- jaccard_exm(v.exm_comp8a,v.exm_comp2a)
  A83 <- jaccard_exm(v.exm_comp8a,v.exm_comp3a)
  A84 <- jaccard_exm(v.exm_comp8a,v.exm_comp4a)
  A85 <- jaccard_exm(v.exm_comp8a,v.exm_comp5a)
  A86 <- jaccard_exm(v.exm_comp8a,v.exm_comp6a)
  A87 <- jaccard_exm(v.exm_comp8a,v.exm_comp7a)
  A88 <- jaccard_exm(v.exm_comp8a,v.exm_comp8a)
  A89 <- jaccard_exm(v.exm_comp8a,v.exm_comp9a)
  A810 <- jaccard_exm(v.exm_comp8a,v.exm_comp10a)
  A811 <- jaccard_exm(v.exm_comp8a,v.exm_comp11a)
  A812 <- jaccard_exm(v.exm_comp8a,v.exm_comp12a)
  A813 <- jaccard_exm(v.exm_comp8a,v.exm_comp13a)
  A814 <- jaccard_exm(v.exm_comp8a,v.exm_comp14a)
  A815 <- jaccard_exm(v.exm_comp8a,v.exm_comp15a)
  A816 <- jaccard_exm(v.exm_comp8a,v.exm_comp16a)
  
  #row9
  A91 <- jaccard_exm(v.exm_comp9a,v.exm_comp1a)
  A92 <- jaccard_exm(v.exm_comp9a,v.exm_comp2a)
  A93 <- jaccard_exm(v.exm_comp9a,v.exm_comp3a)
  A94 <- jaccard_exm(v.exm_comp9a,v.exm_comp4a)
  A95 <- jaccard_exm(v.exm_comp9a,v.exm_comp5a)
  A96 <- jaccard_exm(v.exm_comp9a,v.exm_comp6a)
  A97 <- jaccard_exm(v.exm_comp9a,v.exm_comp7a)
  A98 <- jaccard_exm(v.exm_comp9a,v.exm_comp8a)
  A99 <- jaccard_exm(v.exm_comp9a,v.exm_comp9a)
  A910 <- jaccard_exm(v.exm_comp9a,v.exm_comp10a)
  A911 <- jaccard_exm(v.exm_comp9a,v.exm_comp11a)
  A912 <- jaccard_exm(v.exm_comp9a,v.exm_comp12a)
  A913 <- jaccard_exm(v.exm_comp9a,v.exm_comp13a)
  A914 <- jaccard_exm(v.exm_comp9a,v.exm_comp14a)
  A915 <- jaccard_exm(v.exm_comp9a,v.exm_comp15a)
  A916 <- jaccard_exm(v.exm_comp9a,v.exm_comp16a)
  
  #row10
  A101 <- jaccard_exm(v.exm_comp10a,v.exm_comp1a)
  A102 <- jaccard_exm(v.exm_comp10a,v.exm_comp2a)
  A103 <- jaccard_exm(v.exm_comp10a,v.exm_comp3a)
  A104 <- jaccard_exm(v.exm_comp10a,v.exm_comp4a)
  A105 <- jaccard_exm(v.exm_comp10a,v.exm_comp5a)
  A106 <- jaccard_exm(v.exm_comp10a,v.exm_comp6a)
  A107 <- jaccard_exm(v.exm_comp10a,v.exm_comp7a)
  A108 <- jaccard_exm(v.exm_comp10a,v.exm_comp8a)
  A109 <- jaccard_exm(v.exm_comp10a,v.exm_comp9a)
  A1010 <- jaccard_exm(v.exm_comp10a,v.exm_comp10a)
  A1011 <- jaccard_exm(v.exm_comp10a,v.exm_comp11a)
  A1012 <- jaccard_exm(v.exm_comp10a,v.exm_comp12a)
  A1013 <- jaccard_exm(v.exm_comp10a,v.exm_comp13a)
  A1014 <- jaccard_exm(v.exm_comp10a,v.exm_comp14a)
  A1015 <- jaccard_exm(v.exm_comp10a,v.exm_comp15a)
  A1016 <- jaccard_exm(v.exm_comp10a,v.exm_comp16a)
  
  #row11
  A111 <- jaccard_exm(v.exm_comp11a,v.exm_comp1a)
  A112 <- jaccard_exm(v.exm_comp11a,v.exm_comp2a)
  A113 <- jaccard_exm(v.exm_comp11a,v.exm_comp3a)
  A114 <- jaccard_exm(v.exm_comp11a,v.exm_comp4a)
  A115 <- jaccard_exm(v.exm_comp11a,v.exm_comp5a)
  A116 <- jaccard_exm(v.exm_comp11a,v.exm_comp6a)
  A117 <- jaccard_exm(v.exm_comp11a,v.exm_comp7a)
  A118 <- jaccard_exm(v.exm_comp11a,v.exm_comp8a)
  A119 <- jaccard_exm(v.exm_comp11a,v.exm_comp9a)
  A1110 <- jaccard_exm(v.exm_comp11a,v.exm_comp10a)
  A1111 <- jaccard_exm(v.exm_comp11a,v.exm_comp11a)
  A1112 <- jaccard_exm(v.exm_comp11a,v.exm_comp12a)
  A1113 <- jaccard_exm(v.exm_comp11a,v.exm_comp13a)
  A1114 <- jaccard_exm(v.exm_comp11a,v.exm_comp14a)
  A1115 <- jaccard_exm(v.exm_comp11a,v.exm_comp15a)
  A1116 <- jaccard_exm(v.exm_comp11a,v.exm_comp16a)
  
  #row12
  A121 <- jaccard_exm(v.exm_comp12a,v.exm_comp1a)
  A122 <- jaccard_exm(v.exm_comp12a,v.exm_comp2a)
  A123 <- jaccard_exm(v.exm_comp12a,v.exm_comp3a)
  A124 <- jaccard_exm(v.exm_comp12a,v.exm_comp4a)
  A125 <- jaccard_exm(v.exm_comp12a,v.exm_comp5a)
  A126 <- jaccard_exm(v.exm_comp12a,v.exm_comp6a)
  A127 <- jaccard_exm(v.exm_comp12a,v.exm_comp7a)
  A128 <- jaccard_exm(v.exm_comp12a,v.exm_comp8a)
  A129 <- jaccard_exm(v.exm_comp12a,v.exm_comp9a)
  A1210 <- jaccard_exm(v.exm_comp12a,v.exm_comp10a)
  A1211 <- jaccard_exm(v.exm_comp12a,v.exm_comp11a)
  A1212 <- jaccard_exm(v.exm_comp12a,v.exm_comp12a)
  A1213 <- jaccard_exm(v.exm_comp12a,v.exm_comp13a)
  A1214 <- jaccard_exm(v.exm_comp12a,v.exm_comp14a)
  A1215 <- jaccard_exm(v.exm_comp12a,v.exm_comp15a)
  A1216 <- jaccard_exm(v.exm_comp12a,v.exm_comp16a)
  
  #row13
  A131 <- jaccard_exm(v.exm_comp13a,v.exm_comp1a)
  A132 <- jaccard_exm(v.exm_comp13a,v.exm_comp2a)
  A133 <- jaccard_exm(v.exm_comp13a,v.exm_comp3a)
  A134 <- jaccard_exm(v.exm_comp13a,v.exm_comp4a)
  A135 <- jaccard_exm(v.exm_comp13a,v.exm_comp5a)
  A136 <- jaccard_exm(v.exm_comp13a,v.exm_comp6a)
  A137 <- jaccard_exm(v.exm_comp13a,v.exm_comp7a)
  A138 <- jaccard_exm(v.exm_comp13a,v.exm_comp8a)
  A139 <- jaccard_exm(v.exm_comp13a,v.exm_comp9a)
  A1310 <- jaccard_exm(v.exm_comp13a,v.exm_comp10a)
  A1311 <- jaccard_exm(v.exm_comp13a,v.exm_comp11a)
  A1312 <- jaccard_exm(v.exm_comp13a,v.exm_comp12a)
  A1313 <- jaccard_exm(v.exm_comp13a,v.exm_comp13a)
  A1314 <- jaccard_exm(v.exm_comp13a,v.exm_comp14a)
  A1315 <- jaccard_exm(v.exm_comp13a,v.exm_comp15a)
  A1316 <- jaccard_exm(v.exm_comp13a,v.exm_comp16a)
  
  #row14
  A141 <- jaccard_exm(v.exm_comp14a,v.exm_comp1a)
  A142 <- jaccard_exm(v.exm_comp14a,v.exm_comp2a)
  A143 <- jaccard_exm(v.exm_comp14a,v.exm_comp3a)
  A144 <- jaccard_exm(v.exm_comp14a,v.exm_comp4a)
  A145 <- jaccard_exm(v.exm_comp14a,v.exm_comp5a)
  A146 <- jaccard_exm(v.exm_comp14a,v.exm_comp6a)
  A147 <- jaccard_exm(v.exm_comp14a,v.exm_comp7a)
  A148 <- jaccard_exm(v.exm_comp14a,v.exm_comp8a)
  A149 <- jaccard_exm(v.exm_comp14a,v.exm_comp9a)
  A1410 <- jaccard_exm(v.exm_comp14a,v.exm_comp10a)
  A1411 <- jaccard_exm(v.exm_comp14a,v.exm_comp11a)
  A1412 <- jaccard_exm(v.exm_comp14a,v.exm_comp12a)
  A1413 <- jaccard_exm(v.exm_comp14a,v.exm_comp13a)
  A1414 <- jaccard_exm(v.exm_comp14a,v.exm_comp14a)
  A1415 <- jaccard_exm(v.exm_comp14a,v.exm_comp15a)
  A1416 <- jaccard_exm(v.exm_comp14a,v.exm_comp16a)
  
  #row15
  A151 <- jaccard_exm(v.exm_comp15a,v.exm_comp1a)
  A152 <- jaccard_exm(v.exm_comp15a,v.exm_comp2a)
  A153 <- jaccard_exm(v.exm_comp15a,v.exm_comp3a)
  A154 <- jaccard_exm(v.exm_comp15a,v.exm_comp4a)
  A155 <- jaccard_exm(v.exm_comp15a,v.exm_comp5a)
  A156 <- jaccard_exm(v.exm_comp15a,v.exm_comp6a)
  A157 <- jaccard_exm(v.exm_comp15a,v.exm_comp7a)
  A158 <- jaccard_exm(v.exm_comp15a,v.exm_comp8a)
  A159 <- jaccard_exm(v.exm_comp15a,v.exm_comp9a)
  A1510 <- jaccard_exm(v.exm_comp15a,v.exm_comp10a)
  A1511 <- jaccard_exm(v.exm_comp15a,v.exm_comp11a)
  A1512 <- jaccard_exm(v.exm_comp15a,v.exm_comp12a)
  A1513 <- jaccard_exm(v.exm_comp15a,v.exm_comp13a)
  A1514 <- jaccard_exm(v.exm_comp15a,v.exm_comp14a)
  A1515 <- jaccard_exm(v.exm_comp15a,v.exm_comp15a)
  A1516 <- jaccard_exm(v.exm_comp15a,v.exm_comp16a)
  
  #row16
  A161 <- jaccard_exm(v.exm_comp16a,v.exm_comp1a)
  A162 <- jaccard_exm(v.exm_comp16a,v.exm_comp2a)
  A163 <- jaccard_exm(v.exm_comp16a,v.exm_comp3a)
  A164 <- jaccard_exm(v.exm_comp16a,v.exm_comp4a)
  A165 <- jaccard_exm(v.exm_comp16a,v.exm_comp5a)
  A166 <- jaccard_exm(v.exm_comp16a,v.exm_comp6a)
  A167 <- jaccard_exm(v.exm_comp16a,v.exm_comp7a)
  A168 <- jaccard_exm(v.exm_comp16a,v.exm_comp8a)
  A169 <- jaccard_exm(v.exm_comp16a,v.exm_comp9a)
  A1610 <- jaccard_exm(v.exm_comp16a,v.exm_comp10a)
  A1611 <- jaccard_exm(v.exm_comp16a,v.exm_comp11a)
  A1612 <- jaccard_exm(v.exm_comp16a,v.exm_comp12a)
  A1613 <- jaccard_exm(v.exm_comp16a,v.exm_comp13a)
  A1614 <- jaccard_exm(v.exm_comp16a,v.exm_comp14a)
  A1615 <- jaccard_exm(v.exm_comp16a,v.exm_comp15a)
  A1616 <- jaccard_exm(v.exm_comp16a,v.exm_comp16a)
  
  EN_A <- t(matrix(c(A11  ,A12  ,A13  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,
                     A21  ,A22  ,A23  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,A214 ,A215 ,A216 ,
                     A31  ,A32  ,A33  ,A34  ,A35  ,A36  ,A37  ,A38  ,A39  ,A310 ,A311 ,A312 ,A313 ,A314 ,A315 ,A316 ,
                     A41  ,A42  ,A43  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,A414 ,A415 ,A416 ,
                     A51  ,A52  ,A53  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,A514 ,A515 ,A516 ,
                     A61  ,A62  ,A63  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,A614 ,A615 ,A616 ,
                     A71  ,A72  ,A73  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,A714 ,A715 ,A716 ,
                     A81  ,A82  ,A83  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,A814 ,A815 ,A816 ,
                     A91  ,A92  ,A93  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,A914 ,A915 ,A916 ,
                     A101 ,A102 ,A103 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,A1014,A1015,A1016,
                     A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,A1114,A1115,A1116,
                     A121 ,A122 ,A123 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,A1214,A1215,A1216,
                     A131 ,A132 ,A133 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313,A1314,A1315,A1316,
                     A141 ,A142 ,A143 ,A144 ,A145 ,A146 ,A147 ,A148 ,A149 ,A1410,A1411,A1412,A1413,A1414,A1415,A1416,
                     A151 ,A152 ,A153 ,A154 ,A155 ,A156 ,A157 ,A158 ,A159 ,A1510,A1511,A1512,A1513,A1514,A1515,A1516,
                     A161 ,A162 ,A163 ,A164 ,A165 ,A166 ,A167 ,A168 ,A169 ,A1610,A1611,A1612,A1613,A1614,A1615,A1616), nrow=16, ncol=16))
  
  return(EN_A)
  
}
m.exm <- matrix_dist_exm(v.exm)

v.exm.j <- c(v.exm_compJ1a,v.exm_compJ2a,v.exm_compJ4a,v.exm_compJ5a,v.exm_compJ6a,v.exm_compJ7a,v.exm_compJ8a,v.exm_compJ9a,v.exm_compJ10a,v.exm_compJ11a,v.exm_compJ12a,v.exm_compJ13a)
matrix_dist_exm_j <- function (v.exm.j) {
  A11 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ1a)
  A12 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ2a)
  
  A14 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ4a)
  A15 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ5a)
  A16 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ6a)
  A17 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ7a)
  A18 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ8a)
  A19 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ9a)
  A110 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ10a)
  A111 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ11a)
  A112 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ12a)
  A113 <- jaccard_exm(v.exm_compJ1a,v.exm_compJ13a)
  
  
  #row2
  A21 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ1a)
  A22 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ2a)
  
  A24 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ4a)
  A25 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ5a)
  A26 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ6a)
  A27 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ7a)
  A28 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ8a)
  A29 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ9a)
  A210 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ10a)
  A211 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ11a)
  A212 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ12a)
  A213 <- jaccard_exm(v.exm_compJ2a,v.exm_compJ13a)
  
  
  
  #row4
  A41 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ1a)
  A42 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ2a)
  
  A44 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ4a)
  A45 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ5a)
  A46 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ6a)
  A47 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ7a)
  A48 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ8a)
  A49 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ9a)
  A410 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ10a)
  A411 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ11a)
  A412 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ12a)
  A413 <- jaccard_exm(v.exm_compJ4a,v.exm_compJ13a)
  
  
  #row5
  A51 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ1a)
  A52 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ2a)
  
  A54 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ4a)
  A55 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ5a)
  A56 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ6a)
  A57 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ7a)
  A58 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ8a)
  A59 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ9a)
  A510 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ10a)
  A511 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ11a)
  A512 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ12a)
  A513 <- jaccard_exm(v.exm_compJ5a,v.exm_compJ13a)
  
  
  #row6
  A61 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ1a)
  A62 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ2a)
  
  A64 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ4a)
  A65 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ5a)
  A66 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ6a)
  A67 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ7a)
  A68 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ8a)
  A69 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ9a)
  A610 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ10a)
  A611 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ11a)
  A612 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ12a)
  A613 <- jaccard_exm(v.exm_compJ6a,v.exm_compJ13a)
  
  #row7
  A71 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ1a)
  A72 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ2a)
  
  A74 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ4a)
  A75 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ5a)
  A76 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ6a)
  A77 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ7a)
  A78 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ8a)
  A79 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ9a)
  A710 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ10a)
  A711 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ11a)
  A712 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ12a)
  A713 <- jaccard_exm(v.exm_compJ7a,v.exm_compJ13a)
  
  
  #row8
  A81 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ1a)
  A82 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ2a)
  
  A84 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ4a)
  A85 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ5a)
  A86 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ6a)
  A87 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ7a)
  A88 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ8a)
  A89 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ9a)
  A810 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ10a)
  A811 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ11a)
  A812 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ12a)
  A813 <- jaccard_exm(v.exm_compJ8a,v.exm_compJ13a)
  
  
  #row9
  A91 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ1a)
  A92 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ2a)
  
  A94 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ4a)
  A95 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ5a)
  A96 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ6a)
  A97 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ7a)
  A98 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ8a)
  A99 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ9a)
  A910 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ10a)
  A911 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ11a)
  A912 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ12a)
  A913 <- jaccard_exm(v.exm_compJ9a,v.exm_compJ13a)
  
  
  #row10
  A101 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ1a)
  A102 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ2a)
  
  A104 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ4a)
  A105 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ5a)
  A106 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ6a)
  A107 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ7a)
  A108 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ8a)
  A109 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ9a)
  A1010 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ10a)
  A1011 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ11a)
  A1012 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ12a)
  A1013 <- jaccard_exm(v.exm_compJ10a,v.exm_compJ13a)
  
  
  #row11
  A111 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ1a)
  A112 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ2a)
  
  A114 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ4a)
  A115 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ5a)
  A116 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ6a)
  A117 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ7a)
  A118 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ8a)
  A119 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ9a)
  A1110 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ10a)
  A1111 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ11a)
  A1112 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ12a)
  A1113 <- jaccard_exm(v.exm_compJ11a,v.exm_compJ13a)
  
  
  #row12
  A121 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ1a)
  A122 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ2a)
  
  A124 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ4a)
  A125 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ5a)
  A126 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ6a)
  A127 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ7a)
  A128 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ8a)
  A129 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ9a)
  A1210 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ10a)
  A1211 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ11a)
  A1212 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ12a)
  A1213 <- jaccard_exm(v.exm_compJ12a,v.exm_compJ13a)
  
  
  #row13
  A131 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ1a)
  A132 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ2a)
  
  A134 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ4a)
  A135 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ5a)
  A136 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ6a)
  A137 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ7a)
  A138 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ8a)
  A139 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ9a)
  A1310 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ10a)
  A1311 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ11a)
  A1312 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ12a)
  A1313 <- jaccard_exm(v.exm_compJ13a,v.exm_compJ13a)
  
  
  
  
  EN_A <- t(matrix(c(A11  ,A12  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,
                     A21  ,A22  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,
                     A41  ,A42  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,
                     A51  ,A52  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,
                     A61  ,A62  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,
                     A71  ,A72  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,
                     A81  ,A82  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,
                     A91  ,A92  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,
                     A101 ,A102 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,
                     A111 ,A112 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,
                     A121 ,A122 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,
                     A131 ,A132 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313), nrow=12, ncol=12))
  
  return(EN_A)
  
}
m.exm.j <- matrix_dist_exm_j(v.exm.j)

#inventors
v.iname <- c(v.iname_comp1a,v.iname_comp2a,v.iname_comp3a,v.iname_comp4a,v.iname_comp5a,v.iname_comp6a,v.iname_comp7a,v.iname_comp8a,v.iname_comp9a,v.iname_comp10a,v.iname_comp11a,v.iname_comp12a,v.iname_comp13a,v.iname_comp14a,v.iname_comp15a,v.iname_comp16a)
matrix_dist_iname <- function (v.iname) {
  A11 <- jaccard_iname(v.iname_comp1a,v.iname_comp1a)
  A12 <- jaccard_iname(v.iname_comp1a,v.iname_comp2a)
  A13 <- jaccard_iname(v.iname_comp1a,v.iname_comp3a)
  A14 <- jaccard_iname(v.iname_comp1a,v.iname_comp4a)
  A15 <- jaccard_iname(v.iname_comp1a,v.iname_comp5a)
  A16 <- jaccard_iname(v.iname_comp1a,v.iname_comp6a)
  A17 <- jaccard_iname(v.iname_comp1a,v.iname_comp7a)
  A18 <- jaccard_iname(v.iname_comp1a,v.iname_comp8a)
  A19 <- jaccard_iname(v.iname_comp1a,v.iname_comp9a)
  A110 <- jaccard_iname(v.iname_comp1a,v.iname_comp10a)
  A111 <- jaccard_iname(v.iname_comp1a,v.iname_comp11a)
  A112 <- jaccard_iname(v.iname_comp1a,v.iname_comp12a)
  A113 <- jaccard_iname(v.iname_comp1a,v.iname_comp13a)
  A114 <- jaccard_iname(v.iname_comp1a,v.iname_comp14a)
  A115 <- jaccard_iname(v.iname_comp1a,v.iname_comp15a)
  A116 <- jaccard_iname(v.iname_comp1a,v.iname_comp16a)
  
  #row2
  A21 <- jaccard_iname(v.iname_comp2a,v.iname_comp1a)
  A22 <- jaccard_iname(v.iname_comp2a,v.iname_comp2a)
  A23 <- jaccard_iname(v.iname_comp2a,v.iname_comp3a)
  A24 <- jaccard_iname(v.iname_comp2a,v.iname_comp4a)
  A25 <- jaccard_iname(v.iname_comp2a,v.iname_comp5a)
  A26 <- jaccard_iname(v.iname_comp2a,v.iname_comp6a)
  A27 <- jaccard_iname(v.iname_comp2a,v.iname_comp7a)
  A28 <- jaccard_iname(v.iname_comp2a,v.iname_comp8a)
  A29 <- jaccard_iname(v.iname_comp2a,v.iname_comp9a)
  A210 <- jaccard_iname(v.iname_comp2a,v.iname_comp10a)
  A211 <- jaccard_iname(v.iname_comp2a,v.iname_comp11a)
  A212 <- jaccard_iname(v.iname_comp2a,v.iname_comp12a)
  A213 <- jaccard_iname(v.iname_comp2a,v.iname_comp13a)
  A214 <- jaccard_iname(v.iname_comp2a,v.iname_comp14a)
  A215 <- jaccard_iname(v.iname_comp2a,v.iname_comp15a)
  A216 <- jaccard_iname(v.iname_comp2a,v.iname_comp16a)
  
  #row3
  A31 <- jaccard_iname(v.iname_comp3a,v.iname_comp1a)
  A32 <- jaccard_iname(v.iname_comp3a,v.iname_comp2a)
  A33 <- jaccard_iname(v.iname_comp3a,v.iname_comp3a)
  A34 <- jaccard_iname(v.iname_comp3a,v.iname_comp4a)
  A35 <- jaccard_iname(v.iname_comp3a,v.iname_comp5a)
  A36 <- jaccard_iname(v.iname_comp3a,v.iname_comp6a)
  A37 <- jaccard_iname(v.iname_comp3a,v.iname_comp7a)
  A38 <- jaccard_iname(v.iname_comp3a,v.iname_comp8a)
  A39 <- jaccard_iname(v.iname_comp3a,v.iname_comp9a)
  A310 <- jaccard_iname(v.iname_comp3a,v.iname_comp10a)
  A311 <- jaccard_iname(v.iname_comp3a,v.iname_comp11a)
  A312 <- jaccard_iname(v.iname_comp3a,v.iname_comp12a)
  A313 <- jaccard_iname(v.iname_comp3a,v.iname_comp13a)
  A314 <- jaccard_iname(v.iname_comp3a,v.iname_comp14a)
  A315 <- jaccard_iname(v.iname_comp3a,v.iname_comp15a)
  A316 <- jaccard_iname(v.iname_comp3a,v.iname_comp16a)
  
  #row4
  A41 <- jaccard_iname(v.iname_comp4a,v.iname_comp1a)
  A42 <- jaccard_iname(v.iname_comp4a,v.iname_comp2a)
  A43 <- jaccard_iname(v.iname_comp4a,v.iname_comp3a)
  A44 <- jaccard_iname(v.iname_comp4a,v.iname_comp4a)
  A45 <- jaccard_iname(v.iname_comp4a,v.iname_comp5a)
  A46 <- jaccard_iname(v.iname_comp4a,v.iname_comp6a)
  A47 <- jaccard_iname(v.iname_comp4a,v.iname_comp7a)
  A48 <- jaccard_iname(v.iname_comp4a,v.iname_comp8a)
  A49 <- jaccard_iname(v.iname_comp4a,v.iname_comp9a)
  A410 <- jaccard_iname(v.iname_comp4a,v.iname_comp10a)
  A411 <- jaccard_iname(v.iname_comp4a,v.iname_comp11a)
  A412 <- jaccard_iname(v.iname_comp4a,v.iname_comp12a)
  A413 <- jaccard_iname(v.iname_comp4a,v.iname_comp13a)
  A414 <- jaccard_iname(v.iname_comp4a,v.iname_comp14a)
  A415 <- jaccard_iname(v.iname_comp4a,v.iname_comp15a)
  A416 <- jaccard_iname(v.iname_comp4a,v.iname_comp16a)
  
  #row5
  A51 <- jaccard_iname(v.iname_comp5a,v.iname_comp1a)
  A52 <- jaccard_iname(v.iname_comp5a,v.iname_comp2a)
  A53 <- jaccard_iname(v.iname_comp5a,v.iname_comp3a)
  A54 <- jaccard_iname(v.iname_comp5a,v.iname_comp4a)
  A55 <- jaccard_iname(v.iname_comp5a,v.iname_comp5a)
  A56 <- jaccard_iname(v.iname_comp5a,v.iname_comp6a)
  A57 <- jaccard_iname(v.iname_comp5a,v.iname_comp7a)
  A58 <- jaccard_iname(v.iname_comp5a,v.iname_comp8a)
  A59 <- jaccard_iname(v.iname_comp5a,v.iname_comp9a)
  A510 <- jaccard_iname(v.iname_comp5a,v.iname_comp10a)
  A511 <- jaccard_iname(v.iname_comp5a,v.iname_comp11a)
  A512 <- jaccard_iname(v.iname_comp5a,v.iname_comp12a)
  A513 <- jaccard_iname(v.iname_comp5a,v.iname_comp13a)
  A514 <- jaccard_iname(v.iname_comp5a,v.iname_comp14a)
  A515 <- jaccard_iname(v.iname_comp5a,v.iname_comp15a)
  A516 <- jaccard_iname(v.iname_comp5a,v.iname_comp16a)
  
  #row6
  A61 <- jaccard_iname(v.iname_comp6a,v.iname_comp1a)
  A62 <- jaccard_iname(v.iname_comp6a,v.iname_comp2a)
  A63 <- jaccard_iname(v.iname_comp6a,v.iname_comp3a)
  A64 <- jaccard_iname(v.iname_comp6a,v.iname_comp4a)
  A65 <- jaccard_iname(v.iname_comp6a,v.iname_comp5a)
  A66 <- jaccard_iname(v.iname_comp6a,v.iname_comp6a)
  A67 <- jaccard_iname(v.iname_comp6a,v.iname_comp7a)
  A68 <- jaccard_iname(v.iname_comp6a,v.iname_comp8a)
  A69 <- jaccard_iname(v.iname_comp6a,v.iname_comp9a)
  A610 <- jaccard_iname(v.iname_comp6a,v.iname_comp10a)
  A611 <- jaccard_iname(v.iname_comp6a,v.iname_comp11a)
  A612 <- jaccard_iname(v.iname_comp6a,v.iname_comp12a)
  A613 <- jaccard_iname(v.iname_comp6a,v.iname_comp13a)
  A614 <- jaccard_iname(v.iname_comp6a,v.iname_comp14a)
  A615 <- jaccard_iname(v.iname_comp6a,v.iname_comp15a)
  A616 <- jaccard_iname(v.iname_comp6a,v.iname_comp16a)
  
  #row7
  A71 <- jaccard_iname(v.iname_comp7a,v.iname_comp1a)
  A72 <- jaccard_iname(v.iname_comp7a,v.iname_comp2a)
  A73 <- jaccard_iname(v.iname_comp7a,v.iname_comp3a)
  A74 <- jaccard_iname(v.iname_comp7a,v.iname_comp4a)
  A75 <- jaccard_iname(v.iname_comp7a,v.iname_comp5a)
  A76 <- jaccard_iname(v.iname_comp7a,v.iname_comp6a)
  A77 <- jaccard_iname(v.iname_comp7a,v.iname_comp7a)
  A78 <- jaccard_iname(v.iname_comp7a,v.iname_comp8a)
  A79 <- jaccard_iname(v.iname_comp7a,v.iname_comp9a)
  A710 <- jaccard_iname(v.iname_comp7a,v.iname_comp10a)
  A711 <- jaccard_iname(v.iname_comp7a,v.iname_comp11a)
  A712 <- jaccard_iname(v.iname_comp7a,v.iname_comp12a)
  A713 <- jaccard_iname(v.iname_comp7a,v.iname_comp13a)
  A714 <- jaccard_iname(v.iname_comp7a,v.iname_comp14a)
  A715 <- jaccard_iname(v.iname_comp7a,v.iname_comp15a)
  A716 <- jaccard_iname(v.iname_comp7a,v.iname_comp16a)
  
  #row8
  A81 <- jaccard_iname(v.iname_comp8a,v.iname_comp1a)
  A82 <- jaccard_iname(v.iname_comp8a,v.iname_comp2a)
  A83 <- jaccard_iname(v.iname_comp8a,v.iname_comp3a)
  A84 <- jaccard_iname(v.iname_comp8a,v.iname_comp4a)
  A85 <- jaccard_iname(v.iname_comp8a,v.iname_comp5a)
  A86 <- jaccard_iname(v.iname_comp8a,v.iname_comp6a)
  A87 <- jaccard_iname(v.iname_comp8a,v.iname_comp7a)
  A88 <- jaccard_iname(v.iname_comp8a,v.iname_comp8a)
  A89 <- jaccard_iname(v.iname_comp8a,v.iname_comp9a)
  A810 <- jaccard_iname(v.iname_comp8a,v.iname_comp10a)
  A811 <- jaccard_iname(v.iname_comp8a,v.iname_comp11a)
  A812 <- jaccard_iname(v.iname_comp8a,v.iname_comp12a)
  A813 <- jaccard_iname(v.iname_comp8a,v.iname_comp13a)
  A814 <- jaccard_iname(v.iname_comp8a,v.iname_comp14a)
  A815 <- jaccard_iname(v.iname_comp8a,v.iname_comp15a)
  A816 <- jaccard_iname(v.iname_comp8a,v.iname_comp16a)
  
  #row9
  A91 <- jaccard_iname(v.iname_comp9a,v.iname_comp1a)
  A92 <- jaccard_iname(v.iname_comp9a,v.iname_comp2a)
  A93 <- jaccard_iname(v.iname_comp9a,v.iname_comp3a)
  A94 <- jaccard_iname(v.iname_comp9a,v.iname_comp4a)
  A95 <- jaccard_iname(v.iname_comp9a,v.iname_comp5a)
  A96 <- jaccard_iname(v.iname_comp9a,v.iname_comp6a)
  A97 <- jaccard_iname(v.iname_comp9a,v.iname_comp7a)
  A98 <- jaccard_iname(v.iname_comp9a,v.iname_comp8a)
  A99 <- jaccard_iname(v.iname_comp9a,v.iname_comp9a)
  A910 <- jaccard_iname(v.iname_comp9a,v.iname_comp10a)
  A911 <- jaccard_iname(v.iname_comp9a,v.iname_comp11a)
  A912 <- jaccard_iname(v.iname_comp9a,v.iname_comp12a)
  A913 <- jaccard_iname(v.iname_comp9a,v.iname_comp13a)
  A914 <- jaccard_iname(v.iname_comp9a,v.iname_comp14a)
  A915 <- jaccard_iname(v.iname_comp9a,v.iname_comp15a)
  A916 <- jaccard_iname(v.iname_comp9a,v.iname_comp16a)
  
  #row10
  A101 <- jaccard_iname(v.iname_comp10a,v.iname_comp1a)
  A102 <- jaccard_iname(v.iname_comp10a,v.iname_comp2a)
  A103 <- jaccard_iname(v.iname_comp10a,v.iname_comp3a)
  A104 <- jaccard_iname(v.iname_comp10a,v.iname_comp4a)
  A105 <- jaccard_iname(v.iname_comp10a,v.iname_comp5a)
  A106 <- jaccard_iname(v.iname_comp10a,v.iname_comp6a)
  A107 <- jaccard_iname(v.iname_comp10a,v.iname_comp7a)
  A108 <- jaccard_iname(v.iname_comp10a,v.iname_comp8a)
  A109 <- jaccard_iname(v.iname_comp10a,v.iname_comp9a)
  A1010 <- jaccard_iname(v.iname_comp10a,v.iname_comp10a)
  A1011 <- jaccard_iname(v.iname_comp10a,v.iname_comp11a)
  A1012 <- jaccard_iname(v.iname_comp10a,v.iname_comp12a)
  A1013 <- jaccard_iname(v.iname_comp10a,v.iname_comp13a)
  A1014 <- jaccard_iname(v.iname_comp10a,v.iname_comp14a)
  A1015 <- jaccard_iname(v.iname_comp10a,v.iname_comp15a)
  A1016 <- jaccard_iname(v.iname_comp10a,v.iname_comp16a)
  
  #row11
  A111 <- jaccard_iname(v.iname_comp11a,v.iname_comp1a)
  A112 <- jaccard_iname(v.iname_comp11a,v.iname_comp2a)
  A113 <- jaccard_iname(v.iname_comp11a,v.iname_comp3a)
  A114 <- jaccard_iname(v.iname_comp11a,v.iname_comp4a)
  A115 <- jaccard_iname(v.iname_comp11a,v.iname_comp5a)
  A116 <- jaccard_iname(v.iname_comp11a,v.iname_comp6a)
  A117 <- jaccard_iname(v.iname_comp11a,v.iname_comp7a)
  A118 <- jaccard_iname(v.iname_comp11a,v.iname_comp8a)
  A119 <- jaccard_iname(v.iname_comp11a,v.iname_comp9a)
  A1110 <- jaccard_iname(v.iname_comp11a,v.iname_comp10a)
  A1111 <- jaccard_iname(v.iname_comp11a,v.iname_comp11a)
  A1112 <- jaccard_iname(v.iname_comp11a,v.iname_comp12a)
  A1113 <- jaccard_iname(v.iname_comp11a,v.iname_comp13a)
  A1114 <- jaccard_iname(v.iname_comp11a,v.iname_comp14a)
  A1115 <- jaccard_iname(v.iname_comp11a,v.iname_comp15a)
  A1116 <- jaccard_iname(v.iname_comp11a,v.iname_comp16a)
  
  #row12
  A121 <- jaccard_iname(v.iname_comp12a,v.iname_comp1a)
  A122 <- jaccard_iname(v.iname_comp12a,v.iname_comp2a)
  A123 <- jaccard_iname(v.iname_comp12a,v.iname_comp3a)
  A124 <- jaccard_iname(v.iname_comp12a,v.iname_comp4a)
  A125 <- jaccard_iname(v.iname_comp12a,v.iname_comp5a)
  A126 <- jaccard_iname(v.iname_comp12a,v.iname_comp6a)
  A127 <- jaccard_iname(v.iname_comp12a,v.iname_comp7a)
  A128 <- jaccard_iname(v.iname_comp12a,v.iname_comp8a)
  A129 <- jaccard_iname(v.iname_comp12a,v.iname_comp9a)
  A1210 <- jaccard_iname(v.iname_comp12a,v.iname_comp10a)
  A1211 <- jaccard_iname(v.iname_comp12a,v.iname_comp11a)
  A1212 <- jaccard_iname(v.iname_comp12a,v.iname_comp12a)
  A1213 <- jaccard_iname(v.iname_comp12a,v.iname_comp13a)
  A1214 <- jaccard_iname(v.iname_comp12a,v.iname_comp14a)
  A1215 <- jaccard_iname(v.iname_comp12a,v.iname_comp15a)
  A1216 <- jaccard_iname(v.iname_comp12a,v.iname_comp16a)
  
  #row13
  A131 <- jaccard_iname(v.iname_comp13a,v.iname_comp1a)
  A132 <- jaccard_iname(v.iname_comp13a,v.iname_comp2a)
  A133 <- jaccard_iname(v.iname_comp13a,v.iname_comp3a)
  A134 <- jaccard_iname(v.iname_comp13a,v.iname_comp4a)
  A135 <- jaccard_iname(v.iname_comp13a,v.iname_comp5a)
  A136 <- jaccard_iname(v.iname_comp13a,v.iname_comp6a)
  A137 <- jaccard_iname(v.iname_comp13a,v.iname_comp7a)
  A138 <- jaccard_iname(v.iname_comp13a,v.iname_comp8a)
  A139 <- jaccard_iname(v.iname_comp13a,v.iname_comp9a)
  A1310 <- jaccard_iname(v.iname_comp13a,v.iname_comp10a)
  A1311 <- jaccard_iname(v.iname_comp13a,v.iname_comp11a)
  A1312 <- jaccard_iname(v.iname_comp13a,v.iname_comp12a)
  A1313 <- jaccard_iname(v.iname_comp13a,v.iname_comp13a)
  A1314 <- jaccard_iname(v.iname_comp13a,v.iname_comp14a)
  A1315 <- jaccard_iname(v.iname_comp13a,v.iname_comp15a)
  A1316 <- jaccard_iname(v.iname_comp13a,v.iname_comp16a)
  
  #row14
  A141 <- jaccard_iname(v.iname_comp14a,v.iname_comp1a)
  A142 <- jaccard_iname(v.iname_comp14a,v.iname_comp2a)
  A143 <- jaccard_iname(v.iname_comp14a,v.iname_comp3a)
  A144 <- jaccard_iname(v.iname_comp14a,v.iname_comp4a)
  A145 <- jaccard_iname(v.iname_comp14a,v.iname_comp5a)
  A146 <- jaccard_iname(v.iname_comp14a,v.iname_comp6a)
  A147 <- jaccard_iname(v.iname_comp14a,v.iname_comp7a)
  A148 <- jaccard_iname(v.iname_comp14a,v.iname_comp8a)
  A149 <- jaccard_iname(v.iname_comp14a,v.iname_comp9a)
  A1410 <- jaccard_iname(v.iname_comp14a,v.iname_comp10a)
  A1411 <- jaccard_iname(v.iname_comp14a,v.iname_comp11a)
  A1412 <- jaccard_iname(v.iname_comp14a,v.iname_comp12a)
  A1413 <- jaccard_iname(v.iname_comp14a,v.iname_comp13a)
  A1414 <- jaccard_iname(v.iname_comp14a,v.iname_comp14a)
  A1415 <- jaccard_iname(v.iname_comp14a,v.iname_comp15a)
  A1416 <- jaccard_iname(v.iname_comp14a,v.iname_comp16a)
  
  #row15
  A151 <- jaccard_iname(v.iname_comp15a,v.iname_comp1a)
  A152 <- jaccard_iname(v.iname_comp15a,v.iname_comp2a)
  A153 <- jaccard_iname(v.iname_comp15a,v.iname_comp3a)
  A154 <- jaccard_iname(v.iname_comp15a,v.iname_comp4a)
  A155 <- jaccard_iname(v.iname_comp15a,v.iname_comp5a)
  A156 <- jaccard_iname(v.iname_comp15a,v.iname_comp6a)
  A157 <- jaccard_iname(v.iname_comp15a,v.iname_comp7a)
  A158 <- jaccard_iname(v.iname_comp15a,v.iname_comp8a)
  A159 <- jaccard_iname(v.iname_comp15a,v.iname_comp9a)
  A1510 <- jaccard_iname(v.iname_comp15a,v.iname_comp10a)
  A1511 <- jaccard_iname(v.iname_comp15a,v.iname_comp11a)
  A1512 <- jaccard_iname(v.iname_comp15a,v.iname_comp12a)
  A1513 <- jaccard_iname(v.iname_comp15a,v.iname_comp13a)
  A1514 <- jaccard_iname(v.iname_comp15a,v.iname_comp14a)
  A1515 <- jaccard_iname(v.iname_comp15a,v.iname_comp15a)
  A1516 <- jaccard_iname(v.iname_comp15a,v.iname_comp16a)
  
  #row16
  A161 <- jaccard_iname(v.iname_comp16a,v.iname_comp1a)
  A162 <- jaccard_iname(v.iname_comp16a,v.iname_comp2a)
  A163 <- jaccard_iname(v.iname_comp16a,v.iname_comp3a)
  A164 <- jaccard_iname(v.iname_comp16a,v.iname_comp4a)
  A165 <- jaccard_iname(v.iname_comp16a,v.iname_comp5a)
  A166 <- jaccard_iname(v.iname_comp16a,v.iname_comp6a)
  A167 <- jaccard_iname(v.iname_comp16a,v.iname_comp7a)
  A168 <- jaccard_iname(v.iname_comp16a,v.iname_comp8a)
  A169 <- jaccard_iname(v.iname_comp16a,v.iname_comp9a)
  A1610 <- jaccard_iname(v.iname_comp16a,v.iname_comp10a)
  A1611 <- jaccard_iname(v.iname_comp16a,v.iname_comp11a)
  A1612 <- jaccard_iname(v.iname_comp16a,v.iname_comp12a)
  A1613 <- jaccard_iname(v.iname_comp16a,v.iname_comp13a)
  A1614 <- jaccard_iname(v.iname_comp16a,v.iname_comp14a)
  A1615 <- jaccard_iname(v.iname_comp16a,v.iname_comp15a)
  A1616 <- jaccard_iname(v.iname_comp16a,v.iname_comp16a)
  
  EN_A <- t(matrix(c(A11  ,A12  ,A13  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,
                     A21  ,A22  ,A23  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,A214 ,A215 ,A216 ,
                     A31  ,A32  ,A33  ,A34  ,A35  ,A36  ,A37  ,A38  ,A39  ,A310 ,A311 ,A312 ,A313 ,A314 ,A315 ,A316 ,
                     A41  ,A42  ,A43  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,A414 ,A415 ,A416 ,
                     A51  ,A52  ,A53  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,A514 ,A515 ,A516 ,
                     A61  ,A62  ,A63  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,A614 ,A615 ,A616 ,
                     A71  ,A72  ,A73  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,A714 ,A715 ,A716 ,
                     A81  ,A82  ,A83  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,A814 ,A815 ,A816 ,
                     A91  ,A92  ,A93  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,A914 ,A915 ,A916 ,
                     A101 ,A102 ,A103 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,A1014,A1015,A1016,
                     A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,A1114,A1115,A1116,
                     A121 ,A122 ,A123 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,A1214,A1215,A1216,
                     A131 ,A132 ,A133 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313,A1314,A1315,A1316,
                     A141 ,A142 ,A143 ,A144 ,A145 ,A146 ,A147 ,A148 ,A149 ,A1410,A1411,A1412,A1413,A1414,A1415,A1416,
                     A151 ,A152 ,A153 ,A154 ,A155 ,A156 ,A157 ,A158 ,A159 ,A1510,A1511,A1512,A1513,A1514,A1515,A1516,
                     A161 ,A162 ,A163 ,A164 ,A165 ,A166 ,A167 ,A168 ,A169 ,A1610,A1611,A1612,A1613,A1614,A1615,A1616), nrow=16, ncol=16))
  
  return(EN_A)
  
}
m.iname <- matrix_dist_iname(v.iname)

v.iname.j <- c(v.iname_compJ1a,v.iname_compJ2a,v.iname_compJ4a,v.iname_compJ5a,v.iname_compJ6a,v.iname_compJ7a,v.iname_compJ8a,v.iname_compJ9a,v.iname_compJ10a,v.iname_compJ11a,v.iname_compJ12a,v.iname_compJ13a)
matrix_dist_iname_j <- function (v.iname.j) {
  A11 <- jaccard_iname(v.iname_comp1a,v.iname_comp1a)
  A12 <- jaccard_iname(v.iname_comp1a,v.iname_comp2a)
  
  A14 <- jaccard_iname(v.iname_comp1a,v.iname_comp4a)
  A15 <- jaccard_iname(v.iname_comp1a,v.iname_comp5a)
  A16 <- jaccard_iname(v.iname_comp1a,v.iname_comp6a)
  A17 <- jaccard_iname(v.iname_comp1a,v.iname_comp7a)
  A18 <- jaccard_iname(v.iname_comp1a,v.iname_comp8a)
  A19 <- jaccard_iname(v.iname_comp1a,v.iname_comp9a)
  A110 <- jaccard_iname(v.iname_comp1a,v.iname_comp10a)
  A111 <- jaccard_iname(v.iname_comp1a,v.iname_comp11a)
  A112 <- jaccard_iname(v.iname_comp1a,v.iname_comp12a)
  A113 <- jaccard_iname(v.iname_comp1a,v.iname_comp13a)
  
  #row2
  A21 <- jaccard_iname(v.iname_comp2a,v.iname_comp1a)
  A22 <- jaccard_iname(v.iname_comp2a,v.iname_comp2a)
  
  A24 <- jaccard_iname(v.iname_comp2a,v.iname_comp4a)
  A25 <- jaccard_iname(v.iname_comp2a,v.iname_comp5a)
  A26 <- jaccard_iname(v.iname_comp2a,v.iname_comp6a)
  A27 <- jaccard_iname(v.iname_comp2a,v.iname_comp7a)
  A28 <- jaccard_iname(v.iname_comp2a,v.iname_comp8a)
  A29 <- jaccard_iname(v.iname_comp2a,v.iname_comp9a)
  A210 <- jaccard_iname(v.iname_comp2a,v.iname_comp10a)
  A211 <- jaccard_iname(v.iname_comp2a,v.iname_comp11a)
  A212 <- jaccard_iname(v.iname_comp2a,v.iname_comp12a)
  A213 <- jaccard_iname(v.iname_comp2a,v.iname_comp13a)
  
  #row4
  A41 <- jaccard_iname(v.iname_comp4a,v.iname_comp1a)
  A42 <- jaccard_iname(v.iname_comp4a,v.iname_comp2a)
  
  A44 <- jaccard_iname(v.iname_comp4a,v.iname_comp4a)
  A45 <- jaccard_iname(v.iname_comp4a,v.iname_comp5a)
  A46 <- jaccard_iname(v.iname_comp4a,v.iname_comp6a)
  A47 <- jaccard_iname(v.iname_comp4a,v.iname_comp7a)
  A48 <- jaccard_iname(v.iname_comp4a,v.iname_comp8a)
  A49 <- jaccard_iname(v.iname_comp4a,v.iname_comp9a)
  A410 <- jaccard_iname(v.iname_comp4a,v.iname_comp10a)
  A411 <- jaccard_iname(v.iname_comp4a,v.iname_comp11a)
  A412 <- jaccard_iname(v.iname_comp4a,v.iname_comp12a)
  A413 <- jaccard_iname(v.iname_comp4a,v.iname_comp13a)
  
  #row5
  A51 <- jaccard_iname(v.iname_comp5a,v.iname_comp1a)
  A52 <- jaccard_iname(v.iname_comp5a,v.iname_comp2a)
  
  A54 <- jaccard_iname(v.iname_comp5a,v.iname_comp4a)
  A55 <- jaccard_iname(v.iname_comp5a,v.iname_comp5a)
  A56 <- jaccard_iname(v.iname_comp5a,v.iname_comp6a)
  A57 <- jaccard_iname(v.iname_comp5a,v.iname_comp7a)
  A58 <- jaccard_iname(v.iname_comp5a,v.iname_comp8a)
  A59 <- jaccard_iname(v.iname_comp5a,v.iname_comp9a)
  A510 <- jaccard_iname(v.iname_comp5a,v.iname_comp10a)
  A511 <- jaccard_iname(v.iname_comp5a,v.iname_comp11a)
  A512 <- jaccard_iname(v.iname_comp5a,v.iname_comp12a)
  A513 <- jaccard_iname(v.iname_comp5a,v.iname_comp13a)
  
  #row6
  A61 <- jaccard_iname(v.iname_comp6a,v.iname_comp1a)
  A62 <- jaccard_iname(v.iname_comp6a,v.iname_comp2a)
  
  A64 <- jaccard_iname(v.iname_comp6a,v.iname_comp4a)
  A65 <- jaccard_iname(v.iname_comp6a,v.iname_comp5a)
  A66 <- jaccard_iname(v.iname_comp6a,v.iname_comp6a)
  A67 <- jaccard_iname(v.iname_comp6a,v.iname_comp7a)
  A68 <- jaccard_iname(v.iname_comp6a,v.iname_comp8a)
  A69 <- jaccard_iname(v.iname_comp6a,v.iname_comp9a)
  A610 <- jaccard_iname(v.iname_comp6a,v.iname_comp10a)
  A611 <- jaccard_iname(v.iname_comp6a,v.iname_comp11a)
  A612 <- jaccard_iname(v.iname_comp6a,v.iname_comp12a)
  A613 <- jaccard_iname(v.iname_comp6a,v.iname_comp13a)
  
  #row7
  A71 <- jaccard_iname(v.iname_comp7a,v.iname_comp1a)
  A72 <- jaccard_iname(v.iname_comp7a,v.iname_comp2a)
  
  A74 <- jaccard_iname(v.iname_comp7a,v.iname_comp4a)
  A75 <- jaccard_iname(v.iname_comp7a,v.iname_comp5a)
  A76 <- jaccard_iname(v.iname_comp7a,v.iname_comp6a)
  A77 <- jaccard_iname(v.iname_comp7a,v.iname_comp7a)
  A78 <- jaccard_iname(v.iname_comp7a,v.iname_comp8a)
  A79 <- jaccard_iname(v.iname_comp7a,v.iname_comp9a)
  A710 <- jaccard_iname(v.iname_comp7a,v.iname_comp10a)
  A711 <- jaccard_iname(v.iname_comp7a,v.iname_comp11a)
  A712 <- jaccard_iname(v.iname_comp7a,v.iname_comp12a)
  A713 <- jaccard_iname(v.iname_comp7a,v.iname_comp13a)
  
  #row8
  A81 <- jaccard_iname(v.iname_comp8a,v.iname_comp1a)
  A82 <- jaccard_iname(v.iname_comp8a,v.iname_comp2a)
  
  A84 <- jaccard_iname(v.iname_comp8a,v.iname_comp4a)
  A85 <- jaccard_iname(v.iname_comp8a,v.iname_comp5a)
  A86 <- jaccard_iname(v.iname_comp8a,v.iname_comp6a)
  A87 <- jaccard_iname(v.iname_comp8a,v.iname_comp7a)
  A88 <- jaccard_iname(v.iname_comp8a,v.iname_comp8a)
  A89 <- jaccard_iname(v.iname_comp8a,v.iname_comp9a)
  A810 <- jaccard_iname(v.iname_comp8a,v.iname_comp10a)
  A811 <- jaccard_iname(v.iname_comp8a,v.iname_comp11a)
  A812 <- jaccard_iname(v.iname_comp8a,v.iname_comp12a)
  A813 <- jaccard_iname(v.iname_comp8a,v.iname_comp13a)
  
  #row9
  A91 <- jaccard_iname(v.iname_comp9a,v.iname_comp1a)
  A92 <- jaccard_iname(v.iname_comp9a,v.iname_comp2a)
  
  A94 <- jaccard_iname(v.iname_comp9a,v.iname_comp4a)
  A95 <- jaccard_iname(v.iname_comp9a,v.iname_comp5a)
  A96 <- jaccard_iname(v.iname_comp9a,v.iname_comp6a)
  A97 <- jaccard_iname(v.iname_comp9a,v.iname_comp7a)
  A98 <- jaccard_iname(v.iname_comp9a,v.iname_comp8a)
  A99 <- jaccard_iname(v.iname_comp9a,v.iname_comp9a)
  A910 <- jaccard_iname(v.iname_comp9a,v.iname_comp10a)
  A911 <- jaccard_iname(v.iname_comp9a,v.iname_comp11a)
  A912 <- jaccard_iname(v.iname_comp9a,v.iname_comp12a)
  A913 <- jaccard_iname(v.iname_comp9a,v.iname_comp13a)
  
  #row10
  A101 <- jaccard_iname(v.iname_comp10a,v.iname_comp1a)
  A102 <- jaccard_iname(v.iname_comp10a,v.iname_comp2a)
  
  A104 <- jaccard_iname(v.iname_comp10a,v.iname_comp4a)
  A105 <- jaccard_iname(v.iname_comp10a,v.iname_comp5a)
  A106 <- jaccard_iname(v.iname_comp10a,v.iname_comp6a)
  A107 <- jaccard_iname(v.iname_comp10a,v.iname_comp7a)
  A108 <- jaccard_iname(v.iname_comp10a,v.iname_comp8a)
  A109 <- jaccard_iname(v.iname_comp10a,v.iname_comp9a)
  A1010 <- jaccard_iname(v.iname_comp10a,v.iname_comp10a)
  A1011 <- jaccard_iname(v.iname_comp10a,v.iname_comp11a)
  A1012 <- jaccard_iname(v.iname_comp10a,v.iname_comp12a)
  A1013 <- jaccard_iname(v.iname_comp10a,v.iname_comp13a)
  
  #row11
  A111 <- jaccard_iname(v.iname_comp11a,v.iname_comp1a)
  A112 <- jaccard_iname(v.iname_comp11a,v.iname_comp2a)
  
  A114 <- jaccard_iname(v.iname_comp11a,v.iname_comp4a)
  A115 <- jaccard_iname(v.iname_comp11a,v.iname_comp5a)
  A116 <- jaccard_iname(v.iname_comp11a,v.iname_comp6a)
  A117 <- jaccard_iname(v.iname_comp11a,v.iname_comp7a)
  A118 <- jaccard_iname(v.iname_comp11a,v.iname_comp8a)
  A119 <- jaccard_iname(v.iname_comp11a,v.iname_comp9a)
  A1110 <- jaccard_iname(v.iname_comp11a,v.iname_comp10a)
  A1111 <- jaccard_iname(v.iname_comp11a,v.iname_comp11a)
  A1112 <- jaccard_iname(v.iname_comp11a,v.iname_comp12a)
  A1113 <- jaccard_iname(v.iname_comp11a,v.iname_comp13a)
  A1114 <- jaccard_iname(v.iname_comp11a,v.iname_comp14a)
  A1115 <- jaccard_iname(v.iname_comp11a,v.iname_comp15a)
  A1116 <- jaccard_iname(v.iname_comp11a,v.iname_comp16a)
  
  #row12
  A121 <- jaccard_iname(v.iname_comp12a,v.iname_comp1a)
  A122 <- jaccard_iname(v.iname_comp12a,v.iname_comp2a)
  
  A124 <- jaccard_iname(v.iname_comp12a,v.iname_comp4a)
  A125 <- jaccard_iname(v.iname_comp12a,v.iname_comp5a)
  A126 <- jaccard_iname(v.iname_comp12a,v.iname_comp6a)
  A127 <- jaccard_iname(v.iname_comp12a,v.iname_comp7a)
  A128 <- jaccard_iname(v.iname_comp12a,v.iname_comp8a)
  A129 <- jaccard_iname(v.iname_comp12a,v.iname_comp9a)
  A1210 <- jaccard_iname(v.iname_comp12a,v.iname_comp10a)
  A1211 <- jaccard_iname(v.iname_comp12a,v.iname_comp11a)
  A1212 <- jaccard_iname(v.iname_comp12a,v.iname_comp12a)
  A1213 <- jaccard_iname(v.iname_comp12a,v.iname_comp13a)
  
  #row13
  A131 <- jaccard_iname(v.iname_comp13a,v.iname_comp1a)
  A132 <- jaccard_iname(v.iname_comp13a,v.iname_comp2a)
  
  A134 <- jaccard_iname(v.iname_comp13a,v.iname_comp4a)
  A135 <- jaccard_iname(v.iname_comp13a,v.iname_comp5a)
  A136 <- jaccard_iname(v.iname_comp13a,v.iname_comp6a)
  A137 <- jaccard_iname(v.iname_comp13a,v.iname_comp7a)
  A138 <- jaccard_iname(v.iname_comp13a,v.iname_comp8a)
  A139 <- jaccard_iname(v.iname_comp13a,v.iname_comp9a)
  A1310 <- jaccard_iname(v.iname_comp13a,v.iname_comp10a)
  A1311 <- jaccard_iname(v.iname_comp13a,v.iname_comp11a)
  A1312 <- jaccard_iname(v.iname_comp13a,v.iname_comp12a)
  A1313 <- jaccard_iname(v.iname_comp13a,v.iname_comp13a)
  
  
  
  EN_A <- t(matrix(c(A11  ,A12  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,
                     A21  ,A22  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,
                     A41  ,A42  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,
                     A51  ,A52  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,
                     A61  ,A62  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,
                     A71  ,A72  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,
                     A81  ,A82  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,
                     A91  ,A92  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,
                     A101 ,A102 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,
                     A111 ,A112 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,
                     A121 ,A122 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,
                     A131 ,A132 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313), nrow=12, ncol=12))
  
  return(EN_A)
  
}
m.iname.j <- matrix_dist_iname_j(v.iname.j)

#SOA
v.soa <- c(v.soa_comp1a,v.soa_comp2a,v.soa_comp3a,v.soa_comp4a,v.soa_comp5a,v.soa_comp6a,v.soa_comp7a,v.soa_comp8a,v.soa_comp9a,v.soa_comp10a,v.soa_comp11a,v.soa_comp12a,v.soa_comp13a,v.soa_comp14a,v.soa_comp15a,v.soa_comp16a)
matrix_dist_soa <- function (v.soa) {
  A11 <- jaccard_soa(v.soa_comp1a,v.soa_comp1a)
  A12 <- jaccard_soa(v.soa_comp1a,v.soa_comp2a)
  A13 <- jaccard_soa(v.soa_comp1a,v.soa_comp3a)
  A14 <- jaccard_soa(v.soa_comp1a,v.soa_comp4a)
  A15 <- jaccard_soa(v.soa_comp1a,v.soa_comp5a)
  A16 <- jaccard_soa(v.soa_comp1a,v.soa_comp6a)
  A17 <- jaccard_soa(v.soa_comp1a,v.soa_comp7a)
  A18 <- jaccard_soa(v.soa_comp1a,v.soa_comp8a)
  A19 <- jaccard_soa(v.soa_comp1a,v.soa_comp9a)
  A110 <- jaccard_soa(v.soa_comp1a,v.soa_comp10a)
  A111 <- jaccard_soa(v.soa_comp1a,v.soa_comp11a)
  A112 <- jaccard_soa(v.soa_comp1a,v.soa_comp12a)
  A113 <- jaccard_soa(v.soa_comp1a,v.soa_comp13a)
  A114 <- jaccard_soa(v.soa_comp1a,v.soa_comp14a)
  A115 <- jaccard_soa(v.soa_comp1a,v.soa_comp15a)
  A116 <- jaccard_soa(v.soa_comp1a,v.soa_comp16a)
  
  #row2
  A21 <- jaccard_soa(v.soa_comp2a,v.soa_comp1a)
  A22 <- jaccard_soa(v.soa_comp2a,v.soa_comp2a)
  A23 <- jaccard_soa(v.soa_comp2a,v.soa_comp3a)
  A24 <- jaccard_soa(v.soa_comp2a,v.soa_comp4a)
  A25 <- jaccard_soa(v.soa_comp2a,v.soa_comp5a)
  A26 <- jaccard_soa(v.soa_comp2a,v.soa_comp6a)
  A27 <- jaccard_soa(v.soa_comp2a,v.soa_comp7a)
  A28 <- jaccard_soa(v.soa_comp2a,v.soa_comp8a)
  A29 <- jaccard_soa(v.soa_comp2a,v.soa_comp9a)
  A210 <- jaccard_soa(v.soa_comp2a,v.soa_comp10a)
  A211 <- jaccard_soa(v.soa_comp2a,v.soa_comp11a)
  A212 <- jaccard_soa(v.soa_comp2a,v.soa_comp12a)
  A213 <- jaccard_soa(v.soa_comp2a,v.soa_comp13a)
  A214 <- jaccard_soa(v.soa_comp2a,v.soa_comp14a)
  A215 <- jaccard_soa(v.soa_comp2a,v.soa_comp15a)
  A216 <- jaccard_soa(v.soa_comp2a,v.soa_comp16a)
  
  #row3
  A31 <- jaccard_soa(v.soa_comp3a,v.soa_comp1a)
  A32 <- jaccard_soa(v.soa_comp3a,v.soa_comp2a)
  A33 <- jaccard_soa(v.soa_comp3a,v.soa_comp3a)
  A34 <- jaccard_soa(v.soa_comp3a,v.soa_comp4a)
  A35 <- jaccard_soa(v.soa_comp3a,v.soa_comp5a)
  A36 <- jaccard_soa(v.soa_comp3a,v.soa_comp6a)
  A37 <- jaccard_soa(v.soa_comp3a,v.soa_comp7a)
  A38 <- jaccard_soa(v.soa_comp3a,v.soa_comp8a)
  A39 <- jaccard_soa(v.soa_comp3a,v.soa_comp9a)
  A310 <- jaccard_soa(v.soa_comp3a,v.soa_comp10a)
  A311 <- jaccard_soa(v.soa_comp3a,v.soa_comp11a)
  A312 <- jaccard_soa(v.soa_comp3a,v.soa_comp12a)
  A313 <- jaccard_soa(v.soa_comp3a,v.soa_comp13a)
  A314 <- jaccard_soa(v.soa_comp3a,v.soa_comp14a)
  A315 <- jaccard_soa(v.soa_comp3a,v.soa_comp15a)
  A316 <- jaccard_soa(v.soa_comp3a,v.soa_comp16a)
  
  #row4
  A41 <- jaccard_soa(v.soa_comp4a,v.soa_comp1a)
  A42 <- jaccard_soa(v.soa_comp4a,v.soa_comp2a)
  A43 <- jaccard_soa(v.soa_comp4a,v.soa_comp3a)
  A44 <- jaccard_soa(v.soa_comp4a,v.soa_comp4a)
  A45 <- jaccard_soa(v.soa_comp4a,v.soa_comp5a)
  A46 <- jaccard_soa(v.soa_comp4a,v.soa_comp6a)
  A47 <- jaccard_soa(v.soa_comp4a,v.soa_comp7a)
  A48 <- jaccard_soa(v.soa_comp4a,v.soa_comp8a)
  A49 <- jaccard_soa(v.soa_comp4a,v.soa_comp9a)
  A410 <- jaccard_soa(v.soa_comp4a,v.soa_comp10a)
  A411 <- jaccard_soa(v.soa_comp4a,v.soa_comp11a)
  A412 <- jaccard_soa(v.soa_comp4a,v.soa_comp12a)
  A413 <- jaccard_soa(v.soa_comp4a,v.soa_comp13a)
  A414 <- jaccard_soa(v.soa_comp4a,v.soa_comp14a)
  A415 <- jaccard_soa(v.soa_comp4a,v.soa_comp15a)
  A416 <- jaccard_soa(v.soa_comp4a,v.soa_comp16a)
  
  #row5
  A51 <- jaccard_soa(v.soa_comp5a,v.soa_comp1a)
  A52 <- jaccard_soa(v.soa_comp5a,v.soa_comp2a)
  A53 <- jaccard_soa(v.soa_comp5a,v.soa_comp3a)
  A54 <- jaccard_soa(v.soa_comp5a,v.soa_comp4a)
  A55 <- jaccard_soa(v.soa_comp5a,v.soa_comp5a)
  A56 <- jaccard_soa(v.soa_comp5a,v.soa_comp6a)
  A57 <- jaccard_soa(v.soa_comp5a,v.soa_comp7a)
  A58 <- jaccard_soa(v.soa_comp5a,v.soa_comp8a)
  A59 <- jaccard_soa(v.soa_comp5a,v.soa_comp9a)
  A510 <- jaccard_soa(v.soa_comp5a,v.soa_comp10a)
  A511 <- jaccard_soa(v.soa_comp5a,v.soa_comp11a)
  A512 <- jaccard_soa(v.soa_comp5a,v.soa_comp12a)
  A513 <- jaccard_soa(v.soa_comp5a,v.soa_comp13a)
  A514 <- jaccard_soa(v.soa_comp5a,v.soa_comp14a)
  A515 <- jaccard_soa(v.soa_comp5a,v.soa_comp15a)
  A516 <- jaccard_soa(v.soa_comp5a,v.soa_comp16a)
  
  #row6
  A61 <- jaccard_soa(v.soa_comp6a,v.soa_comp1a)
  A62 <- jaccard_soa(v.soa_comp6a,v.soa_comp2a)
  A63 <- jaccard_soa(v.soa_comp6a,v.soa_comp3a)
  A64 <- jaccard_soa(v.soa_comp6a,v.soa_comp4a)
  A65 <- jaccard_soa(v.soa_comp6a,v.soa_comp5a)
  A66 <- jaccard_soa(v.soa_comp6a,v.soa_comp6a)
  A67 <- jaccard_soa(v.soa_comp6a,v.soa_comp7a)
  A68 <- jaccard_soa(v.soa_comp6a,v.soa_comp8a)
  A69 <- jaccard_soa(v.soa_comp6a,v.soa_comp9a)
  A610 <- jaccard_soa(v.soa_comp6a,v.soa_comp10a)
  A611 <- jaccard_soa(v.soa_comp6a,v.soa_comp11a)
  A612 <- jaccard_soa(v.soa_comp6a,v.soa_comp12a)
  A613 <- jaccard_soa(v.soa_comp6a,v.soa_comp13a)
  A614 <- jaccard_soa(v.soa_comp6a,v.soa_comp14a)
  A615 <- jaccard_soa(v.soa_comp6a,v.soa_comp15a)
  A616 <- jaccard_soa(v.soa_comp6a,v.soa_comp16a)
  
  #row7
  A71 <- jaccard_soa(v.soa_comp7a,v.soa_comp1a)
  A72 <- jaccard_soa(v.soa_comp7a,v.soa_comp2a)
  A73 <- jaccard_soa(v.soa_comp7a,v.soa_comp3a)
  A74 <- jaccard_soa(v.soa_comp7a,v.soa_comp4a)
  A75 <- jaccard_soa(v.soa_comp7a,v.soa_comp5a)
  A76 <- jaccard_soa(v.soa_comp7a,v.soa_comp6a)
  A77 <- jaccard_soa(v.soa_comp7a,v.soa_comp7a)
  A78 <- jaccard_soa(v.soa_comp7a,v.soa_comp8a)
  A79 <- jaccard_soa(v.soa_comp7a,v.soa_comp9a)
  A710 <- jaccard_soa(v.soa_comp7a,v.soa_comp10a)
  A711 <- jaccard_soa(v.soa_comp7a,v.soa_comp11a)
  A712 <- jaccard_soa(v.soa_comp7a,v.soa_comp12a)
  A713 <- jaccard_soa(v.soa_comp7a,v.soa_comp13a)
  A714 <- jaccard_soa(v.soa_comp7a,v.soa_comp14a)
  A715 <- jaccard_soa(v.soa_comp7a,v.soa_comp15a)
  A716 <- jaccard_soa(v.soa_comp7a,v.soa_comp16a)
  
  #row8
  A81 <- jaccard_soa(v.soa_comp8a,v.soa_comp1a)
  A82 <- jaccard_soa(v.soa_comp8a,v.soa_comp2a)
  A83 <- jaccard_soa(v.soa_comp8a,v.soa_comp3a)
  A84 <- jaccard_soa(v.soa_comp8a,v.soa_comp4a)
  A85 <- jaccard_soa(v.soa_comp8a,v.soa_comp5a)
  A86 <- jaccard_soa(v.soa_comp8a,v.soa_comp6a)
  A87 <- jaccard_soa(v.soa_comp8a,v.soa_comp7a)
  A88 <- jaccard_soa(v.soa_comp8a,v.soa_comp8a)
  A89 <- jaccard_soa(v.soa_comp8a,v.soa_comp9a)
  A810 <- jaccard_soa(v.soa_comp8a,v.soa_comp10a)
  A811 <- jaccard_soa(v.soa_comp8a,v.soa_comp11a)
  A812 <- jaccard_soa(v.soa_comp8a,v.soa_comp12a)
  A813 <- jaccard_soa(v.soa_comp8a,v.soa_comp13a)
  A814 <- jaccard_soa(v.soa_comp8a,v.soa_comp14a)
  A815 <- jaccard_soa(v.soa_comp8a,v.soa_comp15a)
  A816 <- jaccard_soa(v.soa_comp8a,v.soa_comp16a)
  
  #row9
  A91 <- jaccard_soa(v.soa_comp9a,v.soa_comp1a)
  A92 <- jaccard_soa(v.soa_comp9a,v.soa_comp2a)
  A93 <- jaccard_soa(v.soa_comp9a,v.soa_comp3a)
  A94 <- jaccard_soa(v.soa_comp9a,v.soa_comp4a)
  A95 <- jaccard_soa(v.soa_comp9a,v.soa_comp5a)
  A96 <- jaccard_soa(v.soa_comp9a,v.soa_comp6a)
  A97 <- jaccard_soa(v.soa_comp9a,v.soa_comp7a)
  A98 <- jaccard_soa(v.soa_comp9a,v.soa_comp8a)
  A99 <- jaccard_soa(v.soa_comp9a,v.soa_comp9a)
  A910 <- jaccard_soa(v.soa_comp9a,v.soa_comp10a)
  A911 <- jaccard_soa(v.soa_comp9a,v.soa_comp11a)
  A912 <- jaccard_soa(v.soa_comp9a,v.soa_comp12a)
  A913 <- jaccard_soa(v.soa_comp9a,v.soa_comp13a)
  A914 <- jaccard_soa(v.soa_comp9a,v.soa_comp14a)
  A915 <- jaccard_soa(v.soa_comp9a,v.soa_comp15a)
  A916 <- jaccard_soa(v.soa_comp9a,v.soa_comp16a)
  
  #row10
  A101 <- jaccard_soa(v.soa_comp10a,v.soa_comp1a)
  A102 <- jaccard_soa(v.soa_comp10a,v.soa_comp2a)
  A103 <- jaccard_soa(v.soa_comp10a,v.soa_comp3a)
  A104 <- jaccard_soa(v.soa_comp10a,v.soa_comp4a)
  A105 <- jaccard_soa(v.soa_comp10a,v.soa_comp5a)
  A106 <- jaccard_soa(v.soa_comp10a,v.soa_comp6a)
  A107 <- jaccard_soa(v.soa_comp10a,v.soa_comp7a)
  A108 <- jaccard_soa(v.soa_comp10a,v.soa_comp8a)
  A109 <- jaccard_soa(v.soa_comp10a,v.soa_comp9a)
  A1010 <- jaccard_soa(v.soa_comp10a,v.soa_comp10a)
  A1011 <- jaccard_soa(v.soa_comp10a,v.soa_comp11a)
  A1012 <- jaccard_soa(v.soa_comp10a,v.soa_comp12a)
  A1013 <- jaccard_soa(v.soa_comp10a,v.soa_comp13a)
  A1014 <- jaccard_soa(v.soa_comp10a,v.soa_comp14a)
  A1015 <- jaccard_soa(v.soa_comp10a,v.soa_comp15a)
  A1016 <- jaccard_soa(v.soa_comp10a,v.soa_comp16a)
  
  #row11
  A111 <- jaccard_soa(v.soa_comp11a,v.soa_comp1a)
  A112 <- jaccard_soa(v.soa_comp11a,v.soa_comp2a)
  A113 <- jaccard_soa(v.soa_comp11a,v.soa_comp3a)
  A114 <- jaccard_soa(v.soa_comp11a,v.soa_comp4a)
  A115 <- jaccard_soa(v.soa_comp11a,v.soa_comp5a)
  A116 <- jaccard_soa(v.soa_comp11a,v.soa_comp6a)
  A117 <- jaccard_soa(v.soa_comp11a,v.soa_comp7a)
  A118 <- jaccard_soa(v.soa_comp11a,v.soa_comp8a)
  A119 <- jaccard_soa(v.soa_comp11a,v.soa_comp9a)
  A1110 <- jaccard_soa(v.soa_comp11a,v.soa_comp10a)
  A1111 <- jaccard_soa(v.soa_comp11a,v.soa_comp11a)
  A1112 <- jaccard_soa(v.soa_comp11a,v.soa_comp12a)
  A1113 <- jaccard_soa(v.soa_comp11a,v.soa_comp13a)
  A1114 <- jaccard_soa(v.soa_comp11a,v.soa_comp14a)
  A1115 <- jaccard_soa(v.soa_comp11a,v.soa_comp15a)
  A1116 <- jaccard_soa(v.soa_comp11a,v.soa_comp16a)
  
  #row12
  A121 <- jaccard_soa(v.soa_comp12a,v.soa_comp1a)
  A122 <- jaccard_soa(v.soa_comp12a,v.soa_comp2a)
  A123 <- jaccard_soa(v.soa_comp12a,v.soa_comp3a)
  A124 <- jaccard_soa(v.soa_comp12a,v.soa_comp4a)
  A125 <- jaccard_soa(v.soa_comp12a,v.soa_comp5a)
  A126 <- jaccard_soa(v.soa_comp12a,v.soa_comp6a)
  A127 <- jaccard_soa(v.soa_comp12a,v.soa_comp7a)
  A128 <- jaccard_soa(v.soa_comp12a,v.soa_comp8a)
  A129 <- jaccard_soa(v.soa_comp12a,v.soa_comp9a)
  A1210 <- jaccard_soa(v.soa_comp12a,v.soa_comp10a)
  A1211 <- jaccard_soa(v.soa_comp12a,v.soa_comp11a)
  A1212 <- jaccard_soa(v.soa_comp12a,v.soa_comp12a)
  A1213 <- jaccard_soa(v.soa_comp12a,v.soa_comp13a)
  A1214 <- jaccard_soa(v.soa_comp12a,v.soa_comp14a)
  A1215 <- jaccard_soa(v.soa_comp12a,v.soa_comp15a)
  A1216 <- jaccard_soa(v.soa_comp12a,v.soa_comp16a)
  
  #row13
  A131 <- jaccard_soa(v.soa_comp13a,v.soa_comp1a)
  A132 <- jaccard_soa(v.soa_comp13a,v.soa_comp2a)
  A133 <- jaccard_soa(v.soa_comp13a,v.soa_comp3a)
  A134 <- jaccard_soa(v.soa_comp13a,v.soa_comp4a)
  A135 <- jaccard_soa(v.soa_comp13a,v.soa_comp5a)
  A136 <- jaccard_soa(v.soa_comp13a,v.soa_comp6a)
  A137 <- jaccard_soa(v.soa_comp13a,v.soa_comp7a)
  A138 <- jaccard_soa(v.soa_comp13a,v.soa_comp8a)
  A139 <- jaccard_soa(v.soa_comp13a,v.soa_comp9a)
  A1310 <- jaccard_soa(v.soa_comp13a,v.soa_comp10a)
  A1311 <- jaccard_soa(v.soa_comp13a,v.soa_comp11a)
  A1312 <- jaccard_soa(v.soa_comp13a,v.soa_comp12a)
  A1313 <- jaccard_soa(v.soa_comp13a,v.soa_comp13a)
  A1314 <- jaccard_soa(v.soa_comp13a,v.soa_comp14a)
  A1315 <- jaccard_soa(v.soa_comp13a,v.soa_comp15a)
  A1316 <- jaccard_soa(v.soa_comp13a,v.soa_comp16a)
  
  #row14
  A141 <- jaccard_soa(v.soa_comp14a,v.soa_comp1a)
  A142 <- jaccard_soa(v.soa_comp14a,v.soa_comp2a)
  A143 <- jaccard_soa(v.soa_comp14a,v.soa_comp3a)
  A144 <- jaccard_soa(v.soa_comp14a,v.soa_comp4a)
  A145 <- jaccard_soa(v.soa_comp14a,v.soa_comp5a)
  A146 <- jaccard_soa(v.soa_comp14a,v.soa_comp6a)
  A147 <- jaccard_soa(v.soa_comp14a,v.soa_comp7a)
  A148 <- jaccard_soa(v.soa_comp14a,v.soa_comp8a)
  A149 <- jaccard_soa(v.soa_comp14a,v.soa_comp9a)
  A1410 <- jaccard_soa(v.soa_comp14a,v.soa_comp10a)
  A1411 <- jaccard_soa(v.soa_comp14a,v.soa_comp11a)
  A1412 <- jaccard_soa(v.soa_comp14a,v.soa_comp12a)
  A1413 <- jaccard_soa(v.soa_comp14a,v.soa_comp13a)
  A1414 <- jaccard_soa(v.soa_comp14a,v.soa_comp14a)
  A1415 <- jaccard_soa(v.soa_comp14a,v.soa_comp15a)
  A1416 <- jaccard_soa(v.soa_comp14a,v.soa_comp16a)
  
  #row15
  A151 <- jaccard_soa(v.soa_comp15a,v.soa_comp1a)
  A152 <- jaccard_soa(v.soa_comp15a,v.soa_comp2a)
  A153 <- jaccard_soa(v.soa_comp15a,v.soa_comp3a)
  A154 <- jaccard_soa(v.soa_comp15a,v.soa_comp4a)
  A155 <- jaccard_soa(v.soa_comp15a,v.soa_comp5a)
  A156 <- jaccard_soa(v.soa_comp15a,v.soa_comp6a)
  A157 <- jaccard_soa(v.soa_comp15a,v.soa_comp7a)
  A158 <- jaccard_soa(v.soa_comp15a,v.soa_comp8a)
  A159 <- jaccard_soa(v.soa_comp15a,v.soa_comp9a)
  A1510 <- jaccard_soa(v.soa_comp15a,v.soa_comp10a)
  A1511 <- jaccard_soa(v.soa_comp15a,v.soa_comp11a)
  A1512 <- jaccard_soa(v.soa_comp15a,v.soa_comp12a)
  A1513 <- jaccard_soa(v.soa_comp15a,v.soa_comp13a)
  A1514 <- jaccard_soa(v.soa_comp15a,v.soa_comp14a)
  A1515 <- jaccard_soa(v.soa_comp15a,v.soa_comp15a)
  A1516 <- jaccard_soa(v.soa_comp15a,v.soa_comp16a)
  
  #row16
  A161 <- jaccard_soa(v.soa_comp16a,v.soa_comp1a)
  A162 <- jaccard_soa(v.soa_comp16a,v.soa_comp2a)
  A163 <- jaccard_soa(v.soa_comp16a,v.soa_comp3a)
  A164 <- jaccard_soa(v.soa_comp16a,v.soa_comp4a)
  A165 <- jaccard_soa(v.soa_comp16a,v.soa_comp5a)
  A166 <- jaccard_soa(v.soa_comp16a,v.soa_comp6a)
  A167 <- jaccard_soa(v.soa_comp16a,v.soa_comp7a)
  A168 <- jaccard_soa(v.soa_comp16a,v.soa_comp8a)
  A169 <- jaccard_soa(v.soa_comp16a,v.soa_comp9a)
  A1610 <- jaccard_soa(v.soa_comp16a,v.soa_comp10a)
  A1611 <- jaccard_soa(v.soa_comp16a,v.soa_comp11a)
  A1612 <- jaccard_soa(v.soa_comp16a,v.soa_comp12a)
  A1613 <- jaccard_soa(v.soa_comp16a,v.soa_comp13a)
  A1614 <- jaccard_soa(v.soa_comp16a,v.soa_comp14a)
  A1615 <- jaccard_soa(v.soa_comp16a,v.soa_comp15a)
  A1616 <- jaccard_soa(v.soa_comp16a,v.soa_comp16a)
  
  EN_A <- t(matrix(c(A11  ,A12  ,A13  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,
                     A21  ,A22  ,A23  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,A214 ,A215 ,A216 ,
                     A31  ,A32  ,A33  ,A34  ,A35  ,A36  ,A37  ,A38  ,A39  ,A310 ,A311 ,A312 ,A313 ,A314 ,A315 ,A316 ,
                     A41  ,A42  ,A43  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,A414 ,A415 ,A416 ,
                     A51  ,A52  ,A53  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,A514 ,A515 ,A516 ,
                     A61  ,A62  ,A63  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,A614 ,A615 ,A616 ,
                     A71  ,A72  ,A73  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,A714 ,A715 ,A716 ,
                     A81  ,A82  ,A83  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,A814 ,A815 ,A816 ,
                     A91  ,A92  ,A93  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,A914 ,A915 ,A916 ,
                     A101 ,A102 ,A103 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,A1014,A1015,A1016,
                     A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,A1114,A1115,A1116,
                     A121 ,A122 ,A123 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,A1214,A1215,A1216,
                     A131 ,A132 ,A133 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313,A1314,A1315,A1316,
                     A141 ,A142 ,A143 ,A144 ,A145 ,A146 ,A147 ,A148 ,A149 ,A1410,A1411,A1412,A1413,A1414,A1415,A1416,
                     A151 ,A152 ,A153 ,A154 ,A155 ,A156 ,A157 ,A158 ,A159 ,A1510,A1511,A1512,A1513,A1514,A1515,A1516,
                     A161 ,A162 ,A163 ,A164 ,A165 ,A166 ,A167 ,A168 ,A169 ,A1610,A1611,A1612,A1613,A1614,A1615,A1616), nrow=16, ncol=16))
  
  return(EN_A)
  
}
m.soa <- matrix_dist_soa(v.soa)

v.soa.j <- c(v.soa_compJ1a,v.soa_compJ2a,v.soa_compJ4a,v.soa_compJ5a,v.soa_compJ6a,v.soa_compJ7a,v.soa_compJ8a,v.soa_compJ9a,v.soa_compJ10a,v.soa_compJ11a,v.soa_compJ12a,v.soa_compJ13a)
matrix_dist_soa_j <- function (v.soa.j) {
  A11 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ1a)
  A12 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ2a)
  
  A14 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ4a)
  A15 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ5a)
  A16 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ6a)
  A17 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ7a)
  A18 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ8a)
  A19 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ9a)
  A110 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ10a)
  A111 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ11a)
  A112 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ12a)
  A113 <- jaccard_soa(v.soa_compJ1a,v.soa_compJ13a)
  
  #row2
  A21 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ1a)
  A22 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ2a)
  
  A24 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ4a)
  A25 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ5a)
  A26 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ6a)
  A27 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ7a)
  A28 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ8a)
  A29 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ9a)
  A210 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ10a)
  A211 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ11a)
  A212 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ12a)
  A213 <- jaccard_soa(v.soa_compJ2a,v.soa_compJ13a)
  
  
  #row4
  A41 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ1a)
  A42 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ2a)
  
  A44 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ4a)
  A45 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ5a)
  A46 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ6a)
  A47 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ7a)
  A48 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ8a)
  A49 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ9a)
  A410 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ10a)
  A411 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ11a)
  A412 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ12a)
  A413 <- jaccard_soa(v.soa_compJ4a,v.soa_compJ13a)
  
  #row5
  A51 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ1a)
  A52 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ2a)
  
  A54 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ4a)
  A55 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ5a)
  A56 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ6a)
  A57 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ7a)
  A58 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ8a)
  A59 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ9a)
  A510 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ10a)
  A511 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ11a)
  A512 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ12a)
  A513 <- jaccard_soa(v.soa_compJ5a,v.soa_compJ13a)
  
  
  #row6
  A61 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ1a)
  A62 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ2a)
  
  A64 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ4a)
  A65 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ5a)
  A66 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ6a)
  A67 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ7a)
  A68 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ8a)
  A69 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ9a)
  A610 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ10a)
  A611 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ11a)
  A612 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ12a)
  A613 <- jaccard_soa(v.soa_compJ6a,v.soa_compJ13a)
  
  
  #row7
  A71 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ1a)
  A72 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ2a)
  
  A74 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ4a)
  A75 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ5a)
  A76 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ6a)
  A77 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ7a)
  A78 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ8a)
  A79 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ9a)
  A710 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ10a)
  A711 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ11a)
  A712 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ12a)
  A713 <- jaccard_soa(v.soa_compJ7a,v.soa_compJ13a)
  
  
  #row8
  A81 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ1a)
  A82 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ2a)
  
  A84 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ4a)
  A85 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ5a)
  A86 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ6a)
  A87 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ7a)
  A88 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ8a)
  A89 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ9a)
  A810 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ10a)
  A811 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ11a)
  A812 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ12a)
  A813 <- jaccard_soa(v.soa_compJ8a,v.soa_compJ13a)
  
  
  #row9
  A91 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ1a)
  A92 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ2a)
  
  A94 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ4a)
  A95 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ5a)
  A96 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ6a)
  A97 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ7a)
  A98 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ8a)
  A99 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ9a)
  A910 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ10a)
  A911 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ11a)
  A912 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ12a)
  A913 <- jaccard_soa(v.soa_compJ9a,v.soa_compJ13a)
  
  
  #row10
  A101 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ1a)
  A102 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ2a)
  
  A104 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ4a)
  A105 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ5a)
  A106 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ6a)
  A107 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ7a)
  A108 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ8a)
  A109 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ9a)
  A1010 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ10a)
  A1011 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ11a)
  A1012 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ12a)
  A1013 <- jaccard_soa(v.soa_compJ10a,v.soa_compJ13a)
  
  #row11
  A111 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ1a)
  A112 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ2a)
  
  A114 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ4a)
  A115 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ5a)
  A116 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ6a)
  A117 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ7a)
  A118 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ8a)
  A119 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ9a)
  A1110 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ10a)
  A1111 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ11a)
  A1112 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ12a)
  A1113 <- jaccard_soa(v.soa_compJ11a,v.soa_compJ13a)
  
  
  #row12
  A121 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ1a)
  A122 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ2a)
  
  A124 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ4a)
  A125 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ5a)
  A126 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ6a)
  A127 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ7a)
  A128 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ8a)
  A129 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ9a)
  A1210 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ10a)
  A1211 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ11a)
  A1212 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ12a)
  A1213 <- jaccard_soa(v.soa_compJ12a,v.soa_compJ13a)
  
  
  #row13
  A131 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ1a)
  A132 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ2a)
  
  A134 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ4a)
  A135 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ5a)
  A136 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ6a)
  A137 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ7a)
  A138 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ8a)
  A139 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ9a)
  A1310 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ10a)
  A1311 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ11a)
  A1312 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ12a)
  A1313 <- jaccard_soa(v.soa_compJ13a,v.soa_compJ13a)
  
  
  
  
  EN_A <- t(matrix(c(A11  ,A12  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,
                     A21  ,A22  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,
                     A41  ,A42  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,
                     A51  ,A52  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,
                     A61  ,A62  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,
                     A71  ,A72  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,
                     A81  ,A82  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,
                     A91  ,A92  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,
                     A101 ,A102 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,
                     A111 ,A112 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,
                     A121 ,A122 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,
                     A131 ,A132 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313), nrow=12, ncol=12))
  
  return(EN_A)
  
}
m.soa.j <- matrix_dist_soa_j(v.soa.j)

##### Network of inventors#####

comp1a = read_excel("comp1a_iname.xlsx")
comp2a = read_excel("comp2a_iname.xlsx")
comp3a = read_excel("comp3a_iname.xlsx")
comp4a = read_excel("comp4a_iname.xlsx")
comp5a = read_excel("comp5a_iname.xlsx")
comp6a = read_excel("comp6a_iname.xlsx")
comp7a = read_excel("comp7a_iname.xlsx")
comp8a = read_excel("comp8a_iname.xlsx")
comp9a = read_excel("comp9a_iname.xlsx")
comp10a = read_excel("comp10a_iname.xlsx")
comp11a = read_excel("comp11a_iname.xlsx")
comp12a = read_excel("comp12a_iname.xlsx")
comp13a = read_excel("comp13a_iname.xlsx")
comp14a = read_excel("comp14a_iname.xlsx")
comp15a = read_excel("comp15a_iname.xlsx")
comp16a = read_excel("comp16a_iname.xlsx")

comp_inet_den <- function(comp1a, comp2a){
  t<- as.numeric(ncol(comp1a))
  exm <- data.frame(comp1a [c(1,3:t)])
  md1a <- melt.data.frame(exm,id.vars = "pn", na.rm = TRUE)
  md1b <- md1a
  net_md1 <- merge(md1a,md1b,by="pn") 
  
  t<- as.numeric(ncol(comp2a))
  exm <- data.frame(comp2a [c(1,3:t)])
  md2a <- melt.data.frame(exm,id.vars = "pn", na.rm = TRUE)
  md2b <- md2a
  net_md2 <- merge(md2a,md2b,by="pn") 
  
  net_md <- rbind(net_md1,net_md2) 
  net_mda <- net_md[c(3,5)]
  net_mdb <- as.data.frame(net_mda[(net_mda$value.x!=net_mda$value.y),]) 
  net_mdc <- distinct(net_mdb)
  net_1 <- network(net_mdc, loops = FALSE, directed = TRUE, matrix.type="edgelist")
  
  gden(net_1)
  centralization(net_1, betweenness)
  centralization(net_1, degree)
  
  return(gden(net_1, diag = FALSE))
  
}
comp_inet_bet <- function(comp1a, comp2a){
  t<- as.numeric(ncol(comp1a))
  exm <- data.frame(comp1a [c(1,3:t)])
  md1a <- melt.data.frame(exm,id.vars = "pn", na.rm = TRUE)
  md1b <- md1a
  net_md1 <- merge(md1a,md1b,by="pn") 
  
  t<- as.numeric(ncol(comp2a))
  exm <- data.frame(comp2a [c(1,3:t)])
  md2a <- melt.data.frame(exm,id.vars = "pn", na.rm = TRUE)
  md2b <- md2a
  net_md2 <- merge(md2a,md2b,by="pn") 
  
  net_md <- rbind(net_md1,net_md2) 
  net_mda <- net_md[c(3,5)]
  net_mdb <- as.data.frame(net_mda[(net_mda$value.x!=net_mda$value.y),]) 
  net_mdc <- distinct(net_mdb)
  net_1 <- network(net_mdc, loops = FALSE, directed = TRUE, matrix.type="edgelist")
  
  gden(net_1)
  centralization(net_1, betweenness)
  centralization(net_1, degree)
  
  return(centralization(net_1, betweenness))
  
}
comp_inet_deg <- function(comp1a, comp2a){
  t<- as.numeric(ncol(comp1a))
  exm <- data.frame(comp1a [c(1,3:t)])
  md1a <- melt.data.frame(exm,id.vars = "pn", na.rm = TRUE)
  md1b <- md1a
  net_md1 <- merge(md1a,md1b,by="pn") 
  
  t<- as.numeric(ncol(comp2a))
  exm <- data.frame(comp2a [c(1,3:t)])
  md2a <- melt.data.frame(exm,id.vars = "pn", na.rm = TRUE)
  md2b <- md2a
  net_md2 <- merge(md2a,md2b,by="pn") 
  
  net_md <- rbind(net_md1,net_md2) 
  net_mda <- net_md[c(3,5)]
  net_mdb <- as.data.frame(net_mda[(net_mda$value.x!=net_mda$value.y),]) 
  net_mdc <- distinct(net_mdb)
  net_1 <- network(net_mdc, loops = FALSE, directed = TRUE, matrix.type="edgelist")
  
  gden(net_1)
  centralization(net_1, betweenness)
  centralization(net_1, degree)
  
  return(centralization(net_1, degree))
  
}

comp_inet_den (comp1a, comp2a)
comp_inet_bet (comp1a, comp2a)
comp_inet_deg (comp1a, comp2a)

v.comp_inet <- c(comp1a,comp2a,comp3a,comp4a,comp5a,comp6a,comp7a,comp8a,comp9a,comp10a,comp11a,comp12a,comp13a,comp14a,comp15a,comp16a)
matrix_inet_den <- function (v.comp_inet) {
  A11 <- comp_inet_den(comp1a,comp1a)
  A12 <- comp_inet_den(comp1a,comp2a)
  A13 <- comp_inet_den(comp1a,comp3a)
  A14 <- comp_inet_den(comp1a,comp4a)
  A15 <- comp_inet_den(comp1a,comp5a)
  A16 <- comp_inet_den(comp1a,comp6a)
  A17 <- comp_inet_den(comp1a,comp7a)
  A18 <- comp_inet_den(comp1a,comp8a)
  A19 <- comp_inet_den(comp1a,comp9a)
  A110 <- comp_inet_den(comp1a,comp10a)
  A111 <- comp_inet_den(comp1a,comp11a)
  A112 <- comp_inet_den(comp1a,comp12a)
  A113 <- comp_inet_den(comp1a,comp13a)
  A114 <- comp_inet_den(comp1a,comp14a)
  A115 <- comp_inet_den(comp1a,comp15a)
  A116 <- comp_inet_den(comp1a,comp16a)
  
  #row2
  A21 <- comp_inet_den(comp2a,comp1a)
  A22 <- comp_inet_den(comp2a,comp2a)
  A23 <- comp_inet_den(comp2a,comp3a)
  A24 <- comp_inet_den(comp2a,comp4a)
  A25 <- comp_inet_den(comp2a,comp5a)
  A26 <- comp_inet_den(comp2a,comp6a)
  A27 <- comp_inet_den(comp2a,comp7a)
  A28 <- comp_inet_den(comp2a,comp8a)
  A29 <- comp_inet_den(comp2a,comp9a)
  A210 <- comp_inet_den(comp2a,comp10a)
  A211 <- comp_inet_den(comp2a,comp11a)
  A212 <- comp_inet_den(comp2a,comp12a)
  A213 <- comp_inet_den(comp2a,comp13a)
  A214 <- comp_inet_den(comp2a,comp14a)
  A215 <- comp_inet_den(comp2a,comp15a)
  A216 <- comp_inet_den(comp2a,comp16a)
  
  #row3
  A31 <- comp_inet_den(comp3a,comp1a)
  A32 <- comp_inet_den(comp3a,comp2a)
  A33 <- comp_inet_den(comp3a,comp3a)
  A34 <- comp_inet_den(comp3a,comp4a)
  A35 <- comp_inet_den(comp3a,comp5a)
  A36 <- comp_inet_den(comp3a,comp6a)
  A37 <- comp_inet_den(comp3a,comp7a)
  A38 <- comp_inet_den(comp3a,comp8a)
  A39 <- comp_inet_den(comp3a,comp9a)
  A310 <- comp_inet_den(comp3a,comp10a)
  A311 <- comp_inet_den(comp3a,comp11a)
  A312 <- comp_inet_den(comp3a,comp12a)
  A313 <- comp_inet_den(comp3a,comp13a)
  A314 <- comp_inet_den(comp3a,comp14a)
  A315 <- comp_inet_den(comp3a,comp15a)
  A316 <- comp_inet_den(comp3a,comp16a)
  
  #row4
  A41 <- comp_inet_den(comp4a,comp1a)
  A42 <- comp_inet_den(comp4a,comp2a)
  A43 <- comp_inet_den(comp4a,comp3a)
  A44 <- comp_inet_den(comp4a,comp4a)
  A45 <- comp_inet_den(comp4a,comp5a)
  A46 <- comp_inet_den(comp4a,comp6a)
  A47 <- comp_inet_den(comp4a,comp7a)
  A48 <- comp_inet_den(comp4a,comp8a)
  A49 <- comp_inet_den(comp4a,comp9a)
  A410 <- comp_inet_den(comp4a,comp10a)
  A411 <- comp_inet_den(comp4a,comp11a)
  A412 <- comp_inet_den(comp4a,comp12a)
  A413 <- comp_inet_den(comp4a,comp13a)
  A414 <- comp_inet_den(comp4a,comp14a)
  A415 <- comp_inet_den(comp4a,comp15a)
  A416 <- comp_inet_den(comp4a,comp16a)
  
  #row5
  A51 <- comp_inet_den(comp5a,comp1a)
  A52 <- comp_inet_den(comp5a,comp2a)
  A53 <- comp_inet_den(comp5a,comp3a)
  A54 <- comp_inet_den(comp5a,comp4a)
  A55 <- comp_inet_den(comp5a,comp5a)
  A56 <- comp_inet_den(comp5a,comp6a)
  A57 <- comp_inet_den(comp5a,comp7a)
  A58 <- comp_inet_den(comp5a,comp8a)
  A59 <- comp_inet_den(comp5a,comp9a)
  A510 <- comp_inet_den(comp5a,comp10a)
  A511 <- comp_inet_den(comp5a,comp11a)
  A512 <- comp_inet_den(comp5a,comp12a)
  A513 <- comp_inet_den(comp5a,comp13a)
  A514 <- comp_inet_den(comp5a,comp14a)
  A515 <- comp_inet_den(comp5a,comp15a)
  A516 <- comp_inet_den(comp5a,comp16a)
  
  #row6
  A61 <- comp_inet_den(comp6a,comp1a)
  A62 <- comp_inet_den(comp6a,comp2a)
  A63 <- comp_inet_den(comp6a,comp3a)
  A64 <- comp_inet_den(comp6a,comp4a)
  A65 <- comp_inet_den(comp6a,comp5a)
  A66 <- comp_inet_den(comp6a,comp6a)
  A67 <- comp_inet_den(comp6a,comp7a)
  A68 <- comp_inet_den(comp6a,comp8a)
  A69 <- comp_inet_den(comp6a,comp9a)
  A610 <- comp_inet_den(comp6a,comp10a)
  A611 <- comp_inet_den(comp6a,comp11a)
  A612 <- comp_inet_den(comp6a,comp12a)
  A613 <- comp_inet_den(comp6a,comp13a)
  A614 <- comp_inet_den(comp6a,comp14a)
  A615 <- comp_inet_den(comp6a,comp15a)
  A616 <- comp_inet_den(comp6a,comp16a)
  
  #row7
  A71 <- comp_inet_den(comp7a,comp1a)
  A72 <- comp_inet_den(comp7a,comp2a)
  A73 <- comp_inet_den(comp7a,comp3a)
  A74 <- comp_inet_den(comp7a,comp4a)
  A75 <- comp_inet_den(comp7a,comp5a)
  A76 <- comp_inet_den(comp7a,comp6a)
  A77 <- comp_inet_den(comp7a,comp7a)
  A78 <- comp_inet_den(comp7a,comp8a)
  A79 <- comp_inet_den(comp7a,comp9a)
  A710 <- comp_inet_den(comp7a,comp10a)
  A711 <- comp_inet_den(comp7a,comp11a)
  A712 <- comp_inet_den(comp7a,comp12a)
  A713 <- comp_inet_den(comp7a,comp13a)
  A714 <- comp_inet_den(comp7a,comp14a)
  A715 <- comp_inet_den(comp7a,comp15a)
  A716 <- comp_inet_den(comp7a,comp16a)
  
  #row8
  A81 <- comp_inet_den(comp8a,comp1a)
  A82 <- comp_inet_den(comp8a,comp2a)
  A83 <- comp_inet_den(comp8a,comp3a)
  A84 <- comp_inet_den(comp8a,comp4a)
  A85 <- comp_inet_den(comp8a,comp5a)
  A86 <- comp_inet_den(comp8a,comp6a)
  A87 <- comp_inet_den(comp8a,comp7a)
  A88 <- comp_inet_den(comp8a,comp8a)
  A89 <- comp_inet_den(comp8a,comp9a)
  A810 <- comp_inet_den(comp8a,comp10a)
  A811 <- comp_inet_den(comp8a,comp11a)
  A812 <- comp_inet_den(comp8a,comp12a)
  A813 <- comp_inet_den(comp8a,comp13a)
  A814 <- comp_inet_den(comp8a,comp14a)
  A815 <- comp_inet_den(comp8a,comp15a)
  A816 <- comp_inet_den(comp8a,comp16a)
  
  #row9
  A91 <- comp_inet_den(comp9a,comp1a)
  A92 <- comp_inet_den(comp9a,comp2a)
  A93 <- comp_inet_den(comp9a,comp3a)
  A94 <- comp_inet_den(comp9a,comp4a)
  A95 <- comp_inet_den(comp9a,comp5a)
  A96 <- comp_inet_den(comp9a,comp6a)
  A97 <- comp_inet_den(comp9a,comp7a)
  A98 <- comp_inet_den(comp9a,comp8a)
  A99 <- comp_inet_den(comp9a,comp9a)
  A910 <- comp_inet_den(comp9a,comp10a)
  A911 <- comp_inet_den(comp9a,comp11a)
  A912 <- comp_inet_den(comp9a,comp12a)
  A913 <- comp_inet_den(comp9a,comp13a)
  A914 <- comp_inet_den(comp9a,comp14a)
  A915 <- comp_inet_den(comp9a,comp15a)
  A916 <- comp_inet_den(comp9a,comp16a)
  
  #row10
  A101 <- comp_inet_den(comp10a,comp1a)
  A102 <- comp_inet_den(comp10a,comp2a)
  A103 <- comp_inet_den(comp10a,comp3a)
  A104 <- comp_inet_den(comp10a,comp4a)
  A105 <- comp_inet_den(comp10a,comp5a)
  A106 <- comp_inet_den(comp10a,comp6a)
  A107 <- comp_inet_den(comp10a,comp7a)
  A108 <- comp_inet_den(comp10a,comp8a)
  A109 <- comp_inet_den(comp10a,comp9a)
  A1010 <- comp_inet_den(comp10a,comp10a)
  A1011 <- comp_inet_den(comp10a,comp11a)
  A1012 <- comp_inet_den(comp10a,comp12a)
  A1013 <- comp_inet_den(comp10a,comp13a)
  A1014 <- comp_inet_den(comp10a,comp14a)
  A1015 <- comp_inet_den(comp10a,comp15a)
  A1016 <- comp_inet_den(comp10a,comp16a)
  
  #row11
  A111 <- comp_inet_den(comp11a,comp1a)
  A112 <- comp_inet_den(comp11a,comp2a)
  A113 <- comp_inet_den(comp11a,comp3a)
  A114 <- comp_inet_den(comp11a,comp4a)
  A115 <- comp_inet_den(comp11a,comp5a)
  A116 <- comp_inet_den(comp11a,comp6a)
  A117 <- comp_inet_den(comp11a,comp7a)
  A118 <- comp_inet_den(comp11a,comp8a)
  A119 <- comp_inet_den(comp11a,comp9a)
  A1110 <- comp_inet_den(comp11a,comp10a)
  A1111 <- comp_inet_den(comp11a,comp11a)
  A1112 <- comp_inet_den(comp11a,comp12a)
  A1113 <- comp_inet_den(comp11a,comp13a)
  A1114 <- comp_inet_den(comp11a,comp14a)
  A1115 <- comp_inet_den(comp11a,comp15a)
  A1116 <- comp_inet_den(comp11a,comp16a)
  
  #row12
  A121 <- comp_inet_den(comp12a,comp1a)
  A122 <- comp_inet_den(comp12a,comp2a)
  A123 <- comp_inet_den(comp12a,comp3a)
  A124 <- comp_inet_den(comp12a,comp4a)
  A125 <- comp_inet_den(comp12a,comp5a)
  A126 <- comp_inet_den(comp12a,comp6a)
  A127 <- comp_inet_den(comp12a,comp7a)
  A128 <- comp_inet_den(comp12a,comp8a)
  A129 <- comp_inet_den(comp12a,comp9a)
  A1210 <- comp_inet_den(comp12a,comp10a)
  A1211 <- comp_inet_den(comp12a,comp11a)
  A1212 <- comp_inet_den(comp12a,comp12a)
  A1213 <- comp_inet_den(comp12a,comp13a)
  A1214 <- comp_inet_den(comp12a,comp14a)
  A1215 <- comp_inet_den(comp12a,comp15a)
  A1216 <- comp_inet_den(comp12a,comp16a)
  
  #row13
  A131 <- comp_inet_den(comp13a,comp1a)
  A132 <- comp_inet_den(comp13a,comp2a)
  A133 <- comp_inet_den(comp13a,comp3a)
  A134 <- comp_inet_den(comp13a,comp4a)
  A135 <- comp_inet_den(comp13a,comp5a)
  A136 <- comp_inet_den(comp13a,comp6a)
  A137 <- comp_inet_den(comp13a,comp7a)
  A138 <- comp_inet_den(comp13a,comp8a)
  A139 <- comp_inet_den(comp13a,comp9a)
  A1310 <- comp_inet_den(comp13a,comp10a)
  A1311 <- comp_inet_den(comp13a,comp11a)
  A1312 <- comp_inet_den(comp13a,comp12a)
  A1313 <- comp_inet_den(comp13a,comp13a)
  A1314 <- comp_inet_den(comp13a,comp14a)
  A1315 <- comp_inet_den(comp13a,comp15a)
  A1316 <- comp_inet_den(comp13a,comp16a)
  
  #row14
  A141 <- comp_inet_den(comp14a,comp1a)
  A142 <- comp_inet_den(comp14a,comp2a)
  A143 <- comp_inet_den(comp14a,comp3a)
  A144 <- comp_inet_den(comp14a,comp4a)
  A145 <- comp_inet_den(comp14a,comp5a)
  A146 <- comp_inet_den(comp14a,comp6a)
  A147 <- comp_inet_den(comp14a,comp7a)
  A148 <- comp_inet_den(comp14a,comp8a)
  A149 <- comp_inet_den(comp14a,comp9a)
  A1410 <- comp_inet_den(comp14a,comp10a)
  A1411 <- comp_inet_den(comp14a,comp11a)
  A1412 <- comp_inet_den(comp14a,comp12a)
  A1413 <- comp_inet_den(comp14a,comp13a)
  A1414 <- comp_inet_den(comp14a,comp14a)
  A1415 <- comp_inet_den(comp14a,comp15a)
  A1416 <- comp_inet_den(comp14a,comp16a)
  
  #row15
  A151 <- comp_inet_den(comp15a,comp1a)
  A152 <- comp_inet_den(comp15a,comp2a)
  A153 <- comp_inet_den(comp15a,comp3a)
  A154 <- comp_inet_den(comp15a,comp4a)
  A155 <- comp_inet_den(comp15a,comp5a)
  A156 <- comp_inet_den(comp15a,comp6a)
  A157 <- comp_inet_den(comp15a,comp7a)
  A158 <- comp_inet_den(comp15a,comp8a)
  A159 <- comp_inet_den(comp15a,comp9a)
  A1510 <- comp_inet_den(comp15a,comp10a)
  A1511 <- comp_inet_den(comp15a,comp11a)
  A1512 <- comp_inet_den(comp15a,comp12a)
  A1513 <- comp_inet_den(comp15a,comp13a)
  A1514 <- comp_inet_den(comp15a,comp14a)
  A1515 <- comp_inet_den(comp15a,comp15a)
  A1516 <- comp_inet_den(comp15a,comp16a)
  
  #row16
  A161 <- comp_inet_den(comp16a,comp1a)
  A162 <- comp_inet_den(comp16a,comp2a)
  A163 <- comp_inet_den(comp16a,comp3a)
  A164 <- comp_inet_den(comp16a,comp4a)
  A165 <- comp_inet_den(comp16a,comp5a)
  A166 <- comp_inet_den(comp16a,comp6a)
  A167 <- comp_inet_den(comp16a,comp7a)
  A168 <- comp_inet_den(comp16a,comp8a)
  A169 <- comp_inet_den(comp16a,comp9a)
  A1610 <- comp_inet_den(comp16a,comp10a)
  A1611 <- comp_inet_den(comp16a,comp11a)
  A1612 <- comp_inet_den(comp16a,comp12a)
  A1613 <- comp_inet_den(comp16a,comp13a)
  A1614 <- comp_inet_den(comp16a,comp14a)
  A1615 <- comp_inet_den(comp16a,comp15a)
  A1616 <- comp_inet_den(comp16a,comp16a)
  
  EN_A <- t(matrix(c(A11  ,A12  ,A13  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,
                     A21  ,A22  ,A23  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,A214 ,A215 ,A216 ,
                     A31  ,A32  ,A33  ,A34  ,A35  ,A36  ,A37  ,A38  ,A39  ,A310 ,A311 ,A312 ,A313 ,A314 ,A315 ,A316 ,
                     A41  ,A42  ,A43  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,A414 ,A415 ,A416 ,
                     A51  ,A52  ,A53  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,A514 ,A515 ,A516 ,
                     A61  ,A62  ,A63  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,A614 ,A615 ,A616 ,
                     A71  ,A72  ,A73  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,A714 ,A715 ,A716 ,
                     A81  ,A82  ,A83  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,A814 ,A815 ,A816 ,
                     A91  ,A92  ,A93  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,A914 ,A915 ,A916 ,
                     A101 ,A102 ,A103 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,A1014,A1015,A1016,
                     A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,A1114,A1115,A1116,
                     A121 ,A122 ,A123 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,A1214,A1215,A1216,
                     A131 ,A132 ,A133 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313,A1314,A1315,A1316,
                     A141 ,A142 ,A143 ,A144 ,A145 ,A146 ,A147 ,A148 ,A149 ,A1410,A1411,A1412,A1413,A1414,A1415,A1416,
                     A151 ,A152 ,A153 ,A154 ,A155 ,A156 ,A157 ,A158 ,A159 ,A1510,A1511,A1512,A1513,A1514,A1515,A1516,
                     A161 ,A162 ,A163 ,A164 ,A165 ,A166 ,A167 ,A168 ,A169 ,A1610,A1611,A1612,A1613,A1614,A1615,A1616), nrow=16, ncol=16))
  
  return(EN_A)
  
}
m.inet.den <- matrix_inet_den(v.comp_inet)

matrix_inet_bet <- function (v.comp_inet) {
  A11 <- comp_inet_bet(comp1a,comp1a)
  A12 <- comp_inet_bet(comp1a,comp2a)
  A13 <- comp_inet_bet(comp1a,comp3a)
  A14 <- comp_inet_bet(comp1a,comp4a)
  A15 <- comp_inet_bet(comp1a,comp5a)
  A16 <- comp_inet_bet(comp1a,comp6a)
  A17 <- comp_inet_bet(comp1a,comp7a)
  A18 <- comp_inet_bet(comp1a,comp8a)
  A19 <- comp_inet_bet(comp1a,comp9a)
  A110 <- comp_inet_bet(comp1a,comp10a)
  A111 <- comp_inet_bet(comp1a,comp11a)
  A112 <- comp_inet_bet(comp1a,comp12a)
  A113 <- comp_inet_bet(comp1a,comp13a)
  A114 <- comp_inet_bet(comp1a,comp14a)
  A115 <- comp_inet_bet(comp1a,comp15a)
  A116 <- comp_inet_bet(comp1a,comp16a)
  
  #row2
  A21 <- comp_inet_bet(comp2a,comp1a)
  A22 <- comp_inet_bet(comp2a,comp2a)
  A23 <- comp_inet_bet(comp2a,comp3a)
  A24 <- comp_inet_bet(comp2a,comp4a)
  A25 <- comp_inet_bet(comp2a,comp5a)
  A26 <- comp_inet_bet(comp2a,comp6a)
  A27 <- comp_inet_bet(comp2a,comp7a)
  A28 <- comp_inet_bet(comp2a,comp8a)
  A29 <- comp_inet_bet(comp2a,comp9a)
  A210 <- comp_inet_bet(comp2a,comp10a)
  A211 <- comp_inet_bet(comp2a,comp11a)
  A212 <- comp_inet_bet(comp2a,comp12a)
  A213 <- comp_inet_bet(comp2a,comp13a)
  A214 <- comp_inet_bet(comp2a,comp14a)
  A215 <- comp_inet_bet(comp2a,comp15a)
  A216 <- comp_inet_bet(comp2a,comp16a)
  
  #row3
  A31 <- comp_inet_bet(comp3a,comp1a)
  A32 <- comp_inet_bet(comp3a,comp2a)
  A33 <- comp_inet_bet(comp3a,comp3a)
  A34 <- comp_inet_bet(comp3a,comp4a)
  A35 <- comp_inet_bet(comp3a,comp5a)
  A36 <- comp_inet_bet(comp3a,comp6a)
  A37 <- comp_inet_bet(comp3a,comp7a)
  A38 <- comp_inet_bet(comp3a,comp8a)
  A39 <- comp_inet_bet(comp3a,comp9a)
  A310 <- comp_inet_bet(comp3a,comp10a)
  A311 <- comp_inet_bet(comp3a,comp11a)
  A312 <- comp_inet_bet(comp3a,comp12a)
  A313 <- comp_inet_bet(comp3a,comp13a)
  A314 <- comp_inet_bet(comp3a,comp14a)
  A315 <- comp_inet_bet(comp3a,comp15a)
  A316 <- comp_inet_bet(comp3a,comp16a)
  
  #row4
  A41 <- comp_inet_bet(comp4a,comp1a)
  A42 <- comp_inet_bet(comp4a,comp2a)
  A43 <- comp_inet_bet(comp4a,comp3a)
  A44 <- comp_inet_bet(comp4a,comp4a)
  A45 <- comp_inet_bet(comp4a,comp5a)
  A46 <- comp_inet_bet(comp4a,comp6a)
  A47 <- comp_inet_bet(comp4a,comp7a)
  A48 <- comp_inet_bet(comp4a,comp8a)
  A49 <- comp_inet_bet(comp4a,comp9a)
  A410 <- comp_inet_bet(comp4a,comp10a)
  A411 <- comp_inet_bet(comp4a,comp11a)
  A412 <- comp_inet_bet(comp4a,comp12a)
  A413 <- comp_inet_bet(comp4a,comp13a)
  A414 <- comp_inet_bet(comp4a,comp14a)
  A415 <- comp_inet_bet(comp4a,comp15a)
  A416 <- comp_inet_bet(comp4a,comp16a)
  
  #row5
  A51 <- comp_inet_bet(comp5a,comp1a)
  A52 <- comp_inet_bet(comp5a,comp2a)
  A53 <- comp_inet_bet(comp5a,comp3a)
  A54 <- comp_inet_bet(comp5a,comp4a)
  A55 <- comp_inet_bet(comp5a,comp5a)
  A56 <- comp_inet_bet(comp5a,comp6a)
  A57 <- comp_inet_bet(comp5a,comp7a)
  A58 <- comp_inet_bet(comp5a,comp8a)
  A59 <- comp_inet_bet(comp5a,comp9a)
  A510 <- comp_inet_bet(comp5a,comp10a)
  A511 <- comp_inet_bet(comp5a,comp11a)
  A512 <- comp_inet_bet(comp5a,comp12a)
  A513 <- comp_inet_bet(comp5a,comp13a)
  A514 <- comp_inet_bet(comp5a,comp14a)
  A515 <- comp_inet_bet(comp5a,comp15a)
  A516 <- comp_inet_bet(comp5a,comp16a)
  
  #row6
  A61 <- comp_inet_bet(comp6a,comp1a)
  A62 <- comp_inet_bet(comp6a,comp2a)
  A63 <- comp_inet_bet(comp6a,comp3a)
  A64 <- comp_inet_bet(comp6a,comp4a)
  A65 <- comp_inet_bet(comp6a,comp5a)
  A66 <- comp_inet_bet(comp6a,comp6a)
  A67 <- comp_inet_bet(comp6a,comp7a)
  A68 <- comp_inet_bet(comp6a,comp8a)
  A69 <- comp_inet_bet(comp6a,comp9a)
  A610 <- comp_inet_bet(comp6a,comp10a)
  A611 <- comp_inet_bet(comp6a,comp11a)
  A612 <- comp_inet_bet(comp6a,comp12a)
  A613 <- comp_inet_bet(comp6a,comp13a)
  A614 <- comp_inet_bet(comp6a,comp14a)
  A615 <- comp_inet_bet(comp6a,comp15a)
  A616 <- comp_inet_bet(comp6a,comp16a)
  
  #row7
  A71 <- comp_inet_bet(comp7a,comp1a)
  A72 <- comp_inet_bet(comp7a,comp2a)
  A73 <- comp_inet_bet(comp7a,comp3a)
  A74 <- comp_inet_bet(comp7a,comp4a)
  A75 <- comp_inet_bet(comp7a,comp5a)
  A76 <- comp_inet_bet(comp7a,comp6a)
  A77 <- comp_inet_bet(comp7a,comp7a)
  A78 <- comp_inet_bet(comp7a,comp8a)
  A79 <- comp_inet_bet(comp7a,comp9a)
  A710 <- comp_inet_bet(comp7a,comp10a)
  A711 <- comp_inet_bet(comp7a,comp11a)
  A712 <- comp_inet_bet(comp7a,comp12a)
  A713 <- comp_inet_bet(comp7a,comp13a)
  A714 <- comp_inet_bet(comp7a,comp14a)
  A715 <- comp_inet_bet(comp7a,comp15a)
  A716 <- comp_inet_bet(comp7a,comp16a)
  
  #row8
  A81 <- comp_inet_bet(comp8a,comp1a)
  A82 <- comp_inet_bet(comp8a,comp2a)
  A83 <- comp_inet_bet(comp8a,comp3a)
  A84 <- comp_inet_bet(comp8a,comp4a)
  A85 <- comp_inet_bet(comp8a,comp5a)
  A86 <- comp_inet_bet(comp8a,comp6a)
  A87 <- comp_inet_bet(comp8a,comp7a)
  A88 <- comp_inet_bet(comp8a,comp8a)
  A89 <- comp_inet_bet(comp8a,comp9a)
  A810 <- comp_inet_bet(comp8a,comp10a)
  A811 <- comp_inet_bet(comp8a,comp11a)
  A812 <- comp_inet_bet(comp8a,comp12a)
  A813 <- comp_inet_bet(comp8a,comp13a)
  A814 <- comp_inet_bet(comp8a,comp14a)
  A815 <- comp_inet_bet(comp8a,comp15a)
  A816 <- comp_inet_bet(comp8a,comp16a)
  
  #row9
  A91 <- comp_inet_bet(comp9a,comp1a)
  A92 <- comp_inet_bet(comp9a,comp2a)
  A93 <- comp_inet_bet(comp9a,comp3a)
  A94 <- comp_inet_bet(comp9a,comp4a)
  A95 <- comp_inet_bet(comp9a,comp5a)
  A96 <- comp_inet_bet(comp9a,comp6a)
  A97 <- comp_inet_bet(comp9a,comp7a)
  A98 <- comp_inet_bet(comp9a,comp8a)
  A99 <- comp_inet_bet(comp9a,comp9a)
  A910 <- comp_inet_bet(comp9a,comp10a)
  A911 <- comp_inet_bet(comp9a,comp11a)
  A912 <- comp_inet_bet(comp9a,comp12a)
  A913 <- comp_inet_bet(comp9a,comp13a)
  A914 <- comp_inet_bet(comp9a,comp14a)
  A915 <- comp_inet_bet(comp9a,comp15a)
  A916 <- comp_inet_bet(comp9a,comp16a)
  
  #row10
  A101 <- comp_inet_bet(comp10a,comp1a)
  A102 <- comp_inet_bet(comp10a,comp2a)
  A103 <- comp_inet_bet(comp10a,comp3a)
  A104 <- comp_inet_bet(comp10a,comp4a)
  A105 <- comp_inet_bet(comp10a,comp5a)
  A106 <- comp_inet_bet(comp10a,comp6a)
  A107 <- comp_inet_bet(comp10a,comp7a)
  A108 <- comp_inet_bet(comp10a,comp8a)
  A109 <- comp_inet_bet(comp10a,comp9a)
  A1010 <- comp_inet_bet(comp10a,comp10a)
  A1011 <- comp_inet_bet(comp10a,comp11a)
  A1012 <- comp_inet_bet(comp10a,comp12a)
  A1013 <- comp_inet_bet(comp10a,comp13a)
  A1014 <- comp_inet_bet(comp10a,comp14a)
  A1015 <- comp_inet_bet(comp10a,comp15a)
  A1016 <- comp_inet_bet(comp10a,comp16a)
  
  #row11
  A111 <- comp_inet_bet(comp11a,comp1a)
  A112 <- comp_inet_bet(comp11a,comp2a)
  A113 <- comp_inet_bet(comp11a,comp3a)
  A114 <- comp_inet_bet(comp11a,comp4a)
  A115 <- comp_inet_bet(comp11a,comp5a)
  A116 <- comp_inet_bet(comp11a,comp6a)
  A117 <- comp_inet_bet(comp11a,comp7a)
  A118 <- comp_inet_bet(comp11a,comp8a)
  A119 <- comp_inet_bet(comp11a,comp9a)
  A1110 <- comp_inet_bet(comp11a,comp10a)
  A1111 <- comp_inet_bet(comp11a,comp11a)
  A1112 <- comp_inet_bet(comp11a,comp12a)
  A1113 <- comp_inet_bet(comp11a,comp13a)
  A1114 <- comp_inet_bet(comp11a,comp14a)
  A1115 <- comp_inet_bet(comp11a,comp15a)
  A1116 <- comp_inet_bet(comp11a,comp16a)
  
  #row12
  A121 <- comp_inet_bet(comp12a,comp1a)
  A122 <- comp_inet_bet(comp12a,comp2a)
  A123 <- comp_inet_bet(comp12a,comp3a)
  A124 <- comp_inet_bet(comp12a,comp4a)
  A125 <- comp_inet_bet(comp12a,comp5a)
  A126 <- comp_inet_bet(comp12a,comp6a)
  A127 <- comp_inet_bet(comp12a,comp7a)
  A128 <- comp_inet_bet(comp12a,comp8a)
  A129 <- comp_inet_bet(comp12a,comp9a)
  A1210 <- comp_inet_bet(comp12a,comp10a)
  A1211 <- comp_inet_bet(comp12a,comp11a)
  A1212 <- comp_inet_bet(comp12a,comp12a)
  A1213 <- comp_inet_bet(comp12a,comp13a)
  A1214 <- comp_inet_bet(comp12a,comp14a)
  A1215 <- comp_inet_bet(comp12a,comp15a)
  A1216 <- comp_inet_bet(comp12a,comp16a)
  
  #row13
  A131 <- comp_inet_bet(comp13a,comp1a)
  A132 <- comp_inet_bet(comp13a,comp2a)
  A133 <- comp_inet_bet(comp13a,comp3a)
  A134 <- comp_inet_bet(comp13a,comp4a)
  A135 <- comp_inet_bet(comp13a,comp5a)
  A136 <- comp_inet_bet(comp13a,comp6a)
  A137 <- comp_inet_bet(comp13a,comp7a)
  A138 <- comp_inet_bet(comp13a,comp8a)
  A139 <- comp_inet_bet(comp13a,comp9a)
  A1310 <- comp_inet_bet(comp13a,comp10a)
  A1311 <- comp_inet_bet(comp13a,comp11a)
  A1312 <- comp_inet_bet(comp13a,comp12a)
  A1313 <- comp_inet_bet(comp13a,comp13a)
  A1314 <- comp_inet_bet(comp13a,comp14a)
  A1315 <- comp_inet_bet(comp13a,comp15a)
  A1316 <- comp_inet_bet(comp13a,comp16a)
  
  #row14
  A141 <- comp_inet_bet(comp14a,comp1a)
  A142 <- comp_inet_bet(comp14a,comp2a)
  A143 <- comp_inet_bet(comp14a,comp3a)
  A144 <- comp_inet_bet(comp14a,comp4a)
  A145 <- comp_inet_bet(comp14a,comp5a)
  A146 <- comp_inet_bet(comp14a,comp6a)
  A147 <- comp_inet_bet(comp14a,comp7a)
  A148 <- comp_inet_bet(comp14a,comp8a)
  A149 <- comp_inet_bet(comp14a,comp9a)
  A1410 <- comp_inet_bet(comp14a,comp10a)
  A1411 <- comp_inet_bet(comp14a,comp11a)
  A1412 <- comp_inet_bet(comp14a,comp12a)
  A1413 <- comp_inet_bet(comp14a,comp13a)
  A1414 <- comp_inet_bet(comp14a,comp14a)
  A1415 <- comp_inet_bet(comp14a,comp15a)
  A1416 <- comp_inet_bet(comp14a,comp16a)
  
  #row15
  A151 <- comp_inet_bet(comp15a,comp1a)
  A152 <- comp_inet_bet(comp15a,comp2a)
  A153 <- comp_inet_bet(comp15a,comp3a)
  A154 <- comp_inet_bet(comp15a,comp4a)
  A155 <- comp_inet_bet(comp15a,comp5a)
  A156 <- comp_inet_bet(comp15a,comp6a)
  A157 <- comp_inet_bet(comp15a,comp7a)
  A158 <- comp_inet_bet(comp15a,comp8a)
  A159 <- comp_inet_bet(comp15a,comp9a)
  A1510 <- comp_inet_bet(comp15a,comp10a)
  A1511 <- comp_inet_bet(comp15a,comp11a)
  A1512 <- comp_inet_bet(comp15a,comp12a)
  A1513 <- comp_inet_bet(comp15a,comp13a)
  A1514 <- comp_inet_bet(comp15a,comp14a)
  A1515 <- comp_inet_bet(comp15a,comp15a)
  A1516 <- comp_inet_bet(comp15a,comp16a)
  
  #row16
  A161 <- comp_inet_bet(comp16a,comp1a)
  A162 <- comp_inet_bet(comp16a,comp2a)
  A163 <- comp_inet_bet(comp16a,comp3a)
  A164 <- comp_inet_bet(comp16a,comp4a)
  A165 <- comp_inet_bet(comp16a,comp5a)
  A166 <- comp_inet_bet(comp16a,comp6a)
  A167 <- comp_inet_bet(comp16a,comp7a)
  A168 <- comp_inet_bet(comp16a,comp8a)
  A169 <- comp_inet_bet(comp16a,comp9a)
  A1610 <- comp_inet_bet(comp16a,comp10a)
  A1611 <- comp_inet_bet(comp16a,comp11a)
  A1612 <- comp_inet_bet(comp16a,comp12a)
  A1613 <- comp_inet_bet(comp16a,comp13a)
  A1614 <- comp_inet_bet(comp16a,comp14a)
  A1615 <- comp_inet_bet(comp16a,comp15a)
  A1616 <- comp_inet_bet(comp16a,comp16a)
  
  EN_A <- t(matrix(c(A11  ,A12  ,A13  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,
                     A21  ,A22  ,A23  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,A214 ,A215 ,A216 ,
                     A31  ,A32  ,A33  ,A34  ,A35  ,A36  ,A37  ,A38  ,A39  ,A310 ,A311 ,A312 ,A313 ,A314 ,A315 ,A316 ,
                     A41  ,A42  ,A43  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,A414 ,A415 ,A416 ,
                     A51  ,A52  ,A53  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,A514 ,A515 ,A516 ,
                     A61  ,A62  ,A63  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,A614 ,A615 ,A616 ,
                     A71  ,A72  ,A73  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,A714 ,A715 ,A716 ,
                     A81  ,A82  ,A83  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,A814 ,A815 ,A816 ,
                     A91  ,A92  ,A93  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,A914 ,A915 ,A916 ,
                     A101 ,A102 ,A103 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,A1014,A1015,A1016,
                     A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,A1114,A1115,A1116,
                     A121 ,A122 ,A123 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,A1214,A1215,A1216,
                     A131 ,A132 ,A133 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313,A1314,A1315,A1316,
                     A141 ,A142 ,A143 ,A144 ,A145 ,A146 ,A147 ,A148 ,A149 ,A1410,A1411,A1412,A1413,A1414,A1415,A1416,
                     A151 ,A152 ,A153 ,A154 ,A155 ,A156 ,A157 ,A158 ,A159 ,A1510,A1511,A1512,A1513,A1514,A1515,A1516,
                     A161 ,A162 ,A163 ,A164 ,A165 ,A166 ,A167 ,A168 ,A169 ,A1610,A1611,A1612,A1613,A1614,A1615,A1616), nrow=16, ncol=16))
  
  return(EN_A)
  
}
m.inet.bet <- matrix_inet_bet(v.comp_inet)

matrix_inet_deg <- function (v.comp_inet) {
  A11 <- comp_inet_deg(comp1a,comp1a)
  A12 <- comp_inet_deg(comp1a,comp2a)
  A13 <- comp_inet_deg(comp1a,comp3a)
  A14 <- comp_inet_deg(comp1a,comp4a)
  A15 <- comp_inet_deg(comp1a,comp5a)
  A16 <- comp_inet_deg(comp1a,comp6a)
  A17 <- comp_inet_deg(comp1a,comp7a)
  A18 <- comp_inet_deg(comp1a,comp8a)
  A19 <- comp_inet_deg(comp1a,comp9a)
  A110 <- comp_inet_deg(comp1a,comp10a)
  A111 <- comp_inet_deg(comp1a,comp11a)
  A112 <- comp_inet_deg(comp1a,comp12a)
  A113 <- comp_inet_deg(comp1a,comp13a)
  A114 <- comp_inet_deg(comp1a,comp14a)
  A115 <- comp_inet_deg(comp1a,comp15a)
  A116 <- comp_inet_deg(comp1a,comp16a)
  
  #row2
  A21 <- comp_inet_deg(comp2a,comp1a)
  A22 <- comp_inet_deg(comp2a,comp2a)
  A23 <- comp_inet_deg(comp2a,comp3a)
  A24 <- comp_inet_deg(comp2a,comp4a)
  A25 <- comp_inet_deg(comp2a,comp5a)
  A26 <- comp_inet_deg(comp2a,comp6a)
  A27 <- comp_inet_deg(comp2a,comp7a)
  A28 <- comp_inet_deg(comp2a,comp8a)
  A29 <- comp_inet_deg(comp2a,comp9a)
  A210 <- comp_inet_deg(comp2a,comp10a)
  A211 <- comp_inet_deg(comp2a,comp11a)
  A212 <- comp_inet_deg(comp2a,comp12a)
  A213 <- comp_inet_deg(comp2a,comp13a)
  A214 <- comp_inet_deg(comp2a,comp14a)
  A215 <- comp_inet_deg(comp2a,comp15a)
  A216 <- comp_inet_deg(comp2a,comp16a)
  
  #row3
  A31 <- comp_inet_deg(comp3a,comp1a)
  A32 <- comp_inet_deg(comp3a,comp2a)
  A33 <- comp_inet_deg(comp3a,comp3a)
  A34 <- comp_inet_deg(comp3a,comp4a)
  A35 <- comp_inet_deg(comp3a,comp5a)
  A36 <- comp_inet_deg(comp3a,comp6a)
  A37 <- comp_inet_deg(comp3a,comp7a)
  A38 <- comp_inet_deg(comp3a,comp8a)
  A39 <- comp_inet_deg(comp3a,comp9a)
  A310 <- comp_inet_deg(comp3a,comp10a)
  A311 <- comp_inet_deg(comp3a,comp11a)
  A312 <- comp_inet_deg(comp3a,comp12a)
  A313 <- comp_inet_deg(comp3a,comp13a)
  A314 <- comp_inet_deg(comp3a,comp14a)
  A315 <- comp_inet_deg(comp3a,comp15a)
  A316 <- comp_inet_deg(comp3a,comp16a)
  
  #row4
  A41 <- comp_inet_deg(comp4a,comp1a)
  A42 <- comp_inet_deg(comp4a,comp2a)
  A43 <- comp_inet_deg(comp4a,comp3a)
  A44 <- comp_inet_deg(comp4a,comp4a)
  A45 <- comp_inet_deg(comp4a,comp5a)
  A46 <- comp_inet_deg(comp4a,comp6a)
  A47 <- comp_inet_deg(comp4a,comp7a)
  A48 <- comp_inet_deg(comp4a,comp8a)
  A49 <- comp_inet_deg(comp4a,comp9a)
  A410 <- comp_inet_deg(comp4a,comp10a)
  A411 <- comp_inet_deg(comp4a,comp11a)
  A412 <- comp_inet_deg(comp4a,comp12a)
  A413 <- comp_inet_deg(comp4a,comp13a)
  A414 <- comp_inet_deg(comp4a,comp14a)
  A415 <- comp_inet_deg(comp4a,comp15a)
  A416 <- comp_inet_deg(comp4a,comp16a)
  
  #row5
  A51 <- comp_inet_deg(comp5a,comp1a)
  A52 <- comp_inet_deg(comp5a,comp2a)
  A53 <- comp_inet_deg(comp5a,comp3a)
  A54 <- comp_inet_deg(comp5a,comp4a)
  A55 <- comp_inet_deg(comp5a,comp5a)
  A56 <- comp_inet_deg(comp5a,comp6a)
  A57 <- comp_inet_deg(comp5a,comp7a)
  A58 <- comp_inet_deg(comp5a,comp8a)
  A59 <- comp_inet_deg(comp5a,comp9a)
  A510 <- comp_inet_deg(comp5a,comp10a)
  A511 <- comp_inet_deg(comp5a,comp11a)
  A512 <- comp_inet_deg(comp5a,comp12a)
  A513 <- comp_inet_deg(comp5a,comp13a)
  A514 <- comp_inet_deg(comp5a,comp14a)
  A515 <- comp_inet_deg(comp5a,comp15a)
  A516 <- comp_inet_deg(comp5a,comp16a)
  
  #row6
  A61 <- comp_inet_deg(comp6a,comp1a)
  A62 <- comp_inet_deg(comp6a,comp2a)
  A63 <- comp_inet_deg(comp6a,comp3a)
  A64 <- comp_inet_deg(comp6a,comp4a)
  A65 <- comp_inet_deg(comp6a,comp5a)
  A66 <- comp_inet_deg(comp6a,comp6a)
  A67 <- comp_inet_deg(comp6a,comp7a)
  A68 <- comp_inet_deg(comp6a,comp8a)
  A69 <- comp_inet_deg(comp6a,comp9a)
  A610 <- comp_inet_deg(comp6a,comp10a)
  A611 <- comp_inet_deg(comp6a,comp11a)
  A612 <- comp_inet_deg(comp6a,comp12a)
  A613 <- comp_inet_deg(comp6a,comp13a)
  A614 <- comp_inet_deg(comp6a,comp14a)
  A615 <- comp_inet_deg(comp6a,comp15a)
  A616 <- comp_inet_deg(comp6a,comp16a)
  
  #row7
  A71 <- comp_inet_deg(comp7a,comp1a)
  A72 <- comp_inet_deg(comp7a,comp2a)
  A73 <- comp_inet_deg(comp7a,comp3a)
  A74 <- comp_inet_deg(comp7a,comp4a)
  A75 <- comp_inet_deg(comp7a,comp5a)
  A76 <- comp_inet_deg(comp7a,comp6a)
  A77 <- comp_inet_deg(comp7a,comp7a)
  A78 <- comp_inet_deg(comp7a,comp8a)
  A79 <- comp_inet_deg(comp7a,comp9a)
  A710 <- comp_inet_deg(comp7a,comp10a)
  A711 <- comp_inet_deg(comp7a,comp11a)
  A712 <- comp_inet_deg(comp7a,comp12a)
  A713 <- comp_inet_deg(comp7a,comp13a)
  A714 <- comp_inet_deg(comp7a,comp14a)
  A715 <- comp_inet_deg(comp7a,comp15a)
  A716 <- comp_inet_deg(comp7a,comp16a)
  
  #row8
  A81 <- comp_inet_deg(comp8a,comp1a)
  A82 <- comp_inet_deg(comp8a,comp2a)
  A83 <- comp_inet_deg(comp8a,comp3a)
  A84 <- comp_inet_deg(comp8a,comp4a)
  A85 <- comp_inet_deg(comp8a,comp5a)
  A86 <- comp_inet_deg(comp8a,comp6a)
  A87 <- comp_inet_deg(comp8a,comp7a)
  A88 <- comp_inet_deg(comp8a,comp8a)
  A89 <- comp_inet_deg(comp8a,comp9a)
  A810 <- comp_inet_deg(comp8a,comp10a)
  A811 <- comp_inet_deg(comp8a,comp11a)
  A812 <- comp_inet_deg(comp8a,comp12a)
  A813 <- comp_inet_deg(comp8a,comp13a)
  A814 <- comp_inet_deg(comp8a,comp14a)
  A815 <- comp_inet_deg(comp8a,comp15a)
  A816 <- comp_inet_deg(comp8a,comp16a)
  
  #row9
  A91 <- comp_inet_deg(comp9a,comp1a)
  A92 <- comp_inet_deg(comp9a,comp2a)
  A93 <- comp_inet_deg(comp9a,comp3a)
  A94 <- comp_inet_deg(comp9a,comp4a)
  A95 <- comp_inet_deg(comp9a,comp5a)
  A96 <- comp_inet_deg(comp9a,comp6a)
  A97 <- comp_inet_deg(comp9a,comp7a)
  A98 <- comp_inet_deg(comp9a,comp8a)
  A99 <- comp_inet_deg(comp9a,comp9a)
  A910 <- comp_inet_deg(comp9a,comp10a)
  A911 <- comp_inet_deg(comp9a,comp11a)
  A912 <- comp_inet_deg(comp9a,comp12a)
  A913 <- comp_inet_deg(comp9a,comp13a)
  A914 <- comp_inet_deg(comp9a,comp14a)
  A915 <- comp_inet_deg(comp9a,comp15a)
  A916 <- comp_inet_deg(comp9a,comp16a)
  
  #row10
  A101 <- comp_inet_deg(comp10a,comp1a)
  A102 <- comp_inet_deg(comp10a,comp2a)
  A103 <- comp_inet_deg(comp10a,comp3a)
  A104 <- comp_inet_deg(comp10a,comp4a)
  A105 <- comp_inet_deg(comp10a,comp5a)
  A106 <- comp_inet_deg(comp10a,comp6a)
  A107 <- comp_inet_deg(comp10a,comp7a)
  A108 <- comp_inet_deg(comp10a,comp8a)
  A109 <- comp_inet_deg(comp10a,comp9a)
  A1010 <- comp_inet_deg(comp10a,comp10a)
  A1011 <- comp_inet_deg(comp10a,comp11a)
  A1012 <- comp_inet_deg(comp10a,comp12a)
  A1013 <- comp_inet_deg(comp10a,comp13a)
  A1014 <- comp_inet_deg(comp10a,comp14a)
  A1015 <- comp_inet_deg(comp10a,comp15a)
  A1016 <- comp_inet_deg(comp10a,comp16a)
  
  #row11
  A111 <- comp_inet_deg(comp11a,comp1a)
  A112 <- comp_inet_deg(comp11a,comp2a)
  A113 <- comp_inet_deg(comp11a,comp3a)
  A114 <- comp_inet_deg(comp11a,comp4a)
  A115 <- comp_inet_deg(comp11a,comp5a)
  A116 <- comp_inet_deg(comp11a,comp6a)
  A117 <- comp_inet_deg(comp11a,comp7a)
  A118 <- comp_inet_deg(comp11a,comp8a)
  A119 <- comp_inet_deg(comp11a,comp9a)
  A1110 <- comp_inet_deg(comp11a,comp10a)
  A1111 <- comp_inet_deg(comp11a,comp11a)
  A1112 <- comp_inet_deg(comp11a,comp12a)
  A1113 <- comp_inet_deg(comp11a,comp13a)
  A1114 <- comp_inet_deg(comp11a,comp14a)
  A1115 <- comp_inet_deg(comp11a,comp15a)
  A1116 <- comp_inet_deg(comp11a,comp16a)
  
  #row12
  A121 <- comp_inet_deg(comp12a,comp1a)
  A122 <- comp_inet_deg(comp12a,comp2a)
  A123 <- comp_inet_deg(comp12a,comp3a)
  A124 <- comp_inet_deg(comp12a,comp4a)
  A125 <- comp_inet_deg(comp12a,comp5a)
  A126 <- comp_inet_deg(comp12a,comp6a)
  A127 <- comp_inet_deg(comp12a,comp7a)
  A128 <- comp_inet_deg(comp12a,comp8a)
  A129 <- comp_inet_deg(comp12a,comp9a)
  A1210 <- comp_inet_deg(comp12a,comp10a)
  A1211 <- comp_inet_deg(comp12a,comp11a)
  A1212 <- comp_inet_deg(comp12a,comp12a)
  A1213 <- comp_inet_deg(comp12a,comp13a)
  A1214 <- comp_inet_deg(comp12a,comp14a)
  A1215 <- comp_inet_deg(comp12a,comp15a)
  A1216 <- comp_inet_deg(comp12a,comp16a)
  
  #row13
  A131 <- comp_inet_deg(comp13a,comp1a)
  A132 <- comp_inet_deg(comp13a,comp2a)
  A133 <- comp_inet_deg(comp13a,comp3a)
  A134 <- comp_inet_deg(comp13a,comp4a)
  A135 <- comp_inet_deg(comp13a,comp5a)
  A136 <- comp_inet_deg(comp13a,comp6a)
  A137 <- comp_inet_deg(comp13a,comp7a)
  A138 <- comp_inet_deg(comp13a,comp8a)
  A139 <- comp_inet_deg(comp13a,comp9a)
  A1310 <- comp_inet_deg(comp13a,comp10a)
  A1311 <- comp_inet_deg(comp13a,comp11a)
  A1312 <- comp_inet_deg(comp13a,comp12a)
  A1313 <- comp_inet_deg(comp13a,comp13a)
  A1314 <- comp_inet_deg(comp13a,comp14a)
  A1315 <- comp_inet_deg(comp13a,comp15a)
  A1316 <- comp_inet_deg(comp13a,comp16a)
  
  #row14
  A141 <- comp_inet_deg(comp14a,comp1a)
  A142 <- comp_inet_deg(comp14a,comp2a)
  A143 <- comp_inet_deg(comp14a,comp3a)
  A144 <- comp_inet_deg(comp14a,comp4a)
  A145 <- comp_inet_deg(comp14a,comp5a)
  A146 <- comp_inet_deg(comp14a,comp6a)
  A147 <- comp_inet_deg(comp14a,comp7a)
  A148 <- comp_inet_deg(comp14a,comp8a)
  A149 <- comp_inet_deg(comp14a,comp9a)
  A1410 <- comp_inet_deg(comp14a,comp10a)
  A1411 <- comp_inet_deg(comp14a,comp11a)
  A1412 <- comp_inet_deg(comp14a,comp12a)
  A1413 <- comp_inet_deg(comp14a,comp13a)
  A1414 <- comp_inet_deg(comp14a,comp14a)
  A1415 <- comp_inet_deg(comp14a,comp15a)
  A1416 <- comp_inet_deg(comp14a,comp16a)
  
  #row15
  A151 <- comp_inet_deg(comp15a,comp1a)
  A152 <- comp_inet_deg(comp15a,comp2a)
  A153 <- comp_inet_deg(comp15a,comp3a)
  A154 <- comp_inet_deg(comp15a,comp4a)
  A155 <- comp_inet_deg(comp15a,comp5a)
  A156 <- comp_inet_deg(comp15a,comp6a)
  A157 <- comp_inet_deg(comp15a,comp7a)
  A158 <- comp_inet_deg(comp15a,comp8a)
  A159 <- comp_inet_deg(comp15a,comp9a)
  A1510 <- comp_inet_deg(comp15a,comp10a)
  A1511 <- comp_inet_deg(comp15a,comp11a)
  A1512 <- comp_inet_deg(comp15a,comp12a)
  A1513 <- comp_inet_deg(comp15a,comp13a)
  A1514 <- comp_inet_deg(comp15a,comp14a)
  A1515 <- comp_inet_deg(comp15a,comp15a)
  A1516 <- comp_inet_deg(comp15a,comp16a)
  
  #row16
  A161 <- comp_inet_deg(comp16a,comp1a)
  A162 <- comp_inet_deg(comp16a,comp2a)
  A163 <- comp_inet_deg(comp16a,comp3a)
  A164 <- comp_inet_deg(comp16a,comp4a)
  A165 <- comp_inet_deg(comp16a,comp5a)
  A166 <- comp_inet_deg(comp16a,comp6a)
  A167 <- comp_inet_deg(comp16a,comp7a)
  A168 <- comp_inet_deg(comp16a,comp8a)
  A169 <- comp_inet_deg(comp16a,comp9a)
  A1610 <- comp_inet_deg(comp16a,comp10a)
  A1611 <- comp_inet_deg(comp16a,comp11a)
  A1612 <- comp_inet_deg(comp16a,comp12a)
  A1613 <- comp_inet_deg(comp16a,comp13a)
  A1614 <- comp_inet_deg(comp16a,comp14a)
  A1615 <- comp_inet_deg(comp16a,comp15a)
  A1616 <- comp_inet_deg(comp16a,comp16a)
  
  EN_A <- t(matrix(c(A11  ,A12  ,A13  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,
                     A21  ,A22  ,A23  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,A214 ,A215 ,A216 ,
                     A31  ,A32  ,A33  ,A34  ,A35  ,A36  ,A37  ,A38  ,A39  ,A310 ,A311 ,A312 ,A313 ,A314 ,A315 ,A316 ,
                     A41  ,A42  ,A43  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,A414 ,A415 ,A416 ,
                     A51  ,A52  ,A53  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,A514 ,A515 ,A516 ,
                     A61  ,A62  ,A63  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,A614 ,A615 ,A616 ,
                     A71  ,A72  ,A73  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,A714 ,A715 ,A716 ,
                     A81  ,A82  ,A83  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,A814 ,A815 ,A816 ,
                     A91  ,A92  ,A93  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,A914 ,A915 ,A916 ,
                     A101 ,A102 ,A103 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,A1014,A1015,A1016,
                     A111 ,A112 ,A113 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,A1114,A1115,A1116,
                     A121 ,A122 ,A123 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,A1214,A1215,A1216,
                     A131 ,A132 ,A133 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313,A1314,A1315,A1316,
                     A141 ,A142 ,A143 ,A144 ,A145 ,A146 ,A147 ,A148 ,A149 ,A1410,A1411,A1412,A1413,A1414,A1415,A1416,
                     A151 ,A152 ,A153 ,A154 ,A155 ,A156 ,A157 ,A158 ,A159 ,A1510,A1511,A1512,A1513,A1514,A1515,A1516,
                     A161 ,A162 ,A163 ,A164 ,A165 ,A166 ,A167 ,A168 ,A169 ,A1610,A1611,A1612,A1613,A1614,A1615,A1616), nrow=16, ncol=16))
  
  return(EN_A)
  
}
m.inet.deg <- matrix_inet_deg(v.comp_inet)

#james

compJ1a = read_excel("compJ1a_iname.xlsx")
compJ2a = read_excel("compJ2a_iname.xlsx")
compJ4a = read_excel("compJ4a_iname.xlsx")
compJ5a = read_excel("compJ5a_iname.xlsx")
compJ6a = read_excel("compJ6a_iname.xlsx")
compJ7a = read_excel("compJ7a_iname.xlsx")
compJ8a = read_excel("compJ8a_iname.xlsx")
compJ9a = read_excel("compJ9a_iname.xlsx")
compJ10a = read_excel("compJ10a_iname.xlsx")
compJ11a = read_excel("compJ11a_iname.xlsx")
compJ12a = read_excel("compJ12a_iname.xlsx")
compJ13a = read_excel("compJ13a_iname.xlsx")

v.compJ_inet <- c(compJ1a,compJ2a,compJ4a,compJ5a,compJ6a,compJ7a,compJ8a,compJ9a,compJ10a,compJ11a,compJ12a,compJ13a)
matrix_inetJ_den <- function (v.compJ_inet) {
  A11 <- comp_inet_den(compJ1a,compJ1a)
  A12 <- comp_inet_den(compJ1a,compJ2a)
  A14 <- comp_inet_den(compJ1a,compJ4a)
  A15 <- comp_inet_den(compJ1a,compJ5a)
  A16 <- comp_inet_den(compJ1a,compJ6a)
  A17 <- comp_inet_den(compJ1a,compJ7a)
  A18 <- comp_inet_den(compJ1a,compJ8a)
  A19 <- comp_inet_den(compJ1a,compJ9a)
  A110 <- comp_inet_den(compJ1a,compJ10a)
  A111 <- comp_inet_den(compJ1a,compJ11a)
  A112 <- comp_inet_den(compJ1a,compJ12a)
  A113 <- comp_inet_den(compJ1a,compJ13a)
  
  
  #row2
  A21 <- comp_inet_den(compJ2a,compJ1a)
  A22 <- comp_inet_den(compJ2a,compJ2a)
  
  A24 <- comp_inet_den(compJ2a,compJ4a)
  A25 <- comp_inet_den(compJ2a,compJ5a)
  A26 <- comp_inet_den(compJ2a,compJ6a)
  A27 <- comp_inet_den(compJ2a,compJ7a)
  A28 <- comp_inet_den(compJ2a,compJ8a)
  A29 <- comp_inet_den(compJ2a,compJ9a)
  A210 <- comp_inet_den(compJ2a,compJ10a)
  A211 <- comp_inet_den(compJ2a,compJ11a)
  A212 <- comp_inet_den(compJ2a,compJ12a)
  A213 <- comp_inet_den(compJ2a,compJ13a)
  
  
  #row4
  A41 <- comp_inet_den(compJ4a,compJ1a)
  A42 <- comp_inet_den(compJ4a,compJ2a)
  
  A44 <- comp_inet_den(compJ4a,compJ4a)
  A45 <- comp_inet_den(compJ4a,compJ5a)
  A46 <- comp_inet_den(compJ4a,compJ6a)
  A47 <- comp_inet_den(compJ4a,compJ7a)
  A48 <- comp_inet_den(compJ4a,compJ8a)
  A49 <- comp_inet_den(compJ4a,compJ9a)
  A410 <- comp_inet_den(compJ4a,compJ10a)
  A411 <- comp_inet_den(compJ4a,compJ11a)
  A412 <- comp_inet_den(compJ4a,compJ12a)
  A413 <- comp_inet_den(compJ4a,compJ13a)
  
  
  #row5
  A51 <- comp_inet_den(compJ5a,compJ1a)
  A52 <- comp_inet_den(compJ5a,compJ2a)
  
  A54 <- comp_inet_den(compJ5a,compJ4a)
  A55 <- comp_inet_den(compJ5a,compJ5a)
  A56 <- comp_inet_den(compJ5a,compJ6a)
  A57 <- comp_inet_den(compJ5a,compJ7a)
  A58 <- comp_inet_den(compJ5a,compJ8a)
  A59 <- comp_inet_den(compJ5a,compJ9a)
  A510 <- comp_inet_den(compJ5a,compJ10a)
  A511 <- comp_inet_den(compJ5a,compJ11a)
  A512 <- comp_inet_den(compJ5a,compJ12a)
  A513 <- comp_inet_den(compJ5a,compJ13a)
  
  
  #row6
  A61 <- comp_inet_den(compJ6a,compJ1a)
  A62 <- comp_inet_den(compJ6a,compJ2a)
  
  A64 <- comp_inet_den(compJ6a,compJ4a)
  A65 <- comp_inet_den(compJ6a,compJ5a)
  A66 <- comp_inet_den(compJ6a,compJ6a)
  A67 <- comp_inet_den(compJ6a,compJ7a)
  A68 <- comp_inet_den(compJ6a,compJ8a)
  A69 <- comp_inet_den(compJ6a,compJ9a)
  A610 <- comp_inet_den(compJ6a,compJ10a)
  A611 <- comp_inet_den(compJ6a,compJ11a)
  A612 <- comp_inet_den(compJ6a,compJ12a)
  A613 <- comp_inet_den(compJ6a,compJ13a)
  
  
  #row7
  A71 <- comp_inet_den(compJ7a,compJ1a)
  A72 <- comp_inet_den(compJ7a,compJ2a)
  
  A74 <- comp_inet_den(compJ7a,compJ4a)
  A75 <- comp_inet_den(compJ7a,compJ5a)
  A76 <- comp_inet_den(compJ7a,compJ6a)
  A77 <- comp_inet_den(compJ7a,compJ7a)
  A78 <- comp_inet_den(compJ7a,compJ8a)
  A79 <- comp_inet_den(compJ7a,compJ9a)
  A710 <- comp_inet_den(compJ7a,compJ10a)
  A711 <- comp_inet_den(compJ7a,compJ11a)
  A712 <- comp_inet_den(compJ7a,compJ12a)
  A713 <- comp_inet_den(compJ7a,compJ13a)
  
  
  #row8
  A81 <- comp_inet_den(compJ8a,compJ1a)
  A82 <- comp_inet_den(compJ8a,compJ2a)
  
  A84 <- comp_inet_den(compJ8a,compJ4a)
  A85 <- comp_inet_den(compJ8a,compJ5a)
  A86 <- comp_inet_den(compJ8a,compJ6a)
  A87 <- comp_inet_den(compJ8a,compJ7a)
  A88 <- comp_inet_den(compJ8a,compJ8a)
  A89 <- comp_inet_den(compJ8a,compJ9a)
  A810 <- comp_inet_den(compJ8a,compJ10a)
  A811 <- comp_inet_den(compJ8a,compJ11a)
  A812 <- comp_inet_den(compJ8a,compJ12a)
  A813 <- comp_inet_den(compJ8a,compJ13a)
  
  
  #row9
  A91 <- comp_inet_den(compJ9a,compJ1a)
  A92 <- comp_inet_den(compJ9a,compJ2a)
  
  A94 <- comp_inet_den(compJ9a,compJ4a)
  A95 <- comp_inet_den(compJ9a,compJ5a)
  A96 <- comp_inet_den(compJ9a,compJ6a)
  A97 <- comp_inet_den(compJ9a,compJ7a)
  A98 <- comp_inet_den(compJ9a,compJ8a)
  A99 <- comp_inet_den(compJ9a,compJ9a)
  A910 <- comp_inet_den(compJ9a,compJ10a)
  A911 <- comp_inet_den(compJ9a,compJ11a)
  A912 <- comp_inet_den(compJ9a,compJ12a)
  A913 <- comp_inet_den(compJ9a,compJ13a)
  
  #row10
  A101 <- comp_inet_den(compJ10a,compJ1a)
  A102 <- comp_inet_den(compJ10a,compJ2a)
  
  A104 <- comp_inet_den(compJ10a,compJ4a)
  A105 <- comp_inet_den(compJ10a,compJ5a)
  A106 <- comp_inet_den(compJ10a,compJ6a)
  A107 <- comp_inet_den(compJ10a,compJ7a)
  A108 <- comp_inet_den(compJ10a,compJ8a)
  A109 <- comp_inet_den(compJ10a,compJ9a)
  A1010 <- comp_inet_den(compJ10a,compJ10a)
  A1011 <- comp_inet_den(compJ10a,compJ11a)
  A1012 <- comp_inet_den(compJ10a,compJ12a)
  A1013 <- comp_inet_den(compJ10a,compJ13a)
  
  
  #row11
  A111 <- comp_inet_den(compJ11a,compJ1a)
  A112 <- comp_inet_den(compJ11a,compJ2a)
  
  A114 <- comp_inet_den(compJ11a,compJ4a)
  A115 <- comp_inet_den(compJ11a,compJ5a)
  A116 <- comp_inet_den(compJ11a,compJ6a)
  A117 <- comp_inet_den(compJ11a,compJ7a)
  A118 <- comp_inet_den(compJ11a,compJ8a)
  A119 <- comp_inet_den(compJ11a,compJ9a)
  A1110 <- comp_inet_den(compJ11a,compJ10a)
  A1111 <- comp_inet_den(compJ11a,compJ11a)
  A1112 <- comp_inet_den(compJ11a,compJ12a)
  A1113 <- comp_inet_den(compJ11a,compJ13a)
  
  
  #row12
  A121 <- comp_inet_den(compJ12a,compJ1a)
  A122 <- comp_inet_den(compJ12a,compJ2a)
  
  A124 <- comp_inet_den(compJ12a,compJ4a)
  A125 <- comp_inet_den(compJ12a,compJ5a)
  A126 <- comp_inet_den(compJ12a,compJ6a)
  A127 <- comp_inet_den(compJ12a,compJ7a)
  A128 <- comp_inet_den(compJ12a,compJ8a)
  A129 <- comp_inet_den(compJ12a,compJ9a)
  A1210 <- comp_inet_den(compJ12a,compJ10a)
  A1211 <- comp_inet_den(compJ12a,compJ11a)
  A1212 <- comp_inet_den(compJ12a,compJ12a)
  A1213 <- comp_inet_den(compJ12a,compJ13a)
  
  
  #row13
  A131 <- comp_inet_den(compJ13a,compJ1a)
  A132 <- comp_inet_den(compJ13a,compJ2a)
  
  A134 <- comp_inet_den(compJ13a,compJ4a)
  A135 <- comp_inet_den(compJ13a,compJ5a)
  A136 <- comp_inet_den(compJ13a,compJ6a)
  A137 <- comp_inet_den(compJ13a,compJ7a)
  A138 <- comp_inet_den(compJ13a,compJ8a)
  A139 <- comp_inet_den(compJ13a,compJ9a)
  A1310 <- comp_inet_den(compJ13a,compJ10a)
  A1311 <- comp_inet_den(compJ13a,compJ11a)
  A1312 <- comp_inet_den(compJ13a,compJ12a)
  A1313 <- comp_inet_den(compJ13a,compJ13a)
  
  
  EN_A <- t(matrix(c(A11  ,A12  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,
                     A21  ,A22  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,
                     A41  ,A42  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,
                     A51  ,A52  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,
                     A61  ,A62  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,
                     A71  ,A72  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,
                     A81  ,A82  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,
                     A91  ,A92  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,
                     A101 ,A102 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,
                     A111 ,A112 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,
                     A121 ,A122 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,
                     A131 ,A132 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313), nrow=12, ncol=12))
  
  return(EN_A)
  
}
m.inet.den.j <- matrix_inetJ_den(v.compJ_inet)

gcor(m.inet.den.j, Jz_all)

matrix_inetJ_bet <- function (v.compJ_inet) {
  A11 <- comp_inet_bet(compJ1a,compJ1a)
  A12 <- comp_inet_bet(compJ1a,compJ2a)
  A14 <- comp_inet_bet(compJ1a,compJ4a)
  A15 <- comp_inet_bet(compJ1a,compJ5a)
  A16 <- comp_inet_bet(compJ1a,compJ6a)
  A17 <- comp_inet_bet(compJ1a,compJ7a)
  A18 <- comp_inet_bet(compJ1a,compJ8a)
  A19 <- comp_inet_bet(compJ1a,compJ9a)
  A110 <- comp_inet_bet(compJ1a,compJ10a)
  A111 <- comp_inet_bet(compJ1a,compJ11a)
  A112 <- comp_inet_bet(compJ1a,compJ12a)
  A113 <- comp_inet_bet(compJ1a,compJ13a)
  
  
  #row2
  A21 <- comp_inet_bet(compJ2a,compJ1a)
  A22 <- comp_inet_bet(compJ2a,compJ2a)
  
  A24 <- comp_inet_bet(compJ2a,compJ4a)
  A25 <- comp_inet_bet(compJ2a,compJ5a)
  A26 <- comp_inet_bet(compJ2a,compJ6a)
  A27 <- comp_inet_bet(compJ2a,compJ7a)
  A28 <- comp_inet_bet(compJ2a,compJ8a)
  A29 <- comp_inet_bet(compJ2a,compJ9a)
  A210 <- comp_inet_bet(compJ2a,compJ10a)
  A211 <- comp_inet_bet(compJ2a,compJ11a)
  A212 <- comp_inet_bet(compJ2a,compJ12a)
  A213 <- comp_inet_bet(compJ2a,compJ13a)
  
  
  #row4
  A41 <- comp_inet_bet(compJ4a,compJ1a)
  A42 <- comp_inet_bet(compJ4a,compJ2a)
  
  A44 <- comp_inet_bet(compJ4a,compJ4a)
  A45 <- comp_inet_bet(compJ4a,compJ5a)
  A46 <- comp_inet_bet(compJ4a,compJ6a)
  A47 <- comp_inet_bet(compJ4a,compJ7a)
  A48 <- comp_inet_bet(compJ4a,compJ8a)
  A49 <- comp_inet_bet(compJ4a,compJ9a)
  A410 <- comp_inet_bet(compJ4a,compJ10a)
  A411 <- comp_inet_bet(compJ4a,compJ11a)
  A412 <- comp_inet_bet(compJ4a,compJ12a)
  A413 <- comp_inet_bet(compJ4a,compJ13a)
  
  
  #row5
  A51 <- comp_inet_bet(compJ5a,compJ1a)
  A52 <- comp_inet_bet(compJ5a,compJ2a)
  
  A54 <- comp_inet_bet(compJ5a,compJ4a)
  A55 <- comp_inet_bet(compJ5a,compJ5a)
  A56 <- comp_inet_bet(compJ5a,compJ6a)
  A57 <- comp_inet_bet(compJ5a,compJ7a)
  A58 <- comp_inet_bet(compJ5a,compJ8a)
  A59 <- comp_inet_bet(compJ5a,compJ9a)
  A510 <- comp_inet_bet(compJ5a,compJ10a)
  A511 <- comp_inet_bet(compJ5a,compJ11a)
  A512 <- comp_inet_bet(compJ5a,compJ12a)
  A513 <- comp_inet_bet(compJ5a,compJ13a)
  
  
  #row6
  A61 <- comp_inet_bet(compJ6a,compJ1a)
  A62 <- comp_inet_bet(compJ6a,compJ2a)
  
  A64 <- comp_inet_bet(compJ6a,compJ4a)
  A65 <- comp_inet_bet(compJ6a,compJ5a)
  A66 <- comp_inet_bet(compJ6a,compJ6a)
  A67 <- comp_inet_bet(compJ6a,compJ7a)
  A68 <- comp_inet_bet(compJ6a,compJ8a)
  A69 <- comp_inet_bet(compJ6a,compJ9a)
  A610 <- comp_inet_bet(compJ6a,compJ10a)
  A611 <- comp_inet_bet(compJ6a,compJ11a)
  A612 <- comp_inet_bet(compJ6a,compJ12a)
  A613 <- comp_inet_bet(compJ6a,compJ13a)
  
  
  #row7
  A71 <- comp_inet_bet(compJ7a,compJ1a)
  A72 <- comp_inet_bet(compJ7a,compJ2a)
  
  A74 <- comp_inet_bet(compJ7a,compJ4a)
  A75 <- comp_inet_bet(compJ7a,compJ5a)
  A76 <- comp_inet_bet(compJ7a,compJ6a)
  A77 <- comp_inet_bet(compJ7a,compJ7a)
  A78 <- comp_inet_bet(compJ7a,compJ8a)
  A79 <- comp_inet_bet(compJ7a,compJ9a)
  A710 <- comp_inet_bet(compJ7a,compJ10a)
  A711 <- comp_inet_bet(compJ7a,compJ11a)
  A712 <- comp_inet_bet(compJ7a,compJ12a)
  A713 <- comp_inet_bet(compJ7a,compJ13a)
  
  
  #row8
  A81 <- comp_inet_bet(compJ8a,compJ1a)
  A82 <- comp_inet_bet(compJ8a,compJ2a)
  
  A84 <- comp_inet_bet(compJ8a,compJ4a)
  A85 <- comp_inet_bet(compJ8a,compJ5a)
  A86 <- comp_inet_bet(compJ8a,compJ6a)
  A87 <- comp_inet_bet(compJ8a,compJ7a)
  A88 <- comp_inet_bet(compJ8a,compJ8a)
  A89 <- comp_inet_bet(compJ8a,compJ9a)
  A810 <- comp_inet_bet(compJ8a,compJ10a)
  A811 <- comp_inet_bet(compJ8a,compJ11a)
  A812 <- comp_inet_bet(compJ8a,compJ12a)
  A813 <- comp_inet_bet(compJ8a,compJ13a)
  
  
  #row9
  A91 <- comp_inet_bet(compJ9a,compJ1a)
  A92 <- comp_inet_bet(compJ9a,compJ2a)
  
  A94 <- comp_inet_bet(compJ9a,compJ4a)
  A95 <- comp_inet_bet(compJ9a,compJ5a)
  A96 <- comp_inet_bet(compJ9a,compJ6a)
  A97 <- comp_inet_bet(compJ9a,compJ7a)
  A98 <- comp_inet_bet(compJ9a,compJ8a)
  A99 <- comp_inet_bet(compJ9a,compJ9a)
  A910 <- comp_inet_bet(compJ9a,compJ10a)
  A911 <- comp_inet_bet(compJ9a,compJ11a)
  A912 <- comp_inet_bet(compJ9a,compJ12a)
  A913 <- comp_inet_bet(compJ9a,compJ13a)
  
  #row10
  A101 <- comp_inet_bet(compJ10a,compJ1a)
  A102 <- comp_inet_bet(compJ10a,compJ2a)
  
  A104 <- comp_inet_bet(compJ10a,compJ4a)
  A105 <- comp_inet_bet(compJ10a,compJ5a)
  A106 <- comp_inet_bet(compJ10a,compJ6a)
  A107 <- comp_inet_bet(compJ10a,compJ7a)
  A108 <- comp_inet_bet(compJ10a,compJ8a)
  A109 <- comp_inet_bet(compJ10a,compJ9a)
  A1010 <- comp_inet_bet(compJ10a,compJ10a)
  A1011 <- comp_inet_bet(compJ10a,compJ11a)
  A1012 <- comp_inet_bet(compJ10a,compJ12a)
  A1013 <- comp_inet_bet(compJ10a,compJ13a)
  
  
  #row11
  A111 <- comp_inet_bet(compJ11a,compJ1a)
  A112 <- comp_inet_bet(compJ11a,compJ2a)
  
  A114 <- comp_inet_bet(compJ11a,compJ4a)
  A115 <- comp_inet_bet(compJ11a,compJ5a)
  A116 <- comp_inet_bet(compJ11a,compJ6a)
  A117 <- comp_inet_bet(compJ11a,compJ7a)
  A118 <- comp_inet_bet(compJ11a,compJ8a)
  A119 <- comp_inet_bet(compJ11a,compJ9a)
  A1110 <- comp_inet_bet(compJ11a,compJ10a)
  A1111 <- comp_inet_bet(compJ11a,compJ11a)
  A1112 <- comp_inet_bet(compJ11a,compJ12a)
  A1113 <- comp_inet_bet(compJ11a,compJ13a)
  
  
  #row12
  A121 <- comp_inet_bet(compJ12a,compJ1a)
  A122 <- comp_inet_bet(compJ12a,compJ2a)
  
  A124 <- comp_inet_bet(compJ12a,compJ4a)
  A125 <- comp_inet_bet(compJ12a,compJ5a)
  A126 <- comp_inet_bet(compJ12a,compJ6a)
  A127 <- comp_inet_bet(compJ12a,compJ7a)
  A128 <- comp_inet_bet(compJ12a,compJ8a)
  A129 <- comp_inet_bet(compJ12a,compJ9a)
  A1210 <- comp_inet_bet(compJ12a,compJ10a)
  A1211 <- comp_inet_bet(compJ12a,compJ11a)
  A1212 <- comp_inet_bet(compJ12a,compJ12a)
  A1213 <- comp_inet_bet(compJ12a,compJ13a)
  
  
  #row13
  A131 <- comp_inet_bet(compJ13a,compJ1a)
  A132 <- comp_inet_bet(compJ13a,compJ2a)
  
  A134 <- comp_inet_bet(compJ13a,compJ4a)
  A135 <- comp_inet_bet(compJ13a,compJ5a)
  A136 <- comp_inet_bet(compJ13a,compJ6a)
  A137 <- comp_inet_bet(compJ13a,compJ7a)
  A138 <- comp_inet_bet(compJ13a,compJ8a)
  A139 <- comp_inet_bet(compJ13a,compJ9a)
  A1310 <- comp_inet_bet(compJ13a,compJ10a)
  A1311 <- comp_inet_bet(compJ13a,compJ11a)
  A1312 <- comp_inet_bet(compJ13a,compJ12a)
  A1313 <- comp_inet_bet(compJ13a,compJ13a)
  
  
  EN_A <- t(matrix(c(A11  ,A12  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,
                     A21  ,A22  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,
                     A41  ,A42  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,
                     A51  ,A52  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,
                     A61  ,A62  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,
                     A71  ,A72  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,
                     A81  ,A82  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,
                     A91  ,A92  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,
                     A101 ,A102 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,
                     A111 ,A112 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,
                     A121 ,A122 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,
                     A131 ,A132 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313), nrow=12, ncol=12))
  
  return(EN_A)
  
}
m.inet.bet.j <- matrix_inetJ_bet(v.compJ_inet)

gcor(m.inet.den.j, m.inet.bet.j)

matrix_inetJ_deg <- function (v.compJ_inet) {
  A11 <- comp_inet_deg(compJ1a,compJ1a)
  A12 <- comp_inet_deg(compJ1a,compJ2a)
  A14 <- comp_inet_deg(compJ1a,compJ4a)
  A15 <- comp_inet_deg(compJ1a,compJ5a)
  A16 <- comp_inet_deg(compJ1a,compJ6a)
  A17 <- comp_inet_deg(compJ1a,compJ7a)
  A18 <- comp_inet_deg(compJ1a,compJ8a)
  A19 <- comp_inet_deg(compJ1a,compJ9a)
  A110 <- comp_inet_deg(compJ1a,compJ10a)
  A111 <- comp_inet_deg(compJ1a,compJ11a)
  A112 <- comp_inet_deg(compJ1a,compJ12a)
  A113 <- comp_inet_deg(compJ1a,compJ13a)
  
  
  #row2
  A21 <- comp_inet_deg(compJ2a,compJ1a)
  A22 <- comp_inet_deg(compJ2a,compJ2a)
  
  A24 <- comp_inet_deg(compJ2a,compJ4a)
  A25 <- comp_inet_deg(compJ2a,compJ5a)
  A26 <- comp_inet_deg(compJ2a,compJ6a)
  A27 <- comp_inet_deg(compJ2a,compJ7a)
  A28 <- comp_inet_deg(compJ2a,compJ8a)
  A29 <- comp_inet_deg(compJ2a,compJ9a)
  A210 <- comp_inet_deg(compJ2a,compJ10a)
  A211 <- comp_inet_deg(compJ2a,compJ11a)
  A212 <- comp_inet_deg(compJ2a,compJ12a)
  A213 <- comp_inet_deg(compJ2a,compJ13a)
  
  
  #row4
  A41 <- comp_inet_deg(compJ4a,compJ1a)
  A42 <- comp_inet_deg(compJ4a,compJ2a)
  
  A44 <- comp_inet_deg(compJ4a,compJ4a)
  A45 <- comp_inet_deg(compJ4a,compJ5a)
  A46 <- comp_inet_deg(compJ4a,compJ6a)
  A47 <- comp_inet_deg(compJ4a,compJ7a)
  A48 <- comp_inet_deg(compJ4a,compJ8a)
  A49 <- comp_inet_deg(compJ4a,compJ9a)
  A410 <- comp_inet_deg(compJ4a,compJ10a)
  A411 <- comp_inet_deg(compJ4a,compJ11a)
  A412 <- comp_inet_deg(compJ4a,compJ12a)
  A413 <- comp_inet_deg(compJ4a,compJ13a)
  
  
  #row5
  A51 <- comp_inet_deg(compJ5a,compJ1a)
  A52 <- comp_inet_deg(compJ5a,compJ2a)
  
  A54 <- comp_inet_deg(compJ5a,compJ4a)
  A55 <- comp_inet_deg(compJ5a,compJ5a)
  A56 <- comp_inet_deg(compJ5a,compJ6a)
  A57 <- comp_inet_deg(compJ5a,compJ7a)
  A58 <- comp_inet_deg(compJ5a,compJ8a)
  A59 <- comp_inet_deg(compJ5a,compJ9a)
  A510 <- comp_inet_deg(compJ5a,compJ10a)
  A511 <- comp_inet_deg(compJ5a,compJ11a)
  A512 <- comp_inet_deg(compJ5a,compJ12a)
  A513 <- comp_inet_deg(compJ5a,compJ13a)
  
  
  #row6
  A61 <- comp_inet_deg(compJ6a,compJ1a)
  A62 <- comp_inet_deg(compJ6a,compJ2a)
  
  A64 <- comp_inet_deg(compJ6a,compJ4a)
  A65 <- comp_inet_deg(compJ6a,compJ5a)
  A66 <- comp_inet_deg(compJ6a,compJ6a)
  A67 <- comp_inet_deg(compJ6a,compJ7a)
  A68 <- comp_inet_deg(compJ6a,compJ8a)
  A69 <- comp_inet_deg(compJ6a,compJ9a)
  A610 <- comp_inet_deg(compJ6a,compJ10a)
  A611 <- comp_inet_deg(compJ6a,compJ11a)
  A612 <- comp_inet_deg(compJ6a,compJ12a)
  A613 <- comp_inet_deg(compJ6a,compJ13a)
  
  
  #row7
  A71 <- comp_inet_deg(compJ7a,compJ1a)
  A72 <- comp_inet_deg(compJ7a,compJ2a)
  
  A74 <- comp_inet_deg(compJ7a,compJ4a)
  A75 <- comp_inet_deg(compJ7a,compJ5a)
  A76 <- comp_inet_deg(compJ7a,compJ6a)
  A77 <- comp_inet_deg(compJ7a,compJ7a)
  A78 <- comp_inet_deg(compJ7a,compJ8a)
  A79 <- comp_inet_deg(compJ7a,compJ9a)
  A710 <- comp_inet_deg(compJ7a,compJ10a)
  A711 <- comp_inet_deg(compJ7a,compJ11a)
  A712 <- comp_inet_deg(compJ7a,compJ12a)
  A713 <- comp_inet_deg(compJ7a,compJ13a)
  
  
  #row8
  A81 <- comp_inet_deg(compJ8a,compJ1a)
  A82 <- comp_inet_deg(compJ8a,compJ2a)
  
  A84 <- comp_inet_deg(compJ8a,compJ4a)
  A85 <- comp_inet_deg(compJ8a,compJ5a)
  A86 <- comp_inet_deg(compJ8a,compJ6a)
  A87 <- comp_inet_deg(compJ8a,compJ7a)
  A88 <- comp_inet_deg(compJ8a,compJ8a)
  A89 <- comp_inet_deg(compJ8a,compJ9a)
  A810 <- comp_inet_deg(compJ8a,compJ10a)
  A811 <- comp_inet_deg(compJ8a,compJ11a)
  A812 <- comp_inet_deg(compJ8a,compJ12a)
  A813 <- comp_inet_deg(compJ8a,compJ13a)
  
  
  #row9
  A91 <- comp_inet_deg(compJ9a,compJ1a)
  A92 <- comp_inet_deg(compJ9a,compJ2a)
  
  A94 <- comp_inet_deg(compJ9a,compJ4a)
  A95 <- comp_inet_deg(compJ9a,compJ5a)
  A96 <- comp_inet_deg(compJ9a,compJ6a)
  A97 <- comp_inet_deg(compJ9a,compJ7a)
  A98 <- comp_inet_deg(compJ9a,compJ8a)
  A99 <- comp_inet_deg(compJ9a,compJ9a)
  A910 <- comp_inet_deg(compJ9a,compJ10a)
  A911 <- comp_inet_deg(compJ9a,compJ11a)
  A912 <- comp_inet_deg(compJ9a,compJ12a)
  A913 <- comp_inet_deg(compJ9a,compJ13a)
  
  #row10
  A101 <- comp_inet_deg(compJ10a,compJ1a)
  A102 <- comp_inet_deg(compJ10a,compJ2a)
  
  A104 <- comp_inet_deg(compJ10a,compJ4a)
  A105 <- comp_inet_deg(compJ10a,compJ5a)
  A106 <- comp_inet_deg(compJ10a,compJ6a)
  A107 <- comp_inet_deg(compJ10a,compJ7a)
  A108 <- comp_inet_deg(compJ10a,compJ8a)
  A109 <- comp_inet_deg(compJ10a,compJ9a)
  A1010 <- comp_inet_deg(compJ10a,compJ10a)
  A1011 <- comp_inet_deg(compJ10a,compJ11a)
  A1012 <- comp_inet_deg(compJ10a,compJ12a)
  A1013 <- comp_inet_deg(compJ10a,compJ13a)
  
  
  #row11
  A111 <- comp_inet_deg(compJ11a,compJ1a)
  A112 <- comp_inet_deg(compJ11a,compJ2a)
  
  A114 <- comp_inet_deg(compJ11a,compJ4a)
  A115 <- comp_inet_deg(compJ11a,compJ5a)
  A116 <- comp_inet_deg(compJ11a,compJ6a)
  A117 <- comp_inet_deg(compJ11a,compJ7a)
  A118 <- comp_inet_deg(compJ11a,compJ8a)
  A119 <- comp_inet_deg(compJ11a,compJ9a)
  A1110 <- comp_inet_deg(compJ11a,compJ10a)
  A1111 <- comp_inet_deg(compJ11a,compJ11a)
  A1112 <- comp_inet_deg(compJ11a,compJ12a)
  A1113 <- comp_inet_deg(compJ11a,compJ13a)
  
  
  #row12
  A121 <- comp_inet_deg(compJ12a,compJ1a)
  A122 <- comp_inet_deg(compJ12a,compJ2a)
  
  A124 <- comp_inet_deg(compJ12a,compJ4a)
  A125 <- comp_inet_deg(compJ12a,compJ5a)
  A126 <- comp_inet_deg(compJ12a,compJ6a)
  A127 <- comp_inet_deg(compJ12a,compJ7a)
  A128 <- comp_inet_deg(compJ12a,compJ8a)
  A129 <- comp_inet_deg(compJ12a,compJ9a)
  A1210 <- comp_inet_deg(compJ12a,compJ10a)
  A1211 <- comp_inet_deg(compJ12a,compJ11a)
  A1212 <- comp_inet_deg(compJ12a,compJ12a)
  A1213 <- comp_inet_deg(compJ12a,compJ13a)
  
  
  #row13
  A131 <- comp_inet_deg(compJ13a,compJ1a)
  A132 <- comp_inet_deg(compJ13a,compJ2a)
  
  A134 <- comp_inet_deg(compJ13a,compJ4a)
  A135 <- comp_inet_deg(compJ13a,compJ5a)
  A136 <- comp_inet_deg(compJ13a,compJ6a)
  A137 <- comp_inet_deg(compJ13a,compJ7a)
  A138 <- comp_inet_deg(compJ13a,compJ8a)
  A139 <- comp_inet_deg(compJ13a,compJ9a)
  A1310 <- comp_inet_deg(compJ13a,compJ10a)
  A1311 <- comp_inet_deg(compJ13a,compJ11a)
  A1312 <- comp_inet_deg(compJ13a,compJ12a)
  A1313 <- comp_inet_deg(compJ13a,compJ13a)
  
  
  EN_A <- t(matrix(c(A11  ,A12  ,A14  ,A15  ,A16  ,A17  ,A18  ,A19  ,A110 ,A111 ,A112 ,A113 ,
                     A21  ,A22  ,A24  ,A25  ,A26  ,A27  ,A28  ,A29  ,A210 ,A211 ,A212 ,A213 ,
                     A41  ,A42  ,A44  ,A45  ,A46  ,A47  ,A48  ,A49  ,A410 ,A411 ,A412 ,A413 ,
                     A51  ,A52  ,A54  ,A55  ,A56  ,A57  ,A58  ,A59  ,A510 ,A511 ,A512 ,A513 ,
                     A61  ,A62  ,A64  ,A65  ,A66  ,A67  ,A68  ,A69  ,A610 ,A611 ,A612 ,A613 ,
                     A71  ,A72  ,A74  ,A75  ,A76  ,A77  ,A78  ,A79  ,A710 ,A711 ,A712 ,A713 ,
                     A81  ,A82  ,A84  ,A85  ,A86  ,A87  ,A88  ,A89  ,A810 ,A811 ,A812 ,A813 ,
                     A91  ,A92  ,A94  ,A95  ,A96  ,A97  ,A98  ,A99  ,A910 ,A911 ,A912 ,A913 ,
                     A101 ,A102 ,A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,A1010,A1011,A1012,A1013,
                     A111 ,A112 ,A114 ,A115 ,A116 ,A117 ,A118 ,A119 ,A1110,A1111,A1112,A1113,
                     A121 ,A122 ,A124 ,A125 ,A126 ,A127 ,A128 ,A129 ,A1210,A1211,A1212,A1213,
                     A131 ,A132 ,A134 ,A135 ,A136 ,A137 ,A138 ,A139 ,A1310,A1311,A1312,A1313), nrow=12, ncol=12))
  
  return(EN_A)
  
}
m.inet.deg.j <- matrix_inetJ_deg(v.compJ_inet)

gcor(m.inet.den.j, m.inet.deg.j)

##### Network of examiners#####

comp1a = read_excel("comp1a_exm.xlsx")
comp2a = read_excel("comp2a_exm.xlsx")
comp3a = read_excel("comp3a_exm.xlsx")
comp4a = read_excel("comp4a_exm.xlsx")
comp5a = read_excel("comp5a_exm.xlsx")
comp6a = read_excel("comp6a_exm.xlsx")
comp7a = read_excel("comp7a_exm.xlsx")
comp8a = read_excel("comp8a_exm.xlsx")
comp9a = read_excel("comp9a_exm.xlsx")
comp10a = read_excel("comp10a_exm.xlsx")
comp11a = read_excel("comp11a_exm.xlsx")
comp12a = read_excel("comp12a_exm.xlsx")
comp13a = read_excel("comp13a_exm.xlsx")
comp14a = read_excel("comp14a_exm.xlsx")
comp15a = read_excel("comp15a_exm.xlsx")
comp16a = read_excel("comp16a_exm.xlsx")

v.comp_enet <- c(comp1a,comp2a,comp3a,comp4a,comp5a,comp6a,comp7a,comp8a,comp9a,comp10a,comp11a,comp12a,comp13a,comp14a,comp15a,comp16a)
m.enet.den <- matrix_inet_den(v.comp_enet)

gcor(m.inet.den, m.enet.den)
plot(m.inet.den, m.enet.den)

m.enet.bet <- matrix_inet_bet(v.comp_enet)

gcor(m.inet.bet, m.enet.bet)
plot(m.inet.bet, m.enet.bet)

m.enet.deg <- matrix_inet_deg(v.comp_enet)

gcor(m.inet.deg, m.enet.deg)
plot(m.inet.deg, m.enet.deg)


#james

setwd("~/Citations/similarity")
compJ1a = read_excel("compJ1a_exm.xlsx")
compJ2a = read_excel("compJ2a_exm.xlsx")
compJ4a = read_excel("compJ4a_exm.xlsx")
compJ5a = read_excel("compJ5a_exm.xlsx")
compJ6a = read_excel("compJ6a_exm.xlsx")
compJ7a = read_excel("compJ7a_exm.xlsx")
compJ8a = read_excel("compJ8a_exm.xlsx")
compJ9a = read_excel("compJ9a_exm.xlsx")
compJ10a = read_excel("compJ10a_exm.xlsx")
compJ11a = read_excel("compJ11a_exm.xlsx")
compJ12a = read_excel("compJ12a_exm.xlsx")
compJ13a = read_excel("compJ13a_exm.xlsx")

v.compJ_enet <- c(compJ1a,compJ2a,compJ4a,compJ5a,compJ6a,compJ7a,compJ8a,compJ9a,compJ10a,compJ11a,compJ12a,compJ13a)
m.enet.den.j <- matrix_inetJ_den(v.compJ_enet)

gcor(m.enet.den.j, Jz_all)

m.enet.bet.j <- matrix_inetJ_bet(v.compJ_enet)

gcor(m.enet.den.j, m.enet.bet.j)
gcor(m.inet.bet.j, m.enet.bet.j)

m.enet.deg.j <- matrix_inetJ_deg(v.compJ_enet)

gcor(m.inet.den.j, m.inet.deg.j)


##### Network of SOAs#####

setwd("~/Citations/similarity")
comp1a = read_excel("comp1a_soa.xlsx")
comp2a = read_excel("comp2a_soa.xlsx")
comp3a = read_excel("comp3a_soa.xlsx")
comp4a = read_excel("comp4a_soa.xlsx")
comp5a = read_excel("comp5a_soa.xlsx")
comp6a = read_excel("comp6a_soa.xlsx")
comp7a = read_excel("comp7a_soa.xlsx")
comp8a = read_excel("comp8a_soa.xlsx")
comp9a = read_excel("comp9a_soa.xlsx")
comp10a = read_excel("comp10a_soa.xlsx")
comp11a = read_excel("comp11a_soa.xlsx")
comp12a = read_excel("comp12a_soa.xlsx")
comp13a = read_excel("comp13a_soa.xlsx")
comp14a = read_excel("comp14a_soa.xlsx")
comp15a = read_excel("comp15a_soa.xlsx")
comp16a = read_excel("comp16a_soa.xlsx")

v.comp_anet <- c(comp1a,comp2a,comp3a,comp4a,comp5a,comp6a,comp7a,comp8a,comp9a,comp10a,comp11a,comp12a,comp13a,comp14a,comp15a,comp16a)
m.anet.den <- matrix_inet_den(v.comp_anet)

gcor(m.inet.den, m.anet.den)
plot(m.inet.den, m.anet.den)

m.anet.bet <- matrix_inet_bet(v.comp_anet)

gcor(m.inet.bet, m.anet.bet)
plot(m.inet.bet, m.anet.bet)

m.anet.deg <- matrix_inet_deg(v.comp_anet)

gcor(m.inet.deg, m.anet.deg)
plot(m.inet.deg, m.anet.deg)

#Preliminary test of matrices comparison

q.net.a2 <-qaptest(list(DSM_R,EN_Ad2),gcor,g1=1,g2=2, reps = 1000)
summary(q.net.a2)
cug.a2<-cugtest(list(DSM_R,EN_Ad2),gcor, reps=1000)
summary.cugtest(cug.a2)

q.net.ja <-qaptest(list(DSM_J,EN_Ajd2),gcor,g1=1,g2=2, reps = 1000)
summary(q.net.ja)
cug.jb <-cugtest(list(DSM_J,EN_Ajd2),gcor,g1=1,g2=2, reps = 1000)
summary(cug.jb)
