library(readxl)
library(readr)
library(installr)
library(sna)
library(network)
library(dplyr)
library(reshape)
library(BBmisc)


####Importing data####
##Set the working directory of the data for Rowles' components
setwd("~/Components/Rowles")

comp1a <- read_excel("comp1a.XLS")
comp2a <- read_excel("comp2a.XLS")
comp3a <- read_excel("comp3a.XLS")
comp4a <- read_excel("comp4a.XLS")
comp5a <- read_excel("comp5a.XLS")
comp6a <- read_excel("comp6a.XLS")
comp7a <- read_excel("comp7a.XLS")
comp8a <- read_excel("comp8a.XLS")
comp9a <- read_excel("comp9a.XLS")
comp10a <- read_excel("comp10a.XLS")
comp11a <- read_excel("comp11a.XLS")
comp12a <- read_excel("comp12a.XLS")
comp13a <- read_excel("comp13a.XLS")
comp14a <- read_excel("comp14a.XLS")
comp15a <- read_excel("comp15a.XLS")
comp16a <- read_excel("comp16a.XLS")

##Set the working directory of the data for James' components
setwd("~/Dropbox_Jul2021/Posdoc/Data/Patsnap searches/Definitive/Public repository final study - modif/Validation test/Components/James")

compJ1a <- read_excel("compJ1a.XLS")
compJ2a <- read_excel("compJ2a.XLS")
compJ4a <- read_excel("compJ4a.XLS")
compJ5a <- read_excel("compJ5a.XLS")
compJ6a <- read_excel("compJ6a.XLS")
compJ7a <- read_excel("compJ7a.XLS")
compJ8a <- read_excel("compJ8a.XLS")
compJ9a <- read_excel("compJ9a.XLS")
compJ10a <- read_excel("compJ10a.XLS")
compJ11a <- read_excel("compJ11a.XLS")
compJ12a <- read_excel("compJ12a.XLS")
compJ13a <- read_excel("compJ13a.XLS")

##Set the working directory of the data for patent citations of components
setwd("~/Dropbox_Jul2021/Posdoc/Data/Patsnap searches/Definitive/Public repository final study - modif/Validation test/Data/Citations")


v.comp1a <- read_csv("v.comp1a.csv")
v.comp2a <- read_csv("v.comp2a.csv")
v.comp3a <- read_csv("v.comp3a.csv")
v.comp4a <- read_csv("v.comp4a.csv")
v.comp5a <- read_csv("v.comp5a.csv")
v.comp6a <- read_csv("v.comp6a.csv")
v.comp7a <- read_csv("v.comp7a.csv")
v.comp8a <- read_csv("v.comp8a.csv")
v.comp9a <- read_csv("v.comp9a.csv")
v.comp10a <- read_csv("v.comp10a.csv")
v.comp11a <- read_csv("v.comp11a.csv")
v.comp12a <- read_csv("v.comp12a.csv")
v.comp13a <- read_csv("v.comp13a.csv")
v.comp14a <- read_csv("v.comp14a.csv")
v.comp15a <- read_csv("v.comp15a.csv")
v.comp16a <- read_csv("v.comp16a.csv")

v.compJ1a <- read_csv("v.compJ1a.csv")
v.compJ2a <- read_csv("v.compJ2a.csv")
v.compJ4a <- read_csv("v.compJ4a.csv")
v.compJ5a <- read_csv("v.compJ5a.csv")
v.compJ6a <- read_csv("v.compJ6a.csv")
v.compJ7a <- read_csv("v.compJ7a.csv")
v.compJ8a <- read_csv("v.compJ8a.csv")
v.compJ9a <- read_csv("v.compJ9a.csv")
v.compJ10a <- read_csv("v.compJ10a.csv")
v.compJ11a <- read_csv("v.compJ11a.csv")
v.compJ12a <- read_csv("v.compJ12a.csv")
v.compJ13a <- read_csv("v.compJ13a.csv")

####count of citations: Emergent citation network ####
t <- 1999
count.citations <- function(v.comp1a,v.comp2a,t){
  v.comp1a$comp2_99 <- ifelse ((match(v.comp1a$out_cit,v.comp2a$pn))& v.comp1a$ay<=t, TRUE, NA) 
  A12_99<- as.numeric(as.vector(table(v.comp1a$comp2_99)))
  if (is.empty(A12_99)) {A12_99=0}
  
  return (A12_99)
}

####All years: Rowles' categories####

t <- 2015
matrix.count <- function(t){
  A11 <- count.citations(v.comp1a, v.comp1a, t)
  A12 <- count.citations(v.comp1a, v.comp2a, t)
  A13 <- count.citations(v.comp1a, v.comp3a, t)
  A14 <- count.citations(v.comp1a, v.comp4a, t)
  A15 <- count.citations(v.comp1a, v.comp5a, t)
  A16 <- count.citations(v.comp1a, v.comp6a, t)
  A17 <- count.citations(v.comp1a, v.comp7a, t)
  A18 <- count.citations(v.comp1a, v.comp8a, t)
  A19 <- count.citations(v.comp1a, v.comp9a, t)
  A110 <- count.citations(v.comp1a, v.comp10a, t)
  A111 <- count.citations(v.comp1a, v.comp11a, t)
  A112 <- count.citations(v.comp1a, v.comp12a, t)
  A113 <- count.citations(v.comp1a, v.comp13a, t)
  A114 <- count.citations(v.comp1a, v.comp14a, t)
  A115 <- count.citations(v.comp1a, v.comp15a, t)
  A116 <- count.citations(v.comp1a, v.comp16a, t)
  
  ####### Row 2 
  A21 <- count.citations(v.comp2a, v.comp1a, t)
  A22 <- count.citations(v.comp2a, v.comp2a, t)
  A23 <- count.citations(v.comp2a, v.comp3a, t)
  A24 <- count.citations(v.comp2a, v.comp4a, t)
  A25 <- count.citations(v.comp2a, v.comp5a, t)
  A26 <- count.citations(v.comp2a, v.comp6a, t)
  A27 <- count.citations(v.comp2a, v.comp7a, t)
  A28 <- count.citations(v.comp2a, v.comp8a, t)
  A29 <- count.citations(v.comp2a, v.comp9a, t)
  A210 <- count.citations(v.comp2a, v.comp10a, t)
  A211 <- count.citations(v.comp2a, v.comp11a, t)
  A212 <- count.citations(v.comp2a, v.comp12a, t)
  A213 <- count.citations(v.comp2a, v.comp13a, t)
  A214 <- count.citations(v.comp2a, v.comp14a, t)
  A215 <- count.citations(v.comp2a, v.comp15a, t)
  A216 <- count.citations(v.comp2a, v.comp16a, t)
  
  ####### Row 3 
  A31 <- count.citations(v.comp3a, v.comp1a, t)
  A32 <- count.citations(v.comp3a, v.comp2a, t)
  A33 <- count.citations(v.comp3a, v.comp3a, t)
  A34 <- count.citations(v.comp3a, v.comp4a, t)
  A35 <- count.citations(v.comp3a, v.comp5a, t)
  A36 <- count.citations(v.comp3a, v.comp6a, t)
  A37 <- count.citations(v.comp3a, v.comp7a, t)
  A38 <- count.citations(v.comp3a, v.comp8a, t)
  A39 <- count.citations(v.comp3a, v.comp9a, t)
  A310 <- count.citations(v.comp3a, v.comp10a, t)
  A311 <- count.citations(v.comp3a, v.comp11a, t)
  A312 <- count.citations(v.comp3a, v.comp12a, t)
  A313 <- count.citations(v.comp3a, v.comp13a, t)
  A314 <- count.citations(v.comp3a, v.comp14a, t)
  A315 <- count.citations(v.comp3a, v.comp15a, t)
  A316 <- count.citations(v.comp3a, v.comp16a, t)
  
  ####### Row 4 
  A41 <- count.citations(v.comp4a, v.comp1a, t)
  A42 <- count.citations(v.comp4a, v.comp2a, t)
  A43 <- count.citations(v.comp4a, v.comp3a, t)
  A44 <- count.citations(v.comp4a, v.comp4a, t)
  A45 <- count.citations(v.comp4a, v.comp5a, t)
  A46 <- count.citations(v.comp4a, v.comp6a, t)
  A47 <- count.citations(v.comp4a, v.comp7a, t)
  A48 <- count.citations(v.comp4a, v.comp8a, t)
  A49 <- count.citations(v.comp4a, v.comp9a, t)
  A410 <- count.citations(v.comp4a, v.comp10a, t)
  A411 <- count.citations(v.comp4a, v.comp11a, t)
  A412 <- count.citations(v.comp4a, v.comp12a, t)
  A413 <- count.citations(v.comp4a, v.comp13a, t)
  A414 <- count.citations(v.comp4a, v.comp14a, t)
  A415 <- count.citations(v.comp4a, v.comp15a, t)
  A416 <- count.citations(v.comp4a, v.comp16a, t)
  
  ####### Row 5 
  A51 <- count.citations(v.comp5a, v.comp1a, t)
  A52 <- count.citations(v.comp5a, v.comp2a, t)
  A53 <- count.citations(v.comp5a, v.comp3a, t)
  A54 <- count.citations(v.comp5a, v.comp4a, t)
  A55 <- count.citations(v.comp5a, v.comp5a, t)
  A56 <- count.citations(v.comp5a, v.comp6a, t)
  A57 <- count.citations(v.comp5a, v.comp7a, t)
  A58 <- count.citations(v.comp5a, v.comp8a, t)
  A59 <- count.citations(v.comp5a, v.comp9a, t)
  A510 <- count.citations(v.comp5a, v.comp10a, t)
  A511 <- count.citations(v.comp5a, v.comp11a, t)
  A512 <- count.citations(v.comp5a, v.comp12a, t)
  A513 <- count.citations(v.comp5a, v.comp13a, t)
  A514 <- count.citations(v.comp5a, v.comp14a, t)
  A515 <- count.citations(v.comp5a, v.comp15a, t)
  A516 <- count.citations(v.comp5a, v.comp16a, t)
  
  ####### Row 6 
  A61 <- count.citations(v.comp6a, v.comp1a, t)
  A62 <- count.citations(v.comp6a, v.comp2a, t)
  A63 <- count.citations(v.comp6a, v.comp3a, t)
  A64 <- count.citations(v.comp6a, v.comp4a, t)
  A65 <- count.citations(v.comp6a, v.comp5a, t)
  A66 <- count.citations(v.comp6a, v.comp6a, t)
  A67 <- count.citations(v.comp6a, v.comp7a, t)
  A68 <- count.citations(v.comp6a, v.comp8a, t)
  A69 <- count.citations(v.comp6a, v.comp9a, t)
  A610 <- count.citations(v.comp6a, v.comp10a, t)
  A611 <- count.citations(v.comp6a, v.comp11a, t)
  A612 <- count.citations(v.comp6a, v.comp12a, t)
  A613 <- count.citations(v.comp6a, v.comp13a, t)
  A614 <- count.citations(v.comp6a, v.comp14a, t)
  A615 <- count.citations(v.comp6a, v.comp15a, t)
  A616 <- count.citations(v.comp6a, v.comp16a, t)
  
  ####### Row 7 
  A71 <- count.citations(v.comp7a, v.comp1a, t)
  A72 <- count.citations(v.comp7a, v.comp2a, t)
  A73 <- count.citations(v.comp7a, v.comp3a, t)
  A74 <- count.citations(v.comp7a, v.comp4a, t)
  A75 <- count.citations(v.comp7a, v.comp5a, t)
  A76 <- count.citations(v.comp7a, v.comp6a, t)
  A77 <- count.citations(v.comp7a, v.comp7a, t)
  A78 <- count.citations(v.comp7a, v.comp8a, t)
  A79 <- count.citations(v.comp7a, v.comp9a, t)
  A710 <- count.citations(v.comp7a, v.comp10a, t)
  A711 <- count.citations(v.comp7a, v.comp11a, t)
  A712 <- count.citations(v.comp7a, v.comp12a, t)
  A713 <- count.citations(v.comp7a, v.comp13a, t)
  A714 <- count.citations(v.comp7a, v.comp14a, t)
  A715 <- count.citations(v.comp7a, v.comp15a, t)
  A716 <- count.citations(v.comp7a, v.comp16a, t)
  
  ####### Row 8 
  A81 <- count.citations(v.comp8a, v.comp1a, t)
  A82 <- count.citations(v.comp8a, v.comp2a, t)
  A83 <- count.citations(v.comp8a, v.comp3a, t)
  A84 <- count.citations(v.comp8a, v.comp4a, t)
  A85 <- count.citations(v.comp8a, v.comp5a, t)
  A86 <- count.citations(v.comp8a, v.comp6a, t)
  A87 <- count.citations(v.comp8a, v.comp7a, t)
  A88 <- count.citations(v.comp8a, v.comp8a, t)
  A89 <- count.citations(v.comp8a, v.comp9a, t)
  A810 <- count.citations(v.comp8a, v.comp10a, t)
  A811 <- count.citations(v.comp8a, v.comp11a, t)
  A812 <- count.citations(v.comp8a, v.comp12a, t)
  A813 <- count.citations(v.comp8a, v.comp13a, t)
  A814 <- count.citations(v.comp8a, v.comp14a, t)
  A815 <- count.citations(v.comp8a, v.comp15a, t)
  A816 <- count.citations(v.comp8a, v.comp16a, t)
  
  ####### Row 9 
  A91 <- count.citations(v.comp9a, v.comp1a, t)
  A92 <- count.citations(v.comp9a, v.comp2a, t)
  A93 <- count.citations(v.comp9a, v.comp3a, t)
  A94 <- count.citations(v.comp9a, v.comp4a, t)
  A95 <- count.citations(v.comp9a, v.comp5a, t)
  A96 <- count.citations(v.comp9a, v.comp6a, t)
  A97 <- count.citations(v.comp9a, v.comp7a, t)
  A98 <- count.citations(v.comp9a, v.comp8a, t)
  A99 <- count.citations(v.comp9a, v.comp9a, t)
  A910 <- count.citations(v.comp9a, v.comp10a, t)
  A911 <- count.citations(v.comp9a, v.comp11a, t)
  A912 <- count.citations(v.comp9a, v.comp12a, t)
  A913 <- count.citations(v.comp9a, v.comp13a, t)
  A914 <- count.citations(v.comp9a, v.comp14a, t)
  A915 <- count.citations(v.comp9a, v.comp15a, t)
  A916 <- count.citations(v.comp9a, v.comp16a, t)
  
  ####### Row 10 
  A101 <- count.citations(v.comp10a, v.comp1a, t)
  A102 <- count.citations(v.comp10a, v.comp2a, t)
  A103 <- count.citations(v.comp10a, v.comp3a, t)
  A104 <- count.citations(v.comp10a, v.comp4a, t)
  A105 <- count.citations(v.comp10a, v.comp5a, t)
  A106 <- count.citations(v.comp10a, v.comp6a, t)
  A107 <- count.citations(v.comp10a, v.comp7a, t)
  A108 <- count.citations(v.comp10a, v.comp8a, t)
  A109 <- count.citations(v.comp10a, v.comp9a, t)
  A1010 <- count.citations(v.comp10a, v.comp10a, t)
  A1011 <- count.citations(v.comp10a, v.comp11a, t)
  A1012 <- count.citations(v.comp10a, v.comp12a, t)
  A1013 <- count.citations(v.comp10a, v.comp13a, t)
  A1014 <- count.citations(v.comp10a, v.comp14a, t)
  A1015 <- count.citations(v.comp10a, v.comp15a, t)
  A1016 <- count.citations(v.comp10a, v.comp16a, t)
  
  ####### Row 11 
  A111 <- count.citations(v.comp11a, v.comp1a, t)
  A112 <- count.citations(v.comp11a, v.comp2a, t)
  A113 <- count.citations(v.comp11a, v.comp3a, t)
  A114 <- count.citations(v.comp11a, v.comp4a, t)
  A115 <- count.citations(v.comp11a, v.comp5a, t)
  A116 <- count.citations(v.comp11a, v.comp6a, t)
  A117 <- count.citations(v.comp11a, v.comp7a, t)
  A118 <- count.citations(v.comp11a, v.comp8a, t)
  A119 <- count.citations(v.comp11a, v.comp9a, t)
  A1110 <- count.citations(v.comp11a, v.comp10a, t)
  A1111 <- count.citations(v.comp11a, v.comp11a, t)
  A1112 <- count.citations(v.comp11a, v.comp12a, t)
  A1113 <- count.citations(v.comp11a, v.comp13a, t)
  A1114 <- count.citations(v.comp11a, v.comp14a, t)
  A1115 <- count.citations(v.comp11a, v.comp15a, t)
  A1116 <- count.citations(v.comp11a, v.comp16a, t)
  
  ####### Row 12 
  A121 <- count.citations(v.comp12a, v.comp1a, t)
  A122 <- count.citations(v.comp12a, v.comp2a, t)
  A123 <- count.citations(v.comp12a, v.comp3a, t)
  A124 <- count.citations(v.comp12a, v.comp4a, t)
  A125 <- count.citations(v.comp12a, v.comp5a, t)
  A126 <- count.citations(v.comp12a, v.comp6a, t)
  A127 <- count.citations(v.comp12a, v.comp7a, t)
  A128 <- count.citations(v.comp12a, v.comp8a, t)
  A129 <- count.citations(v.comp12a, v.comp9a, t)
  A1210 <- count.citations(v.comp12a, v.comp10a, t)
  A1211 <- count.citations(v.comp12a, v.comp11a, t)
  A1212 <- count.citations(v.comp12a, v.comp12a, t)
  A1213 <- count.citations(v.comp12a, v.comp13a, t)
  A1214 <- count.citations(v.comp12a, v.comp14a, t)
  A1215 <- count.citations(v.comp12a, v.comp15a, t)
  A1216 <- count.citations(v.comp12a, v.comp16a, t)
  
  ####### Row 13 
  A131 <- count.citations(v.comp13a, v.comp1a, t)
  A132 <- count.citations(v.comp13a, v.comp2a, t)
  A133 <- count.citations(v.comp13a, v.comp3a, t)
  A134 <- count.citations(v.comp13a, v.comp4a, t)
  A135 <- count.citations(v.comp13a, v.comp5a, t)
  A136 <- count.citations(v.comp13a, v.comp6a, t)
  A137 <- count.citations(v.comp13a, v.comp7a, t)
  A138 <- count.citations(v.comp13a, v.comp8a, t)
  A139 <- count.citations(v.comp13a, v.comp9a, t)
  A1310 <- count.citations(v.comp13a, v.comp10a, t)
  A1311 <- count.citations(v.comp13a, v.comp11a, t)
  A1312 <- count.citations(v.comp13a, v.comp12a, t)
  A1313 <- count.citations(v.comp13a, v.comp13a, t)
  A1314 <- count.citations(v.comp13a, v.comp14a, t)
  A1315 <- count.citations(v.comp13a, v.comp15a, t)
  A1316 <- count.citations(v.comp13a, v.comp16a, t)
  
  ####### Row 14 
  A141 <- count.citations(v.comp14a, v.comp1a, t)
  A142 <- count.citations(v.comp14a, v.comp2a, t)
  A143 <- count.citations(v.comp14a, v.comp3a, t)
  A144 <- count.citations(v.comp14a, v.comp4a, t)
  A145 <- count.citations(v.comp14a, v.comp5a, t)
  A146 <- count.citations(v.comp14a, v.comp6a, t)
  A147 <- count.citations(v.comp14a, v.comp7a, t)
  A148 <- count.citations(v.comp14a, v.comp8a, t)
  A149 <- count.citations(v.comp14a, v.comp9a, t)
  A1410 <- count.citations(v.comp14a, v.comp10a, t)
  A1411 <- count.citations(v.comp14a, v.comp11a, t)
  A1412 <- count.citations(v.comp14a, v.comp12a, t)
  A1413 <- count.citations(v.comp14a, v.comp13a, t)
  A1414 <- count.citations(v.comp14a, v.comp14a, t)
  A1415 <- count.citations(v.comp14a, v.comp15a, t)
  A1416 <- count.citations(v.comp14a, v.comp16a, t)
  
  ####### Row 15 
  A151 <- count.citations(v.comp15a, v.comp1a, t)
  A152 <- count.citations(v.comp15a, v.comp2a, t)
  A153 <- count.citations(v.comp15a, v.comp3a, t)
  A154 <- count.citations(v.comp15a, v.comp4a, t)
  A155 <- count.citations(v.comp15a, v.comp5a, t)
  A156 <- count.citations(v.comp15a, v.comp6a, t)
  A157 <- count.citations(v.comp15a, v.comp7a, t)
  A158 <- count.citations(v.comp15a, v.comp8a, t)
  A159 <- count.citations(v.comp15a, v.comp9a, t)
  A1510 <- count.citations(v.comp15a, v.comp10a, t)
  A1511 <- count.citations(v.comp15a, v.comp11a, t)
  A1512 <- count.citations(v.comp15a, v.comp12a, t)
  A1513 <- count.citations(v.comp15a, v.comp13a, t)
  A1514 <- count.citations(v.comp15a, v.comp14a, t)
  A1515 <- count.citations(v.comp15a, v.comp15a, t)
  A1516 <- count.citations(v.comp15a, v.comp16a, t)
  
  ####### Row 16 
  A161 <- count.citations(v.comp16a, v.comp1a, t)
  A162 <- count.citations(v.comp16a, v.comp2a, t)
  A163 <- count.citations(v.comp16a, v.comp3a, t)
  A164 <- count.citations(v.comp16a, v.comp4a, t)
  A165 <- count.citations(v.comp16a, v.comp5a, t)
  A166 <- count.citations(v.comp16a, v.comp6a, t)
  A167 <- count.citations(v.comp16a, v.comp7a, t)
  A168 <- count.citations(v.comp16a, v.comp8a, t)
  A169 <- count.citations(v.comp16a, v.comp9a, t)
  A1610 <- count.citations(v.comp16a, v.comp10a, t)
  A1611 <- count.citations(v.comp16a, v.comp11a, t)
  A1612 <- count.citations(v.comp16a, v.comp12a, t)
  A1613 <- count.citations(v.comp16a, v.comp13a, t)
  A1614 <- count.citations(v.comp16a, v.comp14a, t)
  A1615 <- count.citations(v.comp16a, v.comp15a, t)
  A1616 <- count.citations(v.comp16a, v.comp16a, t)
  
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

EN_A <- matrix.count(2015)
EN_B <- matrix.count(2011)
EN_C <- matrix.count(1999)
EN_D <- matrix.count(1990)
EN_E <- matrix.count(1980)

##Set the working directory of the data for Rowles' citation z-score 
setwd("~/Data/Citations")

Rz_all = as.matrix(read.csv("Rz_all.csv", sep=";", dec="," ,stringsAsFactors= FALSE, header=FALSE , check.names = TRUE))

##Set the working directory of the data for James' components
setwd("~/Components/James")

####All years: James' categories####

t <- 2015
matrix.count.j <- function(t){
  jA11 <- count.citations(v.compJ1a, v.compJ1a, t)
  jA12 <- count.citations(v.compJ1a, v.compJ2a, t)
  jA13 <- count.citations(v.compJ1a, v.compJ4a, t)
  jA14 <- count.citations(v.compJ1a, v.compJ5a, t)
  jA15 <- count.citations(v.compJ1a, v.compJ6a, t)
  jA16 <- count.citations(v.compJ1a, v.compJ7a, t)
  jA17 <- count.citations(v.compJ1a, v.compJ8a, t)
  jA18 <- count.citations(v.compJ1a, v.compJ9a, t)
  jA19 <- count.citations(v.compJ1a, v.compJ10a, t)
  jA110 <- count.citations(v.compJ1a, v.compJ11a, t)
  jA111 <- count.citations(v.compJ1a, v.compJ12a, t)
  jA112 <- count.citations(v.compJ1a, v.compJ13a, t)
  
  
  ####### Row 2 
  jA21 <- count.citations(v.compJ2a, v.compJ1a, t)
  jA22 <- count.citations(v.compJ2a, v.compJ2a, t)
  jA23 <- count.citations(v.compJ2a, v.compJ4a, t)
  jA24 <- count.citations(v.compJ2a, v.compJ5a, t)
  jA25 <- count.citations(v.compJ2a, v.compJ6a, t)
  jA26 <- count.citations(v.compJ2a, v.compJ7a, t)
  jA27 <- count.citations(v.compJ2a, v.compJ8a, t)
  jA28 <- count.citations(v.compJ2a, v.compJ9a, t)
  jA29 <- count.citations(v.compJ2a, v.compJ10a, t)
  jA210 <- count.citations(v.compJ2a, v.compJ11a, t)
  jA211 <- count.citations(v.compJ2a, v.compJ12a, t)
  jA212 <- count.citations(v.compJ2a, v.compJ13a, t)
  
  ####### Row 3 
  jA31 <- count.citations(v.compJ4a, v.compJ1a, t)
  jA32 <- count.citations(v.compJ4a, v.compJ2a, t)
  jA33 <- count.citations(v.compJ4a, v.compJ4a, t)
  jA34 <- count.citations(v.compJ4a, v.compJ5a, t)
  jA35 <- count.citations(v.compJ4a, v.compJ6a, t)
  jA36 <- count.citations(v.compJ4a, v.compJ7a, t)
  jA37 <- count.citations(v.compJ4a, v.compJ8a, t)
  jA38 <- count.citations(v.compJ4a, v.compJ9a, t)
  jA39 <- count.citations(v.compJ4a, v.compJ10a, t)
  jA310 <- count.citations(v.compJ4a, v.compJ11a, t)
  jA311 <- count.citations(v.compJ4a, v.compJ12a, t)
  jA312 <- count.citations(v.compJ4a, v.compJ13a, t)
  
  ####### Row 4 
  jA41 <- count.citations(v.compJ5a, v.compJ1a, t)
  jA42 <- count.citations(v.compJ5a, v.compJ2a, t)
  jA43 <- count.citations(v.compJ5a, v.compJ4a, t)
  jA44 <- count.citations(v.compJ5a, v.compJ5a, t)
  jA45 <- count.citations(v.compJ5a, v.compJ6a, t)
  jA46 <- count.citations(v.compJ5a, v.compJ7a, t)
  jA47 <- count.citations(v.compJ5a, v.compJ8a, t)
  jA48 <- count.citations(v.compJ5a, v.compJ9a, t)
  jA49 <- count.citations(v.compJ5a, v.compJ10a, t)
  jA410 <- count.citations(v.compJ5a, v.compJ11a, t)
  jA411 <- count.citations(v.compJ5a, v.compJ12a, t)
  jA412 <- count.citations(v.compJ5a, v.compJ13a, t)
  
  ####### Row 5 
  jA51 <- count.citations(v.compJ6a, v.compJ1a, t)
  jA52 <- count.citations(v.compJ6a, v.compJ2a, t)
  jA53 <- count.citations(v.compJ6a, v.compJ4a, t)
  jA54 <- count.citations(v.compJ6a, v.compJ5a, t)
  jA55 <- count.citations(v.compJ6a, v.compJ6a, t)
  jA56 <- count.citations(v.compJ6a, v.compJ7a, t)
  jA57 <- count.citations(v.compJ6a, v.compJ8a, t)
  jA58 <- count.citations(v.compJ6a, v.compJ9a, t)
  jA59 <- count.citations(v.compJ6a, v.compJ10a, t)
  jA510 <- count.citations(v.compJ6a, v.compJ11a, t)
  jA511 <- count.citations(v.compJ6a, v.compJ12a, t)
  jA512 <- count.citations(v.compJ6a, v.compJ13a, t)
  
  ####### Row 6 
  jA61 <- count.citations(v.compJ7a, v.compJ1a, t)
  jA62 <- count.citations(v.compJ7a, v.compJ2a, t)
  jA63 <- count.citations(v.compJ7a, v.compJ4a, t)
  jA64 <- count.citations(v.compJ7a, v.compJ5a, t)
  jA65 <- count.citations(v.compJ7a, v.compJ6a, t)
  jA66 <- count.citations(v.compJ7a, v.compJ7a, t)
  jA67 <- count.citations(v.compJ7a, v.compJ8a, t)
  jA68 <- count.citations(v.compJ7a, v.compJ9a, t)
  jA69 <- count.citations(v.compJ7a, v.compJ10a, t)
  jA610 <- count.citations(v.compJ7a, v.compJ11a, t)
  jA611 <- count.citations(v.compJ7a, v.compJ12a, t)
  jA612 <- count.citations(v.compJ7a, v.compJ13a, t)
  
  ####### Row 7 
  jA71 <- count.citations(v.compJ8a, v.compJ1a, t)
  jA72 <- count.citations(v.compJ8a, v.compJ2a, t)
  jA73 <- count.citations(v.compJ8a, v.compJ4a, t)
  jA74 <- count.citations(v.compJ8a, v.compJ5a, t)
  jA75 <- count.citations(v.compJ8a, v.compJ6a, t)
  jA76 <- count.citations(v.compJ8a, v.compJ7a, t)
  jA77 <- count.citations(v.compJ8a, v.compJ8a, t)
  jA78 <- count.citations(v.compJ8a, v.compJ9a, t)
  jA79 <- count.citations(v.compJ8a, v.compJ10a, t)
  jA710 <- count.citations(v.compJ8a, v.compJ11a, t)
  jA711 <- count.citations(v.compJ8a, v.compJ12a, t)
  jA712 <- count.citations(v.compJ8a, v.compJ13a, t)
  
  ####### Row 8 
  jA81 <- count.citations(v.compJ9a, v.compJ1a, t)
  jA82 <- count.citations(v.compJ9a, v.compJ2a, t)
  jA83 <- count.citations(v.compJ9a, v.compJ4a, t)
  jA84 <- count.citations(v.compJ9a, v.compJ5a, t)
  jA85 <- count.citations(v.compJ9a, v.compJ6a, t)
  jA86 <- count.citations(v.compJ9a, v.compJ7a, t)
  jA87 <- count.citations(v.compJ9a, v.compJ8a, t)
  jA88 <- count.citations(v.compJ9a, v.compJ9a, t)
  jA89 <- count.citations(v.compJ9a, v.compJ10a, t)
  jA810 <- count.citations(v.compJ9a, v.compJ11a, t)
  jA811 <- count.citations(v.compJ9a, v.compJ12a, t)
  jA812 <- count.citations(v.compJ9a, v.compJ13a, t)
  
  ####### Row 9 
  jA91 <- count.citations(v.compJ10a, v.compJ1a, t)
  jA92 <- count.citations(v.compJ10a, v.compJ2a, t)
  jA93 <- count.citations(v.compJ10a, v.compJ4a, t)
  jA94 <- count.citations(v.compJ10a, v.compJ5a, t)
  jA95 <- count.citations(v.compJ10a, v.compJ6a, t)
  jA96 <- count.citations(v.compJ10a, v.compJ7a, t)
  jA97 <- count.citations(v.compJ10a, v.compJ8a, t)
  jA98 <- count.citations(v.compJ10a, v.compJ9a, t)
  jA99 <- count.citations(v.compJ10a, v.compJ10a, t)
  jA910 <- count.citations(v.compJ10a, v.compJ11a, t)
  jA911 <- count.citations(v.compJ10a, v.compJ12a, t)
  jA912 <- count.citations(v.compJ10a, v.compJ13a, t)
  
  ####### Row 10 
  jA101 <- count.citations(v.compJ11a, v.compJ1a, t)
  jA102 <- count.citations(v.compJ11a, v.compJ2a, t)
  jA103 <- count.citations(v.compJ11a, v.compJ4a, t)
  jA104 <- count.citations(v.compJ11a, v.compJ5a, t)
  jA105 <- count.citations(v.compJ11a, v.compJ6a, t)
  jA106 <- count.citations(v.compJ11a, v.compJ7a, t)
  jA107 <- count.citations(v.compJ11a, v.compJ8a, t)
  jA108 <- count.citations(v.compJ11a, v.compJ9a, t)
  jA109 <- count.citations(v.compJ11a, v.compJ10a, t)
  jA1010 <- count.citations(v.compJ11a, v.compJ11a, t)
  jA1011 <- count.citations(v.compJ11a, v.compJ12a, t)
  jA1012 <- count.citations(v.compJ11a, v.compJ13a, t)
  
  ####### Row 11 
  jA111 <- count.citations(v.compJ12a, v.compJ1a, t)
  jA112 <- count.citations(v.compJ12a, v.compJ2a, t)
  jA113 <- count.citations(v.compJ12a, v.compJ4a, t)
  jA114 <- count.citations(v.compJ12a, v.compJ5a, t)
  jA115 <- count.citations(v.compJ12a, v.compJ6a, t)
  jA116 <- count.citations(v.compJ12a, v.compJ7a, t)
  jA117 <- count.citations(v.compJ12a, v.compJ8a, t)
  jA118 <- count.citations(v.compJ12a, v.compJ9a, t)
  jA119 <- count.citations(v.compJ12a, v.compJ10a, t)
  jA1110 <- count.citations(v.compJ12a, v.compJ11a, t)
  jA1111 <- count.citations(v.compJ12a, v.compJ12a, t)
  jA1112 <- count.citations(v.compJ12a, v.compJ13a, t)
  
  ####### Row 12 
  jA121 <- count.citations(v.compJ13a, v.compJ1a, t)
  jA122 <- count.citations(v.compJ13a, v.compJ2a, t)
  jA123 <- count.citations(v.compJ13a, v.compJ4a, t)
  jA124 <- count.citations(v.compJ13a, v.compJ5a, t)
  jA125 <- count.citations(v.compJ13a, v.compJ6a, t)
  jA126 <- count.citations(v.compJ13a, v.compJ7a, t)
  jA127 <- count.citations(v.compJ13a, v.compJ8a, t)
  jA128 <- count.citations(v.compJ13a, v.compJ9a, t)
  jA129 <- count.citations(v.compJ13a, v.compJ10a, t)
  jA1210 <- count.citations(v.compJ13a, v.compJ11a, t)
  jA1211 <- count.citations(v.compJ13a, v.compJ12a, t)
  jA1212 <- count.citations(v.compJ13a, v.compJ13a, t)
  
  EN_jA <- t(matrix(c(jA11  ,jA12  ,jA13  ,jA14  ,jA15  ,jA16  ,jA17  ,jA18  ,jA19  ,jA110 ,jA111 ,jA112 ,
                      jA21  ,jA22  ,jA23  ,jA24  ,jA25  ,jA26  ,jA27  ,jA28  ,jA29  ,jA210 ,jA211 ,jA212 ,
                      jA31  ,jA32  ,jA33  ,jA34  ,jA35  ,jA36  ,jA37  ,jA38  ,jA39  ,jA310 ,jA311 ,jA312 ,
                      jA41  ,jA42  ,jA43  ,jA44  ,jA45  ,jA46  ,jA47  ,jA48  ,jA49  ,jA410 ,jA411 ,jA412 ,
                      jA51  ,jA52  ,jA53  ,jA54  ,jA55  ,jA56  ,jA57  ,jA58  ,jA59  ,jA510 ,jA511 ,jA512 ,
                      jA61  ,jA62  ,jA63  ,jA64  ,jA65  ,jA66  ,jA67  ,jA68  ,jA69  ,jA610 ,jA611 ,jA612 ,
                      jA71  ,jA72  ,jA73  ,jA74  ,jA75  ,jA76  ,jA77  ,jA78  ,jA79  ,jA710 ,jA711 ,jA712 ,
                      jA81  ,jA82  ,jA83  ,jA84  ,jA85  ,jA86  ,jA87  ,jA88  ,jA89  ,jA810 ,jA811 ,jA812 ,
                      jA91  ,jA92  ,jA93  ,jA94  ,jA95  ,jA96  ,jA97  ,jA98  ,jA99  ,jA910 ,jA911 ,jA912 ,
                      jA101 ,jA102 ,jA103 ,jA104 ,jA105 ,jA106 ,jA107 ,jA108 ,jA109 ,jA1010,jA1011,jA1012,
                      jA111 ,jA112 ,jA113 ,jA114 ,jA115 ,jA116 ,jA117 ,jA118 ,jA119 ,jA1110,jA1111,jA1112,
                      jA121 ,jA122 ,jA123 ,jA124 ,jA125 ,jA126 ,jA127 ,jA128 ,jA129 ,jA1210,jA1211,jA1212), nrow=12, ncol=12))
  
  return(EN_jA)
}

EN_Aj <- matrix.count.j(2015)
EN_Bj <- matrix.count.j(2011)
EN_Cj <- matrix.count.j(1999)
EN_Dj <- matrix.count.j(1990)
EN_Ej <- matrix.count.j(1980)

##Set the working directory of the data for James' citation z-score 
setwd("~/Data/Citations")

Jz_all = as.matrix(read.csv("Jz_all.csv", sep=";", dec="," ,stringsAsFactors= FALSE, header=FALSE , check.names = TRUE))

#####Citations per patent (Number of patents in the row) Rowles and James####

t <- 2015
cit.patR.row <- function(EN_A,t){
  ex1 <- EN_A[1,]/nrow(filter(comp1a, comp1a$ay<t))
  ex2 <- EN_A[2,]/nrow(filter(comp2a, comp2a$ay<t))
  ex3 <- EN_A[3,]/nrow(filter(comp3a, comp3a$ay<t))
  ex4 <- EN_A[4,]/nrow(filter(comp4a, comp4a$ay<t))
  ex5 <- EN_A[5,]/nrow(filter(comp5a, comp5a$ay<t))
  ex6 <- EN_A[6,]/nrow(filter(comp6a, comp6a$ay<t))
  ex7 <- EN_A[7,]/nrow(filter(comp7a, comp7a$ay<t))
  ex8 <- EN_A[8,]/nrow(filter(comp8a, comp8a$ay<t))
  ex9 <- EN_A[9,]/nrow(filter(comp6a, comp6a$ay<t))
  ex10 <- EN_A[10,]/nrow(filter(comp10a, comp10a$ay<t))
  ex11 <- EN_A[11,]/nrow(filter(comp11a, comp11a$ay<t))
  ex12 <- EN_A[12,]/nrow(filter(comp12a, comp12a$ay<t))
  ex13 <- EN_A[13,]/nrow(filter(comp13a, comp13a$ay<t))
  ex14 <- EN_A[14,]/nrow(filter(comp14a, comp14a$ay<t))
  ex15 <- EN_A[15,]/nrow(filter(comp15a, comp15a$ay<t))
  ex16 <- EN_A[16,]/nrow(filter(comp16a, comp16a$ay<t))
  
  ex <- rbind(ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12,ex13,ex14,ex15,ex16)
  
  return (ex)
}

EN_A_pt<- cit.patR.row(EN_A,2015)
EN_B_pt<- cit.patR.row(EN_B,2011)
EN_C_pt<- cit.patR.row(EN_C,1999)
EN_D_pt<- cit.patR.row(EN_D,1990)
EN_E_pt<- cit.patR.row(EN_E,1980)

#Dichotomized
EN_Ad_pt <- event2dichot(EN_A_pt,"absolute", thresh = 0.99)
EN_Bd_pt <- event2dichot(EN_B_pt,"absolute", thresh = 0.99)
EN_Cd_pt <- event2dichot(EN_C_pt,"absolute", thresh = 0.99)
EN_Dd_pt <- event2dichot(EN_D_pt,"absolute", thresh = 0.99)

#Dichotomization Mean (1), rmean(2), cmean(3)
EN_Ad1_pt <- event2dichot(EN_A_pt,"mean")
EN_Bd1_pt <- event2dichot(EN_B_pt,"mean")
EN_Cd1_pt <- event2dichot(EN_C_pt,"mean")
EN_Dd1_pt <- event2dichot(EN_D_pt,"mean")

EN_Ad2_pt <- event2dichot(EN_A_pt,"rmean")
EN_Bd2_pt <- event2dichot(EN_B_pt,"rmean")
EN_Cd2_pt <- event2dichot(EN_C_pt,"rmean")
EN_Dd2_pt <- event2dichot(EN_D_pt,"rmean")

EN_Ad3_pt <- event2dichot(EN_A_pt,"cmean")
EN_Bd3_pt <- event2dichot(EN_B_pt,"cmean")
EN_Cd3_pt <- event2dichot(EN_C_pt,"cmean")
EN_Dd3_pt <- event2dichot(EN_D_pt,"cmean")


cit.patJ.row <- function(EN_jA,t){
  ex1 <- EN_jA[1,]/nrow(filter(compJ1a, compJ1a$ay<t))
  ex2 <- EN_jA[2,]/nrow(filter(compJ2a, compJ2a$ay<t))
  ex3 <- EN_jA[3,]/nrow(filter(compJ4a, compJ4a$ay<t))
  ex4 <- EN_jA[4,]/nrow(filter(compJ5a, compJ5a$ay<t))
  ex5 <- EN_jA[5,]/nrow(filter(compJ6a, compJ6a$ay<t))
  ex6 <- EN_jA[6,]/nrow(filter(compJ7a, compJ7a$ay<t))
  ex7 <- EN_jA[7,]/nrow(filter(compJ8a, compJ8a$ay<t))
  ex8 <- EN_jA[8,]/nrow(filter(compJ9a, compJ9a$ay<t))
  ex9 <- EN_jA[9,]/nrow(filter(compJ10a, compJ10a$ay<t))
  ex10 <- EN_jA[10,]/nrow(filter(comp12a, comp12a$ay<t))
  ex11 <- EN_jA[11,]/nrow(filter(compJ12a, compJ12a$ay<t))
  ex12 <- EN_jA[12,]/nrow(filter(compJ13a, compJ13a$ay<t))
  ex <- rbind(ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12)
  
  return (ex)
}

EN_Aj_pt<- cit.patJ.row(EN_Aj,2015)
EN_Bj_pt<- cit.patJ.row(EN_Bj,2011)
EN_Cj_pt<- cit.patJ.row(EN_Cj,1999)
EN_Dj_pt<- cit.patJ.row(EN_Dj,1990)
EN_Ej_pt<- cit.patJ.row(EN_Ej,1980)

#Dichotomized
EN_Ajd_pt <- event2dichot(EN_Aj_pt,"absolute", thresh = 0.99)
EN_Bjd_pt <- event2dichot(EN_Bj_pt,"absolute", thresh = 0.99)
EN_Cjd_pt <- event2dichot(EN_Cj_pt,"absolute", thresh = 0.99)
EN_Djd_pt <- event2dichot(EN_Dj_pt,"absolute", thresh = 0.99)

#Dichotomization Mean (1), rmean(2) sender mean, cmean(3) receiver mean
EN_Ajd1_pt <- event2dichot(EN_Aj_pt,"mean")
EN_Bjd1_pt <- event2dichot(EN_Bj_pt,"mean")
EN_Cjd1_pt <- event2dichot(EN_Cj_pt,"mean")
EN_Djd1_pt <- event2dichot(EN_Dj_pt,"mean")

EN_Ajd2_pt <- event2dichot(EN_Aj_pt,"rmean")
EN_Bjd2_pt <- event2dichot(EN_Bj_pt,"rmean")
EN_Cjd2_pt <- event2dichot(EN_Cj_pt,"rmean")
EN_Djd2_pt <- event2dichot(EN_Dj_pt,"rmean")

EN_Ajd3_pt <- event2dichot(EN_Aj_pt,"cmean")
EN_Bjd3_pt <- event2dichot(EN_Bj_pt,"cmean")
EN_Cjd3_pt <- event2dichot(EN_Cj_pt,"cmean")
EN_Djd3_pt <- event2dichot(EN_Dj_pt,"cmean")


#local normalization and standardization
#local.norm
dr_EN_A <- diag.remove(EN_A, remove.val = NA)
dr_EN_B <- diag.remove(EN_B, remove.val = NA)
dr_EN_C <- diag.remove(EN_C, remove.val = NA)
dr_EN_D <- diag.remove(EN_D, remove.val = NA)
dr_EN_E <- diag.remove(EN_E, remove.val = NA)

dr_EN_Aj <- diag.remove(EN_Aj, remove.val = NA)
dr_EN_Bj <- diag.remove(EN_Bj, remove.val = NA)
dr_EN_Cj <- diag.remove(EN_Cj, remove.val = NA)
dr_EN_Dj <- diag.remove(EN_Dj, remove.val = NA)
dr_EN_Ej <- diag.remove(EN_Ej, remove.val = NA)

ln_EN_A <- normalize(dr_EN_A, method="standardize")
ln_EN_B <- normalize(dr_EN_B, method="standardize")
ln_EN_C <- normalize(dr_EN_C, method="standardize")
ln_EN_D <- normalize(dr_EN_D, method="standardize")
ln_EN_E <- normalize(dr_EN_E, method="standardize")

ln_EN_Aj <- normalize(dr_EN_Aj, method="standardize")
ln_EN_Bj <- normalize(dr_EN_Bj, method="standardize")
ln_EN_Cj <- normalize(dr_EN_Cj, method="standardize")
ln_EN_Dj <- normalize(dr_EN_Dj, method="standardize")
ln_EN_Ej <- normalize(dr_EN_Ej, method="standardize")

#### DSM of Rowles' 99 and Dichotomized variables ####

DSM_R <-    t(matrix(c(1,1,1,0,1,0,1,0,0,1,0,0,0,0,0,0,
                       1,1,1,1,1,0,1,0,0,1,0,0,0,0,0,0,
                       1,1,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
                       0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
                       1,1,0,0,1,0,1,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,1,1,1,1,1,0,0,0,1,0,0,
                       1,1,1,0,0,1,1,1,1,1,0,0,0,0,0,0,
                       0,0,0,0,0,1,1,1,1,1,0,1,0,1,0,0,
                       0,0,1,0,0,1,1,1,1,1,1,0,0,0,0,1,
                       0,1,0,0,0,1,1,1,1,1,0,1,0,0,0,1,
                       0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,0,
                       0,0,0,0,0,0,0,1,0,1,0,1,1,1,1,1,
                       0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,
                       0,0,0,0,0,1,0,0,0,1,0,1,1,1,1,1,
                       0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
                       0,0,0,0,0,1,0,0,1,1,0,1,1,1,1,1), nrow=16, ncol=16))

#Modular dummy
mod_R <-    t(matrix(c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
                       1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
                       1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
                       1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
                       1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,
                       0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,
                       0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,
                       0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,
                       0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
                       0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
                       0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
                       0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
                       0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
                       0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1), nrow=16, ncol=16))

diag_R <-   t(matrix(c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1), nrow=16, ncol=16))

#Dichotomized at 1
EN_Ad <- event2dichot(EN_A,"absolute", thresh = 0.9)
EN_Bd <- event2dichot(EN_B,"absolute", thresh = 0.9)
EN_Cd <- event2dichot(EN_C,"absolute", thresh = 0.9)
EN_Dd <- event2dichot(EN_D,"absolute", thresh = 0.9)
EN_Ed <- event2dichot(EN_E,"absolute", thresh = 0.9)

#Dichotomization Mean (1), rmean(2), cmean(3)
EN_Ad1 <- event2dichot(EN_A,"mean")
EN_Bd1 <- event2dichot(EN_B,"mean")
EN_Cd1 <- event2dichot(EN_C,"mean")
EN_Dd1 <- event2dichot(EN_D,"mean")
EN_Ed1 <- event2dichot(EN_E,"mean")

EN_Ad2 <- event2dichot(EN_A,"rmean")
EN_Bd2 <- event2dichot(EN_B,"rmean")
EN_Cd2 <- event2dichot(EN_C,"rmean")
EN_Dd2 <- event2dichot(EN_D,"rmean")
EN_Ed2 <- event2dichot(EN_E,"rmean")

EN_Ad3 <- event2dichot(EN_A,"cmean")
EN_Bd3 <- event2dichot(EN_B,"cmean")
EN_Cd3 <- event2dichot(EN_C,"cmean")
EN_Dd3 <- event2dichot(EN_D,"cmean")
EN_Ed3 <- event2dichot(EN_D,"cmean")

#### DSM of James' 2011 and Dichotomized variables ####

DSM_J <-    t(matrix(c(1,1,0,1,0,0,0,0,0,0,0,0,
                       0,1,1,0,0,0,0,0,0,0,0,0,
                       0,1,1,1,1,0,0,0,0,0,0,0,
                       1,0,1,1,0,0,0,0,0,0,0,0,
                       0,0,1,0,1,1,0,1,0,1,0,0,
                       1,0,0,0,1,1,0,0,0,0,0,0,
                       0,0,0,0,0,0,1,0,0,1,0,0,
                       0,0,0,0,1,0,0,1,0,1,0,0,
                       0,0,0,0,0,0,0,0,1,1,0,0,
                       0,0,0,0,1,0,1,1,1,1,1,1,
                       0,0,0,0,0,0,0,0,0,1,1,0,
                       0,0,0,0,0,0,0,0,0,1,1,1), nrow=12, ncol=12))

#Modular dummy
mod_J <-    t(matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,
                       1,1,1,1,0,0,0,0,0,0,0,0,
                       1,1,1,1,0,0,0,0,0,0,0,0,
                       1,1,1,1,0,0,0,0,0,0,0,0,
                       0,0,0,0,1,1,1,1,1,1,1,1,
                       0,0,0,0,1,1,1,1,1,1,1,1,
                       0,0,0,0,1,1,1,1,1,1,1,1,
                       0,0,0,0,1,1,1,1,1,1,1,1,
                       0,0,0,0,1,1,1,1,1,1,1,1,
                       0,0,0,0,1,1,1,1,1,1,1,1,
                       0,0,0,0,1,1,1,1,1,1,1,1,
                       0,0,0,0,1,1,1,1,1,1,1,1), nrow=12, ncol=12))

diag_J <-   t(matrix(c(1,0,0,0,0,0,0,0,0,0,0,0,
                       0,1,0,0,0,0,0,0,0,0,0,0,
                       0,0,1,0,0,0,0,0,0,0,0,0,
                       0,0,0,1,0,0,0,0,0,0,0,0,
                       0,0,0,0,1,0,0,0,0,0,0,0,
                       0,0,0,0,0,1,0,0,0,0,0,0,
                       0,0,0,0,0,0,1,0,0,0,0,0,
                       0,0,0,0,0,0,0,1,0,0,0,0,
                       0,0,0,0,0,0,0,0,1,0,0,0,
                       0,0,0,0,0,0,0,0,0,1,0,0,
                       0,0,0,0,0,0,0,0,0,0,1,0,
                       0,0,0,0,0,0,0,0,0,0,0,1), nrow=12, ncol=12))

#Dichotomized at 1
EN_Ajd <- event2dichot(EN_Aj,"absolute", thresh = 0.9)
EN_Bjd <- event2dichot(EN_Bj,"absolute", thresh = 0.9)
EN_Cjd <- event2dichot(EN_Cj,"absolute", thresh = 0.9)
EN_Djd <- event2dichot(EN_Dj,"absolute", thresh = 0.9)
EN_Ejd <- event2dichot(EN_Ej,"absolute", thresh = 0.9)

#Dichotomization Mean (1), rmean(2), cmean(3)
EN_Ajd1 <- event2dichot(EN_Aj,"mean")
EN_Bjd1 <- event2dichot(EN_Bj,"mean")
EN_Cjd1 <- event2dichot(EN_Cj,"mean")
EN_Djd1 <- event2dichot(EN_Dj,"mean")
EN_Ejd1 <- event2dichot(EN_Ej,"mean")

EN_Ajd2 <- event2dichot(EN_Aj,"rmean")
EN_Bjd2 <- event2dichot(EN_Bj,"rmean")
EN_Cjd2 <- event2dichot(EN_Cj,"rmean")
EN_Djd2 <- event2dichot(EN_Dj,"rmean")
EN_Ejd2 <- event2dichot(EN_Ej,"rmean")

EN_Ajd3 <- event2dichot(EN_Aj,"cmean")
EN_Bjd3 <- event2dichot(EN_Bj,"cmean")
EN_Cjd3 <- event2dichot(EN_Cj,"cmean")
EN_Djd3 <- event2dichot(EN_Dj,"cmean")
EN_Ejd3 <- event2dichot(EN_Ej,"cmean")
