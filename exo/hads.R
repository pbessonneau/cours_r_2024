(load("../data/HADS.RData"))
hads <- data

colnames(hads)[c(1,3,4,5,9,13,14)] <- gsub("Y","dep",colnames(hads)[c(1,3,4,5,9,13,14)])
colnames(hads)[c(2,6,7,8,10,11,12)] <- gsub("Y","anx",colnames(hads)[c(2,6,7,8,10,11,12)])

suppressPackageStartupMessages(require(ltm,quietly=T,warn.conflicts=FALSE))
(desc=descript(hads))

str(desc)
apply(desc$perc,1:2,function(x) ifelse(x>0.98|x<0.05, round(x,3), ""))

hads$dep1[hads$dep1==4] <- 3
hads$anx2[hads$anx2==4] <- 3
hads$anx7[hads$anx7==4] <- 3
hads$dep9[hads$dep9==4] <- 3
hads$anx10[hads$anx10==4] <- 3
hads$anx11[hads$anx11==4] <- 3
hads$dep13[hads$dep13==4] <- 3str(a.dep)
a.dep$total$std.alpha < a.dep$alpha.drop$raw_alpha 
a.anx$total$std.alpha < a.anx$alpha.drop$raw_alpha
hads$dep14[hads$dep14==4] <- 3

suppressPackageStartupMessages(require(psych,quietly=T,warn.conflicts=FALSE))
psych::alpha(hads)
(a.dep <- psych::alpha(hads[,grep("dep",colnames(hads))]))
(a.anx <- psych::alpha(hads[,grep("anx",colnames(hads))]))


str(a.dep)
a.dep$total$std.alpha < a.dep$alpha.drop$raw_alpha 
a.anx$total$std.alpha < a.anx$alpha.drop$raw_alpha


psych::fa.parallel(hads)

psych::fa.parallel(hads[,grep("dep",colnames(hads))])

psych::fa.parallel(hads[,grep("dep",colnames(hads))])

psych::fa(hads)

psych::fa(hads[,grep("dep",colnames(hads))])

psych::fa(hads[,grep("anx",colnames(hads))])

suppressPackageStartupMessages(require(lavaan,quietly=T,warn.conflicts=FALSE))
m <- '
  depression =~ dep1 + dep3 + dep4 + dep5 + dep9 + dep13 + dep14
  anxiete =~ anx2 + anx6 + anx7 + anx8 + anx10 + anx11 + anx12
'
lavaan::cfa(m,data=hads)

for (ii in colnames(hads))
  hads[[ii]] <- as.ordered(hads[[ii]])

m <- '
  depression =~ dep1 + dep3 + dep4 + dep5 + dep9 + dep13 + dep14
  anxiete =~ anx2 + anx6 + anx7 + anx8 + anx10 + anx11 + anx12
'
(lav <- lavaan::cfa(m,data=hads))
lavaan::summary(lav,fit.measures = TRUE)


lavaan::modindices(lav)


m <- '
  depression =~ dep1 + dep3 + dep4 + dep5 + dep9 + dep13 + dep14
  anxiete =~ anx2 + anx6 + anx7 + anx8 + anx10 + anx11 + anx12
  F =~ depression + anxiete
'
(lav <- lavaan::cfa(m,data=hads))
lavaan::summary(lav,fit.measures = TRUE)

m <- '
  F =~ dep1 + dep3 + dep4 + dep5 + dep9 + dep13 + dep14 +
       anx2 + anx6 + anx7 + anx8 + anx10 + anx11 + anx12
'
(lav <- lavaan::cfa(m,data=hads))
lavaan::summary(lav,fit.measures = TRUE)


m <- '
  depression =~ dep1 + dep3 + dep4 + dep5 + dep9 + dep13 + dep14
  anxiete =~ anx2 + anx6 + anx7 + anx8 + anx10 + anx11 + anx12
  F =~ dep1 + dep3 + dep4 + dep5 + dep9 + dep13 + dep14 +
       anx2 + anx6 + anx7 + anx8 + anx10 + anx11 + anx12
'
(lav <- lavaan::cfa(m,data=hads,orthogonal = T))
lavaan::summary(lav,fit.measures = TRUE)