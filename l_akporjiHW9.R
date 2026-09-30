rm(list=ls()) 
library(ALL)
data(ALL) 
#1
ALLB <- ALL[, ALL$BT %in% c("B2","B3","T2","T3")] 

y <- exprs(ALLB)["811_at",]
library(lattice)
dotplot(y~ALLB$BT ,xlab="Stages", ylab="811_at")

#A
anova(lm(y ~ ALLB$BT))
#B
summary(lm(y ~ ALLB$BT))
# in group T3
6.33897 + 0.26775
#D
pairwise.t.test(y, ALLB$BT,p.adjust.method='fdr')
#2
#A
ALLB <- ALL[, ALL$BT %in% c("B2","B3","T2","T3")] 

y <- exprs(ALLB)["913_at",]

shapiro.test(residuals(lm(y ~ ALLB$BT)))
y <- exprs(ALLB)["913_at",]
library(lmtest)
bptest(lm(y ~ ALLB$BT), studentize = FALSE)
y <- exprs(ALLB)["913_at",]

kruskal.test(y ~ ALLB$BT)
#B
y <- exprs(ALLB)["1234_at",]

shapiro.test(residuals(lm(y ~ ALLB$BT)))

bptest(lm(y ~ ALLB$BT), studentize = FALSE)

kruskal.test(y ~ ALLB$BT)
#3
rm(list = ls())
data("warpbreaks")
warpbreaks
#A
anova(lm(breaks ~ wool * tension, data = warpbreaks))
#B
summary(lm(breaks ~ wool + tension, data = warpbreaks))
#wool B with tension M
39.278 - 5.778 - 10.000
#C
y <- (lm(breaks ~ wool * tension, data = warpbreaks))
shapiro.test(residuals(y))
library(lmtest)
bptest(y, studentize = FALSE)