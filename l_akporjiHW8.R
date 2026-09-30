library(multtest)
data(golub)
azurocidin <- golub[1069, ]
cd34 <- golub[1087, ]
#1a
shapiro.test(azurocidin)
#1b
shapiro.test(cd34)
#1c Wilcoxon two-sample test
wilcox.test(azurocidin, cd34, paired = F, alternative = "two.sided")
#2a
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL","AML"))
wilcox.test (cd34 ~ gol.fac, alternative= "less")
#2b
wilcox.test (azurocidin ~ gol.fac, alternative= "less")
#3
cystatin <- golub[389, ]
#3a Sample variance
cystatin_all <- cystatin[gol.fac == "ALL"] 
cystatin_aml <- cystatin[gol.fac == "AML"]
var(cystatin_all)
var(cystatin_aml)
#4
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.22")
BiocManager::install("ALL")
library(ALL)
data(ALL)
ALLTexp <-exprs(ALL[,ALL$BT %in% c("T","T1","T2","T3", "T4")])
#4a 
p.values <- apply(ALLTexp, 1, function(x) shapiro.test(x)$p.value)
alpha_bonf <- 0.05 / nrow(ALLTexp)
sum(p.values < alpha_bonf)
#4b
install.packages("outliers")
library(outliers)
grubbs_p <- apply(ALLTexp, 1, function(x) grubbs.test(x)$p.value)
alpha_bonf <- 0.05 / nrow(ALLTexp)
sum(grubbs_p < alpha_bonf)

#5
obesity <- matrix(c(121, 54, 501, 324), nrow = 2)
rownames(obesity) <- c("Low", "Middle-Upper")
colnames(obesity) <- c("Yes", "No")
obesity
chisq.test(obesity)
