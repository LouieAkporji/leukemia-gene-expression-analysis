library(MASS)
data("anorexia")
?anorexia
head(anorexia)
#create groups 
control_grp <- anorexia[anorexia$Treat =="Cont", ]
ft_grp <- anorexia[anorexia$Treat =="FT", ]
#create weight change
control_grp$change <- control_grp$Postwt - control_grp$Prewt
ft_grp$change <- ft_grp$Postwt - ft_grp$Prewt
#1a one sample t-test
t.test(ft_grp$change, mu=4, alternative ="greater")
#1b one sample proportion test
ft_grp$gain10 <- ft_grp$change >= 10
x <- sum(ft_grp$gain10)
n <- nrow(ft_grp)
prop.test(x, n, p =0.3, alternative = "greater")
#1c one sample t-test
t.test(control_grp$change, mu=0)
#1d
xC <- sum(control_grp$change >= 2)
xT <- sum(ft_grp$change >= 2)
nC <- nrow(control_grp)
nT <- nrow(ft_grp)
prop.test(x = c(xC, xT), n = c(nC, nT))
#2
library(multtest)
data(golub) 
golub.gnames
golub.gnames[,2]
gene <- golub[717, ]
gol.fac <- factor(golub.cl, levels = 0:1, labels = c("ALL", "AML"))
#2a
xALL <- mean(gene[gol.fac =="ALL"])
xAML <- mean(gene[gol.fac =="AML"])
xALL
xAML
#2c
t.test(golub[717, gol.fac == "ALL"],
             golub[717, gol.fac == "AML"],
             alternative = "greater")
#Q3 
1-pbinom(150, 14000, 0.01)
#Q4
library(multtest)
data(golub)
gol.fac <- factor(golub.cl, levels = 0:1, labels = c("ALL", "AML"))
p.values <- apply(golub, 1, function(x) {t.test(x[gol.fac =="AML"], mu = 1.4, alternative = "greater")$p.value})
p.bon <- p.adjust(p.values, method = "bonferroni")
p.fdr <- p.adjust(p.values, method = "fdr")
sum(p.bon < 0.05)
sum(p.fdr < 0.05)

