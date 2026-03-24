#LPM
install.packages("wooldridge")
library(wooldridge)
loan = na.omit(wooldridge::loanapp)
"loanprc" %in% names(loan)
lpmodel= lm(approve ~ married + yjob + appinc + liq + loanprc + pubrec + term,data=loan)
summary(lpmodel)
library(lmtest)
bptest(lpmodel)
#p-value=2.2e-16<0,05 #v modelu je pritomna heteroskedasticita
library(sandwich)
lpmodel_HC = coeftest(lpmodel, vcov = vcovHC(lpmodel, type = "HC1"))
lpmodel_HC
library(stargazer)
stargazer(lpmodel_HC, type = "text")
loan$appinc2 <- loan$appinc^2
lpmodel2=lm(approve ~ married + yjob + appinc + appinc2 + liq + loanprc + pubrec + term, data = loan)
summary(lpmodel2)
loan2 <- data.frame(
  married = 0,
  yjob = 0,
  appinc = 0,
  liq = 0,
  loanprc = 1,
  pubrec = 0,
  term = 0
)
predict(lpmodel, newdata = loan2)
#0.8140603 pokud loanprc=1 tak pravděpodobnost, že bude úvěr schválen
#1.2 Logit
logitmodel <- glm(approve ~ married + yjob + appinc + liq + loanprc + pubrec + term,data=loan, family = binomial)
summary(logitmodel)
library(lmtest)
bptest(logitmodel)
#p-value < 2.2e-16, v modelu je přítomna heteroskedasticita
library(sandwich)
logitmodel_HC = coeftest(logitmodel, vcov = vcovHC)
logitmodel_HC
stargazer(logitmodel, logitmodel_HC, type = "text")
install.packages("mfx")
require(mfx)
logit_MEMs <- logitmfx(approve~married + yjob + appinc + liq + loanprc + pubrec + term,data=loan, atmean = T, robust = T)
logit_MEMs
logit_AMEs <- logitmfx(approve~married + yjob + appinc + liq + loanprc + pubrec + term,data=loan, atmean = F, robust=T)
logit_AMEs
poz15 <- loan[15, ]
poz15_married1 <- poz15
poz15_married0 <- poz15
poz15_married1$married <- 1
poz15_married0$married <- 0
p1 <- predict(logitmodel, newdata = poz15_married1, type = "response")
p0 <- predict(logitmodel, newdata = poz15_married0, type = "response")
efektmarried_15 <- p1 - p0
efektmarried_15
#panelova data
math = wooldridge::mathpnl
math = math[math$year %in% c(1992,1993,1994),]
install.packages("plm", type = "source")
library(plm)
math_p <- pdata.frame(math, index = c("distid", "year"))
is.pbalanced(math_p)
eq = math4~lunch + rexpp + avgsal + staff
eq_lsdv <- math4 ~ lunch + rexpp + avgsal + staff + factor(distid)
FEmodel <- plm(eq, data = math_p, model = "within")
summary(FEmodel)
library(lmtest)
bptest(FEmodel)
FEmodel_HC = coeftest(FEmodel, vcov = vcovHC)
library(sandwich)
LSDVmodel <- lm(eq_lsdv, data = math)
summary(LSDVmodel)
library(lmtest)
bptest(LSDVmodel)
LSDVmodel_HC = coeftest(LSDVmodel, vcov = vcovHC)
stargazer(FEmodel_HC, LSDVmodel_HC, type = "text")
LSDVmodel_HC[grep("^(lunch|rexpp|avgsal|staff)", rownames(LSDVmodel_HC)), ]
FEmodel_HC[grep("^(lunch|rexpp|avgsal|staff)", rownames(FEmodel_HC)), ]
REmodel <- plm(eq, data = math_p, model = "random")
REmodel_HC <- coeftest(REmodel, vcov = vcovHC)
phtest(FEmodel, REmodel)
# p-value <2.2e-16, zamitame H0
