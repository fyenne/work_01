library(stargazer)
library(car)
library(lmtest)
library(tidyverse)

df = read.csv("assign1.csv")
names(df)
# a) [6 marks] What is the interpretation of the population parameters βK,βL, and βM?
# b) [2 marks] Estimate the econometric model (2) in R using the method of Ordi- nary Least Squares (OLS) and report the results. Do the signs of your estimated coefficients turn out as you expect? Why or why not?
reg1 = lm(lnroutput ~ lnrmaterials + lnlabour + lnrcapital, data = df)
summary(reg1) #0.959
linearHypothesis(reg1, "lnrmaterials + lnlabour + lnrcapital = 1")
df$y_k = df$lnroutput - df$lnrcapital
df$l_k = df$lnlabour  - df$lnrcapital
df$m_k = df$lnrmaterials - df$lnrcapital

reg2 = lm(y_k ~ l_k + m_k, data = df)
# lm(y_k ~ l_k + m_k, data = df)
summary(reg2)


resettest(reg1, power=2:3, type="fitted")

# RESET test
# 
# data:  reg1
# RESET = 23.878, df1 = 2, df2 = 1934, p-value = 5.702e-11
#--------------------------------------------
# question 2 a)
assign1 <- read.csv("assign1.csv")
meanlnrcapital <- mean(assign1$lnrcapital)
meanlnrlabour  <- mean(assign1$lnlabour)
meanlnrmatrerials <- mean(assign1$lnrmaterials)
print(meanlnrcapital)
print(meanlnrlabour)
print(meanlnrmatrerials)
#--------------------------------------------
# question 2 b)
reg4 = lm(lnroutput ~ . + lnrcapital^2 + lnlabour^2 + lnrmaterials^2 + 
     lnrcapital*lnlabour + lnlabour*lnrmaterials + lnrmaterials*lnrcapital - sector1, 
   data = assign1)
alias(reg4)
summary(reg4)


#--------------------------------------------
# question 2 c)
# the real level of output is increasing over time.
linearHypothesis(reg4, "year = 0")

#--------------------------------------------
# question 2 d)
resettest(reg4, power=2:3, type="fitted")
#--------------------------------------------
# question 2 e)


#--------------------------------------------
# question 2 f)
linearHypothesis(reg4, "lnrmaterials + lnlabour + lnrcapital = 1")

#--------------------------------------------
# question 2 f)

linearHypothesis(reg4, "sector2 + sector3 +
                 sector4 + sector5 + sector6 + sector7 + sector8 = 0")

# Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1   1924 383.44                              
# 2   1923 382.35  1    1.0854 5.4591 0.01957 *