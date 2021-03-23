# install.packages("stargazer") 
# install.packages("car")
# install.packages("IMTest")

# read in all the libraries needed
library(stargazer)
library(car)
library(IMTest)

# 1(b)
# Estimate the econometric model by OLS
assign1<- read.csv("assign1.csv")
reg2<- lm( lnroutput~ lnrcapital+ lnlabour +lnrmaterials, data=assign1)
stargazer(reg2, type = "html", 
          covariate.labels = c("Intercept",
                               " Natural logarithm of real vale of the capital stock",
                               " Natural logarithm of the labour input",
                               " Natural logarithm of the real value of the materials input"),
          dep.var.labels   = c("Natural logarithm of real firm output"),
          digits=4,
          single.row = FALSE,
          intercept.bottom = FALSE,
          out="assign1_reg2.html")
# print the summary of the regression
summary(reg2)

# 1(c)i
hnull_1 <-c("lnrcapital + lnlabour + lnrmaterials = 1")
linearHypothesis(reg2, hnull_1)

# 1(c)ii
lnroutputmlnrcapital <- assign1$lnroutput - assign1$lnrcapital
lnlabourmlnrcapital <- assign1$lnlabour - assign1$lnrcapital
lnrmaterialsmlnrcapital <- assign1$lnrmaterials - assign1$lnrcapital

regii <- lm("lnroutputmlnrcapital ~ lnlabourmlnrcapital + lnrmaterialsmlnrcapital")

# print the summary of the regression
summary(regii)

stargazer(regii, type = "html", 
          covariate.labels = c("Intercept",
                               "lnlabourmlnrcapital",
                               "lnrmaterialsmlnrcapital"),
          dep.var.labels   = c("lnroutputmlnrcapital"),
          digits=4,
          single.row = FALSE,
          intercept.bottom = FALSE,
          out="assign1_regii.html")

df_reg2 <- df.residual(reg2)
RSSUR   <- deviance(reg2)
RSSR    <- deviance(regii)
F_numdf <- 1
F_demdf <- df_reg2
F_reg2  <- ((RSSR-RSSUR)/F_numdf)/(RSSUR/F_demdf)
print(RSSUR)
print(RSSR)
print(F_demdf)
print(F_reg2)



# 1(c)iii
df_reg2 <- df.residual(reg2)
# find two-tailed t critical values
qt(p=0.05/2, df=1936, lower.tail=FALSE)

# t-test with H0:bK+bL+bM=1
bK<- coef(reg2)[["lnrcapital"]]
bL<- coef(reg2)[["lnlabour"]]
bM<- coef(reg2)[["lnrmaterials"]]

# print the covariance matrix
print(vcov(reg2))
bterm <- bK+bL+bM

print(bK)
print(bL)
print(bM)
print(reg2)

# numerator for t test
# pull out required entries from variance-covariance matrix
varbK <- vcov(reg2)[2,2]
varbL <- vcov(reg2)[3,3]
varbM <- vcov(reg2)[4,4]
covbKbL <- vcov(reg2)[2,3]
covbLbM <- vcov(reg2)[2,4]
covbKbM <- vcov(reg2)[3,4]

# Creating the variance term
varterm <- varbK + varbM + varbL + (2*covbKbL) + (2*covbLbM) +(2*covbKbM)
print(varterm)
seterm <- sqrt(varterm)
print(seterm)

# Denominator for t test
t_test <- (bterm-1)/seterm
pval_test <- 2*(1-pt(abs(t_test), df_reg2))
print(t_test)
print(pval_test)

# 1(d)
library(lmtest)
resettest(reg2, power=2:3, type="fitted")


# 2(a)
assign1 <- read.csv("assign1.csv")
meanlnrcapital <- mean(assign1$lnrcapital)
meanlnlabour <- mean(assign1$lnlabour)
meanlnrmatrerials <- mean(assign1$lnrmaterials)
print(meanlnrcapital)
print(meanlnlabour)
print(meanlnrmatrerials)

lnrcapitalsq <- assign1$lnrcapital^2
lnlaboursq <- assign1$lnlabour^2
lnrmaterialssq <- assign1$lnrmaterials^2
lnrcapital_lnlabour <- assign1$lnrcapital * assign1$lnlabour
lnrcapital_lnrmaterials <- assign1$lnrcapital * assign1$lnrmaterials
lnlabour_lnrmaterials <- assign1$lnlabour * assign1$lnrmaterials

reg4 <- lm(lnroutput ~ lnrcapital + lnlabour + lnrmaterials 
           + lnrcapitalsq + lnlaboursq + lnrmaterialssq 
           + lnrcapital_lnlabour + lnrcapital_lnrmaterials + lnlabour_lnrmaterials 
           + exports +  sector2 + sector3 + sector4 + sector5 
           + sector6 + sector7 + sector8  + year, data=assign1)
# print the summary of the regression
summary(reg4)


stargazer(reg4, type = "html", 
          dep.var.labels   = c("lnroutput"),
          digits=4,
          single.row = FALSE,
          intercept.bottom = FALSE,
          out="assign1_reg4.html")

stargazer(reg4, type = "html", 
          covariate.labels = c("Intercept", "lnrcapital", "lnlabour", "lnrmaterials",
                               "lnrcapitalsq", "lnlaboursq", "lnrmaterialssq",
                               "lnrcapital_lnlabour", "lnrcapital_lnrmaterials", "lnlabour_lnrmaterials",
                               "exports", "sector2", "sector3", "sector4", "sector5",
                               "sector6", "sector7", "sector8", "year"),
          dep.var.labels   = c("lnroutput"),
          digits=4,
          single.row = FALSE,
          intercept.bottom = FALSE,
          out="assign1_reg4.html")

#--------------------------------------------
reg4b = lm(lnroutput ~ . + I(lnrcapital^2) + I(lnlabour^2) + I(lnrmaterials^2) +
            lnrcapital:lnlabour + lnlabour:lnrmaterials + lnrmaterials:lnrcapital -
            sector1 - firm, data = assign1)

stargazer(reg4b, type = "text")

#--------------------------------------------

# 2(b)
elasticityK <- coef(reg4)[["lnrcapital"]] + 2*coef(reg4)[["lnrcapitalsq"]]*meanlnrcapital + coef(reg4)[["lnrcapital_lnlabour"]]*meanlnlabour + coef(reg4)[["lnrcapital_lnrmaterials"]]*meanlnrmatrerials
elasticityL <- coef(reg4)[["lnlabour"]] + 2*coef(reg4)[["lnlaboursq"]]*meanlnlabour + coef(reg4)[["lnrcapital_lnlabour"]]*meanlnrcapital + coef(reg4)[["lnlabour_lnrmaterials"]]*meanlnrmatrerials
elasticityM <- coef(reg4)[["lnrmaterials"]] + 2*coef(reg4)[["lnrmaterialssq"]]*meanlnrmatrerials + coef(reg4)[["lnrcapital_lnrmaterials"]]*meanlnrcapital + coef(reg4)[["lnlabour_lnrmaterials"]]*meanlnlabour
print(elasticityK)
print(elasticityL)
print(elasticityM)

# 2(c)
??t <- coef(reg4)[["year"]]
print(??t)
linearHypothesis(reg4, "year=0")


# 2(d)
library(lmtest)
resettest(reg4, power=2:3, type="fitted")

# 2(e)
hnull_2 <- c("lnrcapitalsq=0", "lnlaboursq=0", "lnrmaterialssq=0",
             "lnrcapital_lnlabour=0", "lnrcapital_lnrmaterials=0", "lnlabour_lnrmaterials=0",
             "exports=0", "sector2=0", "sector3=0", "sector4=0", "sector5=0",
             "sector6=0", "sector7=0", "sector8=0", "year=0")
linearHypothesis(reg4 , hnull_2)


# 2(f)
linearHypothesis(reg4, c("lnrmaterials + lnlabour + lnrcapital = 1",
                         "2*lnrcapitalsq + lnrcapital_lnlabour + lnrcapital_lnrmaterials = 0",
                         "2*lnlaboursq + lnrcapital_lnlabour + lnlabour_lnrmaterials = 0",
                         "2*lnrmaterialssq + lnrcapital_lnrmaterials + lnlabour_lnrmaterials = 0"))

# 2(g)
linearHypothesis(reg4, c("sector2=0", "sector3=0", "sector4=0", "sector5=0", "sector6=0", "sector7=0", "sector8=0"))






# covariate.labels = c("Intercept",
#                      "lnrcapital",
#                      "lnlabour",
#                      "lnrmaterials",
#                      "lnrcapitalsq",
#                      "lnlaboursq",
#                      "lnrmaterialssq",
#                      "lnrcapital_lnlabour",
#                      "lnrcapital_lnrmaterials",
#                      "lnlabour_lnrmaterials",
#                      "exports",
#                      "sector2",
#                      "sector3",
#                      "sector4",
#                      "sector5",
#                      "sector6",
#                      "sector7",
#                      "sector8",
#                      "year"),