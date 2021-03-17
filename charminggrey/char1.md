---
title: "Assign1"
author: "abcd"
date:   "17 March, 2021"
output: 
  html_document:
    theme: cerulean
    keep_md: true
    toc: yes
    toc_float: true
    highlight: haddock
---






# Q1
## a) [6 marks] What is the interpretation of the population parameters βK,βL, and βM?
## b) [2 marks] Estimate the econometric model (2) in R using the method of Ordi- nary Least Squares (OLS) and report the results. Do the signs of your estimated coefficients turn out as you expect? Why or why not?


```r
reg1 = lm(lnroutput ~ lnrmaterials + lnlabour + lnrcapital, data = df)
summary(reg1)
```

```
## 
## Call:
## lm(formula = lnroutput ~ lnrmaterials + lnlabour + lnrcapital, 
##     data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.3277 -0.2540 -0.0318  0.2089  5.5588 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.884241   0.100716   28.64   <2e-16 ***
## lnrmaterials 0.746139   0.007687   97.06   <2e-16 ***
## lnlabour     0.262434   0.014097   18.62   <2e-16 ***
## lnrcapital   0.079281   0.006116   12.96   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4524 on 1936 degrees of freedom
## Multiple R-squared:  0.959,	Adjusted R-squared:  0.9589 
## F-statistic: 1.509e+04 on 3 and 1936 DF,  p-value: < 2.2e-16
```

## c

### c-i) Constant returns to scale implies that a doubling of all factor inputs (K, L,and M) exactly doubles production. In terms of the model (2), this imposes the restriction (βK + βL + βM ) = 1


```r
linearHypothesis(reg1, "lnrmaterials + lnlabour + lnrcapital = 1")
```

```
## Linear hypothesis test
## 
## Hypothesis:
## lnrmaterials  + lnlabour  + lnrcapital = 1
## 
## Model 1: restricted model
## Model 2: lnroutput ~ lnrmaterials + lnlabour + lnrcapital
## 
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1   1937 417.39                                  
## 2   1936 396.27  1    21.126 103.21 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
qf(.95, 3, 1935)
```

```
## [1] 2.609501
```

### c-ii) [6 marks] Without using the car package in R, test the hypothesis of constant returns to scale, for model (2), at a 5% level of significance using an F-test.

$$
F =   {{(RSS_R −RSS_{UR})/M} \over {RSS_{UR} /(N − K − 1)}} ∼ F (M ,\  N − K − 1)
$$


```r
df$y_k = df$lnroutput - df$lnrcapital
df$l_k = df$lnlabour - df$lnrcapital
df$m_k = df$lnrmaterials - df$lnrcapital
reg2 = lm(y_k ~ l_k + m_k, data = df)
```



```r
summary(reg1)
```

```
## 
## Call:
## lm(formula = lnroutput ~ lnrmaterials + lnlabour + lnrcapital, 
##     data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.3277 -0.2540 -0.0318  0.2089  5.5588 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.884241   0.100716   28.64   <2e-16 ***
## lnrmaterials 0.746139   0.007687   97.06   <2e-16 ***
## lnlabour     0.262434   0.014097   18.62   <2e-16 ***
## lnrcapital   0.079281   0.006116   12.96   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4524 on 1936 degrees of freedom
## Multiple R-squared:  0.959,	Adjusted R-squared:  0.9589 
## F-statistic: 1.509e+04 on 3 and 1936 DF,  p-value: < 2.2e-16
```

```r
# 0.4524  on 1936 degrees of freedom # 3 and 1936 DF
summary(reg2) 
```

```
## 
## Call:
## lm(formula = y_k ~ l_k + m_k, data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.1670 -0.2704 -0.0320  0.2238  5.5116 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.729699   0.102154   26.72   <2e-16 ***
## l_k         0.140989   0.007667   18.39   <2e-16 ***
## m_k         0.747325   0.007886   94.76   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4642 on 1937 degrees of freedom
## Multiple R-squared:  0.9368,	Adjusted R-squared:  0.9367 
## F-statistic: 1.435e+04 on 2 and 1937 DF,  p-value: < 2.2e-16
```

```r
# 0.4642 on 1937 degrees of freedom #  2 and 1937 DF
n = 1937; k =2
F_value_q1 = ((0.4642 - 0.4524)/k) / ((0.4524)/(n-k-1))
F_value_q1
```

```
## [1] 25.22237
```

```r
qf(.95, 2, 1937)
```

```
## [1] 3.00037
```

```r
# 
# 0.9368/2 / ((1-0.9368)/(n-k-1))
```

 
H0: the two models are different.
F_value = 25.2223696, which is larger than 2.6094989, hence we reject H0. The two models are not different, which implies that ($\beta_K$ + $\beta_L$ + $\beta_M$) = 1.

### c-iii) [8 marks] Without using the car package in R, test the hypothesis of constant returns to scale, for model (2), at a 5% level of significance using a t-test.

$$VAR(aX+bY +cZ)=a^2 VAR(X)+b^2 VAR(Y)+c^2VAR(Z) +2ab COV(X,Y)+2bc COV(Y,Z)+2ac COV(X,Z)$$


```r
# klm
vcov(reg1)
```

```
##                (Intercept)  lnrmaterials      lnlabour    lnrcapital
## (Intercept)   0.0101436764 -5.901678e-04  9.217032e-04 -1.999907e-04
## lnrmaterials -0.0005901678  5.909146e-05 -4.523645e-05 -1.486436e-05
## lnlabour      0.0009217032 -4.523645e-05  1.987276e-04 -5.011792e-05
## lnrcapital   -0.0001999907 -1.486436e-05 -5.011792e-05  3.740002e-05
```

```r
upper = (reg1$coefficients[2] + reg1$coefficients[3] + reg1$coefficients[4] - 1)
lower = vcov(reg1)[2,2] + vcov(reg1)[3,3] + vcov(reg1)[4,4] + 
  2*vcov(reg1)[2,3] + 2*vcov(reg1)[2,4] + 2*vcov(reg1)[3,4]
cat("t-value is: ",upper/sqrt(lower))
```

```
## t-value is:  10.15943
```


## d) [6 marks] Test the hypothesis that model (2) is correctly specified using the Ram- sey RESET test, at the 5% level, using both the squares and the cubes of the fitted values. What is your conclusion?


```r
resettest(reg1, power=2:3, type="fitted")
```

```
## 
## 	RESET test
## 
## data:  reg1
## RESET = 23.878, df1 = 2, df2 = 1934, p-value = 5.702e-11
```

## e) Suppose this alternative model has been estimated using the data in assign1.csv and a value of R2 = 0.8749 was obtained. Consider the following statement: “Given the sample of data, the econometric model (2) is preferred to the alternative econometric model (3) because it has a larger R2.” Do you agree with this statement. Why or why not? 

---

# Question 2 [Total 40 marks] Consider the following extended version of model (1):

## question 2 a)

```r
assign1 <- read.csv("assign1.csv")
meanlnrcapital <- mean(assign1$lnrcapital)
meanlnrlabour  <- mean(assign1$lnlabour)
meanlnrmatrerials <- mean(assign1$lnrmaterials)
meanout <- mean(assign1$lnroutput)
print(meanlnrcapital)
```

```
## [1] 16.23686
```

```r
print(meanlnrlabour)
```

```
## [1] 3.221309
```

```r
print(meanlnrmatrerials)
```

```
## [1] 16.53774
```



```r
# 
# assign1
reg4 = lm(lnroutput ~ . + I(lnrcapital^2) + I(lnlabour^2) + I(lnrmaterials^2) +
            lnrcapital*lnlabour + lnlabour*lnrmaterials + lnrmaterials*lnrcapital -
            sector1 - firm, data =  assign1)
summary(reg4)
```

```
## 
## Call:
## lm(formula = lnroutput ~ . + I(lnrcapital^2) + I(lnlabour^2) + 
##     I(lnrmaterials^2) + lnrcapital * lnlabour + lnlabour * lnrmaterials + 
##     lnrmaterials * lnrcapital - sector1 - firm, data = assign1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.4666 -0.2237 -0.0249  0.2029  3.5422 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             -27.303522   5.947747  -4.591 4.71e-06 ***
## year                      0.017906   0.002989   5.990 2.49e-09 ***
## lnrmaterials             -0.102110   0.070726  -1.444 0.148973    
## lnrcapital               -0.033926   0.057662  -0.588 0.556355    
## lnlabour                  1.646644   0.138069  11.926  < 2e-16 ***
## exports                   0.043101   0.022290   1.934 0.053301 .  
## sector2                  -0.215905   0.045300  -4.766 2.02e-06 ***
## sector3                   0.061296   0.066237   0.925 0.354875    
## sector4                  -0.172781   0.037533  -4.603 4.43e-06 ***
## sector5                  -0.002505   0.045291  -0.055 0.955896    
## sector6                  -0.069793   0.036910  -1.891 0.058785 .  
## sector7                  -0.015910   0.033455  -0.476 0.634436    
## sector8                  -0.009865   0.055611  -0.177 0.859222    
## I(lnrcapital^2)           0.013465   0.002168   6.210 6.50e-10 ***
## I(lnlabour^2)             0.105284   0.011797   8.924  < 2e-16 ***
## I(lnrmaterials^2)         0.044493   0.003142  14.162  < 2e-16 ***
## lnrcapital:lnlabour      -0.021014   0.007462  -2.816 0.004913 ** 
## lnrmaterials:lnlabour    -0.106409   0.010461 -10.172  < 2e-16 ***
## lnrmaterials:lnrcapital  -0.015948   0.004094  -3.895 0.000101 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4187 on 1921 degrees of freedom
## Multiple R-squared:  0.9652,	Adjusted R-squared:  0.9648 
## F-statistic:  2956 on 18 and 1921 DF,  p-value: < 2.2e-16
```

## question 2 b)your estimates for model (4) calculate the following estimated elasticities, evaluated at the sample means:


```r
# reg4 is log-log regression
reg4$coefficients['lnrcapital']
```

```
##  lnrcapital 
## -0.03392613
```

```r
reg4$coefficients['lnlabour']
```

```
## lnlabour 
## 1.646644
```

```r
reg4$coefficients['lnrmaterials']
```

```
## lnrmaterials 
##     -0.10211
```


## question 2 c)

# the real level of output is increasing over time.


```r
summary(reg4)[2]
```

```
## $terms
## lnroutput ~ firm + year + lnrmaterials + lnrcapital + lnlabour + 
##     exports + sector1 + sector2 + sector3 + sector4 + sector5 + 
##     sector6 + sector7 + sector8 + I(lnrcapital^2) + I(lnlabour^2) + 
##     I(lnrmaterials^2) + lnrcapital * lnlabour + lnlabour * lnrmaterials + 
##     lnrmaterials * lnrcapital - sector1 - firm
## attr(,"variables")
## list(lnroutput, firm, year, lnrmaterials, lnrcapital, lnlabour, 
##     exports, sector1, sector2, sector3, sector4, sector5, sector6, 
##     sector7, sector8, I(lnrcapital^2), I(lnlabour^2), I(lnrmaterials^2))
## attr(,"factors")
##                   year lnrmaterials lnrcapital lnlabour exports sector2 sector3
## lnroutput            0            0          0        0       0       0       0
## firm                 0            0          0        0       0       0       0
## year                 1            0          0        0       0       0       0
## lnrmaterials         0            1          0        0       0       0       0
## lnrcapital           0            0          1        0       0       0       0
## lnlabour             0            0          0        1       0       0       0
## exports              0            0          0        0       1       0       0
## sector1              0            0          0        0       0       0       0
## sector2              0            0          0        0       0       1       0
## sector3              0            0          0        0       0       0       1
## sector4              0            0          0        0       0       0       0
## sector5              0            0          0        0       0       0       0
## sector6              0            0          0        0       0       0       0
## sector7              0            0          0        0       0       0       0
## sector8              0            0          0        0       0       0       0
## I(lnrcapital^2)      0            0          0        0       0       0       0
## I(lnlabour^2)        0            0          0        0       0       0       0
## I(lnrmaterials^2)    0            0          0        0       0       0       0
##                   sector4 sector5 sector6 sector7 sector8 I(lnrcapital^2)
## lnroutput               0       0       0       0       0               0
## firm                    0       0       0       0       0               0
## year                    0       0       0       0       0               0
## lnrmaterials            0       0       0       0       0               0
## lnrcapital              0       0       0       0       0               0
## lnlabour                0       0       0       0       0               0
## exports                 0       0       0       0       0               0
## sector1                 0       0       0       0       0               0
## sector2                 0       0       0       0       0               0
## sector3                 0       0       0       0       0               0
## sector4                 1       0       0       0       0               0
## sector5                 0       1       0       0       0               0
## sector6                 0       0       1       0       0               0
## sector7                 0       0       0       1       0               0
## sector8                 0       0       0       0       1               0
## I(lnrcapital^2)         0       0       0       0       0               1
## I(lnlabour^2)           0       0       0       0       0               0
## I(lnrmaterials^2)       0       0       0       0       0               0
##                   I(lnlabour^2) I(lnrmaterials^2) lnrcapital:lnlabour
## lnroutput                     0                 0                   0
## firm                          0                 0                   0
## year                          0                 0                   0
## lnrmaterials                  0                 0                   0
## lnrcapital                    0                 0                   1
## lnlabour                      0                 0                   1
## exports                       0                 0                   0
## sector1                       0                 0                   0
## sector2                       0                 0                   0
## sector3                       0                 0                   0
## sector4                       0                 0                   0
## sector5                       0                 0                   0
## sector6                       0                 0                   0
## sector7                       0                 0                   0
## sector8                       0                 0                   0
## I(lnrcapital^2)               0                 0                   0
## I(lnlabour^2)                 1                 0                   0
## I(lnrmaterials^2)             0                 1                   0
##                   lnrmaterials:lnlabour lnrmaterials:lnrcapital
## lnroutput                             0                       0
## firm                                  0                       0
## year                                  0                       0
## lnrmaterials                          1                       1
## lnrcapital                            0                       1
## lnlabour                              1                       0
## exports                               0                       0
## sector1                               0                       0
## sector2                               0                       0
## sector3                               0                       0
## sector4                               0                       0
## sector5                               0                       0
## sector6                               0                       0
## sector7                               0                       0
## sector8                               0                       0
## I(lnrcapital^2)                       0                       0
## I(lnlabour^2)                         0                       0
## I(lnrmaterials^2)                     0                       0
## attr(,"term.labels")
##  [1] "year"                    "lnrmaterials"           
##  [3] "lnrcapital"              "lnlabour"               
##  [5] "exports"                 "sector2"                
##  [7] "sector3"                 "sector4"                
##  [9] "sector5"                 "sector6"                
## [11] "sector7"                 "sector8"                
## [13] "I(lnrcapital^2)"         "I(lnlabour^2)"          
## [15] "I(lnrmaterials^2)"       "lnrcapital:lnlabour"    
## [17] "lnrmaterials:lnlabour"   "lnrmaterials:lnrcapital"
## attr(,"order")
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2
## attr(,"intercept")
## [1] 1
## attr(,"response")
## [1] 1
## attr(,".Environment")
## <environment: R_GlobalEnv>
## attr(,"predvars")
## list(lnroutput, firm, year, lnrmaterials, lnrcapital, lnlabour, 
##     exports, sector1, sector2, sector3, sector4, sector5, sector6, 
##     sector7, sector8, I(lnrcapital^2), I(lnlabour^2), I(lnrmaterials^2))
## attr(,"dataClasses")
##         lnroutput              firm              year      lnrmaterials 
##         "numeric"         "numeric"         "numeric"         "numeric" 
##        lnrcapital          lnlabour           exports           sector1 
##         "numeric"         "numeric"         "numeric"         "numeric" 
##           sector2           sector3           sector4           sector5 
##         "numeric"         "numeric"         "numeric"         "numeric" 
##           sector6           sector7           sector8   I(lnrcapital^2) 
##         "numeric"         "numeric"         "numeric"         "numeric" 
##     I(lnlabour^2) I(lnrmaterials^2) 
##         "numeric"         "numeric"
```


#--------------------------------------------
# question 2 d)


```r
resettest(reg4, power=2:3, type="fitted")
```

```
## 
## 	RESET test
## 
## data:  reg4
## RESET = 8.5582, df1 = 2, df2 = 1919, p-value = 0.0001994
```

#--------------------------------------------
# question 2 e)




#--------------------------------------------
# question 2 f)


```r
linearHypothesis(reg4, "lnrmaterials + lnlabour + lnrcapital = 1")
```

```
## Linear hypothesis test
## 
## Hypothesis:
## lnrmaterials  + lnrcapital  + lnlabour = 1
## 
## Model 1: restricted model
## Model 2: lnroutput ~ firm + year + lnrmaterials + lnrcapital + lnlabour + 
##     exports + sector1 + sector2 + sector3 + sector4 + sector5 + 
##     sector6 + sector7 + sector8 + I(lnrcapital^2) + I(lnlabour^2) + 
##     I(lnrmaterials^2) + lnrcapital * lnlabour + lnlabour * lnrmaterials + 
##     lnrmaterials * lnrcapital - sector1 - firm
## 
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1   1922 342.87                                  
## 2   1921 336.81  1    6.0619 34.574 4.828e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
linearHypothesis(reg4, "2*I(lnrcapital^2) + lnrcapital:lnlabour + lnrmaterials:lnrcapital = 1")
```

```
## Linear hypothesis test
## 
## Hypothesis:
## 2 I(lnrcapital^2)  + lnrcapital:lnlabour  + lnrmaterials:lnrcapital = 1
## 
## Model 1: restricted model
## Model 2: lnroutput ~ firm + year + lnrmaterials + lnrcapital + lnlabour + 
##     exports + sector1 + sector2 + sector3 + sector4 + sector5 + 
##     sector6 + sector7 + sector8 + I(lnrcapital^2) + I(lnlabour^2) + 
##     I(lnrmaterials^2) + lnrcapital * lnlabour + lnlabour * lnrmaterials + 
##     lnrmaterials * lnrcapital - sector1 - firm
## 
##   Res.Df    RSS Df Sum of Sq     F    Pr(>F)    
## 1   1922 7308.1                                 
## 2   1921  336.8  1    6971.3 39760 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# ....
# reg4$coefficients
```

#--------------------------------------------
# question 2 g)


```r
linearHypothesis(reg4, "sector2 + sector3 +
                 sector4 + sector5 + sector6 + sector7 + sector8 = 0")
```

```
## Linear hypothesis test
## 
## Hypothesis:
## sector2  + sector3  + sector4  + sector5  + sector6  + sector7  + sector8 = 0
## 
## Model 1: restricted model
## Model 2: lnroutput ~ firm + year + lnrmaterials + lnrcapital + lnlabour + 
##     exports + sector1 + sector2 + sector3 + sector4 + sector5 + 
##     sector6 + sector7 + sector8 + I(lnrcapital^2) + I(lnlabour^2) + 
##     I(lnrmaterials^2) + lnrcapital * lnlabour + lnlabour * lnrmaterials + 
##     lnrmaterials * lnrcapital - sector1 - firm
## 
##   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
## 1   1922 337.54                              
## 2   1921 336.81  1   0.72323 4.1249 0.04239 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# F = 4.1249
qf(.95, 7, length(assign1))
```

```
## [1] 2.706627
```
 
