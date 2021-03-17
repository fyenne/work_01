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
qf(.95, 1, 1935)
```

```
## [1] 3.846269
```

```r
# anova(reg1)
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
m = 1; n = 1936; k = 3
F_value_q1 = ((0.4642 - 0.4524)/m) / ((0.4524)/(n-k-1))
F_value_q1
```

```
## [1] 50.39257
```

```r
qf(.95, 3, 1936)
```

```
## [1] 2.609499
```


```md
H0: the two models are different.
F_value = `r F_value_q1`, which is larger than `r qf(.95, 3, 1936)`, hence we reject H0. The two models are not different, which implies that ($\beta_K$ + $\beta_L$ + $\beta_M$) = 1
```

### c-iii) [8 marks] Without using the car package in R, test the hypothesis of constant returns to scale, for model (2), at a 5% level of significance using a t-test.

## d) [6 marks] Test the hypothesis that model (2) is correctly specified using the Ram- sey RESET test, at the 5% level, using both the squares and the cubes of the fitted values. What is your conclusion?

## e) Suppose this alternative model has been estimated using the data in assign1.csv and a value of R2 = 0.8749 was obtained. Consider the following statement: “Given the sample of data, the econometric model (2) is preferred to the alternative econometric model (3) because it has a larger R2.” Do you agree with this statement. Why or why not? 

---

# Question 2 [Total 40 marks] Consider the following extended version of model (1):
