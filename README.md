---
title: "Bayes Multi Testing"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library the packages
```{r}
library(MCMCpack)
library(descr)
library(ggplot2)
library(BEST)
library(psych)
library(lavaan)
library(semPlot)
library(lme4)
library(brms)
library(MASS)
library(semTools)
```
Generate the data
```{r}
power_bayes_multi = function(){
n = 15
timepoints = 6
time = timepoints-1
time = rep(0:time, times=n)
subject = rep(1:n, each=timepoints)
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), n)
intervention = rep(intervention, each = timepoints)

intercept = 0
slopeT = .25
slopeI = .25
slopeTI = .25
randomEffectsCorr = matrix(c(.2,.2,.2, .2), ncol = 2)

randomEffects = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)
dim(randomEffects)
colnames(randomEffects) = c("Int", "SlopeT")

sigma = .05
y1 = (intercept + randomEffects$Int[subject])+(slopeT + randomEffects$SlopeT[subject])*time + slopeI*intervention + slopeTI*time*intervention+ rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, intervention, y1)

brms_model_prior = brm(y1 ~ time*intervention + (1|subject), data = d, prior = set_prior("student_t(1000,.25,.05)", class = "b", coef = "time:intervention"))
brms_model_prior_summary = summary(brms_model_prior)
brms_model_prior_summary$fixed[,3:4]
}

```
Now try running it twice
```{r}
reps = 2
power_rep = replicate(reps, power_bayes_multi())
```


LME Result
```{r}
model1 = lmer(y1 ~ time*intervention + (1|subject), data = d)
summary(model1)
confint(model1)
```
Now run brms
Here for information on priors: https://www.mathworks.com/help/stats/t-location-scale-distribution.html

As v gets bigger, we are more certain and the standard error goes down
V says longer tails (more probaiblity for outliers)

The informed prior does not seem to be making much of an improvement?
```{r}
brms_model_prior = brm(y1 ~ time*intervention + (1|subject), data = d, prior = set_prior("student_t(1000,.25,.05)", class = "b", coef = "time:intervention"))

summary(brms_model_prior)

brms_model_prior_low = brm(y1 ~ time*intervention + (1|subject), data = d, prior = set_prior("student_t(100,.25,.6)", class = "b", coef = "time:intervention"))

summary(brms_model_prior_low)



hypothesis(brms_model_prior, "time - intervention > 0", class = "b", group = "subject")

brms_model = brm(y1 ~ time*intervention + (1|subject), data = d)
summary(brms_model)

```
Try running a loop with brms package
```{r}

```


