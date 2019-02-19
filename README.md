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
library(psych)
library(lavaan)
library(semPlot)
library(lme4)
library(brms)
library(MASS)
library(semTools)
library(dplyr)
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

brms_model_prior = brm(y1 ~ time*intervention + (1|subject), data = d, prior = set_prior("student_t(500,.25,.05)", class = "b", coef = "time:intervention"))
brms_model_prior_summary = summary(brms_model_prior)
lower = brms_model_prior_summary$fixed[4,3]
upper = brms_model_prior_summary$fixed[4,4]
lower = ifelse(lower < 0,1,0)
upper = ifelse(upper > 0,1,0)
power = sum(lower, upper)
power
}
```
Now try running it twice
```{r}
reps = 2
power_rep = replicate(reps, power_bayes_multi())
power_rep_sum = sum(power_rep)
power_bayes= power_rep_sum/reps
power_bayes
```
Now try with a loop for the number of people
```{r}
power_bayes_multi_n = function(){
# Generate the blank data sets where we will put the data
n = list(30,40)
intervention_out = list()
y = list()
time_out = list()
randomEffects_out = list()
subject_out = list()
d = list()

### Effects
intercept = 0
time_effect = .25
intervention_effect = .25
time_intervention_effect = .25

#Set time points
timepoints = 6
time = timepoints-1

for(i in 1:length(n)){
  time_out[[i]] = rep(x = 0:time, times=n[[i]])
  intervention_out[[i]]= c(rep(1,round(n[[i]]*.5,1)), rep(0,round(n[[i]]*.5,0)))
  randomEffectsCorr = matrix(c(.2,.2,.2, .2), ncol = 2)
  randomEffects_out[[i]] = mvrnonnorm(n[[i]], mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
  randomEffects_out[[i]] = data.frame(randomEffects_out[[i]])
  colnames(randomEffects_out[[i]]) = c("Int", "SlopeT")
  subject_out[[i]] = rep(1:n[[i]], each=timepoints)
  ## We are assuming a random intercepts model only
  y[[i]] = (intercept + randomEffects_out[[i]]$Int[subject_out[[i]]]) + time_effect*time_out[[i]] + intervention_effect*intervention_out[[i]] + time_intervention_effect*time_out[[i]]*intervention_out[[i]] + rnorm(n[[i]]*timepoints, mean = 0, sd = .05)
  d[[i]] = data.frame(y = y[[i]], intervention = intervention_out[[i]], time = time_out[[i]], subject = subject_out[[i]])
}

#Generate empty data sets to fill
brms_model_prior_out = list()
brms_model_prior_summary_out = list()
lower_out = list()
upper_out = list()
power_out = list()

for(i in 1:length(d[[i]])){
brms_model_prior_out[[i]] = brm(y ~ time*intervention + (1|subject), data = d[[i]], prior = set_prior("student_t(500,.25,.05)", class = "b", coef = "time:intervention"))
brms_model_prior_summary_out[[i]] = summary(brms_model_prior_out[[i]])
lower_out[[i]] = brms_model_prior_summary_out[[i]]$fixed[4,3]
upper[[i]] = brms_model_prior_summary_out[[i]]$fixed[4,4]
lower_out[[i]] = ifelse(lower_out[[i]] < 0,1,0)
upper[[i]] = ifelse(upper[[i]] > 0,1,0)
power[[i]] = sum(lower[[i]], upper[[i]])
power[[i]]
}
}

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


