# Problem Set #3: Regression Diagnostics, Interaction Terms, and Missing Data
Soo Wan Kim  
May 11, 2017  



## Regression diagnostics

Estimate the following linear regression model of attitudes towards Joseph Biden:

Y= \beta_0 + \beta_1X_1

where Y is the Joe Biden feeling thermometer, X1 is age, X2 is gender, and X3 is education. Report the parameters and standard errors.




For this section, be sure to na.omit() the data frame (listwise deletion) before estimating the regression model. Otherwise you will get a plethora of errors for the diagnostic tests.

### Part 1
**Test the model to identify any unusual and/or influential observations. Identify how you would treat these observations moving forward with this research. Note you do not actually have to estimate a new model, just explain what you would do. This could include things like dropping observations, respecifying the model, or collecting additional variables to control for this influential effect.**



### Part 2
**Test for non-normally distributed errors. If they are not normally distributed, propose how to correct for them.**

### Part 3 
**Test for heteroscedasticity in the model. If present, explain what impact this could have on inference.**

### Part 4
**Test for multicollinearity. If present, propose if/how to solve the problem.**



### Interaction terms

Estimate the following linear regression model:



where Y is the Joe Biden feeling thermometer, X1 is age, and X2 is education. Report the parameters and standard errors.

Again, employ listwise deletion in this section prior to estimating the regression model.

#### Part 1
Evaluate the marginal effect of age on Joe Biden thermometer rating, conditional on education. Consider the magnitude and direction of the marginal effect, as well as its statistical significance.

#### Part 2
Evaluate the marginal effect of education on Joe Biden thermometer rating, conditional on age. Consider the magnitude and direction of the marginal effect, as well as its statistical significance.



### Missing data

Estimate the following linear regression model of attitudes towards Joseph Biden:



where Y is the Joe Biden feeling thermometer, X1 is age, X2 is gender, and X3 is education. This time, use multiple imputation to account for the missingness in the data. Consider the multivariate normality assumption and transform any variables as you see fit for the imputation stage. Calculate appropriate estimates of the parameters and the standard errors and explain how the results differ from the original, non-imputed model.


