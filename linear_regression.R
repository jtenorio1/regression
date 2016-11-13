#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
summary(sat.voting.mod)

sat.mod <- update(sat.mod, data=na.omit(states.data))
summary(sat.mod)
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Congressional voting patterns DO better predict SAT scores over and above expense. 
## The model using the voting data has a greater R-square 

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
sts.ex.energy <- subset(na.omit(states.data), select = c("metro", "energy"))
summary(sts.ex.energy)
plot(sts.ex.energy)

##   2. Print and interpret the model `summary'
energy.mod <- lm(energy ~ metro, data = sts.ex.energy)
summary(energy.mod)

energy.mod$residuals
SSE_metro <- sum(energy.mod$residuals^2)    #580411
RMSE_metro <- sqrt(SSE_metro/nrow(sts.ex.energy))  #109.963

## the resulting model using just metro as the independent variable is not reliable with an R-square of 0.097
## The model needs to be improved in order to make it more presentative of the data. The metro attribute by itself is significant
## with a p-value of 0.031, but not enough to create a model that captures enough of the variation in the response variable. 

##   3. `plot' the model to look for deviations from modeling assumptions
plot(energy.mod) # "which" argument optional
plot(energy.mod, which = c(1, 2))


##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

sts.ex.energy2 <- subset(na.omit(states.data), select = c("metro", "energy", "density", "waste", "green"))
summary(sts.ex.energy2)
energy2.mod <- lm(energy ~ metro + density + green + waste, data = sts.ex.energy2)
summary(energy2.mod)

SSE <- sum(energy2.mod$residuals^2)    #247,051
RMSE <- sqrt(SSE/nrow(sts.ex.energy))  #71.74

##  the resulting model is significantly better than the model using solely metro as the predictor. 
##  the greenhouse gas indicator is a much better predictor of energy use. We can improve the model
##  by getting rid of insignificant predictors such as waste.

energy2.mod <- lm(energy ~ metro + density + green, data = sts.ex.energy2)
summary(energy2.mod)

SSE <- sum(energy2.mod$residuals^2)    #248,962
RMSE <- sqrt(SSE/nrow(sts.ex.energy))  #72.02

## Although the R-square decreased, it only did so slightly; this justifies our elimination of 
## waste as a predictor. Furthermore, the adjusted R-square increased, which further supports
## the elminiation of waste as a factor. Let's try removing metro now. 

energy2.mod <- lm(energy ~ density + green, data = sts.ex.energy2)
summary(energy2.mod)

SSE <- sum(energy2.mod$residuals^2)    #252,985
RMSE <- sqrt(SSE/nrow(sts.ex.energy))  #72.598

## Again, our r-square decreased, but only slightly. The adjusted r-square increased, so we are
## justified in removing metro as a predictor. Let's see the effect of removing density:

energy2.mod <- lm(energy ~ green, data = sts.ex.energy2)
summary(energy2.mod)

SSE <- sum(energy2.mod$residuals^2)    #261,096
RMSE <- sqrt(SSE/nrow(sts.ex.energy))  #73.753


## Lastly, the r-square decreased again (only slightly), but the adjusted r-square decreased
## as well. Given the continued lack of significance of using density as a predictor and the small
## overall drop in the r-square (and adjusted r-square), we are justified in using green as our 
## only predictor in the model. 


## CONCLUSION: The model using only the predictor "green" is better than the model using only "metro". 
## This model has a much greater r-square (.594), than the model using metro (.097). The "green" model
## also benefits from having a lower RMSE (73.75) versus the "metro" model (109.96).

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
summary(sat.expense.by.percent)

#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table
  
## The association between expense and SAT scores does not depend on the median income in the state 

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 

summary(sat.region)

#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

sts.ex.energy3 <- subset(na.omit(states.data), select = c("energy", "green", "toxic"))
energy3.mod <- lm(energy ~ green*toxic, data = sts.ex.energy3)

summary(energy3.mod)
coef(summary(energy3.mod)) # show regression coefficients table

plot(energy3.mod)

## The interaction between "green" and "toxic" is significant. The association between "green" and energy"
## is dependent on "toxic". 

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

sts.ex.energy4 <- subset(states.data, select = c("energy", "green", "region"))
energy4.mod <- lm(energy ~ region, data = sts.ex.energy4)
summary(energy4.mod)

coef(summary(energy4.mod)) # show regression coefficients table
anova(energy4.mod) # show ANOVA table

plot(energy4.mod)

#  when we add region to the model, we can see there are significant differences by region.
#  The most significant difference is between the East and all the other regions 




