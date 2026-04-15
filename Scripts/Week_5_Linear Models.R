
install.packages("faraway")
## ----setup0, echo=FALSE---------------------------------------------------
library(ggplot2)
library(dplyr)
library(faraway)

knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  out.width = '50%',
  out.height = '50%',
  fig.align = "center",
  dev = 'pdf',
  tidy.opts = list(width.cutoff = 60),
  tidy = TRUE
)

set.seed(8204)


## ----example1-r, echo=FALSE-----------------------------------------------
## Using R functions
data(gala, package = "faraway")
## ----example1-r, echo=FALSE-----------------------------------------------
## Using R functions
data(gala, package="faraway")
mod1 = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)


## ----example1-r-out, echo=FALSE-------------------------------------------
summary(mod1)


## ----example1-own-code, echo=FALSE----------------------------------------
## Own implementation
## Generate the X matrix



## ----example1-own-code, echo=FALSE----------------------------------------
## Own implementation
## Generate the X matrix
x = model.matrix(~ Area + Elevation + Nearest + Scruz + Adjacent, gala)

## Outcome variable
y = gala$Species

## Compute (X^T X)^-1
xtxi = solve(t(x) %*% x)

## Compute beta hat  (X^T X)^-1 X^T y
beta = xtxi %*% t(x) %*% y
print(beta)

names(mod1)

mod1_summ = summary(mod1)
names(mod1_summ)

### Sigma estimate
sqrt(deviance(mod1) / df.residual(mod1))
mod1_summ$sigma


## ----example1-betasd, echo=TRUE-------------------------------------------
### Compute standard error of beta hat
xtxi = mod1_summ$cov.unscaled
sqrt(diag(xtxi)) * mod1_summ$sigma

### We can also get them directly
mod1_summ$coef[, 2]


## ----example1-rsquare, echo=TRUE------------------------------------------

### Compute R square
1 - deviance(mod1) / sum((y - mean(y))^2)

### In-built
mod1_summ$r.squared



# Load package and data
data(savings)   # Loads 'savings' data frame
str(savings)    # Peek at variables: sr, pop15, pop75, dpi, ddpi

# Model: sr ~ pop15 + pop75 + dpi + ddpi
mod_sav <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

# Model summary (coefficients, SEs, t, p, R^2, etc.)
summary(mod_sav)

# Coefficients (beta-hat)
coef(mod_sav)

# Residual standard error (sigma)
summary(mod_sav)$sigma

# R-squared and Adjusted R-squared
summary(mod_sav)$r.squared
summary(mod_sav)$adj.r.squared

# Variance-covariance of betas and their standard errors
vc <- vcov(mod_sav)
se <- sqrt(diag(vc))
cbind(Estimate = coef(mod_sav), SE = se)

#Residual vs fitted

plot(fitted(mod_sav), residuals(mod_sav), xlab = 'fitted', ylab = 'Residual')
abline(h=0) # Residuals are fitted as seen, assue linear

#Non-constant variance
plot(fitted(mod_sav),abs(residuals(mod_sav)), xlab = 'fitted', ylab = 'Residual')
summary(lm(abs(residuals(mod_sav))~fitted(mod_sav)))

## transform using sqrt
mod1 = lm(sqrt(Species) ~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)

# Load Library
library(MASS)

# Load the dataset
data("mtcars")

# Define the selected Model 
full_model = lm(mpg ~ ., data = mtcars) # model with all predictors
null_model = lm(mpg ~ 1, data = mtcars) # model with no predictors (only intercept)

# perform forward selection using stepwise AIC
forward_model = step(null_model
                     , scope = list(lower = null_model, upper = full_model)
                     , direction = "forward", trace = TRUE)
#Display the selected model
summary(forward_model)
summary(full_model)


# perform forward selection using stepwise AIC
full_model = lm(mpg ~ ., data = mtcars) # model with all predictors

best_model = step(full_model, direction = "backward", trace = TRUE)

#Display the selected model
summary(best_model)


## Model Diagnositc, echo=False----
## Fit the model
mod_cars= lm(MPG.highway ~ Weight + Wheelbase + Fuel.tank.capacity + Width, data=Cars93)


# We can directly get the plot of fitted against the residuals, QQ-plot, and index of Cook's distance
par(mfrow = c(2, 2))
plot(mod_cars)
par(mfrow=c(1,1))

# We can identify the outliers in the data
# rstudent() extracts standardized residuals
plot(rstudent(mod_cars), col = 2)
identify(rstudent(mod_cars), rstudent(mod_cars), label = Cars93$Make)

# We can inspect leverages using the lm.influence()
plot(lm.influence(mod_cars)$hat, type = "h", col = 2)
identify(lm.influence(mod_cars)$hat, label = Cars93$Type)

# FOR YOU: Update the model to exclude the influential points
# dfbetas
# Cook's distance



# Let us fit a bivariate model
mod_bi <- lm(MPG.highway ~ Weight, data = Cars93)
summary(mod_bi)

# boxcox() function in MASS package helps us generate profile for lambda values, suitable for transformation
boxcox(mod_bi, lambda = seq(-2, 2, by = 0.1))

# The plot suggests a reciprocal (lambda = -1) transformation
y <- Cars93$MPG.highway
x <- Cars93$Weight
y2 <- -(1/(y^(-1)))
mod_bi_2 <- lm(y ~ x)
summary(mod_bi_2)




## ----factors_exp1, echo=TRUE---------------------------------------------

# Polynomial regression model (y = b0 + b1x1 + b2x2^2 + b3x1x2 + e)
library(dplyr)

df <- iris
avg_df <- df |>
  group_by(Species) |>
  summarize(sepal_mean = mean(Sepal.Length),
            sepal_sd   = sd(Sepal.Length)
  )

print(avg_df)



## ----factors_exp2, echo=TRUE----------------------------------------------

# Generate data
nsamples <- 100
df <- data.frame(
  gender = sample(c(0, 1), size = nsamples, replace = TRUE),
  age = runif(nsamples, 18, 100)
)
head(df, 3)

# Convert 0, 1 in gender to factor
df <- (df
       |> mutate(gender = factor(gender, levels = c(0, 1), labels = c("Female", "Male"))))
head(df, 3)


## ---anova_ex1, echo=FALSE------------

plot(Cars93$AirBags, Cars93$Max.Price)
lm_anova <- lm(Max.Price ~ AirBags, data = Cars93)
summary(lm_anova)
plot(lm_anova)

# Notice that R ensures that the columns of the model matrix are not linearly dependent by excluding
# one level from the linear model. The validity of analysis of variance results is dependent on constant
# variance within groups. We can see from the diagnostic plots that this is not entirely unreasonable for these data.

MaxP0 <- Cars93$Max.Price[Cars93$AirBags == "None"]
MaxP1 <- Cars93$Max.Price[Cars93$AirBags == "Driver only"]
