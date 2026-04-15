install.packages("car")

library(car)

data(Prestige)

attach(Prestige)

#---model y = f(xi) + e

# y = Prestige
# income

plot(income, prestige, xlab = "Average Income", ylab = "Prestige")
lines(lowess(income, prestige, f = 0.5, iter =0), lwd = 2)

# the argument f to lowess gives the spanof the local regression smoother, 
#iter=0, specifies that the local regression should not be refit to downweight outlaying observations

# Extend the model in one above to include two covariates (income and eduction)

mod.lo <- loess(prestige ~ income + education, span = .5, degree = 1)
summary(mod.lo)

# Visualize the above

inc <- seq(min(income), max(income), len = 25)
ed <- seq(min(education), max(education), len = 25)

newdata <- expand.grid(income = inc, education = ed)

fit.prestige <- matrix(predict(mod.lo, newdata), 25, 25)

persp(inc, ed, fit.prestige, theta = 45, phi = 30, ticktype = "detailed", xlab = 'income',
      ylab = 'Education', zlab = 'Prestige', expand  = 2/3, shade = 0.5)

mod.lo.income <- loess(prestige ~ income, span = 0.7, degree =1) # omitting education
mod.lo.ed <- loess(prestige ~ education, span =0.7, degree = 1) # omit income

anova(mod.lo.income, mod.lo) #testing for education

anova(mod.lo.ed, mod.lo) #testing for income

#--- smoothing splines

mod.lo.income

plot(income, prestige)

inc.100 <- seq(min(income), len = 100)
pres <- predict(mod.lo.income, data.frame(income = inc.100))

lines(inc.100, pres, lty = 2, lwd = 2) # loess curve
lines(smooth.spline(income, prestige, df = 3.85), lwd = 2)

# additive Non parametric regression

# yi = a + fi(xi1) + f2(xi2) + .... + fk(xik) + e

# package mgcv

library(mgcv)

mod.gam <- gam(prestige ~ s(income) + s(education))

summary(mod.gam)

plot(mod.gam)


detach(Prestige)

data(Mroz)

attach(Mroz)

k5f <- factor(k5)

k618f <- factor(k618)

# Responce = lfp

# Age(smooth), inc(smoot), k5f, k618f, wc, hc, while modlling choose binomial as a family



# Convert to factors
k5f   <- factor(Mroz$k5)
k618f <- factor(Mroz$k618)

# Fit GAM: 
mod_mroz <- gam(lfp ~ s(age) + s(inc) + k5f + k618f + wc + hc,
               family = binomial,
               data = Mroz)

summary(mod_mroz)
