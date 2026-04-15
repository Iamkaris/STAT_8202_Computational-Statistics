x1 <- rep(c("A", "B", "C"), 3)
x1
y1 <- rep(gender, 10)
y1

age <- seq(20, 80, length.out=100)
age

sd(age)
var(age)
income <- seq(100, 1000, length.out=100)
income
cor(age, income)

cov(age, income)

## people older than 20
age_cut <- 20
age[age>age_cut]

## people older than 20 and above
age[age>=age_cut]

## people with ages between 30 and 50
age[age>30 & age<50]

n = 1000
u = runif(n)
x = u^(1/3)

## Density function f(x)
y = seq(0, 1, length.out=n)
fx = 3*y^2
df = data.frame(x = x, y = y, fx = fx)


p1 = (ggplot(df)
      + geom_histogram(aes(x=x, y = ..density..), alpha=0.3)
      + geom_line(aes(x=y, y=fx, colour="f(x)"))
      + labs(fill="", colour="", title = expression(f(x)==3~x^2))
      + theme_minimal(base_size = 12)
)
print(p1)



# 1. Define the "Mountain" (The Target Distribution)
# We want a Standard Normal curve. We can ignore the constant 1/sqrt(2*pi) 
# because it cancels out when we divide the new height by the old height!
target_pdf <- function(x) {
  exp(-0.5 * x^2)
}

# 2. Setup the variables
n_steps <- 10000          # How many footprints to leave
samples <- numeric(n_steps) # Empty list to hold our footprints
current_x <- 0            # Start exactly in the middle

# 3. Start the MCMC Loop
for (i in 1:n_steps) {
  
  # Step A: Propose a random jump (up to 1 unit left or right)
  proposed_x <- current_x + runif(1, min = -1, max = 1)
  
  # Step B: Calculate the Acceptance Ratio (New Height / Old Height)
  ratio <- target_pdf(proposed_x) / target_pdf(current_x)
  
  # Step C: Accept or Reject based on the coin flip
  if (runif(1) < ratio) {
    current_x <- proposed_x  # The move was accepted! Update our position.
  }
  # Note: If rejected, we do nothing. current_x stays exactly where it was.
  
  # Step D: Log our footprint
  samples[i] <- current_x
}

# 4. Plot the results!
# We drop the first 1000 steps as "burn-in" while the climber finds the mountain
valid_samples <- samples[1000:n_steps]

hist(valid_samples, breaks = 50, probability = TRUE, 
     main = "MCMC: Metropolis-Hastings", xlab = "X")
curve(dnorm(x), add = TRUE, col = "red", lwd = 2) # Overlay the true mathematical curve


# Inverse Transform Method

lambda <- 0.5
n <- 10000
u <- runif(n, min = 0, max = 1)
simulated_x <- -(1/lambda)*log(1-u)

hist(simulated_x, breaks = 50,
     main = ("Histogram of Simulated Numbers"),
     probability = TRUE)

curve(dexp(x, rate = lambda), add = TRUE, col = "red", lwd = 2)

# Acceptance Rejection Method

lambda <- 0.5
M <- 15
n <- 10000
samples <- numeric(0)

# Start the loop
while (length(samples) < n) {
  candidate <- runif(1, min = 0, max = M)
  
  u <- runif(1, min = 0, max = 1)
  
  # Calculate the target density (f_t) and proposal density (g_t)
  f_t <- lambda * exp(-lambda * candidate)
  g_t <- 1 / M
  
  if (u <= (f_t / (M * g_t))) {
    samples <- c(samples, candidate)
  }
}

hist(samples, breaks = 50, probability = TRUE)
curve(lambda * exp(-lambda * x), add = TRUE, col = "red", lwd = 2)

threshold <- 180
n_samples <- 1000000

# ==========================================
# METHOD 1: Naive Monte Carlo (The Slow Way)
# ==========================================
# We sample from the true distribution: Normal(mean = 120, sd = 15)
naive_samples <- rnorm(n_samples, mean = 120, sd = 15)

# Count how many are above 180
naive_hits <- sum(naive_samples > threshold)
naive_prob <- naive_hits / n_samples

print(paste("Naive hits:", naive_hits))



hist(naive_hits, breaks = 10, probability = TRUE)


samples <- rnorm(10000, mean = 180, sd = 1)

weight <- dnorm(samples, mean = 120, sd = 15) / dnorm(samples, mean = 180, sd = 15)

prob <- mean((samples > 180) * weight)
prob

# Building  a basic LM
library(faraway)
data(gala)

head(gala)

str(gala)

model_1 <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(model_1)

# (2 rows, 3 columns) so we can see 6 charts at once
par(mfrow = c(2, 4))

# 3. Draw the histograms
# The '$' symbol tells R: "Look inside the 'gala' dataset and grab this specific column"
hist(gala$Species, main = "Total Species (y)", col = "lightblue", xlab = "Species Count")
hist(gala$Area, main = "Island Area", col = "salmon", xlab = "Area")
hist(gala$Elevation, main = "Elevation", col = "wheat", xlab = "Elevation")
hist(gala$Nearest, main = "Dist to Nearest Island", col = "plum", xlab = "Distance")
hist(gala$Scruz, main = "Dist to Santa Cruz", col = "lightcyan", xlab = "Distance")
hist(gala$Adjacent, main = "Area of Adjacent Island", col = "lightgray", xlab = "Area")

# 4. Reset the plotting screen back to normal (1 chart at a time)
par(mfrow = c(1, 1))

# Constant Variance
plot(fitted(model_1), residuals(model_1), abline(h = 0))

# normality

qqnorm(residuals(model_1))
qqline(residuals(model_1))

shapiro.test(residuals(model_1))


# Transformed Model
model_sqr <- lm(sqrt(Species) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(model_sqr)

# Check new variance
plot(fitted(model_sqr), residuals(model_sqr), abline(h = 0))

# QQ Plot
qqnorm(residuals(model_sqr))
qqline(residuals(model_sqr))

shapiro.test(residuals(model_sqr))

par(mfrow = c(2, 2))
plot(model_sqr)
par(mfrow = c(1,1))

library(lmtest)
dwtest(model_sqr)

df$gender <- factor(df$gender,levels = c(0, 1), labels = c("Male", "Female") )


library(mgcv)
#install.packages("ISLR")
library(ISLR)
data(Wage)
head(Wage)

wage_2003 <- Wage[Wage$year == 2003,]
df <- data.frame(
  wage = wage_2003$wage,
  age = wage_2003$age,
  marital_status = wage_2003$maritl,
  education = wage_2003$education
)

head(df)

unique(df$marital_status)

df$marital_status <- factor(df$marital_status)
levels(df$marital_status)

model_1 <- lm(wage ~ age + marital_status + education, data = df)

summary(model_1)

plot(model_1)
shapiro.test(residuals(model_1))
library(lmtest)
dwtest(model_1)

bptest(model_1)

model_gam <- gam(wage ~ s(age) + marital_status + education, data = df)
summary(model_gam)

plot(model_gam)

# 1. Set up the parameters
n_samples <- 5000
rho <- 0.8                            # High correlation
cond_variance <- sqrt(1 - rho^2)      # The standard deviation for our conditionals

# Create empty baskets (vectors) to store our chain of samples
x_chain <- numeric(n_samples)
y_chain <- numeric(n_samples)

# 2. Initialize (Start at a random point)
x_chain[1] <- 0
y_chain[1] <- 0

# 3. The Gibbs Sampler Loop
# Notice how x takes a step using the PREVIOUS y, 
# and then y immediately uses the NEW x!
for (i in 2:n_samples) {
  
  # Step A: Sample new X given the old Y
  old_y <- y_chain[i - 1]
  x_chain[i] <- rnorm(1, mean = rho * old_y, sd = cond_variance)
  
  # Step B: Sample new Y given the NEW X
  new_x <- x_chain[i]
  y_chain[i] <- rnorm(1, mean = rho * new_x, sd = cond_variance)
  
}

# 4. Visualize the results
# Discard the first 500 points as "burn-in" (where the chain was finding its way)
plot(x_chain[500:n_samples], y_chain[500:n_samples], 
     pch = 16, col = rgb(0, 0, 1, 0.2), 
     main = "Gibbs Sampler: Bivariate Normal", xlab = "X", ylab = "Y")
