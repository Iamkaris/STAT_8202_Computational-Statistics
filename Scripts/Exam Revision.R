# 1. Define the function to integrate
h <- function(x) {
  -4*x^3 + 3*x^2 + 15*x + 35
}

# 2. Set the limits and number of simulations
a <- -3
b <- 15
N <- 100000  # High N for better accuracy

# 3. Generate random x values uniformly between a and b
x_values <- runif(N, min = a, max = b)

# 4. Calculate the function height at these points
heights <- h(x_values)

# 5. Apply the Monte Carlo formula: (b - a) * average height
mc_integral <- (b - a) * mean(heights)

# 6. Print the result
print(mc_integral)


# 1. Setup
N <- 10000          # Total iterations
samples <- numeric() # Vector to store accepted values
M <- 3               # Our calculated constant

# 2. Rejection Loop
while(length(samples) < 2000) { # Stop when we have 2000 'accepted' samples
  
  # Step A: Generate a candidate X from g(x) using Inverse Transform
  u1 <- runif(1)
  x_cand <- 1 - sqrt(1 - u1)
  
  # Step B: Generate a uniform height to decide acceptance
  u2 <- runif(1)
  
  # Step C: The Acceptance Condition
  # Accept if u2 <= f(x) / (M * g(x))
  f_x <- 6 * x_cand * (1 - x_cand)
  g_x <- 2 * (1 - x_cand)
  
  if (u2 <= f_x / (M * g_x)) {
    samples <- c(samples, x_cand)
  }
}

# 3. Check Results
hist(samples, prob=TRUE, main="Acceptance-Rejection Results")
curve(6*x*(1-x), add=TRUE, col="red", lwd=2)


generate_lcg <- function(n = 1000, x0 = 27, a = 7, c = 43, m = 100) {
  x <- numeric(n)
  x[1] <- x0
  for (i in 2:n) {
    x[i] <- (a * x[i-1] + c) %% m
  }
  return(x)
}

# Generate 1000 random numbers
nums <- generate_lcg()
head(nums)

hist(nums, main="Histogram of LCG Output", xlab="Values", col="lightblue")


# 1. Define the parameters
x_0 <- 27
a <- 7
c <- 43
m <- 97
n_samples <- 1000

# 2. Function to generate LCG numbers
generate_lcg <- function(n, seed, a, c, m) {
  random_nums <- numeric(n)
  current_x <- seed
  
  for(i in 1:n) {
    current_x <- (a * current_x + c) %% m
    random_nums[i] <- current_x / m  # Scale to (0,1)
  }
  return(random_nums)
}

# 3. Generate the numbers
my_randoms <- generate_lcg(n_samples, x_0, a, c, m)

# 4. Simple Randomness Test (Autocorrelation Plot)
# If numbers are random, a value shouldn't be predictable from the one before it.
acf(my_randoms, main="Checking for Randomness Patterns")

mean(my_randoms) # Should be close to 0.5

# 1. Define the function h(x)
h <- function(x) {
  -x^3 + 6*x^2 - x + 17
}

# 2. Set the integration limits
a <- -2
b <- 5
N <- 100000  # Number of random "darts"

# 3. Generate random x values uniformly between a and b
x_values <- runif(N, min = a, max = b)

# 4. Calculate the height of the function at those x values
heights <- h(x_values)

# 5. Monte Carlo Formula: (Width) * (Average Height)
mc_estimate <- (b - a) * mean(heights)

# 6. Display the result
print(paste("The estimated integral value is:", mc_estimate))

# Calculate the Variance of the heights
var_heights <- var(heights)

# Calculate the Variance of the Monte Carlo Estimate
var_estimate <- ((b - a)^2 * var_heights) / N

# Calculate the Standard Error (Square root of variance)
std_error <- sqrt(var_estimate)

print(paste("The Variance of the estimate is:", var_estimate))
print(paste("The Standard Error is:", std_error))

install.packages("GGally")


# 1. Set the parameters
N <- 10000
lambda <- 1

# 2. Method A: The Inverse Transform Method (Our manual formula)
set.seed(123) # For reproducibility
U <- runif(N)
x_transform <- -log(U) / lambda

# 3. Method B: R's Built-in Generator
x_builtin <- rexp(N, rate = lambda)

# ---------------------------------------------------------
# COMPARISON 1: Summary Statistics (Mean and Variance)
# ---------------------------------------------------------
cat("Transform Method - Mean:", mean(x_transform), " Variance:", var(x_transform), "\n")
cat("Built-in Method  - Mean:", mean(x_builtin), " Variance:", var(x_builtin), "\n")

# ---------------------------------------------------------
# COMPARISON 2: Visual Comparison (Density Plot)
# ---------------------------------------------------------
# Plot the Transform method in blue
plot(density(x_transform), col="blue", lwd=2, main="Comparing Exponential Generators", xlab="X")
# Overlay the Built-in method in red (dashed)
lines(density(x_builtin), col="red", lwd=2, lty=2)
legend("topright", legend=c("Transform (-ln(U))", "Built-in (rexp)"), col=c("blue", "red"), lty=c(1,2), lwd=2)

# ---------------------------------------------------------
# COMPARISON 3: Formal K-S Test
# ---------------------------------------------------------
# Let's use the K-S test to see if the two samples come from the same distribution!
ks_test_result <- ks.test(x_transform, x_builtin)
print(ks_test_result)


data <- read.csv("C:/Users/engungi/Downloads/gapminder.csv")
head(data)


# subset a dataset
data_1982 <- subset(data, year==1982)

head(data_1982)

# data set for the American countries in 1997
data_america_1987 <- subset(data, year == 1987 & continent== "Americas")

head(data_america_1987)

# Insert a column on gdp

data$gdp <- data$pop * data$gdpPercap

head(data)

# extract a row
# Do it all in one line!
subset(data, country == "Afghanistan" & year == 1982)$lifeExp

# Minimizing Function

f <- function(x) {
  abs(x - 3.5) + (x-2)^2
}

a <- 0 # Left boundary
b <- 5 # Right boundary
tolerance <- 0.001 # Tolerance
r <- (sqrt(5) - 1)/2 # r value

while((b-a) > tolerance) {
  
  # Calculate the interiour points
  
  x1 = b - r*(b - a)
  x2 = a + r*(b - a)
  
  # Evaluate to decide which boundary to remove
  
  if (f(x1) < f(x2)) {
    b <- x2
  }
  else {
    a <- x1
  }
}

best_x <- (a + b)/2
print(best_x)


genes_data <- data.frame(
  group_1 = c(2, 3, 1, 2),
  group_2 = c(8, 7, 9, 8),
  group_3 = c(11, 12, 13, 12)
)

head(genes_data)

# 3. Run the ANOVA 
# We test if 'values' depend on 'ind' (the indicator/group)
my_model <- aov(values ~ ind, data = long_data)

# 4. View the results
summary(my_model)


mmr_data <- read.csv("C:/Users/engungi/Downloads/mmr.csv")
head(mmr_data)

mmr_2017 <- subset(mmr_data, mmr_data$year == 2017)

sorted_mmr <- mmr_2017[order(mmr_2017$mmr, decreasing = TRUE), ]
head(sorted_mmr)

top_10 <- sorted_mmr[1:10, ]

top_10_sorted <- top_10[order(top_10$iso), ]
head(top_10_sorted)

final_table <- top_10_sorted[ , c("Country", "MMR_estimate", "MMR_lower", "MMR_upper")]

head(final_table)
