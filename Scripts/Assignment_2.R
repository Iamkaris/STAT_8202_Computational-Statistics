# Load the data (assuming the file is in your working directory)

data <- read.csv("C:/Users/engungi/OneDrive - Retirement Benefit Authority/Desktop/Classes/Masters/Yr1 Sem 2/STA 8204 - Computational Statistics/STAT_8202_Computational-Statistics/Data/contraceptive_data.csv")

# Assign labels/categories  ---
data$wife_edu <- factor(data$wife_edu, levels = c(1,2,3,4), 
                        labels = c("Low", "Level 2", "Level 3", "High"))

data$hus_edu <- factor(data$hus_edu, levels = c(1,2,3,4), 
                       labels = c("Low", "Level 2", "Level 3", "High"))

data$num_child <- factor(data$num_child, levels = c(1,2,3,4), 
                         labels = c("0", "1-2", "3-4", "5+"))

data$wife_rel <- factor(data$wife_rel, levels = c(0,1), 
                        labels = c("Non-Islam", "Islam"))

data$con_method <- factor(data$con_method, levels = c(1,2,3), 
                          labels = c("No-use", "Long-term", "Short-term"))

# Education Comparison
# Generate proportions for both
w_edu_prop <- prop.table(table(data$wife_edu))
h_edu_prop <- prop.table(table(data$hus_edu))
print(w_edu_prop)
print(h_edu_prop)

#  Women with >4 children
# Category '4' in the raw data (labeled "5+") represents more than 4 children.
gt4_count <- sum(data$num_child == "5+")
print(paste("Number of women with >4 children:", gt4_count))

# --- Step 1d: Distribution of wife's age  ---
hist(data$wife_age, col="steelblue", main="Distribution of Wife's Age", 
     xlab="Age", border="white")
summary(data$wife_age)

# --- Step 1e: Relationship between Contraceptive and Religion  ---
# We use a mosaic plot or a stacked bar chart
plot(data$wife_rel, data$con_method, 
     main="Contraceptive Usage vs Religion", 
     xlab="Religion", ylab="Usage Type", col=c("gray", "blue", "green"))

# --- Step 1f: Median Age Comparison [cite: 23, 24] ---
median_short <- median(data$wife_age[data$con_method == "Short-term"])
median_none <- median(data$wife_age[data$con_method == "No-use"])
print(paste("Median Age (Short-term):", median_short))
print(paste("Median Age (No-use):", median_none))




# Question 2:  Calculate the empirical CDF of the number of children
# ---  Calculate Empirical Probabilities ---
counts <- table(data$num_child)
probs <- prop.table(counts)
cum_probs <- cumsum(probs) # This is our Empirical CDF F(x)

# --- Step 2: Inverse Transform Function ---
inverse_transform <- function(n_samples, cumulative_distribution) {
  # Generate n uniform random numbers
  u <- runif(n_samples)
  
  # Map U to the categories based on cum_probs
  # findInterval returns the index where U falls in the CDF steps
  samples <- findInterval(u, c(0, cumulative_distribution), all.inside = TRUE)
  return(samples)
}

# --- Step 3: Generate Samples ---
set.seed(123) # For reproducibility
n <- nrow(data) # Generate same size as original data
simulated_data <- inverse_transform(n, cum_probs)

# --- Step 4: Comparison ---
# Compare original vs simulated proportions
original_props <- probs
simulated_props <- prop.table(table(simulated_data))

print("Original Data Proportions:")
print(original_props)
print("Simulated Data Proportions:")
print(simulated_props)

# Visualization for comparison
barplot(rbind(original_props, simulated_props), beside=TRUE, 
        col=c("blue", "orange"), legend=c("Original", "Simulated"),
        main="Comparison: Original vs. Simulated Children Count")





#  Question 3, Monte Carlo Estimation ---

# 1. Define the number of MC simulations
m <- 10000 
set.seed(42)

# 2. Generate samples using the Empirical Distribution (from Q2)
# Recall: Category 3 corresponds to '3-4' children
mc_samples <- inverse_transform(m, cum_probs)

# 3. Apply the indicator function: 1 if category is 3, else 0
hits <- ifelse(mc_samples == 3, 1, 0)

# 4. Calculate the MC Estimate (Mean of indicators)
theta_hat <- mean(hits)

# 5. Calculate True Proportion from original data for comparison
true_prop <- prop.table(table(data$num_child))["3-4"]

print(paste("Monte Carlo Estimate:", round(theta_hat, 4)))
print(paste("True Proportion:", round(true_prop, 4)))

# --- Step 3b: Confidence Interval and Plotting ---

# 6. Standard Error calculation
se <- sqrt((theta_hat * (1 - theta_hat)) / m)

# 7. 95% CI bounds
ci_lower <- theta_hat - (1.96 * se)
ci_upper <- theta_hat + (1.96 * se)

# 8. Plotting 
plot(1, theta_hat, ylim=c(ci_lower - 0.05, ci_upper + 0.05), xlim=c(0, 2),
     main="MC Estimate with 95% Confidence Interval",
     xlab="", ylab="Probability Estimate", xaxt='n', pch=19, col="blue")
arrows(1, ci_lower, 1, ci_upper, angle=90, code=3, length=0.1, lwd=2)
abline(h=true_prop, col="red", lty=2) # True value line
legend("topright", legend=c("MC Estimate", "95% CI", "True Value"), 
       col=c("blue", "black", "red"), pch=c(19, NA, NA), lty=c(NA, 1, 2))


# Question 4: 

# --- Step 4a: Calculate Maximum Age ---
# Define the coefficients from the prompt
a <- -0.0008
b <- 0.2
c <- -2.6

# The formula for the vertex of a parabola x = -b / 2a
max_age <- -b / (2 * a)
print(paste("Maximum reproductive age:", max_age))

# --- Step 4b: Compute Average Children for a range of ages ---
ages <- seq(15, 130, by = 1)

# Define the function
calc_children <- function(x) {
  return(-2.6 + 0.2*x - 0.0008*x^2)
}

# Calculate expected values
avg_children <- calc_children(ages)

# --- Step 4c: Plotting ---
plot(ages, avg_children, type="l", lwd=2, col="darkgreen",
     main="Expected Number of Children vs. Age",
     xlab="Age of Woman", ylab="Average Number of Children")

# Add a dotted line for the maximum age (a) 
# Note: Since 125 is far outside the 15-50 range, 
# we show it for theoretical completeness.
abline(v = max_age, col="red", lty=3) 
text(max_age - 5, 2, "Maximum Age", col="red", srt=90)