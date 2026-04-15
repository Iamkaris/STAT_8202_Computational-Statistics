library(ggplot2)

## Target density: Normal BMI
ft = function(x, mu, sigma){
  dnorm(x, mean = mu, sd = sigma)
}

## Proposal distribution: Uniform(a, b)
gt = function(x, a, b){
  if(x >= a && x <= b){
    return(1 / (b - a))
  } else {
    return(0)
  }
}

## Acceptance–Rejection algorithm
ar_algo = function(n_samples, mu, sigma, a, b){
  
  samples = numeric(0)
  
  # Compute M (upper bound)
  M = (b - a) * dnorm(mu, mu, sigma)
  
  while(length(samples) < n_samples){
    x_candidate = runif(1, a, b)   # proposal sample
    u = runif(1)
    
    # Acceptance criterion
    if(u <= ft(x_candidate, mu, sigma) / (M * gt(x_candidate, a, b))){
      samples = c(samples, x_candidate)
    }
  }
  
  x = seq(a, b, length.out = 500)
  df = data.frame(
    x = x,
    samples = samples,
    fx = dnorm(x, mean = mu, sd = sigma)
  )
  
  return(df)
}

## Run the simulation
df = ar_algo(
  n_samples = 10000,
  mu = 25,
  sigma = 4,
  a = 10,
  b = 50
)

## Plot
p_bmi = ggplot(df) +
  geom_histogram(aes(x = samples, y = ..density..),
                 bins = 50, alpha = 0.4, fill = "steelblue") +
  geom_line(aes(x = x, y = fx, colour = "Normal PDF"), linewidth = 1) +
  labs(
    x = "BMI",
    y = "Density",
    title = "Simulated BMI Distribution Using Acceptance–Rejection Method",
    colour = ""
  ) +
  theme_minimal(base_size = 12)

print(p_bmi)



## Monte Carlo siulation
set.seed(8204)

fx = function(x){
  sin(x) + 1
  }
n = 50
# Define integral bounderies
a = 0; b = pi

# Define intergral bounderies
a = 0; b = pi

# Deterministic (evenly spaced)
x_det <- seq(a, b, length.out = n)
y_det <- fx(x_det)
def_det = data.frame(x=x_det, y=y_det, type="Deterministic")

# Monte Carlo ( Random samples)
x_mc <- runif(n, a, b)
y_mc <- fx(x_mc)
df_mc = data.frame(x = x_mc, y=y_mc, type= "MCI")
df = rbind(def_det, df_mc)

ill_plot =(ggplot(df, aes(x=x, y=y, colour=type))
           + geom_line()
           + geom_point()
           + facet_wrap(~ type)
           + theme_minimal()
)

print(ill_plot)

##
set.seed(8204)

fx = function(x){sin(x) + 1}
n = 50 #values btn 0 and pi

# Define integral boundaries
a = 0; b = pi

#Deterministic(Evenly spaced)
x_det <- seq(a,b, length.out = n)
y_det <- fx(x_det)
df_det = data.frame( x=x_det, y=y_det, type = "Deterministic")

#Monte Carlo( Random samples)
x_mc <- runif(n, a, b) #values of x
y_mc <- fx(x_mc)
df_mc = data.frame(x=x_mc, y=y_mc, type="MCI")
df = rbind(df_det, df_mc)

ill_plot = (ggplot(df,aes(x=x, y=y, colour=type))
            +geom_line()
            +geom_point()
            +facet_wrap(~type, scale="free_y")
            +theme_minimal()
)

print(ill_plot)


##............montecarlo example. echo=False.........................

set.seed(8204)
m= 10000
ci = 0.95

##the function

gx_fun =  function(x){exp(-x)}

##Monte carlo estimate
theta_hat_fun = function(gx_fun, x) {
  
  mean(gx_fun(x))
  
}

## Standard error estimate

sigma_hat_fun = function (gx, theta_hat){
  
  v=(1/length(gx))*sum(gx - theta_hat)^2
  sqrt(v)
}

## confidenc einterval

ci_interval_fun = function(theta_hat, sigma_hat, z_critical, m, dp=4){
  
  error_margin = z_critical * sigma_hat/sqrt(m)
  upper = theta_hat +error_margin
  lower = theta_hat - error_margin
  estimate = paste0(round(theta_hat, dp),"[", round(lower,dp),",",round(upper),"]")
}

## x~ u(0,1) random sample
p = 1-(1-ci)/2
z_critical = qnorm(p) #qnirm means quatlies p is probability
x=runif(m,0,1)
gx=gx_fun(x)
theta_hat = theta_hat_fun(gx_fun,x)
sigma_hat = sigma_hat_fun(gx, theta_hat)
estimate=ci_interval_fun(theta_hat, sigma_hat, z_critical, m)
cat("The estimate, together with the 95% CI\n", estimate)


## --Monte-Carlo example 2, echo = False------
# Monte Carlo intergration
## Example 2

## The function
gx_fun = function(x, u) { 
  cdf = 0.5 + mean(gx_fun(x,u))/sqrt(2*pi)
  return(cdf)
}


## Generate for x_i
generate_multiple = function(x, u0 {
  cdf = null
  
}

# 11/02/2026

f = function(x) {
  abs(x-2) +2 * abs(x-1)
  
}
curve(f, from =0, to = 5)
                  
                  
                  
# The golden search algo,, echo= False
golden_section_search = function(f, a, b, tol = 1e-5, max_iter =1000) {
  phi = (1 + sqrt(5)) / 2 # the golden ratio
  resphi = 1/phi
  
  # Initialize points
  x1 = b - resphi * (b-a)
  x2 = a - resphi * (b-a)
  fx1 = f(x1)
  fx2 = f(x2)
    
    
  iter = 0
  while (abs(b-a) > tol && iter < max_iter) {
    if (fx1 <fx2) {
      b = x2
      x2 =x1
      fx2 =fx1
      x1= b -resphi * (b-a)
      fx1 = f(x1)
    } else {
      a = x1
      x1 = x2
      fx1 =fx2
      x2 = a + resphi * (b -a)
      fx2 = f(x2)
    }
    iter = iter +1 #increment
  }
  # Return the midpoint of the final interval
  return((a +b) / 2)
  }


# ---golden - search -ex ----
print(golden_section_search(f, a =0, b =5))

#R in built 
optimize(f, lower =0, upper=5)

f3 = function(x) {
  exp(-x) +x^4
}
curve(f3, from =-2, to = 5)

optimize(f3, lower = -2, upper = 5)

f4 = function(theta) {
  exp(-theta) + theta^4
}

curve(f4, from = -2, to = 5)

# newton raphson example using f3
fprime = function (x) {
  -exp(-x) + 4X^3
}
curve(f, from=-2, to=5)



# Newton - Raphson
newton_raphson = function(x0, tol = 1e-6, max_iter =100) {
  x = x0
  x_vals
}


## ---newton - raphson -ex-compare, echo = True -----
x0 = 0.5 # startup value
est = newt_raphson(x0)
print(est$est)

## in-built R
optim(par =x0, fn =f, method = "BFGS")$par

## ----ewton_raphson-ex1-compare2, echo=True -----
plot(est$vals, type="l")

## ----em-ex-data----
set.seed(8204)
tau_1_true = 0.25


n = 100
read.cs