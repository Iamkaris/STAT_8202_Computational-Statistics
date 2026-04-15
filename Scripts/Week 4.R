set.seed(234)
tau_1_true = 0.25
x = y = rep(0,10000)

for (i in 1:1000) {
  if(runif(1) < tau_1_true){
    x[i] = rnorm(1, mean = 1)
    y[i] = "heads"
  } else {
    x[i] = rnorm(1, mean=7)
    y[i] = "tails"
  }
}

library(lattice)
print(densityplot(~x,
                  main = "Density of a Mixture Distribution",
                  xlab = "Value",
                  plot.points = FALSE))


mu_1 = 0
mu_2 = 1
tau_1 = 0.5
tau_2 = 0.5

t1 = tau_1 * dnorm(x, mean=0)
t2 = tau_2 * dnorm(x, mean= 1)
P_1 = t1 / (t1 + t2)
P_2 = t2 /(t1 + t2)
## Em Step
mu_1 = sum(P_1 * x) / sum(P_1)
mu_2 = sum(P_2 * x) / sum(P_2)
tau_1 = mean(P_1)
tau_2 = mean(P_2)


#___Em _ Step -rep

for(i in 1:10) {
  ## E_Step
  T_1 = tau_1 * dnorm(x, mu_1)
  T_2 = tau_2 * dnorm(x, mu_2)
  P_1 = T_1 / (T_1 + T_2)
  P_2 = T_2 / (T_2 + T_2)
  
  ## m_step
  mu_1 = sum(P_1 * x) / sum(P_1)
  mu_2 = sum(P_2 * x) / sum(P_2)
  tau_1 = mean(P_1)
  tau_2 = mean(P_2)
  
  ## PRNT CURRRENT
  print(c(mu_1, mu_2, tau_1, tau_2))
}

sin(-5)
      
      
      
