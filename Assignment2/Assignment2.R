secondMomentCalc <- function(input_data, x_bar){
  temp = 0
  for (i in input_data){
    temp = temp + (i - x_bar)^2
  }
  return (temp/length(input_data))
}

firstMomentCalc <- function(input_data){
  temp = 0
  for (i in input_data){
    temp = temp + i
  }
  return (temp/length(input_data))
}

mom_point <- function(input_data){
  a = mean(input_data)
  print(paste("First theoretical moment (Population mean) about origin:", a))
  # Estimating the parameters 
  a_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", a_hat))
}

mom_uniform <- function(input_data){
  mu_hat = mean(input_data)
  print(mu_hat)
  var_hat = secondMomentCalc(input_data, mu_hat)
  print(var_hat)
  a_hat = mu_hat - sqrt(3)*sqrt(var_hat)
  b_hat = mu_hat + sqrt(3)*sqrt(var_hat)
  print(paste("Estimated parameter 1 through MOM:", a_hat))
  print(paste("Estimated parameter 2 through MOM:", b_hat))
}

mom_binomial <- function(input_data){
  mu_hat = mean(input_data)
  var_hat = secondMomentCalc(input_data, mu_hat)
  p_hat = (mu_hat + mu_hat^2 - var_hat)/mu_hat
  k_hat = mu_hat^2/(mu_hat + mu_hat^2 - var_hat)
  print(paste("Estimated parameter 1 through MOM:", p_hat))
  print(paste("Estimated parameter 2 through MOM:", k_hat))
}

mom_bernoulli <- function(input_data){
  p = mean(input_data)
  print(paste("First theoretical moment (Population mean) about origin:", p))
  # Estimating the parameters 
  p_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", p_hat))
}

mom_poisson <- function(input_data){ 
  lambda_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", lambda_hat))
}

mom_geometric <- function(input_data){ 
  p_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", 1/p_hat))
}

mom_normal <- function(input_data){
  mu = mean(input_data)
  variance = var(input_data)
  th_2 = mu^2 + variance
  print(paste("First theoretical moment (Population mean) about origin:", mu))
  print(paste("Second theoretical moment about origin:", th_2))
  print(paste("Population variance:", variance))
  # Estimating the parameters
  mu_hat = firstMomentCalc(input_data)
  var_hat = secondMomentCalc(input_data, mu_hat)
  print(paste("Estimated parameter 1 through MOM:", mu_hat))
  print(paste("Estimated parameter 2 through MOM:", var_hat))
}

mom_exponential <- function(input_data){ 
  theta_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", theta_hat))
}

mom_gamma <- function(input_data){
  mu_hat = firstMomentCalc(input_data)
  var_hat = secondMomentCalc(input_data, mu_hat)
  # We know that in gamma function, there are two parameters alpha and beta, with relation alpha*beta = mu and alpha*(beta^2) = variance
  theta_hat = var_hat/mu_hat
  alpha_hat = mu_hat/ theta_hat
  print(paste("Estimated parameter 1 through MOM:", alpha_hat))
  print(paste("Estimated parameter 2 through MOM:", theta_hat))
}

mom_beta <- function(input_data){
  mu_hat = firstMomentCalc(input_data)
  var_hat = secondMomentCalc(input_data, mu_hat)
  beta_hat = (1-mu_hat)*(var_hat-mu_hat)/(mu_hat^2-var_hat)
  alpha_hat = mu_hat*beta_hat/(1-mu_hat)
  print(paste("Estimated parameter 1 through MOM:", alpha_hat))
  print(paste("Estimated parameter 2 through MOM:", beta_hat))
}

mom_t <- function(input_data){
  mu_hat = firstMomentCalc(input_data)
  var_hat = secondMomentCalc(input_data, mu_hat)
  dof_hat = 2*var_hat/(var_hat-1)
  print(paste("Estimated parameter 1 through MOM:", dof_hat))
}

mom_chisq <- function(input_data){
  p_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", 1/p_hat))
}


mom_wrapper <- function(input_data, distribution){
  if (distribution == "point"){
    print("Point distribution has 1 parametes, hence 1st moment will give an estimator for a")
    estimator <- mom_point(input_data)
  }
  else if (distribution == "bernoulli"){
    print("Bernoulli distribution only has 1 parameter, hence 1st moment will give an estimator for p")
    estimator <- mom_bernoulli(input_data)
  }
  else if (distribution == "binomial"){
    print("Binomial distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
    estimator <- mom_binomial(input_data)
  }
  else if (distribution == "geometric"){
    print("Geometric distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
    estimator <- mom_geometric(input_data)
  }
  else if (distribution == "poisson"){
    print("Poisson distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
    estimator <- mom_poisson(input_data)
  }
  else if (distribution == "uniform"){
    print("Uniform distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
    estimator <- mom_uniform(input_data)
  }
  else if (distribution == "normal"){
    print("Normal distribution has 2 parameters, mu and sigma, hence we need to find first two moments to give an estimator for its parameters")
    estimator <- mom_normal(input_data)
  }
  else if (distribution == "exponential"){
    print("Gamma distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
    estimator <- mom_exponential(input_data)
  }
  else if (distribution == "gamma"){
    print("Gamma distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
    estimator <- mom_gamma(input_data)
  }
  else if (distribution == "beta"){
    print("Gamma distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
    estimator <- mom_beta(input_data)
  }
  else if (distribution == "t"){
    print("Gamma distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
    estimator <- mom_t(input_data)
  }
  # else if (distribution == "chi square"){
  #   print("Gamma distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
  #   estimator <- mom_gamma(input_data)
  # }
  # else if (distribution == "multinomial"){
  #   print("Gamma distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
  #   estimator <- mom_gamma(input_data)
  # }
  # else if (distribution == "multi-variatenormal"){
  #   print("Gamma distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
  #   estimator <- mom_gamma(input_data)
  # }
}

population = sample(seq(1, 10000), 10000)
# population = rnorm(10000, 40, 20)
# population = rbeta(10000, 40, 70, ncp = 0)

print("Population mean: ")
print(mean(population))
print("Population variance: ")
print(var(population))
mom_wrapper(sample(population, 100), "t")
