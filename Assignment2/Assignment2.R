install.packages("Rlab")

library(Rlab)

############################################################################
########################## UTILITY FUNCTIONS ###############################
############################################################################
secondMomentCalc <- function(input_data, x_bar){
  temp = 0
  for (i in input_data){
    temp = temp + (i - x_bar)^2
  }
  return (temp/length(input_data))
}

# secondMomentCalc_Origin <- function(input_data, x_bar){
#   temp = 0
#   for (i in input_data){
#     temp = temp + (i)^2
#   }
#   return (temp/length(input_data))
# }

firstMomentCalc <- function(input_data){
  temp = 0
  for (i in input_data){
    temp = temp + i
  }
  return (temp/length(input_data))
}

############################################################################
######### METHOD OF MOMENTS FUNCTIONS FOR DIFFERENT DISTRIBUTIONS ##########
############################################################################
mom_point <- function(input_data){
  # a = mean(input_data)
  # print(paste("First theoretical moment (Population mean) about origin:", a))
  # Estimating the parameters 
  a_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", a_hat))
}

mom_bernoulli <- function(input_data){
  # Estimating the parameters 
  p_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", p_hat))
}

mom_binomial <- function(input_data){
  mu_hat = mean(input_data)
  var_hat = secondMomentCalc(input_data, mu_hat)
  p_hat = (mu_hat - var_hat)/mu_hat
  n_hat = mu_hat^2/((mu_hat - var_hat))
  print(paste("Estimated parameter 1 through MOM:", p_hat))
  print(paste("Estimated parameter 2 through MOM:", n_hat))
}

mom_geometric <- function(input_data){ 
  p_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", 1/p_hat))
}

mom_poisson <- function(input_data){ 
  lambda_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", lambda_hat))
}

mom_uniform <- function(input_data){
  mu_hat = firstMomentCalc(input_data)
  var_hat = secondMomentCalc(input_data, mu_hat)
  a_hat = mu_hat - sqrt(3)*sqrt(var_hat)
  b_hat = mu_hat + sqrt(3)*sqrt(var_hat)
  print(paste("Estimated parameter 1 through MOM:", a_hat))
  print(paste("Estimated parameter 2 through MOM:", b_hat))
}

mom_normal <- function(input_data){
  # Estimating the parameters
  mu_hat = firstMomentCalc(input_data)
  var_hat = secondMomentCalc(input_data, mu_hat)
  print(paste("Estimated parameter 1 through MOM:", mu_hat))
  print(paste("Estimated parameter 2 through MOM:", var_hat))
  return(c(mu_hat,var_hat))
}

mom_exponential <- function(input_data){ 
  theta_hat = firstMomentCalc(input_data)
  print(paste("Estimated parameter 1 through MOM:", 1/theta_hat))
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
  beta_hat = (1-mu_hat)*((mu_hat*(1-mu_hat)/var_hat) - 1)
  alpha_hat = mu_hat*((mu_hat*(1-mu_hat)/var_hat) - 1)
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
  print(paste("Estimated parameter 1 through MOM:", p_hat))
}

############################################################################
########### MAIN CALL FUNCTION FOR METHOD OF MOMENTS FUNCTIONS #############
############################################################################
mom_wrapper <- function(distribution, population = 0){
  if (distribution == "point"){
    print("Point distribution has 1 parameter, hence 1st moment will give an estimator for a")
    estimator <- mom_point(input_data)
  }
  else if (distribution == "bernoulli"){
    if (population == 0){
      p = 0.5
      input_data = rbern(10000,p)  
    } else{
      input_data = population
    }
    print("Population parameter: ")
    print(p)
    sample_data = sample(input_data, 1000)
    print("Bernoulli distribution has 1 parameter, hence 1st moment will give an estimator for p")
    estimator <- mom_bernoulli(sample_data)
  }
  else if (distribution == "binomial"){
    if (population == 0){
      n = 100
      p = 0.5
      input_data = rbinom(10000,n,p)  
    } else{
      input_data = population
    }
    print("Population parameters: ")
    print(paste(p,",",n))
    sample_data = sample(input_data, 1000)
    print("Binomial distribution has 2 parameters - n and p - hence first two moments will give its parameter estimates")
    estimator <- mom_binomial(sample_data)
  }
  else if (distribution == "geometric"){
    if (population == 0){
      p = 0.5
      input_data = rgeom(10000,p)  
    } else{
      input_data = population
    }
    print("Population parameters: ")
    print(p)
    sample_data = sample(input_data, 1000)
    print("Geometric distribution has 1 parameter, hence 1st moment will give an estimator for p")
    estimator <- mom_geometric(sample_data)
  }
  else if (distribution == "poisson"){
    if (population == 0){
      lambda = 0.5
      input_data = rpois(10000,lambda)  
    } else{
      input_data = population
    }
    print("Population parameters: ")
    print(lambda)
    sample_data = sample(input_data, 1000)
    print("Poisson distribution has 1 parameter, hence 1st moment will give an estimator for lambda")
    estimator <- mom_poisson(input_data)
  }
  else if (distribution == "uniform"){
    if (population == 0){
      a = 0
      b = 100
      input_data = runif(10000,a,b)  
    } else{
      input_data = population
    }
    print("Population parameters: ")
    print(paste(a,",",b))
    print("Uniform distribution has 2 parameters - a and b - hence first two moments will give its parameter estimates")
    sample_data = sample(input_data, 1000)
    estimator <- mom_uniform(sample_data)
  }
  else if (distribution == "normal"){
    if (population == 0){
      input_data = rnorm(10000,0,1)  
    } else{
      input_data = population
    }
    m = mean(input_data)
    v = var(input_data)
    print("Population mean: ")
    print(m)
    print("Population variance: ")
    print(v)
    print("Normal distribution has 2 parameters - mu and sigma - hence first two moments will give its parameter estimates")
    sample_data = sample(input_data, 1000)
    estimator <- mom_normal(sample_data)
    plot(input_data, dnorm(input_data, m, v), title("Population vs Sample"), col='red')
    points(sample_data, dnorm(sample_data, estimator[1], estimator[2]), col='green')
  }
  else if (distribution == "exponential"){
    if (population == 0){
      theta = 4
      input_data = rexp(10000,theta)  
    } else{
      input_data = population
    }
    print("Population parameter: ")
    print(theta)
    print("Exponential distribution has 1 parameter, hence 1st moment will give an estimator for theta")
    sample_data = sample(input_data, 1000)
    estimator <- mom_exponential(sample_data)
  }
  else if (distribution == "gamma"){
    if (population == 0){
      alpha = 2
      beta = 1
      input_data = rgamma(10000, alpha = alpha, beta = beta)  
    } else{
      input_data = population
    }
    print("Population parameters: ")
    print(paste(alpha,",",beta))
    print("Gamma distribution has 2 parameters - alpha and theta - hence first two moments will give its parameter estimates")
    sample_data = sample(input_data, 1000)
    estimator <- mom_gamma(sample_data)
  }
  else if (distribution == "beta"){
    # Assuming alpha nad beta are in the range [0,1]
    if (population == 0){
      alpha = 0.5
      beta = 0.3
      input_data = rbeta(10000, shape1 = alpha, shape2 = beta)  
    } else{
      input_data = population
    }
    print("Population parameters: ")
    print(paste(alpha,",",beta))
    print("Beta distribution has 2 parameters - alpha and beta - hence first two moments will give its parameter estimates")
    sample_data = sample(input_data, 1000)
    estimator <- mom_beta(sample_data)
  }
  else if (distribution == "t"){
    # Assumg degree of freedom to be above 2
    if (population == 0){
      dog = 5
      input_data = rt(10000, df = dog)  
    } else{
      input_data = population
    }
    print("Population parameter: ")
    print(dog)
    print("T distribution has 1 parameter, hence the 1st moment will give an estimator for v (degree of freedoms)")
    sample_data = sample(input_data, 1000)
    estimator <- mom_t(sample_data)
  }
  else if (distribution == "chi square"){
    if (population == 0){
      dog = 5
      input_data = rchisq(10000, df = dog)  
    } else{
      input_data = population
    }
    print("Population parameter: ")
    print(dog)
    print("Chi-Square distribution has 1 parameter, hence the 1st moment will give an estimator for p")
    sample_data = sample(input_data, 1000)
    estimator <- mom_chisq(sample_data)
  }
  # else if (distribution == "multinomial"){
  #   print("Gamma distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
  #   estimator <- mom_gamma(input_data)
  # }
  # else if (distribution == "multi-variatenormal"){
  #   print("Gamma distribution has 2 parameters, alpha and theta, hence we need to find first two moments to give an estimator for its parameters")
  #   estimator <- mom_gamma(input_data)
  # }
}

# STATEMENTS TO BE EXECUTED BEFORE RUNNING THE MAIN M.O.M. FUNCTION

population = sample(seq(1, 10000), 10000)
print("Valid Distributions: Point, Bernoulli, Binomial, Geometric, Poisson, Uniform, Normal, Exponential, Gamma, Beta, T, Chi-Square, Multinomial, Multinormal")
distri = readline("For which distribution, do you want M.O.M. estimation (Please input valid distributions): ")
if_pop = readline("Do you want to send a random population or use system's default populations (y/n): ")

if (tolower(if_pop) == "y"){
  mom_wrapper(distri, population)
}else{
  mom_wrapper(distri)
}

