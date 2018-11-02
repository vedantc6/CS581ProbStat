mom_bernoulli <- function(input_data = c(1:1000), moments = 1){
  pop_mean = mean(input_data)
  print(pop_mean)
  sample_mean = 0
  for (i in input_data){
    sample_mean = sample_mean + i
  }
  sample_mean = sample_mean/length(input_data)
  print(sample_mean)
}

mom_wrapper <- function(input_data, distribution){
  if (distribution == "bernoulli"){
    print("Bernoulli only has 1 parameter, hence 1st moment will give an estimator for p")
    estimator <- mom_bernoulli(input_data, 1)
  }
}

mom_wrapper(c(1:1000), "bernoulli")
