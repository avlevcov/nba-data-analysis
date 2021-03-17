

standard_error <- function(mean, sample_size){
  standard_error <- sqrt(mean * (1 - mean) / sample_size)
  return(standard_error)
}

B <- function(c_se, h_se){
 B <- c_se^2 / (c_se^2 + h_se^2)
 return(B)
}

ev_posterior_distribution <- function(c_mean, h_mean, B){
  h_mean + (1 - B) * (c_mean - h_mean)
}

se_posterior_distribution <- function(c_se, h_se){
  sqrt(1 / ((1/h_se^2) + (1/c_se^2)))
}