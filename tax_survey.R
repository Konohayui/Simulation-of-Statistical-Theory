set.seed(2048)

tax_survey <- function(n_sample) {
num_sample <- n_sample # number of total sample

ob_samples <- rep(0, num_sample) # observed samples
tr_samples <- rbinom(num_sample, size = 2, prob = 1/5) # true lables

dice_rolls <- sample(2, num_sample, replace = T, prob = c(2,1)/3) # 1 is green and 2 is red


# 1 a subject rolls a die 
# 2 die outcomes a green, says yes (1) if the subject cheated on last year tax
#   else says no (0)
# 3 die outcomes a red, says yes

for(i in 1:num_sample) {
  if(dice_rolls[i] == 1) {
    if(tr_samples[i] == 1) {
      ob_samples[i] <- 1
    }
  }
  else {
    ob_samples[i] <- 1
  }
}

return(sum(ob_samples))

}

# simulates sample sizes from 100 to 1000
num_sample_list <- seq(100, 1000, 100)
list_l <- length(num_sample_list)
num_sim <- 10
observed_yes <- matrix(0, list_l, num_sim)

for(s in 1:num_sim) {
  for(n in 1:list_l) {
    observed_yes[n, s] <- tax_survey(num_sample_list[n])
  }
}

matplot(observed_yes, type = "l",
        xlab = "number of samples x 100", ylab = "number of yes")

