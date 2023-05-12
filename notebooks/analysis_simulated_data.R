library(tidyverse)

## State 1 = healthy
## State 2 = contagious
## State 3 = environmental
trans_mat <- matrix(rep(NA, 9), ncol = 3)
## From state 1
trans_mat[1, ] <- c(.9, .05, .05)
## From state 2
trans_mat[2, ] <- c(.1, .9, 0)
## From state 3
trans_mat[3, ] <- c(1, 0, 0)

## Simulating data
mast <- sim_mast(n_cows = 100,
         n_times = 12,
         n_states = 3,
         trans_mat = trans_mat)
## some columns are modified to allow the code to work at this stage
mast <- mast |>
  rename(test_res = x_cat) |>
  mutate(calv_date = "2022-01-01",
         parity = rep(1))

## running the model with 3 states
## estimation using the forward algorithm
## strong priors put on the probabilities of transition
## here, staying in the same state was used as the reference (0 on the log scale)
## other transitions defined in relation to these
## with normal distributions on the log scale
## each lactation is considered as a unit
md0 <- hmm_3s_2t_f(mast, level = "cow",
                   chains = 2,
                   iter_sampling = 500)

## parameter summary
## test_char[1] is the probability of a false positive
## B[,] is the transition matrix on the probability scale
View(md0$summary)

## MCMC draws
draws <- md0$draws

## plotting the transition matrix
transition_labels <- matrix(
  c("H to H", "H to C", "H to E",
    "C to H", "C to C", "C to E",
    "E to H", "E to C", "E to E"),
  ncol = 3, byrow = TRUE)
par(mfrow = c(3, 3))
for(i in 1:3){

  ii <- grep(paste0("B\\[", i), colnames(draws))

  for(j in 1:3){

    hist(draws[,ii[j]], breaks = 100, xlim = c(0, 1),
         main = transition_labels[i,j])

  }
}

