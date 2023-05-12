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


mast <- sim_mast(n_cows = 100,
         n_times = 12,
         n_states = 3,
         trans_mat = trans_mat)

mast <- mast |>
  rename(test_res = x_cat) |>
  mutate(calv_date = "2022-01-01",
         parity = rep(1))

test1 <- check_mastitis_data(mast)
test2 <- aggregate_by_cow(mast)
test3 <- expand_scc_data(mast)

testn <- format_mastitis_data(mast)



## running the model with 3 states
## estimation using the forward algorithm
## strong priors put on the probabilities of transition
## here, staying in the same state was used as the reference (0 on the log scale)
## other transitions defined in relation to these
## with normal distributions on the log scale
## each lactation is considered as a unit
md0 <- hmm_3s_f(mast, level = "cow")
