sim_mast <- function(n_cows = 100,
                     n_times = 12,
                     n_states = 3,
                     trans_mat = matrix(rep(NA, n_states * n_states),
                                        ncol = n_states)){

## The objective of this piece of code is to define
## the initial probabilities of being in each state
vi <- matrix(rep(1, n_states)/n_states, nrow = 1)

for(i in 1:100){

 vi <- vi %*% trans_mat

}

## vector of dates to use as recording dates
today <- paste0(format(as.Date(Sys.time()), "%Y"), "-01-01")
sq_dates <- seq(as.Date(today), length.out = n_times, by = "1 month")

## Mastitis dataset
mast <- tibble::tibble(
  cow_id = rep(1:n_cows, each = n_times),
  time = rep(1:n_times, n_cows),
  rec_date = sq_dates[time])
## adding infection status
inf <- matrix(rep(NA, nrow(mast) * n_states), ncol = n_states)
nms_inf <- paste0("S", 1:n_states)
colnames(inf) <- nms_inf

mast <- dplyr::bind_cols(mast, inf)

## sampling infection status
mast[mast$time == 1, nms_inf] <- t(rmultinom(n_cows, 1, vi))

for(i in 2:n_times){

  t_mat <- as.matrix(mast[mast$time == (i - 1), nms_inf])  %*% trans_mat
  mast[mast$time == i, nms_inf] <- t(apply(t_mat, 1, function(x) rmultinom(1, 1, x)))

  }

## status
mast$status <- apply(mast[, nms_inf], 1, function(x) which(x == 1))

## continuous variable
## normal distribution
mu_cont <- c(1.5, 2.5, 3.5)
sd_cont <- c(.01, .01, .01)

mast$x_cont <- rnorm(n = nrow(mast),
                     mean = mu_cont[mast$status],
                     sd = sd_cont[mast$status])

## categorical variable
p_catg <- c(0, .2, 1)

mast$x_cat <- rbinom(n = nrow(mast),
                     size = 1,
                     prob = p_catg[mast$status])

return(mast)

}
