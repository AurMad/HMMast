scc <- scc1 |>
  dplyr::mutate(test_res = ifelse(scc > 200, 1, 0))


z <- format_mastitis_data(scc)

n_obs <- nrow(z)

n_months <- max(z$month_id)
lg_months <- sapply(1:max(z$month_id), function(x) length(which(z$month_id == x)))
lg_months_cumul <- cumsum(lg_months)
month_order <- unlist(sapply(1:max(z$month_id), function(x) which(z$month_id == x)))
month_start <- cumsum(c(1, lg_months[-length(lg_months)]))
month_prev <- matrix(rep(NA, n_months * 2), ncol = 2)

prev_cumul <- matrix(rep(NA, n_obs * 2), ncol = 2)

gamma <- cbind(z$milk, z$scc)
gamma[is.na(gamma[,1]),1] <- mean(gamma[,1], na.rm = TRUE)
gamma[is.na(gamma[,2]),2] <- mean(gamma[,2], na.rm = TRUE)

m <- 1
j <- 1

for(m in 1:n_months){

  prev_cumul[month_start[m], 1] <- gamma[month_start[m], 1]
  prev_cumul[month_start[m], 2] <- gamma[month_start[m], 2]


  for(j in 1:(lg_months[m] - 1)){

    prev_cumul[month_start[m] + j, 1] <- prev_cumul[month_start[m] + j - 1, 1] + gamma[month_order[month_start[m] + j], 1]
    prev_cumul[month_start[m] + j, 2] <- prev_cumul[month_start[m] + j - 1, 2] + gamma[month_order[month_start[m] + j], 2]


  }

  month_prev[m, 1] <- prev_cumul[lg_months_cumul[m], 1] / lg_months[m]
  month_prev[m, 2] <- prev_cumul[lg_months_cumul[m], 2] / lg_months[m]

}



