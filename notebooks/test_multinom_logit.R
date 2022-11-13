z <- rep(1, 3)
z[2] <- 2
z[3] <- .5


y <- exp(z) / sum(exp(z))

softmax <- function(x) exp(x) / sum(exp(x))

softmax(z)

sim_prior_transit <- function(nstates = 3, pos_ref = 1, nsim = 1000, means = rep(NA, nstates - 1), sds = rep(NA, nstates - 1)){

  ## matrix of simulated values
  z <- matrix(rep(0, nsim * nstates),
              ncol = nstates)

  ## indices for the loop
  ## state of reference excluded
  j_states <- (1:nstates)[-pos_ref]

  for(i in 1:nsim){

    z[i, j_loop] <- rnorm(n = nstates-1, mean = means, sd = sds)


  }

  z <- t(apply(z, 1, softmax))


}


y <- sim_prior_transit(nstates = 3,
                       pos_ref = 1,
                       nsim = 100000,
                       means = c(5, 0),
                       sds = c(1, 1))

par(mfrow = c(1, 3))
for(i in 1:3) hist(y[,i], breaks = 100, xlim = c(0, 1))


