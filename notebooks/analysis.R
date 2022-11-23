## data used for the analysis
scc <- scc1 |>
       dplyr::mutate(test_res = ifelse(scc > 200, 1, 0))

## running the model with 3 states
## estimation using the forward algorithm
## strong priors put on the probabilities of transition
## here, staying in the same state was used as the reference (0 on the log scale)
## other transitions defined in relation to these
## with normal distributions on the log scale
## each lactation is considered as a unit
md0 <- hmm_3s_f(scc)

md1 <- hmm_3s_fb(scc)
## this version estimates monthly prevalences
## see month_prev in the summary
md2 <- hmm_3s_fb1(scc)

## parameter summary
## test_char[1] is the probability of a false positive
## B[,] is the transition matrix on the probability scale
md0$summary
md1$summary
View(md1$summary)
View(md2$summary)

## MCMC draws
draws <- md0$draws
draws <- md1$draws

md1$summary$variable

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
