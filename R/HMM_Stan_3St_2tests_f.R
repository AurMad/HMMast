#################################################################
## Stan model for 3 states
## Estimation using the forward algorithm
#################################################################
#' Three state model
#'
#' @param .data
#' @param level Level of interest for the analysis. Either 'cow' or 'lactation'.
#' @param nrec_min Minimum number of records for a unit ('cow' or 'lactation') to be included in the analysis
#' @param Sp_a
#' @param Sp_b
#' @param Se1_a
#' @param Se1_b
#' @param Se2_a
#' @param Se2_b
#' @param H_to_C_mean Transition from healthy to contagious - mean.
#' @param H_to_C_sd   Transition from healthy to contagious - standard deviation.
#' @param H_to_E_mean Transition from healthy to environmental - mean.
#' @param H_to_E_sd  Transition from healthy to environmental - standard deviation.
#' @param C_to_H_mean Transition from contagious to healthy - mean.
#' @param C_to_H_sd Transition from contagious to healthy - standard deviation.
#' @param C_to_E_mean Transition from contagious to environmental - mean.
#' @param C_to_E_sd Transition from contagious to environmental - standard deviation.
#' @param E_to_H_mean Transition from environmental to healthy - mean.
#' @param E_to_H_sd Transition from environmental to healthy - standard deviation.
#' @param E_to_C_mean Transition from environmental to contagious - mean.
#' @param E_to_C_sd Transition from environmental to contagious - standard deviation.
#' @param chains Number of MCMC chains
#' @param iter_sampling Number of sampling iterations
#'
#' @return
#' @export
#'
#' @examples
hmm_3s_2t_f <- function(.data = data.frame(),
                          level = "lactation",
                          nrec_min = 2,
                          Sp_a = 40,
                          Sp_b = 5,
                          Se1_a = 40,
                          Se1_b = 5,
                          Se2_a = 40,
                          Se2_b = 5,
                          H_to_C_mean = -1,
                          H_to_C_sd = 2,
                          H_to_E_mean = -1,
                          H_to_E_sd = 2,
                          C_to_H_mean = -3,
                          C_to_H_sd = 2,
                          C_to_E_mean = -100,
                          C_to_E_sd = 1,
                          E_to_H_mean = 10,
                          E_to_H_sd = 1,
                          E_to_C_mean = -100,
                          E_to_C_sd = 1,
                          chains = 3,
                          iter_sampling = 1000){


  ## formatting the SCC data for analysis
  z <- format_mastitis_data(.data, level = level, nrec_min = nrec_min)

  ## indices for the start/end of each unit
  if(!level %in% c("lactation", "cow")) stop("level must be either 'cow' or 'lactation")

  if(level == "lactation"){

    z_agg <- aggregate_by_lac(z)

    z <- z |>
      dplyr::rename(unit_id = lac_id)

  }

  if(level == "cow"){

    z_agg <- aggregate_by_cow(z)

    z <- z |>
      dplyr::rename(unit_id = cow_id)

  }

  ## Stan model
stan_model_txt <-
    "data{

  int<lower = 1> K;      // number of states
  int<lower = 1> n_obs;   // number of observations
  int<lower = 1> n_units;   // number of animals
  array[n_obs] int<lower = 0, upper = 2> y_cat; // categorical test results
  array[n_obs] real y_cont;  // continuous test results
  array[n_units] int<lower = 0, upper = n_obs> row_first; // id of first row for each cow
  array[n_units] int<lower = 0, upper = n_obs> row_sec;   // id of second row for each cow
  array[n_units] int<lower = 0, upper = n_obs> row_last;  // id of last row for each cow

  // characteristics of the test with binary results
  real<lower = 1> Sp_a;
  real<lower = 1> Sp_b;
  real<lower = 1> Se1_a;
  real<lower = 1> Se1_b;
  real<lower = 1> Se2_a;
  real<lower = 1> Se2_b;

  // characteristics of the test with binary results
  array[K] real mu_test_cont;
  array[K] real<lower = 0> sd_test_cont;

  // parameters for the transition matrix
  real H_to_C_mean;
  real<lower = 0> H_to_C_sd;
  real H_to_E_mean;
  real<lower = 0> H_to_E_sd;
  real C_to_H_mean;
  real<lower = 0> C_to_H_sd;
  real C_to_E_mean;
  real<lower = 0> C_to_E_sd;
  real E_to_H_mean;
  real<lower = 0> E_to_H_sd;
  real E_to_C_mean;
  real<lower = 0> E_to_C_sd;

}
parameters{

  array[K+1] real<lower = 0, upper = 1> test_char; // test characteristics.
  // Probabilities of positive test result conditionnally on latent state
  simplex[K] pi1;          // initial state probabilities
  array[6] real zeta;
  // characteristics of the continuous test
  array[K] real mu_test;
  array[K] real<lower = .1> sd_test;


}
transformed parameters{
  // test for transition matrix
  array[K] vector[K] BB;

  BB[1, 1] = 0;
  BB[1, 2] = zeta[1];
  BB[1, 3] = zeta[2];

  BB[2, 1] = zeta[3];
  BB[2, 2] = 0;
  BB[2, 3] = zeta[4];

  BB[3, 1] = zeta[5];
  BB[3, 2] = zeta[6];
  BB[3, 3] = 0;

  array[K] simplex[K] B;   // transition matrix
                           // B[i, j] = p(z_t = j | z{t-1} = i)

  B[1] = softmax(BB[1]);
  B[2] = softmax(BB[2]);
  B[3] = softmax(BB[3]);

  // definition of alpha and logalpha for the forward algorithm
  array[n_obs] vector[K] logalpha;

   // loop over all animals
    for(a in 1:n_units){

   { // forward algorithm

      array[K] real accumulator;

      // first test in sequence
      for(k1 in 1:K){

       if(y_cat[row_first[a]] == 2){

        logalpha[row_first[a], k1] = log(pi1[k1]);

           } else {

        logalpha[row_first[a], k1] = log(pi1[k1]) + normal_lpdf(y_cont | mu_test[k1], sd_test[k1]) + bernoulli_lpmf(y_cat[row_first[a]] | test_char[k1]);

           }
      }

      // test at times 2 to T
      for(t in row_sec[a]:row_last[a]){

       for(j in 1:K){     // current state

         for(i in 1:K){   // state at t-1

      if(y_cat[t] == 2){

           accumulator[i] = logalpha[t-1, i] + log(B[i, j]);

        } else {

           accumulator[i] = logalpha[t-1, i] + log(B[i, j]) + normal_lpdf(y_cont | mu_test[j], sd_test[j]) + bernoulli_lpmf(y_cat[t] | test_char[j]);

           } // if statement

       } // i

        logalpha[t, j] = log_sum_exp(accumulator);

       } // j

       } // end of loop for time

      } // end of loop for the forward algorithm
    }


  }
model{

    // prior distributions for test characteristics
    // vector of the probability of getting a positive test result
    // conditionnaly on the latent state
    // the first element of the vector is 1 - specificity
    // the other ones are the sensitivities of the different tests
    test_char[1] ~ beta(Sp_b, Sp_a);
    test_char[2] ~ beta(Se1_a, Se1_b);
    test_char[3] ~ beta(Se2_a, Se2_b);

    // prior distributions for the characteristics of the continuous test
    for(i_cont in 1:K){

    mu_test[i_cont] ~ normal(mu_test_cont[i_cont], sd_test_cont[i_cont] / 2);
    sd_test[i_cont] ~ normal(sd_test_cont[i_cont], 10);

    }

   // parameters of the transition matrix
    zeta[1] ~ normal(H_to_C_mean, H_to_C_sd);
    zeta[2] ~ normal(H_to_E_mean, H_to_E_sd);
    zeta[3] ~ normal(C_to_H_sd, C_to_H_sd);
    zeta[4] ~ normal(C_to_E_mean, C_to_E_sd);
    zeta[5] ~ normal(E_to_H_mean, E_to_H_sd);
    zeta[6] ~ normal(E_to_C_mean, E_to_C_sd );

   // loop over all animals
   for(aa in 1:n_units){

    target += log_sum_exp(logalpha[row_last[aa]]);

   }

  }"

stan_data <- list(
  K = 3,
  n_obs = nrow(z),
  n_units = length(unique(z$unit_id)),
  y_cat = z$test_res,
  y_cont = z$x_cont,
  row_first = z_agg$row_first,
  row_sec = z_agg$row_first + 1,
  row_last = z_agg$row_last,
  Sp_a = Sp_a,
  Sp_b = Sp_b,
  Se1_a = Se1_a,
  Se1_b = Se1_b,
  Se2_a = Se2_a,
  Se2_b = Se2_b,
  mu_test_cont = c(1, 2, 3),
  sd_test_cont = c(.2, .2, .2),
  H_to_C_mean = H_to_C_mean,
  H_to_C_sd = H_to_C_sd,
  H_to_E_mean = H_to_E_mean,
  H_to_E_sd = H_to_E_sd,
  C_to_H_mean = C_to_H_mean,
  C_to_H_sd = C_to_H_sd,
  C_to_E_mean = C_to_E_mean,
  C_to_E_sd = C_to_E_sd,
  E_to_H_mean = E_to_H_mean,
  E_to_H_sd = E_to_H_sd,
  E_to_C_mean = E_to_C_mean,
  E_to_C_sd = E_to_C_sd,
  chains = chains,
  iter_sampling = iter_sampling
)


stan_file <- cmdstanr::write_stan_file(stan_model_txt)

stan_model <- cmdstanr::cmdstan_model(stan_file)

stan_fit <- stan_model$sample(
  data = stan_data,
  chains = chains,
  iter_sampling = iter_sampling
)

## extracting parameter summaries
smry <- tibble::as_tibble(stan_fit$summary())

smry <- smry %>%
  dplyr::filter(!stringr::str_detect(variable, "logalpha")) %>%
  dplyr::filter(!stringr::str_detect(variable, "BB")) %>%
  dplyr::filter(!stringr::str_detect(variable, "zeta")) %>%
  dplyr::filter(!stringr::str_detect(variable, "lp__"))

## MCMC draws
draws <- stan_fit$draws(format = "matrix")

draws <- draws[,-grep("lp__", colnames(draws))]
draws <- draws[,-grep("logalpha", colnames(draws))]
draws <- draws[,-grep("BB", colnames(draws))]
draws <- draws[,-grep("zeta", colnames(draws))]

z <- list(
  summary = smry,
  draws = draws
)


}
