#' Simulated data based on PhD thesis of P.J.A. Stam (2007)
#'
#' @param nobs Number of observations
#' @param seed Optional seed number
#'
#' @return A data frame with 63 variables
#' @export
#'
#'
#' @examples
#' df <- phd_data()            # df w/ 100 observations and random seed
#' df <- phd_data(1000)        # df w/ 1000 observations and random seed
#' df <- phd_data(1000, 1234)  # df w/ 1000 observations and seed 1234
phd_data <- function(nobs = 100, seed = NULL) {

  # Set seed (or not)

  if (is.null(seed) == FALSE) {
    set.seed(seed)
    print(paste("Reminder: seed set at", seed))
  } else {
    print("Reminder: random seed")
  }

  #########################################
  # Set beta coefficients risk adjusters
  #########################################

  # Table 6.2 (REF weights)
  #beta_constant <- c(623)
  #beta_gender_age <- c(0,-209,-316,464,380,1493,2796,1598,-109,345,0,194,274,1003,2289,1662)
  #beta_incsrc <- c(1437,0,211,214,341,-197)
  #beta_region <- c(457,262,138,239,37,-121,-31,-164,29,0)
  #beta_pcg <- c(0,1883,1803,1098,2001,3848,3199,3366,7791,3823,8030,11895,20748)
  #beta_dcg <- c(0,1356,6319,3565,5591,4262,7820,6038,8869,7983,18152,12626,9050,77982)

  # Table 6.9 (Adjusted REF weights)
  beta_constant <- 653
  beta_gender_age <-
    c(0,
      -215,
      -325,
      462,
      377,
      1482,
      2780,
      1585,
      -120,
      339,
      -6,
      190,
      268,
      989,
      2271,
      1634)
  beta_incsrc <- c(1434, 0, 188, 215, 343, -195)
  beta_region <- c(224, 54, -69, 32, -156, -295, -197, -265, -89, 0)
  beta_pcg <-
    c(0,
      1881,
      1799,
      1089,
      2005,
      3848,
      3177,
      3367,
      7796,
      3829,
      8032,
      11877,
      20722)
  beta_dcg <-
    c(0,
      1350,
      6316,
      3573,
      5592,
      4262,
      7855,
      6046,
      8887,
      8014,
      18157,
      12605,
      9037,
      77953)
  beta_hprice <- 33
  beta_hdist <- -5
  beta_gpdist <- -112

  #########################################
  # Set probabilities risk adjusters
  #########################################

  # Table 6.3
  population_means_gender_age <-
    c(
      0.042,
      0.071,
      0.069,
      0.063,
      0.057,
      0.050,
      0.023,
      0.003,
      0.062,
      0.113,
      0.128,
      0.113,
      0.089,
      0.072,
      0.040,
      0.007
    )
  # Table 6.7
  population_means_incsrc <- c(0.090, 0.595, 0.041, 0.042, 0.205, 0.028)
  # Table 6.8
  population_means_region <-
    c(0.072,
      0.206,
      0.095,
      0.092,
      0.146,
      0.099,
      0.167,
      0.028,
      0.033,
      0.061)
  # Table 6.4
  population_means_PCG <-
    c(
      0.912,
      0.034,
      0.005,
      0.002,
      0.028,
      0.003,
      0.001,
      0.012,
      0.001,
      0.0005,
      0.001,
      0.001,
      0.0005
    )
  # Table 6.5
  population_means_DCG <-
    c(
      0.972,
      0.004,
      0.005,
      0.004,
      0.004,
      0.003,
      0.001,
      0.002,
      0.002,
      0.0005,
      0.001,
      0.001,
      0.001,
      0.0005
    )

  #########################################
  # Set probabilities self-assessed-health-based subgroups
  #########################################

  # Table 6.6
  population_means_OECD <- c(0.796, 0.103, 0.049, 0.052)

  #########################################
  # Rescale probabilities to ensure [0,1] range
  #########################################

  fn_scale_probs <- function(x) {
    x / sum(x)
  }

  population_means_gender_age <-
    fn_scale_probs(population_means_gender_age)
  population_means_incsrc <- fn_scale_probs(population_means_incsrc)
  population_means_region <- fn_scale_probs(population_means_region)
  population_means_PCG <- fn_scale_probs(population_means_PCG)
  population_means_DCG <- fn_scale_probs(population_means_DCG)

  population_means_OECD <- fn_scale_probs(population_means_OECD)

  #########################################
  # Create the risk adjusters
  #########################################

  gender_age <-
    matrix(
      stats::rmultinom(nobs, 1, prob = population_means_gender_age),
      nrow = nobs,
      ncol = length(population_means_gender_age),
      byrow = TRUE
    )
  colnames(gender_age) <-
    c(paste0("M_AGE", 1:(
      length(population_means_gender_age) / 2
    )), paste0("F_AGE", 1:(
      length(population_means_gender_age) / 2
    )))

  incsrc <-
    matrix(
      stats::rmultinom(nobs, 1, prob = population_means_incsrc),
      nrow = nobs,
      ncol = length(population_means_incsrc),
      byrow = TRUE
    )
  colnames(incsrc) <-
    paste0("INCSRC", 1:(length(population_means_incsrc)))

  region <-
    matrix(
      stats::rmultinom(nobs, 1, prob = population_means_region),
      nrow = nobs,
      ncol = length(population_means_region),
      byrow = TRUE
    )
  colnames(region) <-
    paste0("REGION", 1:(length(population_means_region)))

  PCG <-
    matrix(
      stats::rmultinom(nobs, 1, prob = population_means_PCG),
      nrow = nobs,
      ncol = length(population_means_PCG),
      byrow = TRUE
    )
  colnames(PCG) <- paste0("PCG", 0:(length(population_means_PCG) - 1))

  DCG <-
    matrix(
      stats::rmultinom(nobs, 1, prob = population_means_DCG),
      nrow = nobs,
      ncol = length(population_means_DCG),
      byrow = TRUE
    )
  colnames(DCG) <- paste0("DCG", 0:(length(population_means_DCG) - 1))

  hprice <- stats::rnorm(nobs, mean = 5.94)
  gpdist <- stats::rnorm(nobs, mean = 0.16)
  hdist <- stats::rnorm(nobs, mean = 4.38)

  #########################################
  # Create self-assessed-health-based subgroups
  #########################################

  PF <-
    truncnorm::rtruncnorm(
      nobs,
      a = 0,
      b = 1,
      mean = 0.80,
      sd = 0.24
    ) # Source: Medical Care
  RP <-
    truncnorm::rtruncnorm(
      nobs,
      a = 0,
      b = 1,
      mean = 0.72,
      sd = 0.40
    ) # Source: Medical Care
  BP <-
    truncnorm::rtruncnorm(
      nobs,
      a = 0,
      b = 1,
      mean = 0.73,
      sd = 0.25
    ) # Source: Medical Care
  GH <-
    truncnorm::rtruncnorm(
      nobs,
      a = 0,
      b = 1,
      mean = 0.67,
      sd = 0.21
    ) # Source: Medical Care
  VT <-
    truncnorm::rtruncnorm(
      nobs,
      a = 0,
      b = 1,
      mean = 0.64,
      sd = 0.20
    ) # Source: Medical Care
  SF <-
    truncnorm::rtruncnorm(
      nobs,
      a = 0,
      b = 1,
      mean = 0.81,
      sd = 0.24
    ) # Source: Medical Care
  RE <-
    truncnorm::rtruncnorm(
      nobs,
      a = 0,
      b = 1,
      mean = 0.79,
      sd = 0.36
    ) # Source: Medical Care
  MH <-
    truncnorm::rtruncnorm(
      nobs,
      a = 0,
      b = 1,
      mean = 0.74,
      sd = 0.19
    ) # Source: Medical Care

  OECD <-
    matrix(
      stats::rmultinom(nobs, 1, prob = population_means_OECD),
      nrow = nobs,
      ncol = length(population_means_OECD),
      byrow = TRUE
    )
  colnames(OECD) <-
    paste0("OECD", 0:(length(population_means_OECD) - 1))

  CCI <-
    truncnorm::rtruncnorm(
      nobs,
      a = 0,
      b = 0.79,
      mean = 0.04,
      sd = 0.07
    ) # Source: Medical Care

  # Table A4.4
  PCS <-
    sum(
      c(
        0.44622,
        0.30722,
        0.35220,
        0.23948,
        0.00060,
        0.02284,
        -0.18696,
        -0.28056
      ) * c(PF, RP, BP, GH, VT, SF, RE, MH)
    )
  MCS <-
    sum(
      c(
        -0.26540,
        -0.09193,
        -0.15782,
        -0.01826,
        0.25079,
        0.22837,
        0.41798,
        0.52900
      ) * c(PF, RP, BP, GH, VT, SF, RE, MH)
    )

  #########################################
  # Create one table w/ all risk adjusters
  #########################################

  X <-
    data.frame(gender_age, incsrc, region, PCG, DCG, hprice, gpdist, hdist)
  beta <-
    c(
      beta_gender_age,
      beta_incsrc,
      beta_region,
      beta_pcg,
      beta_dcg,
      beta_hprice,
      beta_gpdist,
      beta_hdist
    )

  #########################################
  # Create health expenses variable
  #########################################

  Xbeta <- beta_constant + as.matrix(X) %*% as.matrix(beta)

  Y <- Xbeta + stats::rlnorm(nobs, meanlog = 0, sdlog = 1)

  #########################################
  # Create data set
  #########################################

  phd_data <- data.frame(Y, X)

}
