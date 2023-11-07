#' get sample size using rate
#'
#' @param x current rate
#' @param mde relative minimum detectable effect
#' @param daily_traffic daily eligible traffic in tested area - unique visitors
#' @param n_variants number of variants, including control
#' @param alpha level of statistical significance. probability of committing a type 1 error - false positive
#' @param beta probability of committing a type 2 error - false negative
#' @param avg_gcr_success optional
#'
#' @return tbl
#'
calc_sample_rate <- function(x,
                             mde,
                             daily_traffic,
                             n_variants,
                             alpha,
                             beta,
                             avg_gcr_success = NULL) {

  rate <- x * (1 + mde)
  conf_low <- stats::qnorm(1 - alpha / 2)
  conf_upp <- stats::qnorm(1 - beta)
  var_control <- x * (1 - x)
  var_test <- rate * (1 - rate)

  sample_size_variant <- (conf_low + conf_upp)^2 * (var_control + var_test) / (x - rate)^2
  sample_size_total <- sample_size_variant * n_variants

  test_dur <- sample_size_total / daily_traffic
  ann_success <- x * mde * daily_traffic * 365

  if(!is.null(avg_gcr_success)){
    ann_gcr_impact <- ann_success * avg_gcr_success
  } else {
    ann_gcr_impact <- NULL
  }

  rate_tbl <- tibble::tibble(current_rate = x,
                    new_rate = rate,
                    sample_size_per_variant = sample_size_variant,
                    total_sample_size = sample_size_total,
                    test_duration_days = test_dur,
                    annualised_success = ann_success,
                    annualised_gcr_impact = ann_gcr_impact)

  rate_tbl
}
