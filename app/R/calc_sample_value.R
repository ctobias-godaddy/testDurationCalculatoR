#' get sample size using value
#'
#' @param x current value
#' @param mde relative minimum detectable effect
#' @param daily_traffic daily eligible traffic in tested area - unique visitors
#' @param n_variants number of variants, including control
#' @param alpha level of statistical significance. probability of committing a type 1 error - false positive
#' @param beta probability of committing a type 2 error - false negative
#' @param sd_x standard deviation of x
#' @param avg_gcr_success optional
#'
#' @return tbl
#'
calc_sample_value <- function(x,
                              mde,
                              daily_traffic,
                              n_variants,
                              alpha,
                              beta,
                              sd_x,
                              avg_gcr_success = NULL) {

  value <- x * (1 + mde)
  conf_low <- stats::qnorm(1 - alpha / 2)
  conf_upp <- stats::qnorm(1 - beta)
  sd_test <- sd_x

  sample_size_variant <- (conf_low + conf_upp)^2 * (sd_x^2 + sd_test^2)/(x-value)^2
  sample_size_total <- sample_size_variant * n_variants

  test_dur <- sample_size_total / daily_traffic

  if(!is.null(avg_gcr_success)){
    ann_gcr_impact <- x * mde * daily_traffic * avg_gcr_success * 365
  } else {
    ann_gcr_impact <- NULL
  }

  value_tbl <- tibble::tibble(current_value = x,
                    new_value = value,
                    sample_size_per_variant = sample_size_variant,
                    total_sample_size = sample_size_total,
                    test_duration_days = test_dur,
                    annualised_gcr_impact = ann_gcr_impact)

  value_tbl
}
