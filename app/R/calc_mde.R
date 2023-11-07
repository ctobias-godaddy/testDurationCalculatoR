#' calculate mde
#'
#' @param x current rate
#' @param planned_duration planned test duration in days
#' @param daily_traffic daily eligibal traffic in tested area - unique visitors
#' @param n_variants number of variants, including control
#' @param alpha prob of type 1 error
#' @param beta prob of type 2 error
#' @param avg_gcr_success dollar value of success
#'
#' @return tbl_df
#' @export
#'
calc_mde <- function(x,
                     planned_duration,
                     daily_traffic,
                     n_variants,
                     alpha,
                     beta,
                     avg_gcr_success = NULL) {

  sample_size_total <- daily_traffic * planned_duration
  sample_size_variant <- sample_size_total / n_variants

  conf_low <- stats::qnorm(1 - alpha / 2)
  conf_upp <- stats::qnorm(1 - beta)
  sd_control <- x * (1 - x)

  mde <- sqrt((conf_low+conf_upp)^2*2*sd_control/sample_size_variant)/x
  rate <- x * (1 + mde)

  ann_success <- x * mde * daily_traffic * 365

  if(!is.null(avg_gcr_success)){
    ann_gcr_impact <- ann_success * avg_gcr_success
  } else {
    ann_gcr_impact <- NULL
  }

  mde_tbl <- tibble::tibble(current_rate = x,
                   new_rate = rate,
                   sample_size_per_variant = sample_size_variant,
                   total_sample_size = sample_size_total,
                   min_detectable_effect = mde,
                   annualised_success = ann_success,
                   annualised_gcr_impact = ann_gcr_impact)

  mde_tbl
}
