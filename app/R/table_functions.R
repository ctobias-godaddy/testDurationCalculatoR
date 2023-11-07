
build_mde_tbl <- function(test_type = c("rate", "value"), current_val, mde_val, mde_increment, traffic, n_var, a, b, sdx = NULL, gcr = NULL) {

  # mde_increment <- mde_increment/100
  seq_mde_pos <- seq(from = mde_val, by = mde_increment, length.out = 6)
  seq_mde_neg <- seq(from = mde_val-5*mde_increment, to = mde_val-mde_increment, by = mde_increment)
  seq_mde <- c(seq_mde_neg, seq_mde_pos)

  seq_mde_tbl <- tibble::tibble(mde_var = seq_mde)

  if(test_type == "rate") {
    mde_tbl <- seq_mde_tbl %>%
      dplyr::group_by(mde_var) %>%
      purrr::map(~calc_sample_rate(
        x = current_val,
        mde = .,
        daily_traffic = traffic,
        n_variants = n_var,
        alpha = a,
        beta = b,
        avg_gcr_success = gcr
        )
      ) %$%
      mde_var %>%
      dplyr::bind_cols(seq_mde_tbl) %>%
      dplyr::select(mde = mde_var, new_rate, sample_size_per_variant, test_duration_days, annualised_gcr_impact)
  }

  if(test_type == "value") {

    if(is.null(sdx)) stop("std dev of current value required to use type value")

    mde_tbl <- seq_mde_tbl %>%
      dplyr::group_by(mde_var) %>%
      purrr::map(~calc_sample_value(
        x = current_val,
        mde = .,
        daily_traffic = traffic,
        n_variants = n_var,
        alpha = a,
        beta = b,
        sd_x = sdx,
        avg_gcr_success = gcr
        )
      ) %$%
      mde_var %>%
      dplyr::bind_cols(seq_mde_tbl) %>%
      dplyr::select(mde = mde_var, new_value, sample_size_per_variant, test_duration_days, annualised_gcr_impact)
  }

  mde_tbl
}

build_test_tbl <- function(data, test_type = c("rate", "value"), alpha, mde, sd_x = NULL) {

  visitors_control <- data$sample_size_per_variant
  visitors_test <- visitors_control

  if(test_type == "rate"){

    rate_control <- data$current_rate
    r_ctrl <- rate_control * (1 - rate_control)
    rate_test <- data$current_rate * (1+mde)
    r_tst <- rate_test * (1 - rate_test)

    lift <- rate_test / rate_control - 1

    rate_diff <- rate_test - rate_control
    r_df <- sqrt((r_ctrl / visitors_control) + (r_tst / visitors_test))

    conf_int <- stats::qnorm(1-alpha/2) * r_df

    lift_ci <- conf_int / rate_control

    pval <- 2 * (1 - stats::pnorm(abs(rate_diff)/r_df))

    test_tbl <- tibble::tibble(control_visitors = round(visitors_control, 0),
                               test_visitors = round(visitors_test, 0),
                               control_rate = round(rate_control, 3),
                               test_rate = round(rate_test, 3),
                               diff = round(rate_diff, 3),
                               lift = round(lift, 3),
                               confidence_int = round(conf_int, 3),
                               lift_conf_int = round(lift_ci, 3),
                               p_value = round(pval, 3)
                               )
  }

  if(test_type == "value"){

    value_control <- data$current_value
    value_test <- data$current_value * (1+mde)

    lift <- value_test / value_control - 1
    val_diff <- value_test - value_control

    v_df <- sqrt((sd_x^2 / visitors_control) + (sd_x^2 / visitors_test))

    conf_int <- stats::qnorm(1-alpha/2) * v_df

    lift_ci <- conf_int / value_control

    pval <- stats::pnorm(abs(val_diff)/v_df)

    test_tbl <- tibble::tibble(control_visitors = round(visitors_control, 0),
                               test_visitors = round(visitors_test, 0),
                               control_value = round(value_control, 3),
                               test_value = round(value_test, 3),
                               diff = round(val_diff, 3),
                               lift = round(lift, 3),
                               confidence_int = round(conf_int, 3),
                               lift_conf_int = round(lift_ci, 3),
                               p_value = round(pval, 3)
    )
  }

  test_tbl
}

get_pval_tbl <- function(x_control, x_treatment, inc_type, inc_value, sample_size, n_variants, alpha, two_tailed) {

  if(inc_type == "treatment") {
    seq_pos <- seq(from = x_treatment, by = inc_value, length.out = 6)
    seq_neg <- seq(from = x_treatment-5*inc_value, to = x_treatment-inc_value, by = inc_value)
    seq_req <- c(seq_neg, seq_pos)

    seq_tbl <- tibble::tibble(seq_var = seq_req)

    pval_tbl <- seq_tbl %>%
      split(.$seq_var) %>%
      purrr::map(~run_sig_test(
        control_val = x_control,
        treatment_val = .[[1]],
        control_n = sample_size / n_variants,
        treatment_n = sample_size / n_variants,
        alpha = alpha,
        two_tailed = two_tailed
      )
      ) %>%
      tibble::enframe(name = "treatment_value") %>%
      tidyr::unnest(value) %>%
      tidyr::unnest(value) %>%
      dplyr::select(treatment_value, metric, value) %>%
      tidyr::drop_na()
  }

  if(inc_type == "sample") {
    seq_pos <- seq(from = sample_size, by = inc_value, length.out = 6)
    seq_neg <- seq(from = sample_size-5*inc_value, to = sample_size-inc_value, by = inc_value)
    seq_req <- c(seq_neg, seq_pos)

    seq_tbl <- tibble::tibble(seq_var = seq_req)

    pval_tbl <- seq_tbl %>%
      split(.$seq_var) %>%
      purrr::map(~run_sig_test(
        control_val = x_control,
        treatment_val = x_treatment,
        control_n = .[[1]] / n_variants,
        treatment_n = .[[1]] / n_variants,
        alpha = alpha,
        two_tailed = two_tailed
      )
      ) %>%
      tibble::enframe(name = "total_sample") %>%
      tidyr::unnest(value) %>%
      tidyr::unnest(value) %>%
      dplyr::select(total_sample, metric, value) %>%
      tidyr::drop_na()
  }

  pval_tbl_wide <- pval_tbl %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)

  pval_tbl_wide
}


