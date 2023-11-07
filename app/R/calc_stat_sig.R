run_sig_test <- function(control_val, treatment_val, control_n, treatment_n, alpha, two_tailed) {

  # sample proportions for treatment and control
  # p_pool <- (control_val + treatment_val)/(control_n + treatment_n)

  p_control <- (control_val * (1-control_val)) / control_n
  p_treatment <- (treatment_val * (1-treatment_val)) / treatment_n

  # pooled std error
  se_pool<- sqrt(p_control + p_treatment)

  # margin of error for the pool
  if(two_tailed) {
    prob <- 1 - alpha/2
  } else {
    prob <- 1 - alpha
  }
  moe <- se_pool * qnorm(prob)

  # difference in proportion
  d_hat <- treatment_val - control_val

  z_score <- abs(d_hat)/se_pool

  p_value <- pnorm(q = -z_score, mean = 0, sd = 1)

  if(two_tailed) {
    p_value <- p_value * 2
  }

  ci <- c(d_hat - moe, d_hat + moe)

  # control CI
  x_hat_control <- control_val
  se_hat_control <- sqrt(x_hat_control * (1 - x_hat_control)/control_n)
  ci_control <- c(x_hat_control - qnorm(prob)*se_hat_control, x_hat_control + qnorm(prob)*se_hat_control)

  # treatment CI
  x_hat_treatment <- treatment_val
  se_hat_treatment <- sqrt(x_hat_treatment*(1-x_hat_treatment)/treatment_n)
  ci_treatment <- c(x_hat_treatment - qnorm(prob)*se_hat_treatment, x_hat_treatment + qnorm(prob)*se_hat_treatment)

  result_pool <- tibble::tibble(
    metric = c(
      'alpha',
      'estimated_diff',
      'relative_uplift',
      'diff_std_error',
      'z_score',
      'p_value',
      'margin_of_error',
      'ci_lower',
      'ci_upper'),
    value = c(
      alpha,
      treatment_val - control_val,
      (treatment_val - control_val) / control_val,
      se_pool,
      z_score,
      p_value,
      moe,
      ci[1],
      ci[2]
    ))

  result_variant <- tibble::tibble(
    variant = c("control", "treatment"),
    sample_size = c(control_n, treatment_n),
    value = c(control_val, treatment_val),
    standard_error = c(se_hat_control, se_hat_treatment),
    ci_lower = c(ci_control[1], ci_treatment[1]),
    ci_upper = c(ci_control[2], ci_treatment[2])
  )

  result_list <- list(experiment_results = result_pool,
                      variant_data = result_variant)

  result_list
}

 # stat_sig_data <- run_sig_test(control_val = 0.02773925, treatment_val = 0.05068493, control_n = 721, treatment_n = 730, alpha = 0.05, two_tailed = TRUE)
