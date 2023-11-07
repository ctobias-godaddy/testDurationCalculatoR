build_mde_chart <- function(mde_data, y_var = c("mde", "annualised_gcr_impact")) {

  y_ttl <- switch(y_var,
                  mde = "Minimum Detectable Effect",
                  annualised_gcr_impact = "Annualised GCR Impact ($)")

  ttl <- paste0(y_ttl, " vs Test Duration (Days)")

  p <- ggplot2::ggplot(mde_data,
                       ggplot2::aes_string(x = "test_duration_days", y = y_var))
    p <- p + ggplot2::geom_point()
    p <- p + ggplot2::geom_smooth(se = FALSE)
    p <- p + ggplot2::labs(title = ttl,
                           x = "Test Duration (Days)",
                           y = y_ttl)
    p <- p + ggplot2::theme_minimal()

    if(y_var == "mde") {
      p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
    }

    if(y_var == "annualised_gcr_impact") {
      p <- p + ggplot2::scale_y_continuous(labels = scales::dollar_format())
    }

    p
}

get_variant_density_data <- function(stat_sig_data) {

  variant_data <- stat_sig_data$variant_data
  experiment_results <- stat_sig_data$experiment_results %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)

  use_ss <- min(variant_data$sample_size) # ss_control and treatment are the same size here by design but necessarily have to be exactly equal
  # if(min_ss > 1*10^6) {use_ss = 1*10^6} else {use_ss = min_ss} # limit to 1m to save on geom_density computation

  stat_sig_data <- tibble::tibble(
    variant = factor(rep(c(variant_data$variant[1], variant_data$variant[2]), each = use_ss)),
    conversion_rate = c(rnorm(use_ss, variant_data$value[1], variant_data$standard_error[1]),
                        rnorm(use_ss, variant_data$value[2], variant_data$standard_error[2]))
  )

  conv_rate_data <- tibble::tibble(
    variant = c(variant_data$variant[1], variant_data$variant[2]),
    conversion_rate = c(variant_data$value[1], variant_data$value[2]),
    ci_lower = c(variant_data$ci_lower[1], variant_data$ci_lower[2]),
    ci_upper = c(variant_data$ci_upper[1], variant_data$ci_upper[2])
  )

  result <- list(density_data = stat_sig_data,
                 conv_rate_data = conv_rate_data,
                 stats = experiment_results)

  result
}

build_variant_distro_chart <- function(variant_plot_data) {

  density_plot_data <- variant_plot_data$density_data
  cr_data <- variant_plot_data$conv_rate_data

  y_max <- max(density(variant_plot_data$density_data$conversion_rate)$y)
  alpha <- variant_plot_data$alpha

  p <- ggplot2::ggplot(density_plot_data, ggplot2::aes(x = conversion_rate))

  p <- p + ggplot2::geom_density(ggplot2::aes(fill = variant), alpha = 0.3)

  p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = conversion_rate,
                                            color = variant),
                               data = cr_data,
                               linetype = "dashed")

  p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = ci_lower,
                                            color = variant),
                               data = cr_data,
                               linetype = "dotted")

  p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = ci_upper,
                                            color = variant),
                               data = cr_data,
                               linetype = "dotted")

  p <- p + ggplot2::annotate(geom = "text", x = cr_data$conversion_rate[1],  y = y_max*0.75,
             hjust = 0,     # zero left justifies the text annotation horizontally. 0.5 is center, 1 is right justified.
             vjust = 0.5,   # > +0.1 range pushes the text annotation down vertically
             label = paste0("Control: ", round(cr_data$conversion_rate[1],4)*100,"%"),
             fontface = "bold",
             angle = 90)

  p <- p + ggplot2::annotate(geom = "text", x = cr_data$conversion_rate[2],  y = y_max*0.75,
             hjust = 0,
             vjust = 0.5,
             label = paste0("Treatment: ", round(cr_data$conversion_rate[2],4)*100,"%"),
             fontface = "bold",
             angle = 90)

  p <- p + ggplot2::annotate(geom="text", x = c(cr_data$ci_lower[1], cr_data$ci_upper[1]), y = y_max*0.5,
                             hjust = 0,
                             vjust = 0.5,
                             label = paste0(100 - alpha * 100, "%CL Control"),
                             angle = 90,
                             size = 2.5)

  p <- p + ggplot2::annotate(geom="text", x = c(cr_data$ci_lower[2], cr_data$ci_upper[2]), y = y_max*0.5,
                             hjust = 0,
                             vjust = 0.5,
                             label = paste0(100 - alpha * 100, "% CL Treatment"),
                             angle = 90,
                             size = 2.5)

  p <- p + ggplot2::labs(title = "Expected Distributions of Variants A (Control) and B (Treatment)",
         x = "Conversion",
         y = "Density")

  p <- p + ggplot2::theme_minimal()

  p
}

get_model_density_data <- function(stat_sig_data) {

  experiment_results <- stat_sig_data$experiment_results %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)

  use_ss <- sum(stat_sig_data$variant_data$sample_size)

  random_norm_distro <- tibble::tibble(diff_distro = rnorm(use_ss, experiment_results$estimated_diff, experiment_results$diff_std_error))

  point_data <- tibble::tibble(
    estimated_diff = experiment_results$estimated_diff,
    ci_lower = experiment_results$ci_lower,
    ci_upper = experiment_results$ci_upper,
    alpha = experiment_results$alpha,
    p_value = experiment_results$p_value,
    z_score = experiment_results$z_score)

  result <- list(density_data = random_norm_distro,
                 point_data = point_data)

  result
}

build_model_distro_chart <- function(model_plot_data) {

  density_plot_data <- model_plot_data$density_data
  point_value_data <- model_plot_data$point_data

  y_max <- max(density(model_plot_data$density_data$diff_distro)$y)
  alpha <- point_value_data$alpha

  p <- ggplot2::ggplot(density_plot_data, ggplot2::aes(x = diff_distro))

  p <- p + ggplot2::geom_density()

  p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = estimated_diff),
                               data = point_value_data,
                               linetype = "dashed")

  p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = ci_lower),
                               data = point_value_data,
                               linetype = "dotted")

  p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = ci_upper),
                               data = point_value_data,
                               linetype = "dotted")

  p <- p + ggplot2::annotate(geom = "text", x = point_value_data$estimated_diff,  y = y_max*0.75,
                             hjust = 0,     # zero left justifies the text annotation horizontally. 0.5 is center, 1 is right justified.
                             vjust = 0.5,   # > +0.1 range pushes the text annotation down vertically
                             label = paste0("Diff: ", round(point_value_data$estimated_diff, 4)*100,"%"),
                             fontface = "bold",
                             angle = 90)

  p <- p + ggplot2::annotate(geom="text", x = point_value_data$ci_upper, y = y_max*0.5,
                             hjust = 0,
                             vjust = 0.5,
                             label = paste0(100 - alpha * 100, "% Conf Level"),
                             angle = 90,
                             size = 2.5)

  p <- p + ggplot2::annotate(geom="text", x = point_value_data$ci_lower, y = y_max*0.5,
                             hjust = 0,
                             vjust = 0.5,
                             label = paste0(100 - alpha * 100, "% Conf Level"),
                             angle = 90,
                             size = 2.5)

  p <- p + ggplot2::labs(title = "Expected Distribution of Difference",
                         x = "Conversion Difference",
                         y = "Density")

  p <- p + ggplot2::theme_minimal()

  p

}
