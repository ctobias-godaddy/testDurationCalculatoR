server.testDurationCalculatoR <- function(input, output, session) {

  # shinyjs::disable("n_var") # maths only supports A/B testing at the moment so number of variants restricted to 2

  # don't render anything initially
  output$mdeTbl <- DT::renderDT(NULL)
  output$testTbl <- DT::renderDT(NULL)
  output$mdePlot <- plotly::renderPlotly(NULL)
  output$gcrPlot <- plotly::renderPlotly(NULL)
  output$distroPlot <- plotly::renderPlotly(NULL)
  output$pvalPlot <- plotly::renderPlotly(NULL)
  output$pvalTbl <- DT::renderDT(NULL)

  # render ui outputs in sidebar
  output$current_rate <- renderUI({
    req(input$calcType == "rate" | input$calcType == "mde")
    shinyWidgets::currencyInput(inputId = "currentRate",
                                label = "Current Rate",
                                value = 0.04,
                                format = "percentageUS2dec")
    })

  output$sd_x <- renderUI({
    req(input$calcType == "value")
    numericInput(inputId = "sdX",
                 label = "Std Dev. Value",
                 value = 21)
  })

  output$current_value <- renderUI({
    req(input$calcType == "value")
    numericInput(inputId = "currentValue",
                 label = "Current Value",
                 value = 2.13)
  })

  output$test_dur <- renderUI({
    req(input$calcType == "mde")
    numericInput(inputId = "testDuration",
                 label = "Planned Test Duration (Days)",
                 value = 14)
  })

  output$mde_rate <- renderUI({
    req(input$calcType == "rate")
    shinyWidgets::currencyInput(inputId = "mdeRate",
                                label = "Relative Min Detectable Effect",
                                value = 0.02,
                                format = "percentageUS2dec")
  })

  output$mde_value <- renderUI({
    req(input$calcType == "value")
    shinyWidgets::currencyInput(inputId = "mdeValue",
                                label = "Min Detectable Effect",
                                value = 0.02,
                                format = "percentageUS2dec")
  })

  # calculated test duration and sample size
  test_durn_data <- reactive({
    switch(input$calcType,
           rate = calc_sample_rate(x = input$currentRate,
                                   mde = input$mdeRate,
                                   daily_traffic = input$traffic,
                                   n_variants = input$n_var,
                                   alpha = input$a/100,
                                   beta = input$b/100,
                                   avg_gcr_success = input$gcr),
           value = calc_sample_value(x = input$currentValue,
                                     mde = input$mdeValue,
                                     daily_traffic = input$traffic,
                                     n_variants = input$n_var,
                                     alpha = input$a/100,
                                     beta = input$b/100,
                                     sd_x = input$sdX,
                                     avg_gcr_success = input$gcr),
           mde = calc_mde(x = input$currentRate,
                          planned_duration = input$testDuration,
                          daily_traffic = input$traffic,
                          n_variants = input$n_var,
                          alpha = input$a/100,
                          beta = input$b/100,
                          avg_gcr_success = input$gcr)
           )
  })

  output$sample_size_total <- renderValueBox({
    infoBox(
      "Sample Size (Total)",
      format(round(test_durn_data()$total_sample_size, digits = 0), big.mark=","),
      icon = icon("alien-8bit", lib = "font-awesome"),
      color = "lightblue"
    )
  })

  output$sample_size_variant <- renderValueBox({
    infoBox(
      "Sample Size (Variant)",
      format(round(test_durn_data()$sample_size_per_variant, digits = 0), big.mark=","),
      "Sample size per variant",
      icon = icon("alien-8bit", lib = "font-awesome"),
      color = "lightblue"
    )
  })

  output$test_dur_box <- renderValueBox({
    req(input$calcType == "rate" | input$calcType == "value")
    infoBox(
      "Test Duration (Days)",
      format(round(test_durn_data()$test_duration_days, digits = 0), big.mark=","),
      icon = icon("alien-8bit", lib = "font-awesome"),
      color = "lightblue"
    )
  })

  output$mde_box <- renderValueBox({
    req(input$calcType == "mde")
    infoBox(
      "Minimum Detectable Effect",
      paste0(round(test_durn_data()$min_detectable_effect*100, digits = 2),"%"),
      icon = icon("alien-8bit", lib = "font-awesome"),
      color = "lightblue"
    )
  })

  output$gcr_box <- renderValueBox({
    infoBox(
      "Annualised GCR Impact",
      paste0("$",format(round(test_durn_data()$annualised_gcr_impact, digits = 0), big.mark=",")),
      icon = icon("alien-8bit", lib = "font-awesome"),
      color = "lightblue"
    )
  })

  ###################

  mde_data <- reactive({

    req(input$calcType == "rate" | input$calcType == "value")

    if(input$calcType == "rate"){
      curr_val = input$currentRate
      mde = input$mdeRate}
    if(input$calcType == "value"){
      curr_val = input$currentValue
      mde = input$mdeValue}

    reqd_data <- build_mde_tbl(test_type = input$calcType,
                  current_val = curr_val,
                  mde_val = mde,
                  mde_increment = input$mdeInc/100,
                  traffic = input$traffic,
                  n_var = input$n_var,
                  a = input$a/100,
                  b = input$b/100,
                  sdx = input$sdX,
                  gcr = input$gcr)

    reqd_data
  })

  output$mdePlot <- plotly::renderPlotly({
    req(input$calcType == "rate" | input$calcType == "value")
    plotly::ggplotly(build_mde_chart(mde_data = mde_data(), y_var = "mde"))
  })

  output$gcrPlot <- plotly::renderPlotly({
    req(input$calcType == "rate" | input$calcType == "value")
    plotly::ggplotly(build_mde_chart(mde_data = mde_data(), y_var = "annualised_gcr_impact"))
  })

  output$mdeTbl <- DT::renderDataTable({

    req(input$calcType == "rate" | input$calcType == "value")

    if(input$calcType == "rate") {

      background <- paste0("value == ",input$mdeRate," ? 'lightblue' : value != 'else' ? 'white' : ''")
      class(background) <- "JS_EVAL"

      tbl <- DT::datatable(mde_data(),
                           rownames = FALSE,
                           options = list(
                             pageLength = 11,
                             searching = FALSE,
                             paging = FALSE
                           ),
                           colnames = c('New Rate' = 'new_rate',
                                        'Relative Min Detectable Effect' = 'mde',
                                        'Sample Size per Variant' = 'sample_size_per_variant',
                                        'Test Duration (Days)' = 'test_duration_days',
                                        'Ann. GCR Impact ($)' = 'annualised_gcr_impact')) %>%
        DT::formatPercentage('New Rate', digits = 2) %>%
        DT::formatPercentage('Relative Min Detectable Effect', digits = 2) %>%
        DT::formatCurrency('Ann. GCR Impact ($)') %>%
        DT::formatRound('Test Duration (Days)', digits = 0) %>%
        DT::formatRound('Sample Size per Variant', digits = 0) %>%
        DT::formatStyle(
          'Relative Min Detectable Effect',
          target = 'row',
          backgroundColor = background)
    }

    if(input$calcType == "value") {

      background <- paste0("value == ",input$mdeValue," ? 'lightblue' : value != 'else' ? 'white' : ''")
      class(background) <- "JS_EVAL"

      tbl <- DT::datatable(mde_data(),
                           rownames = FALSE,
                           options = list(
                             pageLength = 11,
                             searching = FALSE,
                             paging = FALSE
                           ),
                           colnames = c('New Value' = 'new_value',
                                        'Min Detectable Effect' = 'mde',
                                        'Sample Size per Variant' = 'sample_size_per_variant',
                                        'Test Duration (Days)' = 'test_duration_days',
                                        'Ann. GCR Impact ($)' = 'annualised_gcr_impact')) %>%
        DT::formatRound('New Value', digits = 2) %>%
        DT::formatPercentage('Min Detectable Effect', digits = 2) %>%
        DT::formatCurrency('Ann. GCR Impact ($)') %>%
        DT::formatRound('Test Duration (Days)', digits = 0) %>%
        DT::formatRound('Sample Size per Variant', digits = 0) %>%
        DT::formatStyle(
          'Min Detectable Effect',
          target = 'row',
          backgroundColor = background)

    }

    tbl
    })

  output$testResultTbl <- DT::renderDataTable({

    req(input$calcType == "rate" | input$calcType == "value")

    tbl <- switch (input$calcType,
      rate = build_test_tbl(data = test_durn_data(), test_type = input$calcType, mde = input$mdeRate, alpha = input$a/100),
      value = build_test_tbl(data = test_durn_data(), test_type = input$calcType, mde = input$mdeValue, alpha = input$a/100, sd_x = input$sdX)
    )

    DT::datatable(tbl,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE
                    )
                  )
  })


  ################# export results

  export_data <- reactive({

    if(input$calcType == "rate"){curr_val = input$currentRate}
    if(input$calcType == "value"){curr_val = input$currentValue}

    mde_data() %>%
      dplyr::mutate(current_value = curr_val,
                    traffic = input$traffic,
                    n_variants = input$n_var,
                    alpha = input$a/100,
                    beta = input$b/100,
                    sd_current_value = input$sdX,
                    gcr = input$gcr)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("testDurationCalculator_export_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(export_data(), file)
    }
  )

  ########### stat sig testing

  output$x_control <- renderUI({
    numericInput(inputId = "xControl",
                label = "Control Value",
                value = if(input$calcType == "rate" | input$calcType == "mde") {
                input$currentRate}
                else {
                input$currentValue})
  })

  output$x_treatment <- renderUI({
    numericInput(inputId = "xTreatment",
                 label = "Treatment Value",
                 value = if(input$calcType == "rate" | input$calcType == "mde") {
                   input$currentRate * (1+input$mdeRate)}
                 else {
                   input$currentValue * (1+input$mdeValue)})
  })

  output$total_sample <- renderUI({
    shinyWidgets::autonumericInput(inputId = "totalSample",
                 label = "Total Sample Size",
                 value = test_durn_data()$total_sample_size,
                 decimalPlaces = 0)
    })

  output$inc_treatment <- renderUI({
    req(input$selectInc == "treatment", input$xTreatment)

    sliderInput(inputId = "incTreatment",
                label = "Increment Treatment Value",
                min = 0,
                max = input$xTreatment,
                value = input$xTreatment/10
                )
  })

  output$inc_sample <- renderUI({
    req(input$selectInc == "sample")

    sliderInput(inputId = "incSample",
                label = "Increment Sample Size",
                min = 0,
                max = test_durn_data()$total_sample_size,
                value = test_durn_data()$total_sample_size/10
                )
  })

  stat_sig_data <- reactive({
    run_sig_test(control_val = input$xControl,
                 treatment_val = input$xTreatment,
                 control_n = input$totalSample / input$n_var,
                 treatment_n = input$totalSample / input$n_var,
                 alpha = input$a/100,
                 two_tailed = input$two_tailed)
  })

  variant_density_data <- reactive({
    get_variant_density_data(stat_sig_data())
  })

  output$distroPlot <- plotly::renderPlotly({
    plotly::ggplotly(build_variant_distro_chart(variant_density_data()))
  })

  output$varStatTbl <- DT::renderDataTable({

    DT::datatable(stat_sig_data()$variant_data,
                         rownames = FALSE,
                         options = list(
                           searching = FALSE,
                           paging = FALSE
                         )
    )

  })

  output$testStatTbl <- DT::renderDataTable({

    DT::datatable(stat_sig_data()$experiment_results,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    paging = FALSE
                  )
    )

  })

  pval_tbl_data <- reactive({
    get_pval_tbl(x_control = input$xControl,
                 x_treatment = input$xTreatment,
                 inc_type = input$selectInc,
                 inc_value = if(input$selectInc == "treatment") {
                   input$incTreatment}
                 else {input$incSample},
                 sample_size = input$totalSample,
                 n_variants = input$n_var,
                 alpha = input$a,
                 two_tailed = input$two_tailed)
  })

  model_density_data <- reactive({
    get_model_density_data(stat_sig_data())
  })

  output$pvalPlot <- plotly::renderPlotly({
    plotly::ggplotly(build_model_distro_chart(model_density_data()))
  })

  output$pvalTbl <- DT::renderDataTable({

    tbl <- pval_tbl_data() %>%
      dplyr::select(-alpha, -diff_std_error)

    DT::datatable(tbl,
                  rownames = FALSE,
                  options = list(
                    pageLength = 11,
                    searching = FALSE,
                    paging = FALSE
                  )
    )

  })
}

