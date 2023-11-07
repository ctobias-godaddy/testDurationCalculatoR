
# define sidebar
sidebar <- shinydashboard::dashboardSidebar(

  shinyjs::useShinyjs(),
  width = 300,

  shinydashboard::sidebarMenu(

    style = "position: fixed; width: 300px; overflow: auto; height: calc(100vh - 50px) !important;",

    menuItem("Sample Size & Duration", tabName = "ssd"),
    menuItem("Significance Testing", tabName = "sigtest"),

    selectizeInput(inputId = "calcType",
                   label = "Select Calculation Type",
                   choices = list("Duration using Rate" = "rate", "Duration using Value" = "value", "Min Detectable Effect" = "mde"),
                   selected = "rate",
                   multiple = FALSE),

    br(),

    fluidRow(
      column(6, uiOutput("current_rate")
      ),
      column(6, uiOutput("current_value")
      )
    ),

    fluidRow(
      column(6, sliderInput(inputId = "a",
                            label = "Alpha",
                            min = 1,
                            max = 10,
                            post  = " %",
                            value = 5,
                            step = 0.5)
      ),
      column(6, sliderInput(inputId = "b",
                            label = "Beta",
                            min = 1,
                            max = 20,
                            post  = " %",
                            value = 10,
                            step = 0.5)
      )
    ),

    fluidRow(
      column(6, numericInput(inputId = "n_var",
                             label = "No. Variants",
                             value = 2)),
      column(6, uiOutput("sd_x"))
    ),

    shinyWidgets::currencyInput(inputId = "traffic",
                                label = "Daily Eligible Traffic - Unique Visitors",
                                value = 60353,
                                format = "dotDecimalCharCommaSeparator"),

    shinyWidgets::currencyInput(inputId = "gcr",
                                label = "Avg GCR per Success (Optional)",
                                value = 30,
                                format = "dollar"),

    uiOutput("test_dur"),

    fluidRow(
      column(6, uiOutput("mde_rate")),
      column(6, uiOutput("mde_value"))
    ),

    hr(),

    column(12, align = "left", offset = 0,
           downloadButton(outputId = "downloadData",
                          label = "Export",
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
    )



)

# define body
body <- shinydashboard::dashboardBody(

  tabItems(
    tabItem(tabName = "ssd",
            fluidPage(
              fluidRow(
                valueBoxOutput("sample_size_total"),
                valueBoxOutput("sample_size_variant"),
                valueBoxOutput("test_dur_box"),
                valueBoxOutput("gcr_box"),
                valueBoxOutput("mde_box")
                ),

              fluidRow(
                column(width = 6, plotly::plotlyOutput("mdePlot") %>% withSpinner()),
                column(width = 6, plotly::plotlyOutput("gcrPlot") %>% withSpinner())
                ),

              hr(),
              sliderInput(inputId = "mdeInc",
                          label = "MDE Increment (%)",
                          min = 0,
                          max = 5,
                          post  = " %",
                          value = 0.2,
                          step = 0.1),
              hr(),

              DT::dataTableOutput("mdeTbl") %>% withSpinner(),
              DT::dataTableOutput("testResultTbl")%>% withSpinner()
              )
            ),
    tabItem(tabName = "sigtest",
            fluidPage(
              fluidRow(
                shinyWidgets::materialSwitch(
                  inputId = "two_tailed",
                  label = "Two-Tailed Test?",
                  value = TRUE,
                  status = "primary"
                )
              ),
                column(width = 6,
                       fluidRow(
                         column(3, uiOutput("x_control")),
                         column(3, uiOutput("x_treatment")),
                         column(3, uiOutput("total_sample"))
                         ),
                       fluidRow(
                         plotly::plotlyOutput('distroPlot') %>% withSpinner()
                         ),
                       fluidRow(
                         DT::dataTableOutput("varStatTbl") %>% withSpinner()
                         ),
                       fluidRow(
                         DT::dataTableOutput("testStatTbl") %>% withSpinner()
                       )
                       ),
                column(width = 6,
                       fluidRow(
                         column(4, radioButtons(inputId = "selectInc",
                                                label = "Increment Variable",
                                                choices = list("Treatment Value" = "treatment", "Sample Size" = "sample"),
                                                selected = "treatment")),
                         column(4, uiOutput("inc_treatment")),
                         column(4, uiOutput("inc_sample"))
                         ),
                       fluidRow(
                         plotly::plotlyOutput('pvalPlot') %>% withSpinner()
                         ),
                       fluidRow(
                         DT::dataTableOutput("pvalTbl") %>% withSpinner()
                       )
                       )
                )
            )
    )

)

# Put them together into a dashboardPage
ui.testDurationCalculatoR <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = "Test Duration Calculator",
    titleWidth = 300),
  sidebar,
  body
)
