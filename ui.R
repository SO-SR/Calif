function(request)
{
  navbarPage(
    title = 'Calif 4.0',
    tabPanel(
      title = 'Overview',
      tags$div(style = 'text-align: center; font-size: 120%', 
               tags$h2('Welcome to Calif!'), 
               tags$p('Calif is the Shiny web application used for calibration of weights in statistical surveys. 
                      It is an open-source software and you are free to use it, also without any knowledge of R programming language.'),
               tags$p('Detailed information on how to use Calif can be found in the', 
                      a(href = 'https://slovak.statistics.sk/wps/wcm/connect/e87eb546-3a98-448c-9022-c9d4c54d2dcd/Calif_Manual_v3_3.pdf?MOD=AJPERES&CVID=lvrhxQ7&CVID=lvrhxQ7',
                        'Manual', class = "btn btn-primary btn-xs")),
               tags$hr(),
               tags$h3('Getting started'),
               tags$p('To get started working with Calif, you first need to prepare the data and the table of totals (which are the known population marginals). 
                      You can learn it in the Manual, in fact it is very easy and intuitive.'), 
               tags$p('Once they are ready, you need to load them into Calif in the', actionButton('next_tab', 'next tab', class = "btn btn-primary btn-xs"), 
                      ' After that, you select the calibration variables, either categorical or numerical, initial weights for calibration, 
                      possible stratification and calibration methods.'),
               tags$hr(),
               tags$h3('Optimal strategy'),
               tags$p('Then you can calibrate the weights. In Calif, after each calibration step, you are provided with useful statistics that can help you 
                      to decide which method and parameters are most suitable for performing the best possible calibration.'),
               tags$p('The optimal strategy is to find the new weights that reproduce the population totals and are as close as possible to the initial 
                      weights such that the lower and upper bounds form a narrow interval (optimally close to 1 from both sides)'),
               tags$p('and the average difference between the initial and calibration weights is low in comparison to the linear solution. 
                      The feasibility of the average difference is demonstrated by a pie chart on the', tags$b('Calibration'), 'tab.'),
               tags$p('If you set too tense bounds, even if the solution is found, the histogram of quotients on the', 
                      tags$b('Calibration'), 'tab will look unnaturally, the average difference will be high and such calibration will not be appropriate.'),
               tags$hr(),
               tags$h3('Working directory'),
               tags$p('Your actual working directory is set to', tags$code(textOutput('my_wd', inline = TRUE)), '. If you want to change it, 
                      you can do it by clicking on this', actionButton('change_wd', 'button', class = "btn btn-primary btn-xs"), 'or later when saving the outputs.'),
               tags$hr(),
               tags$h3('Bookmarking'),
               tags$p('You can bookmark your work at any time by clicking on the', tags$b('Bookmark'), 'button on the', tags$b('Calibration'), 'tab. 
                      The files and settings are saved into your working directory and you obtain a link that can be used to reproduce your work when Calif is run again.'),
               tags$hr(),
               tags$h3('Output'),
               tags$p('You can save your work at any time by clicking on the', tags$b('Save'), 'button on the', tags$b('Calibration'), 'tab. 
                      The output is an identical copy of your data with column', tags$em('CalibrationWeight'), 'added. 
                      This column represents your latest calibration attempt, i.e.'),
               tags$p('if you tried several calibrations with different settings, the last one is recorded. 
                      If you haven\'t performed any calibration yet,', tags$em('CalibrationWeight'), 'column contains the initial weights.'),
               tags$p('If you carry out stratified calibration,', tags$em('CalibrationWeight'), 'column contains the new weights for 
                      strata that have been already calibrated and the initial weights for strata that have remained untouched.'),
               tags$p('Although the latest state of play is still remembered throughout all strata, you are recommended to regularly save or bookmark your work in order not 
                      to lose it if some unexpected error occurs.')
               )
               ),
    tabPanel(
      title = 'Data',
      sidebarLayout(
        sidebarPanel(width = 3,
                     tags$h4('Load data', style = 'color: #367AB4'),
                     fileInput('load_data', label = NULL, accept = c('.txt', '.csv', '.sas7bdat')),
                     radioButtons('separator1', 'Separator', c('Semicolon' = ';', 'Comma' = ',', 'Space' = ' ', 'Tab' = '\t'), inline = TRUE),
                     radioButtons('decimal1', 'Decimal', c('Comma' = ',', 'Period' = '.'), inline = TRUE),
                     tags$hr(),
                     tags$h4('Load totals', style = 'color: #367AB4'),
                     fileInput('load_totals', label = NULL, accept = c('.txt', '.csv', '.sas7bdat')),
                     radioButtons('separator2', 'Separator', c('Semicolon' = ';', 'Comma' = ',', 'Space' = ' ', 'Tab' = '\t'), inline = TRUE),
                     radioButtons('decimal2', 'Decimal', c('Comma' = ',', 'Period' = '.'), inline = TRUE),
                     tags$hr(),
                     tags$h4('Specify calibration variables', style = 'color: #367AB4'),
                     checkboxInput('stratification', 'Stratification'),
                     fluidRow(
                       column(6,
                              selectInput('weights', 'Weights', NULL)
                       ),
                       column(6,
                              conditionalPanel(
                                condition = 'input.stratification == true',
                                selectInput('stra', 'Stratification variable', NULL)
                              )
                       )
                     ),
                     fluidRow(
                       column(6,
                              selectInput('num', 'Numerical variables', NULL, multiple = TRUE, selectize = FALSE, size = 8)),
                       column(6,
                              selectInput('cat', 'Categorical variables', NULL, multiple = TRUE, selectize = FALSE, size = 8))
                     ),
                     tags$h4('Other settings', style = 'color: #367AB4'),
                     fluidRow(
                       column(6,
                              selectInput('indicators', 'Indicators monitoring (optional)', NULL, multiple = TRUE, selectize = FALSE, size = 5)),
                       column(6,
                              tags$br(),
                              tags$br(),
                              checkboxGroupInput('indicators_stats', NULL, choices = c('Total (%)', 'Total (value)', 'Mean (%)', 'Mean (value)')))
                     ),
                     textInput('miss', 'Missings', placeholder = 'Missings for categorical, separated with commas'),
                     checkboxInput('eliminate_miss', 'Eliminate rows with missings'),
                     numericInput('maxit', 'Maximum nr of iterations', 20000, step = 1000, width = '45%'),
                     numericInput('tol', 'Tolerance', 0.00001, step = 0.000001, width = '45%'),
                     fluidRow(
                       actionButton('proceed', 'Proceed', class = "btn btn-primary btn-block")
                     )
        ),
        mainPanel(width = 9,
                  tags$h3('Loaded data', style = 'color: #367AB4'),
                  tags$p('Dataset with', tags$span(textOutput('data_rows', inline = TRUE), style = 'color: red'), 'rows and', tags$span(textOutput('data_columns', inline = TRUE), style = 'color: red'), 'columns'),
                  dataTableOutput('show_data'),
                  tags$h3('Loaded totals', style = 'color: #367AB4'),
                  tags$p('Dataset with', tags$span(textOutput('totals_rows', inline = TRUE), style = 'color: red'), 'rows and', tags$span(textOutput('totals_columns', inline = TRUE), style = 'color: red'), 'columns'),
                  dataTableOutput('show_totals'),
                  tags$h3('Explore variables', style = 'color: #367AB4'),
                  selectInput('explore', NULL, NULL, width = '17%'),
                  conditionalPanel(
                    condition = 'input.explore',
                    checkboxInput('explore_as_cat', 'Explore as categorical')
                  ),
                  fluidRow(
                    column(6,
                           plotOutput('explore_plot')),
                    column(6,
                           tableOutput('explore_summary'))
                  )
        )
      )
    ),
    tabPanel(
      title = 'Calibration',
      sidebarLayout(
        sidebarPanel(width = 2,
                     conditionalPanel(
                       condition = 'input.stratification == true',
                       tags$h4('Choose strata', style = 'color: #367AB4'),
                       selectInput('strata', NULL, NULL, multiple = TRUE, selectize = FALSE, size = 12)
                     ),
                     actionButton('show_initial', 'Show with initial weights', class = 'btn btn-block'),
                     tags$h4('Method & Solver', style = 'color: #367AB4'),
                     fluidRow(
                       column(7,
                              radioButtons('method', 'Method', choices = c('Linear', 'Raking ratio', 'Logit', 'Linear bounded'), selected = 'Linear')),
                       column(5,
                              radioButtons('solver', 'Solver', choices = c('calib', 'nleqslv'), selected = 'calib'))
                     ),
                     conditionalPanel(
                       condition = 'input.method == "Logit" || input.method == "Linear bounded"',
                       fluidRow(
                         column(6,
                                numericInput('L', 'Lower bound', NULL, step = 0.01)),
                         column(6,
                                numericInput('U', 'Upper bound', NULL, step = 0.01))
                       )
                     ),
                     actionButton('calibrate', 'CALIBRATE', class = 'btn btn-danger btn-block', icon = icon('gear')),
                     tags$p(),
                     fluidRow(
                       column(6,
                              bookmarkButton('Bookmark', class = 'btn btn-block', icon = icon('bookmark-o'), title = "Bookmark Calif's state and get URL for ")),
                       column(6,
                              actionButton('save_result', 'Save', class = 'btn btn-primary btn-block', icon = icon('save')))
                     )
        ),
        mainPanel(width = 10,
                  uiOutput('main')
        )
      )
    ),
    id = 'page', inverse = TRUE
               )
}