function(input, output, session)
{
  # reactive values
  values <- reactiveValues(wd = getwd(), data = NULL, totals = NULL, calib_settings = NULL, weightsOUT = NULL, werecalibrated = NULL, out_totals1 = NULL, 
                           out_totals2 = NULL, out_stats = NULL, out_pie = NULL, out_hist = NULL, out_box = NULL, out_weights = NULL, main = NULL,
                           out_text1 = 'calif_output', out_text2 = 'calif_settings', out_info = NULL,
                           select_num = NULL, select_weights = NULL, select_stra = NULL, select_cat = NULL, select_indicators = NULL, select_explore = NULL, 
                           selected_num = NULL, selected_weights = '', selected_stra = '', selected_cat = NULL, selected_indicators = NULL)
  
  # to the data tab
  observeEvent(input$next_tab, {
    updateNavbarPage(session, 'page', 'Data')
  })
  
  # to the calibration tab
  observeEvent(input$proceed, {
    updateNavbarPage(session, 'page', 'Calibration')
  })
  
  # working directory
  output$my_wd <- renderText({
    values$wd
  })
  
  # choose working directory
  observeEvent(input$change_wd, {
    path <- choose.dir(getwd())
    if (!is.na(path)) {
      setwd(path)
      values$wd <- path
    }
  })
  
  # data loading
  observe({
    req(input$load_data)
    if (!tools::file_ext(input$load_data$name) %in% c('txt', 'csv', 'sas7bdat')) {
      showModal(modalDialog('Incorrect format! Only .csv, .txt or .sas7bdat extensions are allowed.', footer = modalButton('OK'), easyClose = TRUE))
      return()
    }
    ifelse(tools::file_ext(input$load_data$name) == 'sas7bdat', dat <- try(read_sas(input$load_data$datapath), silent = TRUE), dat <- try(read.table(input$load_data$datapath, sep = input$separator1, dec = input$decimal1, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE))
    if (is.data.frame(dat)) {
      values$calib_settings <- NULL
      dat <- cbind(IDcalif = 1:nrow(dat), dat)
      values$data <- as.data.frame(dat)
      values$weightsOUT <- numeric(nrow(dat))
      values$werecalibrated <- NULL
      updateSelectInput(session, 'weights', choices = c(Choose = '', names(dat)[-1]))
      updateSelectInput(session, 'num', choices = names(dat)[-1])
      updateSelectInput(session, 'cat', choices = names(dat)[-1])
      updateSelectInput(session, 'indicators', choices = names(dat)[-1])
      updateSelectInput(session, 'stra', choices = c(Choose = '', names(dat)[-1]))
      updateSelectInput(session, 'explore', choices = c(Choose = '', names(dat)[-1]))
      updateSelectInput(session, 'strata', choices = character(0))
      values$select_weights <- c(Choose = '', names(dat)[-1])
      values$select_stra <- c(Choose = '', names(dat)[-1])
      values$select_num <- names(dat)[-1]
      values$select_cat <- names(dat)[-1]
      values$select_indicators <- names(dat)[-1]
      values$select_explore <- c(Choose = '', names(dat)[-1])
    }
    if (!is.data.frame(dat)) showModal(modalDialog('Data hasn\'t been loaded correctly!', footer = modalButton('OK'), easyClose = TRUE))
  })
  
  # totals loading
  observe({
    req(input$load_totals)
    if (!tools::file_ext(input$load_totals$name) %in% c('txt', 'csv', 'sas7bdat')) {
      showModal(modalDialog('Incorrect format! Only .csv, .txt or .sas7bdat extensions are allowed.', footer = modalButton('OK'), easyClose = TRUE))
      return()
    }
    ifelse(tools::file_ext(input$load_totals$name) == 'sas7bdat', dat <- try(read_sas(input$load_totals$datapath), silent = TRUE), dat <- try(read.table(input$load_totals$datapath, sep = input$separator2, dec = input$decimal2, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE))
    if (is.data.frame(dat)) {
      if (nrow(dat) > 1 && ncol(dat) == 1) {
        showModal(modalDialog('Incorrect form of the Totals!', footer = modalButton('OK'), easyClose = TRUE))
        return()
      }
      values$totals <- as.data.frame(dat)
    }
    if (!is.data.frame(dat)) showModal(modalDialog('Totals hasn\'t been loaded correctly!', footer = modalButton('OK'), easyClose = TRUE))
  })
  
  # show loaded data
  output$data_rows <- renderText({
    if (is.null(values$data)) return(0)
    nrow(values$data)
  })
  
  output$data_columns <- renderText({
    if (is.null(values$data)) return(0)
    ncol(values$data)
  })
  
  output$show_data <- renderDataTable({
    values$data[-1]
  }, options = list(pageLength = 5, lengthMenu = c(5, 10, 20, 50, 100, 500)))
  
  # show loaded totals
  output$totals_rows <- renderText({
    if (is.null(values$totals)) return(0)
    nrow(values$totals)
  })
  
  output$totals_columns <- renderText({
    if (is.null(values$totals)) return(0)
    ncol(values$totals)
  })
  
  output$show_totals <- renderDataTable({
    values$totals
  }, options = list(pageLength = 5, lengthMenu = c(5, 10, 20, 50, 100, 500)))
  
  # explore variables
  output$explore_plot <- renderPlot({
    req(input$explore)
    if (input$explore_as_cat == TRUE || is.character(isolate(values$data)[, input$explore]))
      return(barplot(table(isolate(values$data)[,input$explore]), border = FALSE, xlab = input$explore, ylab = 'Frequency', main = paste('Barplot of', input$explore, sep = ' ')))
    return(hist(isolate(values$data)[,input$explore], col = 'grey', border = FALSE, xlab = input$explore, main = paste('Histogram of', input$explore, sep = ' ')))
  })
  
  output$explore_summary <- renderTable({
    req(input$explore)
    if (input$explore_as_cat == TRUE || is.character(isolate(values$data)[, input$explore])) {
      x <- as.data.frame(table(isolate(values$data)[,input$explore], exclude = NULL))
      names(x) <- c('Value', 'Frequency')
      return(x)
    }
    x <- as.data.frame(as.array(summary(isolate(values$data)[, input$explore])))
    names(x) <- rep('', 2)
    return(x)
  })
  
  # propose the most suitable solver for specific method
  observe({
    if (input$method == 'Logit') updateRadioButtons(session, 'solver', selected = 'nleqslv')
    if (input$method == 'Linear bounded') updateRadioButtons(session, 'solver', selected = 'calib')
  })
  
  # data adjustment
  adj <- reactive({
    data <- values$data
    totals <- values$totals
    if (input$stratification == TRUE) choosecolumns <- c(1, which(names(data) %in% c(input$cat, input$num)), 
                                                         which(names(data) %in% input$indicators),
                                                         which(names(data) == input$stra), which(names(data) == input$weights))
    if (input$stratification == FALSE) {
      data <- cbind(data, STRAcalif = 0)
      choosecolumns <- c(1, which(names(data) %in% c(input$cat, input$num)), which(names(data) %in% input$indicators),
                         which(names(data) == 'STRAcalif'), which(names(data) == input$weights))
    }
    data <- data[, choosecolumns]
    if (input$stratification == TRUE) names(data)[ncol(data) - 1] <- 'STRAcalif'
    names(data)[ncol(data)] <- 'WEIGHTcalif'
    miss <- NULL
    if (isTruthy(input$miss)) miss <- unlist(strsplit(input$miss, ','))
    if (input$eliminate_miss == TRUE && !is.null(miss) && isTruthy(input$cat)) {
      for (i in miss) data[input$cat][data[input$cat] == i] <- NA
      data <- data[complete.cases(data),]
    }
    if (input$stratification == TRUE) {
      if (!isTruthy(intersect(data$STRAcalif, totals[, 1]))) {
        showModal(modalDialog('Strata in data and strata in totals are completely disjoint!', footer = actionButton('modal_ok', 'OK')))
        req(FALSE)
      }
      data <- data[data$STRAcalif %in% intersect(data$STRAcalif, totals[, 1]), ]
    }
    ncat <- length(input$cat)
    nnum <- length(input$num)
    k <- 0
    n <- 0
    if (isTruthy(input$cat)) k <- which(names(data)[-1] %in% input$cat)
    if (isTruthy(input$num)) n <- which(names(data)[-1] %in% input$num)
    totcol <- NULL
    for (i in 1:(ncat + nnum)) {
      if (i %in% k) {
        values <- setdiff(sort(unique(data[, i+1])), miss)
        totcol <- c(totcol, paste(names(data[i+1]), values, sep = '_'))
      }
      if (i %in% n) totcol <- c(totcol, names(data[i+1]))
    }
    if (!isTruthy(intersect(names(totals), totcol))) {
      showModal(modalDialog('Calibration variables you have selected are completely disjoint with the columns in the table of totals!', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    totcol <- c(totcol, input$indicators)
    if (input$stratification == TRUE) {
      totals <- totals[totals[, 1] %in% intersect(data$STRAcalif, totals[, 1]), c(1, which(names(totals) %in% totcol))]
      columns <- setdiff(names(totals), input$indicators)[-1]
      updateSelectInput(session, 'strata', choices = totals[, 1])
    }
    if (input$stratification == FALSE) {
      totals <- totals[names(totals) %in% totcol]
      columns <- setdiff(names(totals), input$indicators)
    }
    list(data = data, totals = totals, miss = miss, ncat = ncat, nnum = nnum, k = k, n = n, columns = columns)
  })
  
  # filtering selected strata
  val <- reactive({
    data <- adj()$data
    totals <- adj()$totals
    if (input$stratification == TRUE && isTruthy(input$strata)) {
      data <- adj()$data[adj()$data$STRAcalif %in% input$strata, ]
      totals <- adj()$totals[adj()$totals[, 1] %in% input$strata, ]
    }
    list(data = data, totals = totals)
  })
  
  # calculate average difference for linear solution
  AD_linear <- reactive({
    initial <- val()$data$WEIGHTcalif
    lin <- cal(val()$data[, -1], val()$totals, input$stratification, adj()$miss, input$indicators, input$indicators_stats, 
               input$num, adj()$ncat, adj()$nnum, adj()$k, adj()$n, 'Linear', 'calib', NULL, NULL, input$maxit, input$tol)$result
    mean(abs(lin - initial))
  })
  
  # check the inputs prior to calibration
  observeEvent(req(input$page == 'Calibration'), {
    if (!isTruthy(values$data)) {
      showModal(modalDialog('Data are not loaded!', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (!isTruthy(values$totals)) {
      showModal(modalDialog('Totals are not loaded!', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (!isTruthy(input$weights)) {
      showModal(modalDialog('Specify initial weights!', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (!isTruthy(input$num) && !isTruthy(input$cat)) {
      showModal(modalDialog('Specify some auxiliary calibration variables!', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (input$stratification == TRUE && !isTruthy(input$stra)) {
      showModal(modalDialog('Specify stratification variable!', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (isTruthy(intersect(input$num, input$cat))) { 
      showModal(modalDialog('Variable(s) cannot be both numerical and categorical!', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (isTruthy(intersect(input$indicators, c(input$num, input$cat)))) { 
      showModal(modalDialog('Indicator variables cannot be chosen from among variables for calibration.', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (!isTruthy(input$stratification) && nrow(values$totals) > 1) {
      showModal(modalDialog('Table \'totals\' should consist of only 1 row in case of non-stratified calibration 
                            (with specified names in the header)', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    adj()  # strata list is set up
  })
  
  observeEvent(input$modal_ok, {
    updateNavbarPage(session, 'page', 'Data')
    removeModal()
  })
  
  # CALIBRATE button
  observeEvent(input$calibrate, {
    showModal(modalDialog('Please wait...', footer = NULL))
    
    # creating output UI first
    values$main <- tagList(
      fluidRow(
        column(4,
               conditionalPanel(
                 condition = 'output.result_stats',
                 tags$h4('Results', style = 'color: #367AB4; margin-left: 0.67em'),
                 tags$br()
               ),
               tableOutput('result_stats'),
               helpText(textOutput('hover_info'), style = 'text-align: justify')),
        column(2,
               conditionalPanel(
                 condition = 'output.result_pie',
                 tags$h4('Average difference feasibility', style = 'text-align: center; color: #367AB4')
               ),
               plotOutput('result_pie', hover = hoverOpts('hover_pie_plot', delay = 0), height = '260px')),
        column(3,
               conditionalPanel(
                 condition = 'output.result_hist',
                 tags$h4('Histogram of quotients', style = 'text-align: center; color: #367AB4')
               ),
               plotOutput('result_hist', hover = hoverOpts('hover_hist_plot', delay = 0), height = '300px')),
        column(3,
               conditionalPanel(
                 condition = 'output.result_box',
                 tags$h4('Boxplots of weights', style = 'text-align: center; color: #367AB4')
               ),
               plotOutput('result_box', hover = hoverOpts('hover_box_plot', delay = 0), height = '305px'))
      ),
      conditionalPanel(
        condition = 'output.result_totals',
        tags$h4('Totals obtained', style = 'color: #367AB4'),
        checkboxInput('show_as_values', 'Show obtained totals as values')
      ),
      tableOutput('result_totals'),
      conditionalPanel(
        condition = 'output.result_weights',
        tags$br(),
        tags$br(),
        tags$h4('Weights & Quotients', style = 'color: #367AB4')
      ),
      fluidRow(
        column(4,
               dataTableOutput('result_weights'))
      )
    )
    
    initial <- val()$data$WEIGHTcalif
    arecalibrated <- val()$data$IDcalif
    values$werecalibrated <- sort(unique(c(values$werecalibrated, arecalibrated)))
    werenotcalibrated <- setdiff(values$data$IDcalif, values$werecalibrated)
    if (input$method %in% c('Logit', 'Linear bounded')) {
      if (!isTruthy(input$L) || !isTruthy(input$U)) {
        showModal(modalDialog('Specify lower and upper bound, ideally L < 1 < U!', footer = modalButton('OK'), easyClose = TRUE))
        req(FALSE)
      }
      if (input$L <= 1 && input$U <= 1 && (input$method != 'Linear bounded' || input$solver != 'calib')) {
        showModal(modalDialog('Combination of Lower bound <= 1 as well as Upper bound <= 1 can be used only with Linear bounded method and calib as a solver.', 
                              footer = modalButton('OK'), easyClose = TRUE))
        req(FALSE)
      }
      if (input$L >= 1 && input$U >= 1 && (input$method != 'Linear bounded' || input$solver != 'calib')) {
        showModal(modalDialog('Combination of Lower bound >= 1 as well as Upper bound >= 1 can be used only with Linear bounded method and calib as a solver.', 
                              footer = modalButton('OK'), easyClose = TRUE))
        req(FALSE)
      }
      if (input$L == 1 || input$U == 1) {
        showModal(modalDialog('Some of the bounds equal to 1 can be used only with Linear bounded method and calib as a solver.', footer = modalButton('OK'), easyClose = TRUE))
        req(FALSE)
      }
      if (input$L == input$U) {
        showModal(modalDialog('Calibration with equal bounds does not make sense.', footer = modalButton('OK'), easyClose = TRUE))
        req(FALSE)
      }
      if (input$L > input$U) {
        showModal(modalDialog('Lower bound must be smaller than upper bound, ideally L < 1 < U.', footer = modalButton('OK'), easyClose = TRUE))
        req(FALSE)
      }
    }
    columns <- adj()$columns
    if (isTruthy(input$indicators) && isTruthy(input$indicators_stats)) 
      columns <- c(apply(expand.grid(input$indicators, input$indicators_stats, stringsAsFactors = FALSE), 1, paste, collapse = '.'), columns)
    res <- cal(val()$data[, -1], val()$totals, input$stratification, adj()$miss, input$indicators, input$indicators_stats, 
               input$num, adj()$ncat, adj()$nnum, adj()$k, adj()$n, input$method, input$solver, input$L, input$U, input$maxit, input$tol)
    removeModal()
    colnames(res$sums1) <- colnames(res$sums2) <- columns
    if (input$stratification == TRUE) {
      res$sums1 <- cbind(Stratum = res$rows, res$sums1)
      res$sums2 <- cbind(Stratum = res$rows, res$sums2)
    }
    values$calib_settings <- rbind(values$calib_settings, res$calib_settings)
    values$weightsOUT[arecalibrated] <- res$result
    values$weightsOUT[werenotcalibrated] <- values$data[werenotcalibrated, input$weights]
    ratios <- res$result / initial
    L_w1 <- round(1 / min(initial), 3) + 0.001
    if (min(res$result) < 1) {
      L_w1 <- round(1 / min(initial[res$result < 1]), 3) + 0.001
      showNotification('Some of the calibration weights are less than 1! Use the Minimum realistic lower bound to get all weights greater or equal than 1.', 
                       duration = NULL, id = 'notification', type = 'error')
      updateNumericInput(session, 'L', value = L_w1)
    }
    if (min(res$result) >= 1) removeNotification('notification')
    values$out_stats <- data.frame(c('Initial weights interval', 'Calibration weights interval', 'Lower bound obtained', 'Upper bound obtained', 
                                     'Average weight quotient', 'Average difference', 'Minimum realistic lower bound'),
                                   c(min(initial), min(res$result), min(ratios), max(ratios), mean(ratios), mean(abs(res$result - initial)), L_w1),
                                   c(max(initial), max(res$result), rep(NA, 5)))
    values$out_pie <- AD_linear() / mean(abs(res$result - initial))
    values$out_hist <- ratios
    values$out_box <- list(initial, res$result)
    values$out_totals1 <- res$sums1
    values$out_totals2 <- res$sums2
    values$out_weights <- data.frame(Row = arecalibrated, Initial = round(initial, 5), Calibration = round(res$result, 5), Quotients = round(ratios, 5))
  })
  
  # show with initial weights
  observeEvent(input$show_initial, {
    # creating output UI first
    values$main <- tagList(
      tags$h4('Totals obtained', style = 'color: #367AB4'),
      checkboxInput('show_as_values', 'Show obtained totals as values'),
      tableOutput('result_totals')
    )
    
    values$out_stats <- values$out_pie <- values$out_hist <- values$out_box <- values$out_weights <- NULL
    columns <- adj()$columns
    if (isTruthy(input$indicators) && isTruthy(input$indicators_stats)) 
      columns <- c(apply(expand.grid(input$indicators, input$indicators_stats, stringsAsFactors = FALSE), 1, paste, collapse = '.'), columns)
    res <- cal(val()$data[, -1], val()$totals, input$stratification, adj()$miss, input$indicators, input$indicators_stats, 
               input$num, adj()$ncat, adj()$nnum, adj()$k, adj()$n, input$method, 'initial', NULL, NULL, input$maxit, input$tol)
    colnames(res$sums1) <- colnames(res$sums2) <- columns
    if (input$stratification == TRUE) {
      res$sums1 <- cbind(Stratum = res$rows, res$sums1)
      res$sums2 <- cbind(Stratum = res$rows, res$sums2)
    }
    values$out_totals1 <- res$sums1
    values$out_totals2 <- res$sums2
  })
  
  # RENDERING OUTPUTS
  output$main <- renderUI({
    values$main
  })
  
  output$result_totals <- renderTable({
    if (input$show_as_values == TRUE) values$out_totals1
    else values$out_totals2
  }, align = 'r')
  
  output$result_stats <- renderTable({
    values$out_stats
  }, colnames = FALSE, na = '', digits = 3)
  
  output$result_pie <- renderPlot({
    req(!values$out_pie %in% c(NaN, NA, Inf, -Inf))
    par(mar = rep(0, 4))
    new_pie(c(values$out_pie, max(0, 1 - values$out_pie)), labels = paste0(sprintf("%.1f", values$out_pie * 100), '%'), col = c('#367AB4', 'white'), 
            clockwise = TRUE, init.angle = 90, border = '#367AB4')
  })
  
  output$result_hist <- renderPlot({
    req(values$out_hist)
    par(mar = c(3,4,2,2))
    hist(values$out_hist, col = '#5AC5CB', border = FALSE, main = NULL, xlab = NULL, ylab = NULL)
  })
  
  output$result_box <- renderPlot({
    req(values$out_box)
    par(mar = c(3,3,1.5,2),  bty = 'n')
    boxplot(values$out_box[[1]], values$out_box[[2]], names = NULL, col = c('#D0EDF3', '#D9534F'), xaxt = 'n')
    axis(1, at = 1:2, labels = c('initial', 'calibration'), tick = FALSE, line = -0.5, cex.axis = 1.2)
    abline(h = 1, lty = 2, col = 'lightgrey')
  })
  
  output$hover_info <- renderText({
    if (!is.null(input$hover_pie_plot))
      return('Linear method (or sometimes Raking Ratio) introduces the lowest average difference for each calibration task. 
             The piechart reflects the quotient of the average differences of linear and current method. 
             It should not be significantly less than 100%, especially for bounded calibrations.')
    if (!is.null(input$hover_hist_plot))
      return('Displays the distribution of quotients \'calibration weight/initial weight\'.
             It should resemble normal distribution as much as possible.')
    if (!is.null(input$hover_box_plot))
      return('Displays the boxplots of initial and calibration weights.
             They shouldn\'t be very different from each other.')
  })
  
  output$result_weights <- renderDataTable({
    values$out_weights
  }, options = list(pageLength = 20, lengthMenu = c(20, 50, 100, 500), searching = FALSE, order = c(2, 'asc')))
  
  # save results - show modal
  observeEvent(input$save_result, {
    showModal(modalDialog(
      textInput('save_folder', 'Output folder - your Working directory', value = values$wd, width = '100%'),
      textInput('save_data', 'Output data', value = values$out_text1, width = '100%'),
      textInput('save_settings', 'Calibration settings', value = values$out_text2, width = '100%'),
      checkboxInput('save_overwrite', 'Overwrite existing outputs', value = TRUE),
      tags$em(textOutput('save_info')),
      tags$br(),
      fluidRow(
        column(12, align = 'center',
               actionButton('save_it', 'Save', icon = icon('save'), width = '18%', class = 'btn btn-primary'))
      ),
      title = 'Save results',
      footer = NULL,
      easyClose = TRUE
    ))
    values$out_info <- NULL
  })
  
  # save results
  observeEvent(input$save_it, {
    values$out_text1 <- input$save_data
    values$out_text2 <- input$save_settings
    if (file.exists(input$save_folder)) {
      setwd(input$save_folder)
      values$wd <- input$save_folder
    }
    else {
      values$out_info <- 'Output folder does not exist!'
      req(FALSE)
    }
    if (input$save_overwrite == FALSE) {
      if (file.exists(paste0(file.path(input$save_folder, input$save_data), '.csv')) || file.exists(paste0(file.path(input$save_folder, input$save_settings), '.csv'))) {
        values$out_info <- 'The files already exist and are not allowed to be overwritten!'
        req(FALSE)
      }
    }
    if (input$save_data == input$save_settings) {
      values$out_info <- 'The files cannot have the same name!'
      req(FALSE)
    }
    data <- values$data
    settings <- values$calib_settings
    data <- cbind(data, CalibrationWeight = values$weightsOUT)
    if (!is.null(settings)) {
      settings <- as.data.frame(settings)
      settings[settings$method %in% c('Linear', 'Raking ratio'), c('lower_bound', 'upper_bound')] <- NA
      settings <- settings[!duplicated(settings$stratum, fromLast = TRUE), ]
      settings <- settings[order(settings$stratum), ]
    }
    write.table(data[, -1], paste0(file.path(input$save_folder, input$save_data), '.csv'), sep = input$separator1, dec = input$decimal1, na = '', row.names = FALSE)
    write.table(settings, paste0(file.path(input$save_folder, input$save_settings), '.csv'), sep = input$separator1, dec = input$decimal1, na = '', row.names = FALSE)
    values$out_info <- 'The files have been saved.'
  })
  
  # show save info
  output$save_info <- renderText({
    values$out_info
  })
  
  # save selected values in order to bookmark them later
  observe({
    values$selected_weights <- input$weights
    values$selected_stra <- input$stra
    values$selected_num <- input$num
    values$selected_cat <- input$cat
    values$selected_indicators <- input$indicators
  })
  
  # bookmarking
  onBookmark(function(state) {
    state$values$data <- values$data
    state$values$totals <- values$totals
    state$values$select_weights <- values$select_weights
    state$values$select_stra <- values$select_stra
    state$values$select_num <- values$select_num
    state$values$select_cat <- values$select_cat
    state$values$select_indicators <- values$select_indicators
    state$values$select_explore <- values$select_explore
    state$values$selected_weights <- values$selected_weights
    state$values$selected_stra <- values$selected_stra
    state$values$selected_num <- values$selected_num
    state$values$selected_cat <- values$selected_cat
    state$values$selected_indicators <- values$selected_indicators
    state$values$wd <- values$wd
    state$values$calib_settings <- values$calib_settings
    state$values$weightsOUT <- values$weightsOUT
    state$values$werecalibrated <- values$werecalibrated
    state$values$out_totals1 <- values$out_totals1
    state$values$out_totals2 <- values$out_totals2
    state$values$out_stats <- values$out_stats
    state$values$out_pie <- values$out_pie
    state$values$out_hist <- values$out_hist
    state$values$out_box <- values$out_box
    state$values$out_weights <- values$out_weights
    state$values$main <- values$main
    state$values$out_text1 <- values$out_text1
    state$values$out_text2 <- values$out_text2
    state$values$out_info <- values$out_info
  })
  
  onRestore(function(state) {
    values$data <- state$values$data
    values$totals <- state$values$totals
    values$select_weights <- state$values$select_weights
    values$select_stra <- state$values$select_stra
    values$select_num <- state$values$select_num
    values$select_cat <- state$values$select_cat
    values$select_indicators <- state$values$select_indicators
    values$select_explore <- state$values$select_explore
    values$selected_weights <- state$values$selected_weights
    values$selected_stra <- state$values$selected_stra
    values$selected_num <- state$values$selected_num
    values$selected_cat <- state$values$selected_cat
    values$selected_indicators <- state$values$selected_indicators
    updateSelectInput(session, 'weights', choices = state$values$select_weights, selected = state$values$selected_weights)
    updateSelectInput(session, 'stra', choices = state$values$select_stra, selected = state$values$selected_stra)
    updateSelectInput(session, 'num', choices = state$values$select_num, selected = state$values$selected_num)
    updateSelectInput(session, 'cat', choices = state$values$select_cat, selected = state$values$selected_cat)
    updateSelectInput(session, 'indicators', choices = state$values$select_indicators, selected = state$values$selected_indicators)
    updateSelectInput(session, 'explore', choices = state$values$select_explore)
    values$wd <- state$values$wd
    values$calib_settings <- state$values$calib_settings
    values$weightsOUT <- state$values$weightsOUT
    values$werecalibrated <- state$values$werecalibrated
    values$out_totals1 <- state$values$out_totals1
    values$out_totals2 <- state$values$out_totals2
    values$out_stats <- state$values$out_stats
    values$out_pie <- state$values$out_pie
    values$out_hist <- state$values$out_hist
    values$out_box <- state$values$out_box
    values$out_weights <- state$values$out_weights
    values$main <- state$values$main
    values$out_text1 <- state$values$out_text1
    values$out_text2 <- state$values$out_text2
    values$out_info <- state$values$out_info
  })
  
  setBookmarkExclude(c('load_data', 'load_totals', 'page', 'calibrate', 'change_wd', 'show_initial', 'next_tab', 'proceed', 'save_result', 'modal_ok', 'save_it'))
}
  