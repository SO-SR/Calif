function(input, output, session)
{
  # reactive values
  values <- reactiveValues(wd = getwd(), data = NULL, data_HH = NULL, data_I = NULL, totals = NULL, calib_settings = NULL, weightsOUT = NULL, werecalibrated = NULL, 
                           out_totals1 = NULL, out_totals2 = NULL, out_stats = NULL, out_pie = NULL, out_hist = NULL, out_box = NULL, out_weights = NULL, 
                           ui_main = NULL, out_text1 = 'calif_output', out_text2 = 'calif_settings', out_info = NULL, multistage = FALSE,
                           num = NULL, cat = NULL, select_num = NULL, select_weights = NULL, select_stra = NULL, select_cat = NULL, select_indicators = NULL, 
                           select_explore = NULL, select_ID_HH = NULL, select_ID_I = NULL, selected_num = NULL, selected_weights = '', selected_stra = '', 
                           selected_cat = NULL, selected_indicators = NULL, selected_ID_HH = NULL, selected_ID_I = NULL, update_strata = NULL)
  
  # to the data tab
  observeEvent(input$next_tab, {
    updateNavbarPage(session, 'page', 'Data')
  })
  
  # switch between type of calibration
  observeEvent(input$switch_to_one, {
    values$multistage <- FALSE
  })
  
  observeEvent(input$switch_to_two, {
    values$multistage <- TRUE
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
  
  # data loading - one-stage calibration
  observe({
    req(input$load_data)
    if (!tools::file_ext(input$load_data$name) %in% c('txt', 'csv', 'sas7bdat')) {
      showModal(modalDialog('Incorrect format! Only .csv, .txt or .sas7bdat extensions are allowed.', footer = modalButton('OK'), easyClose = TRUE))
      return()
    }
    if (tools::file_ext(input$load_data$name) == 'sas7bdat') dat <- try(read_sas(input$load_data$datapath), silent = TRUE)
    if (tools::file_ext(input$load_data$name) != 'sas7bdat') dat <- try(read.table(input$load_data$datapath, sep = input$separator1, dec = input$decimal1, 
                                                                                   header = TRUE, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
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
  
  # data loading - two-stage calibration
  observe({
    req(input$load_data_HH)
    if (!tools::file_ext(input$load_data_HH$name) %in% c('txt', 'csv', 'sas7bdat')) {
      showModal(modalDialog('Incorrect format! Only .csv, .txt or .sas7bdat extensions are allowed.', footer = modalButton('OK'), easyClose = TRUE))
      return()
    }
    if (tools::file_ext(input$load_data_HH$name) == 'sas7bdat') dat <- try(read_sas(input$load_data_HH$datapath), silent = TRUE)
    if (tools::file_ext(input$load_data_HH$name) != 'sas7bdat') dat <- try(read.table(input$load_data_HH$datapath, sep = input$separator1, dec = input$decimal1, 
                                                                                   header = TRUE, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
    if (is.data.frame(dat)) {
      values$calib_settings <- NULL
      dat <- cbind(IDcalif = 1:nrow(dat), dat)
      values$data_HH <- as.data.frame(dat)
      values$weightsOUT <- numeric(nrow(dat))
      values$werecalibrated <- NULL
      updateSelectInput(session, 'weights', choices = c(Choose = '', names(dat)[-1]))
      updateSelectInput(session, 'stra', choices = c(Choose = '', names(dat)[-1]))
      updateSelectInput(session, 'strata', choices = character(0))
      updateSelectInput(session, 'ID_HH', choices = c(Choose = '', names(dat)[-1]))
      values$select_weights <- c(Choose = '', names(dat)[-1])
      values$select_stra <- c(Choose = '', names(dat)[-1])
      values$select_ID_HH <- c(Choose = '', names(dat)[-1])
    }
    if (!is.data.frame(dat)) showModal(modalDialog('Data hasn\'t been loaded correctly!', footer = modalButton('OK'), easyClose = TRUE))
  })
  
  observe({
    req(input$load_data_I)
    if (!tools::file_ext(input$load_data_I$name) %in% c('txt', 'csv', 'sas7bdat')) {
      showModal(modalDialog('Incorrect format! Only .csv, .txt or .sas7bdat extensions are allowed.', footer = modalButton('OK'), easyClose = TRUE))
      return()
    }
    if (tools::file_ext(input$load_data_I$name) == 'sas7bdat') dat <- try(read_sas(input$load_data_I$datapath), silent = TRUE)
    if (tools::file_ext(input$load_data_I$name) != 'sas7bdat') dat <- try(read.table(input$load_data_I$datapath, sep = input$separator3, dec = input$decimal3, 
                                                                                      header = TRUE, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
    if (is.data.frame(dat)) {
      values$data_I <- as.data.frame(dat)
      updateSelectInput(session, 'ID_I', choices = c(Choose = '', names(dat)))
      values$select_ID_I <- c(Choose = '', names(dat))
    }
    if (!is.data.frame(dat)) showModal(modalDialog('Data hasn\'t been loaded correctly!', footer = modalButton('OK'), easyClose = TRUE))
  })
  
  observe({
    req(input$load_data_HH, input$load_data_I, input$ID_HH, input$ID_I)
    req(input$ID_HH %in% names(values$data_HH), input$ID_I %in% names(values$data_I))
    if (max(duplicated(values$data_HH[, input$ID_HH])) > 0) {
      showModal(modalDialog('There are duplicates in Household file ID\'s!', footer = modalButton('OK'), easyClose = TRUE))
      req(FALSE)
    }
    if (!setequal(values$data_HH[, input$ID_HH], values$data_I[, input$ID_I])) {
      showModal(modalDialog('Household ID\'s in both files do not exactly match!', footer = modalButton('OK'), easyClose = TRUE))
      req(FALSE)
    }
    if (length(setdiff(intersect(names(values$data_HH), names(values$data_I)), input$ID_HH)) > 0) {
      showModal(modalDialog('There are some variables with the same names in both files. Each variable name (except for Household ID\'s) must be unique.
                            Please rename it, in order to avoid possible joining problems.', footer = modalButton('OK'), easyClose = TRUE))
      req(FALSE)
    }
    updateSelectInput(session, 'num', choices = list('Household file' = names(values$data_HH)[-1], 'Individual file' = names(values$data_I)))
    updateSelectInput(session, 'cat', choices = list('Household file' = names(values$data_HH)[-1], 'Individual file' = names(values$data_I)))
    updateSelectInput(session, 'indicators', choices = list('Household file' = names(values$data_HH)[-1], 'Individual file' = names(values$data_I)))
    updateSelectInput(session, 'explore', choices = c(Choose = '', list('Household file' = names(values$data_HH)[-1], 'Individual file' = names(values$data_I))))
    values$select_num <- list('Household file' = names(values$data_HH)[-1], 'Individual file' = names(values$data_I))
    values$select_cat <- list('Household file' = names(values$data_HH)[-1], 'Individual file' = names(values$data_I))
    values$select_indicators <- list('Household file' = names(values$data_HH)[-1], 'Individual file' = names(values$data_I))
    values$select_explore <- c(Choose = '', list('Household file' = names(values$data_HH)[-1], 'Individual file' = names(values$data_I)))
  })
  
  # totals loading
  observe({
    req(input$load_totals)
    if (!tools::file_ext(input$load_totals$name) %in% c('txt', 'csv', 'sas7bdat')) {
      showModal(modalDialog('Incorrect format! Only .csv, .txt or .sas7bdat extensions are allowed.', footer = modalButton('OK'), easyClose = TRUE))
      return()
    }
    if (tools::file_ext(input$load_totals$name) == 'sas7bdat') dat <- try(read_sas(input$load_totals$datapath), silent = TRUE)
    if (tools::file_ext(input$load_totals$name) != 'sas7bdat') dat <- try(read.table(input$load_totals$datapath, sep = input$separator2, dec = input$decimal2, 
                                                                                     header = TRUE, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
    if (is.data.frame(dat)) {
      if (nrow(dat) > 1 && ncol(dat) == 1) {
        showModal(modalDialog('Incorrect form of the Totals! Please check the decimal/separator
                              first.', footer = modalButton('OK'), easyClose = TRUE))
        return()
      }
      values$totals <- as.data.frame(dat)
    }
    if (!is.data.frame(dat)) showModal(modalDialog('Totals hasn\'t been loaded correctly!', footer = modalButton('OK'), easyClose = TRUE))
  })
  
  # show loaded data - one-stage calibration
  output$data_rows <- renderText({
    if (is.null(values$data)) return(0)
    nrow(values$data)
  })
  
  output$data_columns <- renderText({
    if (is.null(values$data)) return(0)
    ncol(values$data)
  })
  
  output$show_data <- renderDataTable({
    req(values$data)
    d <- values$data
    names(d)[1] <- 'Row'
    d
  }, options = list(pageLength = 5, lengthMenu = c(5, 10, 20, 50, 100, 500)))
  
  # show loaded data - two-stage calibration
  output$data_rows_HH <- renderText({
    if (is.null(values$data_HH)) return(0)
    nrow(values$data_HH)
  })
  
  output$data_rows_I <- renderText({
    if (is.null(values$data_I)) return(0)
    nrow(values$data_I)
  })
  
  output$data_columns_HH <- renderText({
    if (is.null(values$data_HH)) return(0)
    ncol(values$data_HH)
  })
  
  output$data_columns_I <- renderText({
    if (is.null(values$data_I)) return(0)
    ncol(values$data_I)
  })
  
  output$show_data_HH <- renderDataTable({
    req(values$data_HH)
    d <- values$data_HH
    names(d)[1] <- 'Row'
    d
  }, options = list(pageLength = 5, lengthMenu = c(5, 10, 20, 50, 100, 500)))
  
  output$show_data_I <- renderDataTable({
    values$data_I
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
    if (isolate(!values$multistage)) data <- isolate(values$data)
    else if (input$explore %in% names(isolate(values$data_HH))) data <- isolate(values$data_HH)
    else if (input$explore %in% names(isolate(values$data_I))) data <- isolate(values$data_I)
    if (input$explore_as_cat == TRUE || is.character(data[, input$explore]))
      return(barplot(table(data[,input$explore]), border = FALSE, xlab = input$explore, ylab = 'Frequency', main = paste('Barplot of', input$explore, sep = ' ')))
    return(hist(data[,input$explore], col = 'grey', border = FALSE, xlab = input$explore, main = paste('Histogram of', input$explore, sep = ' ')))
  })
  
  output$explore_summary <- renderTable({
    req(input$explore)
    if (isolate(!values$multistage)) data <- isolate(values$data)
    else if (input$explore %in% names(isolate(values$data_HH))) data <- isolate(values$data_HH)
    else if (input$explore %in% names(isolate(values$data_I))) data <- isolate(values$data_I)
    if (input$explore_as_cat == TRUE || is.character(data[, input$explore])) {
      x <- as.data.frame(table(data[,input$explore], exclude = NULL))
      names(x) <- c('Value', 'Frequency')
      return(x)
    }
    x <- as.data.frame(as.array(summary(data[, input$explore])))
    names(x) <- rep('', 2)
    return(x)
  })
  
  # propose the most suitable solver for specific method
  observe({
    if (input$method == 'Logit') updateRadioButtons(session, 'solver', selected = 'nleqslv')
    if (input$method == 'Linear bounded') updateRadioButtons(session, 'solver', selected = 'calib')
  })
  
  # create values$num and values$cat in case of one-stage calibration
  observe({
    req(!values$multistage)
    values$num <- input$num
    values$cat <- input$cat
  })
  
  # data adjustment
  adj <- reactive({
    data <- values$data
    totals <- values$totals
    if (input$stratification == TRUE) choosecolumns <- c(which(names(data) %in% 'IDcalif'), which(names(data) %in% c(values$cat, values$num)), 
                                                         which(names(data) %in% input$indicators),
                                                         which(names(data) == input$stra), which(names(data) == input$weights))
    if (input$stratification == FALSE) {
      data <- cbind(data, STRAcalif = 0)
      choosecolumns <- c(which(names(data) %in% 'IDcalif'), which(names(data) %in% c(values$cat, values$num)), which(names(data) %in% input$indicators),
                         which(names(data) == 'STRAcalif'), which(names(data) == input$weights))
    }
    data <- data[, choosecolumns]
    if (input$stratification == TRUE) names(data)[ncol(data) - 1] <- 'STRAcalif'
    names(data)[ncol(data)] <- 'WEIGHTcalif'
    miss <- NULL
    if (isTruthy(input$miss)) miss <- unlist(strsplit(input$miss, ','))
    if (input$eliminate_miss == TRUE && !is.null(miss) && isTruthy(values$cat)) {
      for (i in miss) data[values$cat][data[values$cat] == i] <- NA
      data <- data[complete.cases(data),]
    }
    if (input$stratification == TRUE) {
      if (!isTruthy(intersect(data$STRAcalif, totals[, 1]))) {
        showModal(modalDialog('Strata in data and strata in totals are completely disjoint!', footer = actionButton('modal_ok', 'OK')))
        req(FALSE)
      }
      data <- data[data$STRAcalif %in% intersect(data$STRAcalif, totals[, 1]), ]
    }
    ncat <- length(values$cat)
    nnum <- length(values$num)
    k <- 0
    n <- 0
    if (isTruthy(values$cat)) k <- which(names(data)[-1] %in% values$cat)
    if (isTruthy(values$num)) n <- which(names(data)[-1] %in% values$num)
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
               values$num, adj()$ncat, adj()$nnum, adj()$k, adj()$n, 'Linear', 'calib', NULL, NULL, input$maxit, input$tol)$result
    mean(abs(lin - initial))
  })
  
  # check the inputs prior to calibration
  observeEvent(req(input$page == 'Calibration'), {
    if (!isTruthy(values$data) && !values$multistage) {
      showModal(modalDialog('Data are not loaded!', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (!isTruthy(values$data_HH) && values$multistage) {
      showModal(modalDialog('Household file is not loaded!', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (!isTruthy(values$data_I) && values$multistage) {
      showModal(modalDialog('Individual file is not loaded!', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if ((!isTruthy(input$ID_HH) || !isTruthy(input$ID_I)) && values$multistage) {
      showModal(modalDialog('Specify Household ID for both files!', footer = actionButton('modal_ok', 'OK')))
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
    else if (!is.numeric(values$data[, input$weights]) && !values$multistage) {
      showModal(modalDialog('Weights are not in numeric form! You have probably set a wrong 
                            decimal point. Please check it.', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (!is.numeric(values$data_HH[, input$weights]) && values$multistage) {
      showModal(modalDialog('Weights are not in numeric form! You have probably set a wrong 
                            decimal point. Please check it.', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (input$stratification == TRUE && min(sapply(values$totals[-1], is.numeric)) == 0) {
      showModal(modalDialog('Some of the totals are not in numeric form! You have probably set a wrong 
                            decimal point. Please check it.', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (input$stratification == FALSE && min(sapply(values$totals, is.numeric)) == 0) {
      showModal(modalDialog('Some of the totals are not in numeric form! You have probably set a wrong 
                            decimal point. Please check it.', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    else if (isTruthy(input$num)) {
      if (!values$multistage) {
        if (min(sapply(values$data[input$num], is.numeric)) == 0) {
          showModal(modalDialog('Numerical variables are not in numeric form! You have probably set a wrong 
                            decimal point. Please check it.', footer = actionButton('modal_ok', 'OK')))
          req(FALSE)
        }
      }
      if (values$multistage) {
        if (length(intersect(names(values$data_HH), input$num)) > 0) if (min(sapply(values$data_HH[names(values$data_HH) %in% input$num], is.numeric)) == 0) {
          showModal(modalDialog('Numerical variables are not in numeric form! You have probably set a wrong 
                                decimal point. Please check it.', footer = actionButton('modal_ok', 'OK')))
          req(FALSE)
        }
        if (length(intersect(names(values$data_I), input$num)) > 0) if (min(sapply(values$data_I[names(values$data_I) %in% input$num], is.numeric)) == 0) {
          showModal(modalDialog('Numerical variables are not in numeric form! You have probably set a wrong 
                                decimal point. Please check it.', footer = actionButton('modal_ok', 'OK')))
          req(FALSE)
        }
      }
    }
    
    if (values$multistage) {
      num_HH <- intersect(names(values$data_HH), input$num)
      cat_HH <- intersect(names(values$data_HH), input$cat)
      indicators_HH <- intersect(names(values$data_HH), input$indicators)
      num_I <- intersect(names(values$data_I), input$num)
      cat_I <- intersect(names(values$data_I), input$cat)
      indicators_I <- intersect(names(values$data_I), input$indicators)
      values$cat <- cat_HH
      values$num <- c(num_HH, num_I)
      
      miss <- NULL
      if (isTruthy(input$miss)) miss <- unlist(strsplit(input$miss, ','))
      dat1 <- values$data_HH[names(values$data_HH) %in% c('IDcalif', input$ID_HH, input$weights, input$stra, cat_HH, num_HH, indicators_HH)]
      dat2 <- values$data_I[names(values$data_I) %in% c(input$ID_I, num_I, indicators_I)]
      if (isTruthy(cat_I)) {
        for (x in cat_I) {
          v <- setdiff(sort(unique(values$data_I[, x])), miss)
          dat3 <- as.data.frame(do.call(cbind, lapply(v, function(z) as.numeric(values$data_I[, x] == z))))
          names(dat3) <- paste(x, v, sep = '_')
          dat2 <- cbind(dat2, dat3)
          values$num <- c(values$num, names(dat3))
        }
      }
      dat2 <- aggregate(dat2[names(dat2) %in% c(values$num, indicators_I)], dat2[input$ID_I], FUN = sum)
      values$data <- merge(dat1, dat2, by.x = input$ID_HH, by.y = input$ID_I, all = FALSE, sort = FALSE)
      if (!identical(values$data[input$ID_HH], values$data_HH[input$ID_HH])) {
        showModal(modalDialog('Joining error! Check the joining ID\'s.', footer = actionButton('modal_ok', 'OK')))
        req(FALSE)
      }
      showModal(modalDialog('Joined file has been created.', footer = modalButton('OK'), easyClose = TRUE))
    }
    
    adj()  # strata list is set up
    
    updateSelectInput(session, 'strata', selected = values$update_strata)  # re-set selected strata from previous task
  })
  
  observeEvent(input$modal_ok, {
    updateNavbarPage(session, 'page', 'Data')
    removeModal()
  })
  
  # CALIBRATE button
  observeEvent(input$calibrate, {
    showModal(modalDialog('Please wait...', footer = NULL))
    
    # creating output UI first
    values$ui_main <- tagList(
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
      if (input$L == 1 || input$U == 1 && (input$method != 'Linear bounded' || input$solver != 'calib')) {
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
    if (input$stratification == TRUE && !isTruthy(input$strata)) {
      showModal(modalDialog('Choose some strata!', footer = modalButton('OK'), easyClose = TRUE))
      req(FALSE)
    }
    columns <- adj()$columns
    if (isTruthy(input$indicators) && isTruthy(input$indicators_stats)) 
      columns <- c(apply(expand.grid(input$indicators, input$indicators_stats, stringsAsFactors = FALSE), 1, paste, collapse = '.'), columns)
    res <- cal(val()$data[, -1], val()$totals, input$stratification, adj()$miss, input$indicators, input$indicators_stats, 
               values$num, adj()$ncat, adj()$nnum, adj()$k, adj()$n, input$method, input$solver, input$L, input$U, input$maxit, input$tol)
    removeModal()
    colnames(res$sums1) <- colnames(res$sums2) <- columns
    if (input$stratification == TRUE) {
      res$sums1 <- cbind(Stratum = res$rows, res$sums1)
      res$sums2 <- cbind(Stratum = res$rows, res$sums2)
    }
    initial <- val()$data$WEIGHTcalif
    arecalibrated <- val()$data$IDcalif
    values$werecalibrated <- sort(unique(c(values$werecalibrated, arecalibrated)))
    werenotcalibrated <- setdiff(values$data$IDcalif, values$werecalibrated)
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
                                     'Average weight quotient', 'Average difference', 'SD of cal. and initial weights', 'Minimum realistic lower bound'),
                                   c(min(initial), min(res$result), min(ratios), max(ratios), mean(ratios), mean(abs(res$result - initial)), sd(res$result), L_w1),
                                   c(max(initial), max(res$result), rep(NA, 4), sd(initial), NA))
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
    values$ui_main <- tagList(
      tags$h4('Totals obtained', style = 'color: #367AB4'),
      checkboxInput('show_as_values', 'Show obtained totals as values'),
      tableOutput('result_totals')
    )
    
    if (input$stratification == TRUE && !isTruthy(input$strata)) {
      showModal(modalDialog('Choose some strata!', footer = modalButton('OK'), easyClose = TRUE))
      req(FALSE)
    }
    values$out_stats <- values$out_pie <- values$out_hist <- values$out_box <- values$out_weights <- NULL
    columns <- adj()$columns
    if (isTruthy(input$indicators) && isTruthy(input$indicators_stats)) 
      columns <- c(apply(expand.grid(input$indicators, input$indicators_stats, stringsAsFactors = FALSE), 1, paste, collapse = '.'), columns)
    res <- cal(val()$data[, -1], val()$totals, input$stratification, adj()$miss, input$indicators, input$indicators_stats, 
               values$num, adj()$ncat, adj()$nnum, adj()$k, adj()$n, input$method, 'initial', NULL, NULL, input$maxit, input$tol)
    colnames(res$sums1) <- colnames(res$sums2) <- columns
    if (input$stratification == TRUE) {
      res$sums1 <- cbind(Stratum = res$rows, res$sums1)
      res$sums2 <- cbind(Stratum = res$rows, res$sums2)
    }
    values$out_totals1 <- res$sums1
    values$out_totals2 <- res$sums2
  })
  
  # RENDERING OUTPUTS
  
  # UI for mainPanel of Calibration tab
  output$ui_main <- renderUI({
    values$ui_main
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
  }, options = list(pageLength = 20, lengthMenu = c(20, 50, 100, 500), searching = FALSE))
  
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
    settings <- values$calib_settings
    if (!is.null(settings)) {
      settings <- as.data.frame(settings)
      settings[settings$method %in% c('Linear', 'Raking ratio'), c('lower_bound', 'upper_bound')] <- NA
      settings <- settings[!duplicated(settings$stratum, fromLast = TRUE), ]
      settings <- settings[order(settings$stratum), ]
    }
    if (!values$multistage) {
      data <- values$data
      data <- cbind(data, CalibrationWeight = values$weightsOUT)
      w1 <- try(write.table(data[, -1], paste0(file.path(input$save_folder, input$save_data), '.csv'), 
                            sep = input$separator1, dec = input$decimal1, na = '', row.names = FALSE), silent = TRUE)
      w2 <- try(write.table(settings, paste0(file.path(input$save_folder, input$save_settings), '.csv'), 
                            sep = input$separator1, dec = input$decimal1, na = '', row.names = FALSE), silent = TRUE)
      if (class(w1) == 'try-error' || class(w2) == 'try-error') {
        values$out_info <- 'Cannot write to file(s)!'
        req(FALSE)
      }
    }
    if (values$multistage) {
      data_HH <- cbind(values$data_HH, CalibrationWeight = values$weightsOUT)
      data_I <- merge(values$data_I, data_HH[c(input$ID_HH, 'CalibrationWeight')], by.x = input$ID_I, by.y = input$ID_HH, sort = FALSE)
      w1 <- try(write.table(data_HH[, !names(data_HH) %in% 'IDcalif'], paste0(file.path(input$save_folder, input$save_data), '_HH.csv'), 
                            sep = input$separator1, dec = input$decimal1, na = '', row.names = FALSE), silent = TRUE)
      w2 <- try(write.table(data_I, paste0(file.path(input$save_folder, input$save_data), '_IND.csv'), 
                            sep = input$separator3, dec = input$decimal3, na = '', row.names = FALSE), silent = TRUE)
      w3 <- try(write.table(settings, paste0(file.path(input$save_folder, input$save_settings), '.csv'), 
                            sep = input$separator1, dec = input$decimal1, na = '', row.names = FALSE), silent = TRUE)
      if (class(w1) == 'try-error' || class(w2) == 'try-error' || class(w3) == 'try-error') {
        values$out_info <- 'Cannot write to file(s)!'
        req(FALSE)
      }
    }
    values$out_info <- 'The files have been saved.'
  })
  
  # show save info
  output$save_info <- renderText({
    values$out_info
  })
  
  # save selected strata in order to update it when toggling tabs
  observe({
    values$update_strata <- input$strata
  })
  
  # save selected values in order to bookmark them later
  observe({
    values$selected_weights <- input$weights
    values$selected_stra <- input$stra
    values$selected_num <- input$num
    values$selected_cat <- input$cat
    values$selected_indicators <- input$indicators
    values$selected_ID_HH <- input$ID_HH
    values$selected_ID_I <- input$ID_I
  })
  
  # bookmarking
  onBookmark(function(state) {
    state$values$data <- values$data
    state$values$data_HH <- values$data_HH
    state$values$data_I <- values$data_I
    state$values$totals <- values$totals
    state$values$multistage <- values$multistage
    state$values$num <- values$num
    state$values$cat <- values$cat
    state$values$select_weights <- values$select_weights
    state$values$select_stra <- values$select_stra
    state$values$select_num <- values$select_num
    state$values$select_cat <- values$select_cat
    state$values$select_indicators <- values$select_indicators
    state$values$select_explore <- values$select_explore
    state$values$select_ID_HH <- values$select_ID_HH
    state$values$select_ID_I <- values$select_ID_I
    state$values$selected_weights <- values$selected_weights
    state$values$selected_stra <- values$selected_stra
    state$values$selected_num <- values$selected_num
    state$values$selected_cat <- values$selected_cat
    state$values$selected_indicators <- values$selected_indicators
    state$values$selected_ID_HH <- values$selected_ID_HH
    state$values$selected_ID_I <- values$selected_ID_I
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
    state$values$ui_main <- values$ui_main
    state$values$out_text1 <- values$out_text1
    state$values$out_text2 <- values$out_text2
    state$values$out_info <- values$out_info
  })
  
  onRestore(function(state) {
    values$data <- state$values$data
    values$data_HH <- state$values$data_HH
    values$data_I <- state$values$data_I
    values$totals <- state$values$totals
    values$multistage <- state$values$multistage
    values$num <- state$values$num
    values$cat <- state$values$cat
    values$select_weights <- state$values$select_weights
    values$select_stra <- state$values$select_stra
    values$select_num <- state$values$select_num
    values$select_cat <- state$values$select_cat
    values$select_indicators <- state$values$select_indicators
    values$select_explore <- state$values$select_explore
    values$select_ID_HH <- state$values$select_ID_HH
    values$select_ID_I <- state$values$select_ID_I
    values$selected_weights <- state$values$selected_weights
    values$selected_stra <- state$values$selected_stra
    values$selected_num <- state$values$selected_num
    values$selected_cat <- state$values$selected_cat
    values$selected_indicators <- state$values$selected_indicators
    values$selected_ID_HH <- state$values$selected_ID_HH
    values$selected_ID_I <- state$values$selected_ID_I
    updateSelectInput(session, 'weights', choices = state$values$select_weights, selected = state$values$selected_weights)
    updateSelectInput(session, 'stra', choices = state$values$select_stra, selected = state$values$selected_stra)
    updateSelectInput(session, 'num', choices = state$values$select_num, selected = state$values$selected_num)
    updateSelectInput(session, 'cat', choices = state$values$select_cat, selected = state$values$selected_cat)
    updateSelectInput(session, 'indicators', choices = state$values$select_indicators, selected = state$values$selected_indicators)
    updateSelectInput(session, 'explore', choices = state$values$select_explore)
    updateSelectInput(session, 'ID_HH', choices = state$values$select_ID_HH, selected = state$values$selected_ID_HH)
    updateSelectInput(session, 'ID_I', choices = state$values$select_ID_I, selected = state$values$selected_ID_I)
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
    values$ui_main <- state$values$ui_main
    values$out_text1 <- state$values$out_text1
    values$out_text2 <- state$values$out_text2
    values$out_info <- state$values$out_info
  })
  
  setBookmarkExclude(c('load_data', 'load_data_HH', 'load_data_I', 'load_totals', 'page', 'calibrate', 'change_wd', 'show_initial', 'next_tab', 'proceed', 'save_result', 'modal_ok', 'save_it'))
}
