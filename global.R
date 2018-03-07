if (!require(shiny)) install.packages('shiny')
if (!require(sampling)) install.packages('sampling')
if (!require(nleqslv)) install.packages('nleqslv')
if (!require(haven)) install.packages('haven')
library(shiny)
library(sampling)
library(nleqslv)
library(haven)

options(shiny.maxRequestSize = Inf)

enableBookmarking(store = 'server')

# function for pie chart with label inside, based on graphics::pie
new_pie <- function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
                     init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
                     col = NULL, border = NULL, lty = NULL, main = NULL, ...) 
{
  if (!is.numeric(x) || any(is.na(x) | x < 0)) 
    stop("'x' values must be positive.")
  if (is.null(labels)) 
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col)) 
    col <- if (is.null(density)) 
      c("white", "lightblue", "mistyrose", "lightcyan", 
        "lavender", "cornsilk")
  else par("fg")
  if (!is.null(col)) 
    col <- rep_len(col, nx)
  if (!is.null(border)) 
    border <- rep_len(border, nx)
  if (!is.null(lty)) 
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density)) 
    density <- rep_len(density, nx)
  twopi <- if (clockwise) 
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
            angle = angle[i], border = border[i], col = col[i], 
            lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      # changed part
      #lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
      text(0.45 * P$x, 0.45 * P$y, labels[i], xpd = TRUE, 
           cex = 1.8, col = 'white', ...)
    }
  }
  title(main = main, ...)
  invisible(NULL)
}
environment(new_pie) <- getNamespace('graphics')

# calib function from sampling package with minor adjustments
calib <- function (Xs, d, total, q = rep(1, length(d)), method = c("linear", 
                                                                   "raking", "truncated", "logit"), bounds = c(low = 0, upp = 10), 
                   description = FALSE, max_iter = 500) 
{
  if (any(is.na(Xs)) | any(is.na(d)) | any(is.na(total)) | 
      any(is.na(q))) 
    stop("the input should not contain NAs")
  if (!(is.matrix(Xs) | is.array(Xs))) 
    Xs = as.matrix(Xs)
  if (is.matrix(Xs)) 
    if (length(total) != ncol(Xs)) 
      stop("Xs and total have different dimensions")
  if (is.vector(Xs) & length(total) != 1) 
    stop("Xs and total have different dimensions")
  if (any(is.infinite(q))) 
    stop("there are Inf values in the q vector")
  if (missing(method)) 
    stop("specify a method")
  if (!(method %in% c("linear", "raking", "logit", "truncated"))) 
    stop("the specified method is not in the list")
  if (method %in% c("linear", "raking") & !missing(bounds)) 
    stop("for the linear and raking the bounds are not allowed")
  EPS = .Machine$double.eps
  EPS1 = 1e-06
  n = length(d)
  lambda = as.matrix(rep(0, n))
  lambda1 = ginv(t(Xs * d * q) %*% Xs, tol = EPS) %*% (total - 
                                                         as.vector(t(d) %*% Xs))
  if (method == "linear" | max(abs(lambda1)) < EPS) 
    g = 1 + q * as.vector(Xs %*% lambda1)
  else if (method == "truncated") {
    if (!missing(bounds)) {
      # deleted part
      #if (bounds[2] <= 1 || bounds[1] >= 1 || bounds[1] > 
      #    bounds[2]) 
      #  warning("The conditions low<1<upp are not fulfilled")
    }
    else stop("Specify the bounds")
    g = 1 + q * as.vector(Xs %*% lambda1)
    list = 1:length(g)
    l = 0
    g1 = g
    Xs1 = Xs
    d1 = d
    t2 = total
    list1 = 1:length(g)
    q1 = q
    while (l < max_iter) {
      l = l + 1
      if (any(g < bounds[1]) | any(g > bounds[2])) {
        g[g < bounds[1]] = bounds[1]
        g[g > bounds[2]] = bounds[2]
        list = (1:length(g))[g > bounds[1] & g < bounds[2]]
        if (length(list) != 0) {
          g1 = g[list]
          t2 = total - as.vector(t(g[-list] * d[-list]) %*% 
                                   Xs[-list, ])
          Xs1 = Xs[list, ]
          d1 = d[list]
          q1 = q[list]
          list1 = list
        }
      }
      t1 = as.vector(t(d1) %*% Xs1)
      lambda1 = ginv(t(Xs1 * d1 * q1) %*% Xs1, tol = EPS) %*% 
        (t2 - t1)
      if (length(list1) > 1) 
        g1 = 1 + q1 * as.vector(Xs1 %*% lambda1)
      else if (length(list1) == 1) {
        g1 = 1 + q1 * as.vector(as.vector(Xs1) %*% as.vector(lambda1))
      }
      g[list1] = g1
      tr = crossprod(Xs, g * d)
      if (max(abs(tr - total)/total) < EPS1 & all(g >= 
                                                  bounds[1] & g <= bounds[2])) 
        break
    }
    if (l == max_iter) {
      # deleted part
      #cat("No convergence in", max_iter, "iterations with the given bounds. \n")
      #cat("The bounds for the g-weights are:", min(g), 
      #    " and ", max(g), "\n")
      #cat(" and the g-weights are given by g\n")
    }
  }
  else if (method == "raking") {
    lambda = as.matrix(rep(0, ncol(Xs)))
    w1 = as.vector(d * exp(Xs %*% lambda * q))
    for (l in 1:max_iter) {
      phi = t(Xs) %*% w1 - total
      T1 = t(Xs * w1)
      phiprim = T1 %*% Xs
      lambda = lambda - ginv(phiprim, tol = EPS) %*% phi
      w1 = as.vector(d * exp(Xs %*% lambda * q))
      if (any(is.na(w1)) | any(is.infinite(w1))) {
        # changed part
        #warning("No convergence")
        g = rep(1, length(d))
        break
      }
      tr = crossprod(Xs, w1)
      if (max(abs(tr - total)/total) < EPS1) 
        break
    }
    if (l == max_iter) {
      # changed part
      #warning("No convergence")
      g = rep(1, length(d))
    }
    else g = w1/d
  }
  else if (method == "logit") {
    if (bounds[2] <= 1 || bounds[1] >= 1 || bounds[1] > 
        bounds[2]) {}
    # deleted stop
    #stop("The conditions low<1<upp are not fulfilled")
    A = (bounds[2] - bounds[1])/((1 - bounds[1]) * (bounds[2] - 
                                                      1))
    u = rep(1, length(d))
    F = (bounds[1] * (bounds[2] - 1) + bounds[2] * (1 - 
                                                      bounds[1]) * u)/(bounds[2] - 1 + (1 - bounds[1]) * 
                                                                         u)
    w1 = as.vector(d * F)
    T = t(Xs * w1)
    phiprim = ginv(T %*% Xs, tol = EPS)
    g = F
    tr = crossprod(Xs, w1)
    if (max(abs(tr - total)/total) > EPS1 | any(g < bounds[1]) | 
        any(g > bounds[2])) {
      lambda1 = rep(0, ncol(Xs))
      list = 1:length(g)
      t2 = total
      Xs1 = Xs
      d1 = d
      g1 = g
      q1 = q
      list1 = 1:length(g)
      for (l in 1:max_iter) {
        if (any(g < bounds[1]) | any(g > bounds[2])) {
          g[g < bounds[1]] = bounds[1]
          g[g > bounds[2]] = bounds[2]
          list = (1:length(g))[g > bounds[1] & g < bounds[2]]
          if (length(list) != 0) {
            g1 = g[list]
            t2 = total - as.vector(t(g[-list] * d[-list]) %*% 
                                     Xs[-list, ])
            Xs1 = Xs[list, ]
            d1 = d[list]
            q1 = q[list]
            list1 = list
          }
        }
        t1 = as.vector(t(d1) %*% Xs1)
        phi = t(Xs1) %*% as.vector(d1 * g1) - t1
        T = t(Xs1 * as.vector(d1 * g1))
        phiprime = T %*% Xs1
        lambda1 = lambda1 - ginv(phiprime, tol = EPS) %*% 
          (as.vector(phi) - t2 + t1)
        u = exp(A * (Xs1 %*% lambda1 * q1))
        F = g1 = (bounds[1] * (bounds[2] - 1) + bounds[2] * 
                    (1 - bounds[1]) * u)/(bounds[2] - 1 + (1 - 
                                                             bounds[1]) * u)
        if (any(is.na(g1))) {
          # changed part
          #warning("no convergence")
          g1 = g = rep(1, length(d))
          break
        }
        g[list1] = g1
        tr = crossprod(Xs, g * d)
        if (max(abs(tr - total)/total) < EPS1 & all(g >= 
                                                    bounds[1] & g <= bounds[2])) 
          break
      }
      if (l == max_iter) {
        # deleted part
        #cat("no convergence in", max_iter, "iterations with the given bounds. \n")
        #cat("the bounds for the g-weights are:", min(g), 
        #    " and ", max(g), "\n")
        #cat(" and the g-weights are given by g\n")
        g = rep(1, length(d))
      }
    }
  }
  if (description && !is.null(g)) {
    par(mfrow = c(3, 2), pty = "s")
    hist(g)
    boxplot(g, main = "Boxplot of g")
    hist(d)
    boxplot(d, main = "Boxplot of d")
    hist(g * d)
    boxplot(g * d, main = "Boxplot of w=g*d")
    if (method %in% c("truncated", "raking", "logit")) 
      cat("number of iterations ", l, "\n")
    cat("summary - initial weigths d\n")
    print(summary(d))
    cat("summary - final weigths w=g*d\n")
    print(summary(as.vector(g * d)))
  }
  g
}
environment(calib) <- getNamespace('sampling')
assignInNamespace('calib', calib, 'sampling')

# inverse function of the first derivative of the distance function
F <- function(method, x, lambda, L, U) {
  if (method == 'Linear') return(1 + (x %*% lambda))
  if (method == 'Raking ratio') return(exp(x %*% lambda))
  if (method == 'Logit') {
    A <- (U - L) / ((1 - L) * (U - 1))
    u <- x %*% lambda
    B <- L * (U - 1) + U * (1 - L) * exp(A * u)
    C <- U - 1 + (1 - L) * exp(A * u)
    return(B / C)
  }
  if (method == 'Linear bounded') {
    u <- x %*% lambda
    if (u >= L - 1 && u <= U - 1) return(1 + u)
    return(Inf)
  }
}

# create 'x' matrix
make_x <- function(x, k, data, indices, miss) {
  if (x %in% k) {
    values <- setdiff(sort(unique(data[indices, x])), miss)
    return(do.call(cbind, lapply(values, function(z) as.numeric(data[indices, x] == z))))
  }
  return(as.matrix(data[indices, x], ncol = 1))
}

# calibration function
cal <- function(data, totals, stratification, miss, indicators, indicators_stats, num, ncat, nnum, k, n, method, solver, L, U, maxit, tol) {
  sums1 <- sums2 <- calib_settings <- rows <- NULL
  result <- numeric(nrow(data))
  current_strata <- 0
  if (stratification == TRUE) current_strata <- totals[, 1]
  for (j in current_strata) {
    indices <- which(data$STRAcalif == j)
    widthx <- 0
    if (stratification == TRUE) Xj <- totals[totals[, 1] == j, ][2:ncol(totals)]
    else Xj <- totals
    if (isTruthy(num)) if (0 %in% Xj[names(Xj) %in% num]) {
      showModal(modalDialog('There are zeros in numerical variables in the table of totals. Remove these numerical variables from the list of 
                            calibrated numerical variables in strata where this happens.', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    Xj <- Xj[which(!names(Xj) %in% indicators)]
    Xj1 <- Xj2 <- Xj
    Xj <- Xj[which(Xj > 0)]
    if (0 %in% apply(as.data.frame(data[indices, names(data) %in% num]), 2, sum))
      showModal(modalDialog('There are zero columns for some numerical variable in the data.', footer = modalButton('OK'), easyClose = TRUE))
    numsum <- NULL
    data_order <- NULL
    for (i in 1:(ncat + nnum)) {
      if (i %in% k) {
        values <- setdiff(sort(unique(data[indices, i])), miss)
        widthx <- widthx + length(values)
        data_order <- c(data_order, paste(names(data[i]), values, sep = '_'))
      }
      if (i %in% n) {
        widthx <- widthx + 1
        numsum <- c(numsum, widthx)
        data_order <- c(data_order, names(data[i]))
      }
    }
    if (setequal(data_order, names(Xj)) == FALSE) {
      # if more variables were set as auxiliary than the table of totals contains
      showModal(modalDialog('Calibration variables do not match to the table of totals. Please check the data and totals for possible error, or make sure that you have
                            specified auxiliary calibration variables correctly.', footer = actionButton('modal_ok', 'OK')))
      req(FALSE)
    }
    totals_order <- match(names(Xj), data_order)
    Xj <- Xj[data_order]
    Xj <- as.numeric(Xj)
    if (isTruthy(num)) {
      nr_places <- apply(as.matrix(data[indices, n]), 2, function(x) floor(log10(abs(mean(x)))))
      data[indices, n] <- t(t(data[indices, n]) / (10 ^ nr_places))
      Xj[numsum] <- t(t(Xj[numsum]) / (10 ^ nr_places))
    }
    x <- do.call(cbind, lapply(1:(ncat + nnum), function(i) make_x(i, k, data, indices, miss)))
    if (solver == 'initial') result[indices] <- data$WEIGHTcalif[indices]
    if (solver == 'nleqslv') {
      nr_lambda <- widthx
      d <- as.matrix(data$WEIGHTcalif[indices])
      nr_w <- length(d)
      equations <- function(z) {
        f <- sapply(1:nr_lambda, function(i) sum(sapply(1:nr_w, function(h) d[h] * F(method, x[h,], z, L, U) * x[h, i])) - Xj[i])
        f
      }
      p <- rep(0, nr_lambda)
      slv <- try(nleqslv(p, equations, control = list(maxit = maxit, xtol = tol)), silent = TRUE)
      if (class(slv) == 'try-error') result[indices] <- data$WEIGHTcalif[indices]
      else result[indices] <- d * F(method, x, slv$x, L, U)
      if (max(c(NA, NaN, Inf, -Inf) %in% result[indices]) == 1) result[indices] <- data$WEIGHTcalif[indices]
    }
    if (solver == 'calib') {
      d <- data[indices, 'WEIGHTcalif']
      if (method == 'Linear') g <- try(sampling::calib(Xs = x, d = d, total = Xj, method = 'linear', max_iter = maxit), silent = TRUE)
      if (method == 'Raking ratio') g <- try(sampling::calib(Xs = x, d = d, total = Xj, method = 'raking', max_iter = maxit), silent = TRUE)
      if (method == 'Logit') g <- try(sampling::calib(Xs = x, d = d, total = Xj, method = 'logit', bounds = c(low = L, upp = U), max_iter = maxit), silent = TRUE)
      if (method == 'Linear bounded') g <- try(sampling::calib(Xs = x, d = d, total = Xj, method = 'truncated', bounds = c(low = L, upp = U), max_iter = maxit), silent = TRUE)
      if (class(g) == 'try-error') result[indices] <- data$WEIGHTcalif[indices]
      else result[indices] <- g * d
      if (max(c(NA, NaN, Inf, -Inf) %in% result[indices]) == 1) result[indices] <- data$WEIGHTcalif[indices]
    }
    forcontrol1 <- result[indices] %*% x
    if (isTruthy(num)) {
      forcontrol1[numsum] <- forcontrol1[numsum] * (10 ^ nr_places)
      Xj[numsum] <- t(t(Xj[numsum]) * (10 ^ nr_places))
    }
    forcontrol2 <- paste0(sprintf("%.2f", forcontrol1 * 100 / Xj), '%')
    forcontrol1 <- forcontrol1[totals_order]
    forcontrol2 <- forcontrol2[totals_order]
    Xj1[Xj1 != 0] <- forcontrol1
    Xj2[Xj2 != 0] <- forcontrol2
    if (isTruthy(indicators) && isTruthy(indicators_stats)) {
      if ('Mean (value)' %in% indicators_stats) {
        Xj1 <- c(round(result[indices] %*% as.matrix(data[indices, indicators]) / sum(result[indices]), 2), Xj1)
        Xj2 <- c(round(result[indices] %*% as.matrix(data[indices, indicators]) / sum(result[indices]), 2), Xj2)
      }
      if ('Mean (%)' %in% indicators_stats) {
        if (stratification == TRUE) denom <- totals[totals[, 1] == j, indicators]
        if (stratification == FALSE) denom <- totals[indicators]
        Xj1 <- c(paste0(sprintf("%.2f", result[indices] %*% as.matrix(data[indices, indicators]) * 100 / sum(result[indices]) / denom), '%'), Xj1)
        Xj2 <- c(paste0(sprintf("%.2f", result[indices] %*% as.matrix(data[indices, indicators]) * 100 / sum(result[indices]) / denom), '%'), Xj2)
      }
      if ('Total (value)' %in% indicators_stats) {
        Xj1 <- c(round(result[indices] %*% as.matrix(data[indices, indicators]), 2), Xj1)
        Xj2 <- c(round(result[indices] %*% as.matrix(data[indices, indicators]), 2), Xj2)
      }
      if ('Total (%)' %in% indicators_stats) {
        if (stratification == TRUE) denom <- totals[totals[, 1] == j, indicators]
        if (stratification == FALSE) denom <- totals[indicators]
        Xj1 <- c(paste0(sprintf("%.2f", result[indices] %*% as.matrix(data[indices, indicators]) * 100 / denom), '%'), Xj1)
        Xj2 <- c(paste0(sprintf("%.2f", result[indices] %*% as.matrix(data[indices, indicators]) * 100 / denom), '%'), Xj2)
      }
    }
    names(Xj1) <- names(Xj2) <- ''
    rows <- c(rows, j)
    sums1 <- rbind(sums1, Xj1)
    sums2 <- rbind(sums2, Xj2)
    calib_settings <- rbind(calib_settings, c(stratum = j, solver = solver, method = method, lower_bound = L, upper_bound = U))
  }
  list(result = result, sums1 = sums1, sums2 = sums2, calib_settings = calib_settings, rows = rows)
}