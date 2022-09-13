function (list_of_returns, event_start, event_end) 
{
  if (!inherits(event_start, "Date")) {
    stop("event_start must be an object of class Date.")
  }
  if (!inherits(event_end, "Date")) {
    stop("event_end must be an object of class Date.")
  }
  if (event_start > event_end) {
    stop("event_start must be earlier than event_end.")
  }
  estimation_abnormal <- NULL
  event_abnormal <- NULL
  estimation_market <- NULL
  event_market <- NULL
  delta <- numeric(length(list_of_returns))
  for (i in seq_along(list_of_returns)) {
    if (!inherits(list_of_returns[[i]], "returns")) {
      stop("Each element of list_of_rates must have class returns.")
    }
    if (list_of_returns[[i]]$estimation_end >= event_start) {
      message(paste0("For ", as.character(i), "-th company estimation", 
                     " period overlaps with event period."))
    }
    if (list_of_returns[[i]]$market_model != "sim") {
      stop("Patell's test is applicable only for Single-Index market model.")
    }
    if (is.null(estimation_market)) {
      estimation_market <- zoo::as.zoo(list_of_returns[[i]]$regressor[zoo::index(list_of_returns[[i]]$regressor) >= 
                                                                        list_of_returns[[i]]$estimation_start & zoo::index(list_of_returns[[i]]$regressor) <= 
                                                                        list_of_returns[[i]]$estimation_end])
    }
    else if (identical(estimation_market, list_of_returns[[i]]$regressor)) {
      stop("regressor must be the same for all companies.")
    }
    if (is.null(event_market)) {
      event_market <- zoo::as.zoo(list_of_returns[[i]]$regressor[zoo::index(list_of_returns[[i]]$regressor) >= 
                                                                   event_start & zoo::index(list_of_returns[[i]]$regressor) <= 
                                                                   event_end])
    }
    else if (identical(event_market, list_of_returns[[i]]$regressor)) {
      stop("regressor must be the same for all companies.")
    }
    company_estimation_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[zoo::index(list_of_returns[[i]]$abnormal) >= 
                                                                               list_of_returns[[i]]$estimation_start & zoo::index(list_of_returns[[i]]$abnormal) <= 
                                                                               list_of_returns[[i]]$estimation_end])
    company_event_abnormal <- zoo::as.zoo(list_of_returns[[i]]$abnormal[zoo::index(list_of_returns[[i]]$abnormal) >= 
                                                                          event_start & zoo::index(list_of_returns[[i]]$abnormal) <= 
                                                                          event_end])
    if (is.null(estimation_abnormal)) {
      estimation_abnormal <- company_estimation_abnormal
    }
    else {
      estimation_abnormal <- merge(estimation_abnormal, 
                                   company_estimation_abnormal, all = TRUE)
    }
    if (is.null(event_abnormal)) {
      event_abnormal <- company_event_abnormal
    }
    else {
      event_abnormal <- merge(event_abnormal, company_event_abnormal, 
                              all = TRUE)
    }
    delta[i] <- list_of_returns[[i]]$estimation_length
  }
  event_means <- rowMeans(event_abnormal, na.rm = TRUE)
  event_means[is.nan(event_means)] <- NA
  estimation_means <- rowMeans(estimation_abnormal, na.rm = TRUE)
  estimation_means[is.nan(estimation_means)] <- NA
  estimation_market_mean <- mean(estimation_market, na.rm = TRUE)
  sum_estimation_market <- sum((estimation_market - estimation_market_mean)^2, 
                               na.rm = TRUE)
  result <- data.frame(date = zoo::index(event_abnormal), weekday = weekdays(zoo::index(event_abnormal)), 
                       percentage = rowSums(!is.na(as.matrix(event_abnormal)), 
                                            na.rm = TRUE)/ncol(event_abnormal) * 100, mean = event_means)
  estimation_abnormal <- as.matrix(estimation_abnormal)
  event_abnormal <- as.matrix(event_abnormal)
  mean_delta <- mean(delta)
  sd <- stats::sd(estimation_means, na.rm = TRUE) * sqrt(1 + 
                                                           1/mean_delta + (event_market - estimation_market_mean)^2/sum_estimation_market)
  statistics <- event_means/sd
  statistics[is.nan(statistics)] <- NA
  significance <- rep("", length(statistics))
  significance[abs(statistics) >= const_q1] <- "*"
  significance[abs(statistics) >= const_q2] <- "**"
  significance[abs(statistics) >= const_q3] <- "***"
  result <- cbind(result, data.frame(lmb_stat = statistics, 
                                     lmb_signif = significance))
  rownames(result) <- NULL
  return(result)
}
<bytecode: 0x000002517556b290>
  <environment: namespace:estudy2>
  
  
  
  
  
  
  
  
  
  function (rates,
            regressor,
            market_model = c("mean_adj", "mrkt_adj",
                             "sim"),
            estimation_method = c("ols"),
            estimation_start,
            estimation_end)
  {
    UseMethod("returns")
  }
< bytecode:0x00000251601afd18 >
  < environment:namespace:estudy2 > 
  

  
  
  
  function (rates,
            regressor,
            market_model = c("mean_adj", "mrkt_adj",
                             "sim"),
            estimation_method = c("ols"),
            estimation_start,
            estimation_end)
  {
    market_model <- match.arg(market_model)
    estimation_method <- match.arg(estimation_method)
    if (market_model != "mean_adj" & missing(regressor)) {
      stop(paste("For market model", market_model, "specify the regressor."))
    }
    if (estimation_start >= estimation_end) {
      stop("estimation_start should be earlier than estimation_end")
    }
    if (market_model == "mean_adj") {
      k_qnorm <- stats::qnorm(1 - 0.05 / 2)
      estimation_data <- rates[stats::complete.cases(rates),]
      estimation_data <- estimation_data[estimation_data[,
                                                         1] >= estimation_start &
                                           estimation_data[, 1] <=
                                           estimation_end,]
      delta <- nrow(estimation_data)
      if (delta == 0) {
        stop(
          paste0(
            "The estimation window contains no data. Check ",
            paste(names(estimation_data)[-1], collapse = " and "),
            " for missing values."
          )
        )
      }
      estimation_mean <- mean(estimation_data[, 2])
      estimation_sd <- stats::sd(estimation_data[, 2])
      result <- list(
        observed = zoo::zoo(rates[, 2], rates[,
                                              1]),
        predicted = zoo::zoo(rep(estimation_mean, nrow(rates)),
                             rates[, 1]),
        lower95CI = zoo::zoo(
          rep(
            estimation_mean -
              k_qnorm /
              sqrt(delta) * estimation_sd,
            nrow(rates)
          ),
          rates[, 1]
        ),
        upper95CI = zoo::zoo(
          rep(
            estimation_mean +
              k_qnorm /
              sqrt(delta) * estimation_sd,
            nrow(rates)
          ),
          rates[, 1]
        ),
        abnormal = zoo::zoo(rates[, 2], rates[,
                                              1]) - zoo::zoo(rep(estimation_mean, nrow(rates)),
                                                             rates[, 1]),
        market_model = market_model,
        full_name_market_model = "Mean adjusted market model",
        estimation_start = estimation_start,
        estimation_end = estimation_end,
        estimation_length = delta
      )
    }
    else if (market_model == "mrkt_adj") {
      data <- merge(rates, regressor, by = "date", all = TRUE)
      estimation_data <- data[stats::complete.cases(data),]
      estimation_data <- estimation_data[estimation_data[,
                                                         1] >= estimation_start &
                                           estimation_data[, 1] <=
                                           estimation_end,]
      delta <- nrow(estimation_data)
      if (delta == 0) {
        stop(
          paste0(
            "The estimation window contains no data. Check ",
            paste(names(estimation_data)[-1], collapse = " and "),
            " for missing values."
          )
        )
      }
      y <- estimation_data[, 2]
      x <- estimation_data[, 3]
      lm_fit <- stats::lm(y ~ x)
      lm_fit$coefficients <- c(0, 1)
      predicted <-
        stats::predict.lm(
          object = lm_fit,
          newdata = data.frame(x = data[,
                                        3]),
          interval = c("confidence"),
          level = 0.95
        )
      rownames(predicted) <- NULL
      result <- list(
        observed = zoo::zoo(data[, 2], data[,
                                            1]),
        predicted = zoo::zoo(predicted[, 1], data[,
                                                  1]),
        lower95CI = zoo::zoo(predicted[, 2], data[,
                                                  1]),
        upper95CI = zoo::zoo(predicted[, 3], data[,
                                                  1]),
        abnormal = zoo::zoo(data[, 2], data[, 1]) -
          zoo::zoo(predicted[, 1], data[, 1]),
        regressor = zoo::zoo(data[,
                                  3], data[, 1]),
        market_model = market_model,
        full_name_market_model = "Market Adjusted Market Model",
        estimation_start = estimation_start,
        estimation_end = estimation_end,
        estimation_length = delta
      )
    }
    else if (market_model == "sim") {
      if (estimation_method == "ols") {
        data <- merge(rates, regressor, by = "date", all = TRUE)
        estimation_data <- data[stats::complete.cases(data),]
        estimation_data <- estimation_data[estimation_data[,
                                                           1] >= estimation_start &
                                             estimation_data[, 1] <=
                                             estimation_end,]
        delta <- nrow(estimation_data)
        if (delta == 0) {
          stop(
            paste0(
              "The estimation window contains no data. Check ",
              paste(names(estimation_data)[-1], collapse = " and "),
              " for missing values."
            )
          )
        }
        y <- estimation_data[, 2]
        x <- estimation_data[, 3]
        lm_fit <- stats::lm(y ~ x)
        predicted <-
          stats::predict.lm(
            object = lm_fit,
            newdata = data.frame(x = data[,
                                          3]),
            interval = c("confidence"),
            level = 0.95
          )
        rownames(predicted) <- NULL
        result <- list(
          observed = zoo::zoo(data[, 2], data[,
                                              1]),
          predicted = zoo::zoo(predicted[, 1], data[,
                                                    1]),
          lower95CI = zoo::zoo(predicted[, 2], data[,
                                                    1]),
          upper95CI = zoo::zoo(predicted[, 3], data[,
                                                    1]),
          abnormal = zoo::zoo(data[, 2], data[, 1]) -
            zoo::zoo(predicted[, 1], data[, 1]),
          regressor = zoo::zoo(data[,
                                    3], data[, 1]),
          market_model = market_model,
          full_name_market_model = "Single-Index Market Model",
          estimation_method = estimation_method,
          full_name_estimation_method = "Ordinary Least Squares",
          coefficients = c(
            alpha = unname(lm_fit$coefficients)[1],
            beta = unname(lm_fit$coefficients)[2]
          ),
          estimation_start = estimation_start,
          estimation_end = estimation_end,
          estimation_length = delta
        )
      }
    }
    class(result) <- "returns"
    return(result)
  }
< bytecode:0x0000025176bfd860 >
  < environment:namespace:estudy2 >
  











