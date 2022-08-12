function (rates,
          regressors,
          same_regressor_for_all = TRUE,
          market_model = c("mean_adj",
                           "mrkt_adj", "sim"),
          estimation_method = c("ols"),
          estimation_start,
          estimation_end)
{
  market_model <- match.arg(market_model)
  estimation_method <- match.arg(estimation_method)
  if (market_model != "mean_adj") {
    if (missing(regressors)) {
      stop(paste(
        "For market model",
        market_model,
        "specify the",
        "regressors."
      ))
    }
    if (!same_regressor_for_all &&
        ncol(rates) != ncol(regressors)) {
      stop(
        paste(
          "The number of regressors columns should be the same",
          "as the number of rates columns"
        )
      )
    }
    if (same_regressor_for_all && ncol(regressors) > 2) {
      message("Only second column of regressors will be used")
    }
  }
  if (estimation_start >= estimation_end) {
    stop("estimation_start should be earlier than estimation_end")
  }
  if (market_model != "mean_adj" && same_regressor_for_all) {
    first_element <- regressors[, c(1, 2)]
    regressors <-
      data.frame(date = first_element[, 1], first_element[,
                                                          rep(2, ncol(rates) - 1)])
  }
  list_of_returns <- list()
  if (market_model == "mean_adj") {
    for (i in 2:ncol(rates)) {
      list_of_returns[[i - 1]] <- returns(
        rates = rates[,
                      c(1, i)],
        market_model = market_model,
        estimation_start = estimation_start,
        estimation_end = estimation_end
      )
    }
  }
  else if (market_model == "mrkt_adj") {
    for (i in 2:ncol(rates)) {
      list_of_returns[[i - 1]] <- returns(
        rates = rates[,
                      c(1, i)],
        regressor = regressors[, c(1, i)],
        market_model = market_model,
        estimation_start = estimation_start,
        estimation_end = estimation_end
      )
    }
  }
  else if (market_model == "sim") {
    for (i in 2:ncol(rates)) {
      list_of_returns[[i - 1]] <- returns(
        rates = rates[,
                      c(1, i)],
        regressor = regressors[, c(1, i)],
        market_model = market_model,
        estimation_method = estimation_method,
        estimation_start = estimation_start,
        estimation_end = estimation_end
      )
    }
  }
  return(list_of_returns)
}
< bytecode:0x000001a2c98368f8 >
  < environment:namespace:estudy2 > 