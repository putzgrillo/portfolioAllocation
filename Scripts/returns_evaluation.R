returns_evaluation <- function(weights_portfolio, actual_returns, colname = "wealth") {
  # RETURNS
  portfolio_returns <- PerformanceAnalytics::Return.portfolio(R = actual_returns,
                                                              weights = weights_portfolio,
                                                              wealth.index = F, value = 1)
  # WEALTH
  portfolio_wealth <- PerformanceAnalytics::Return.portfolio(R = actual_returns,
                                                             weights = weights_portfolio,
                                                             wealth.index = T, value = 1)
  # PERFORMANCE METRICS
  portfolio_metrics <- list(
    table.AnnualizedReturns(portfolio_returns),
    table.DownsideRisk(portfolio_returns)[8:9, , drop = FALSE],
    # SharpeRatio(portfolio_returns, method = "historical"),
    SortinoRatio(portfolio_returns, method = "historical"),
    data.frame(portfolio.returns = DownsideFrequency(portfolio_returns), row.names = "Downside Frequency"),
    data.frame(portfolio.returns = maxDrawdown(portfolio_returns), row.names = "Maximum Drawdown"),
    AverageDrawdown(portfolio_returns, method = "historical"),
    DrawdownDeviation(portfolio_returns),
    AverageLength(portfolio_returns, method = "historical")
  ) %>% 
    lapply(., function(x) {data.frame(x) %>% tibble::rownames_to_column("metric")}) %>% 
    bind_rows()
  
  # RENAMES COLUMNS
  colnames(portfolio_returns) <- colname
  colnames(portfolio_wealth) <- colname
  colnames(portfolio_metrics)[2] <- colname
  
  # RESULT
  resultado <- list(
    returns = portfolio_returns,
    wealth = portfolio_wealth,
    metrics = portfolio_metrics
  )
  
return(resultado)
}
