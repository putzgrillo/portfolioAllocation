generate_portfolio_weights <- function(next_day_returns, target) {
  # ESPECIFICAÇÕES PORTFOLIO
  portfolio_spec <- fPortfolio::portfolioSpec(
    model = list(type = "CVaR",
                 params = list(alpha = 0.05),
                 tailRisk = list()),
    portfolio = list(
      targetReturn = target
    ),
    optim = list(
      solver = "solveRglpk.CVAR"
    )
  )
  # OTIMIZAÇÃO PORTFOLIO
  portfolio_return <- as.timeSeries(next_day_returns)
  portfolio_final <- fPortfolio::efficientPortfolio(data = portfolio_return,
                                                    spec = portfolio_spec)
  # PESOS DO PORTFÓLIO
  portfolio_weights <- portfolio_final@portfolio@portfolio$weights
  portfolio_weights <- data.frame(t(portfolio_weights / sum(portfolio_weights)))
  
return(portfolio_weights)
}
