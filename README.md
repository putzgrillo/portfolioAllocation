# portfolioAllocation
Determine optimal portfolio using ARMA-GARCH and Copulae

The files are:
- 'description.pdf' is the file with explanation of procedure
- 'script_central.R' is the script that calls all function and executes the strategy procedures
- 'arma_garch.R' is the function that estimates ARCH-GARCH model with asymmetric t-Student innovations
- 'pseudo_uniform_marginal.R' is the function that create pseudo-margins vectors from GARCH parameters
- 'copulae_fit.R' is the function that compare candidate copulae and returns (i) the copula with smallest AIC and (ii) a table comparing metrics of candidate copulae (optionally, it returns the results of goodness-of-fit test, which is very slow)
- 'simulate_returns.R' is the function that simulate the next-day returns using ARMA-GARCH + Copula
- 'generate_portfolio_weights.R' is the function that determines the weight of each asset in the portfolio given the simulated returns (currently using CVaR optimization)
- 'returns_evaluation.R' is the function that returns out-of-sample performance metrics of each strategy
