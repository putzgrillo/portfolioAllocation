simulate_returns <- function(df_assets, n_simulations = 500) {
  n_assets <- ncol(df_assets)
  
  # # ARMA_GARCH + PSEUDO_OBSERVATIONS ----
  garch_models <- lapply(df_assets, function(x) {arma_garch(y = x)})
  pseudo_obs <- lapply(garch_models, function(x) {pseudo_uniform_marginal(garch_model = x, skewed_t = TRUE)})
  pseudo_obs_matrix <- pseudo_obs %>% bind_cols() %>% as.matrix()
  
  df_std_resid <- lapply(garch_models, function(x) {residuals(x) / x@fit$sigma}) %>% bind_cols() %>% as.matrix() # será convertida em pseudo pela função pobs()

  # # ESTIMAR COPULA
  copula_output <- copulae_fit(x_df = df_std_resid, pseudo_observations = pseudo_obs_matrix)
  final_copula <- copula_output$final_copula
  
  # # SIMULAR RETORNOS PARA T+1
          # # gerar valores cópulas
  copulae_simulate <- copula::rCopula(n = n_simulations, copula = final_copula@copula)
          # # previsao + simulacao
  returns_simulation <- mapply(function(garch, copulae) {
              # # # converter "pseudo" gerado pelas cópulas para estimativas de erro 
    garch_coefs <- as.data.frame(t(coef(garch)))
    q_garch <- fGarch::qsstd(p = copulae,
                             nu = garch_coefs$shape,
                             xi = garch_coefs$skew)
    z_sim <- q_garch / sd(q_garch)
              # # # prever série e sigma
    garch_forecast <- rugarch::ugarchforecast(garch, n.ahead = 1)
    
              # # # retorno
    retornos <- as.vector(garch_forecast@forecast$seriesFor) + as.vector(garch_forecast@forecast$sigmaFor) * z_sim
    
  }, garch = garch_models, copulae = as.data.frame(copulae_simulate))

return(returns_simulation)
}