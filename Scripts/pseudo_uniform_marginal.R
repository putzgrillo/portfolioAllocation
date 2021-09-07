# ETAPA CRIAR CDF DAS MARGINAIS 
    # A PARTIR DE UM OBJETO DO MODELO GARCH
    # ESTIMA AS OBSERVAÇÕES PSEUDO-UNIFORMES
    # SE skewed_t = TRUE, USA SKEWED T-STUDENT
    # SE skewed_t = FALSE, USA FUNÇÃO POBS DO PACOTE COPULA

pseudo_uniform_marginal <- function(garch_model, skewed_t = TRUE) {
  # VECTORS
  errors_vector <- residuals(garch_model)
  sigma_vector <- garch_model@fit$sigma
  coefficients_vector <- garch_model@fit$coef 
  
  if (skewed_t) {
    coefs_df <- tibble::enframe(coefficients_vector)
    uniform_vector <- fGarch::psstd(q = errors_vector / sigma_vector,
                                    nu = coefs_df$value[coefs_df$name == 'shape'],
                                    xi = coefs_df$value[coefs_df$name == 'skew'])
  } else {
    uniform_vector <- copula::pobs(errors_vector / sigma_vector)
  }
  return(uniform_vector)
}