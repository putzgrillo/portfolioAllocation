# COPULAS
    # A PARTIR DE UM DATAFRAME COM LOG.RETORNO DE ATIVOS & PSEUDO_OBSERVATIONS
    # ESTIMA CÓPULAS:
      # ELÍPTICAS: t
      # ARQUIMEDIANAS: gumbel, Joe
      # DE VALOR EXTREMO: gumbel

# library(copula)
copulae_fit <- function(x_df, pseudo_observations, goft = FALSE) {
  dimension <- ncol(x_df)
  # ESTIMAR COPULAS ----
      # # ESTIMAR COPULAS: GUMBEL
  copulaGumbel <- fitCopula(copula = gumbelCopula(dim = dimension),
                            data = pseudo_observations, method = "mpl")
  
      # # ESTIMAR COPULAS: t
  copulaT <- fitCopula(copula = tCopula(dim = dimension, 
                                        dispstr = "ex",
                                        df.fixed = FALSE),
                       data = pseudo_observations, method = "mpl")
  
      # # ESTIMAR COPULAS: JOE
  copulaJoe <- fitCopula(copula = joeCopula(dim = dimension),
                         data = pseudo_observations, method = "mpl")
  
      # # ESTIMAR COPULAS: MISTA
  copulaMix <- fitCopula(copula = mixCopula(list(gumbelCopula(dim = dimension),
                                                 # rotCopula(gumbelCopula(dim = dimension)),
                                                 tCopula(dim = dimension, df.fixed = T),
                                                 joeCopula(dim = dimension))),
                         data = pseudo_observations, method = "mpl")
  
  # MÉTRICAS PADRÃO CÓPULAS ----
  copulae_metrics <- data.frame(
    Copula = c("Gumbel", "t", "Joe", "Mixture"),
    AIC = c(AIC(copulaGumbel), AIC(copulaT), AIC(copulaJoe), AIC(copulaMix)),
    BIC = c(BIC(copulaGumbel), BIC(copulaT), BIC(copulaJoe), BIC(copulaMix)),
    LL = c(logLik(copulaGumbel), logLik(copulaT), logLik(copulaJoe), logLik(copulaMix))
  )
  
  # REALIZA TESTE DE QUALIDADE DE AJUSTE? (MUITO DEVAGAR)
  if (goft) {
    # QUALIDADE DO AJUSTE
    gofGumbel <- gofCopula(copula = copulaGumbel@copula, x = x_df, N = 200, method = "Sn", simulation = c("pb", "mult")[1])
    gofJoe <- gofCopula(copula = copulaJoe@copula, x = x_df, N = 200, method = "Sn", simulation = c("pb", "mult")[1])
    
    # # # APÓS ESTIMAR DF, CALCULAR COMO DF.FIXED PARA AGILIZAR TESTE QUALIDADE DE AJUSTE (t e mista)
        # # # # COPULA T
    degrees_freedom <- as.integer(round(copulaT@copula@parameters[2], digits = 0))
    copulaTT <- fitCopula(copula = tCopula(dim = dimension, dispstr = "ex", df = degrees_freedom, df.fixed = TRUE),
                          data = pseudo_observations,
                          method = "mpl")
    gofT <- copula::gofCopula(copula = copulaTT@copula, x = x_df, N = 50, method = "Sn", simulation = c("pb", "mult")[1])
    
        # # # # COPULA MISTA (T NA POSIÇÃO 2)
    degrees_freedom_mista <- as.integer(round( copulaMix@copula@cops@.Data[[2]]@parameters[2], digits = 0))
    copulaMixx <- fitCopula(copula = mixCopula(list(gumbelCopula(dim = dimension),
                                                    # rotCopula(gumbelCopula(dim = dimension)),
                                                    tCopula(dim = dimension, df.fixed = T, df = degrees_freedom_mista),
                                                    joeCopula(dim = dimension))),
                            data = pseudo_observations, method = "mpl")
    
    gofMix <- gofCopula(copula = copulaMixx@copula, x = x_df, N = 50, method = "Sn", simulation = c("pb", "mult")[1])
    
    # COMPILAR MÉTRICAS
    copulae_metrics <- bind_cols(list(copulae_metrics,
                                      data.frame(GoF = c(gofGumbel$p.value, gofT$p.value, 
                                                         gofJoe$p.value, gofMix$p.value)
                                      )))
  }
  
  # VENCEDORA
  index_best <- which.min(copulae_metrics$AIC)
  copulaFinal <- list(copulaGumbel, copulaT, copulaJoe, copulaMix)[[index_best]]

  # RETORNAR
  result <- list(final_copula = copulaFinal, metricas = copulae_metrics)
return(result)
}
