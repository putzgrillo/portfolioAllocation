# ETAPA ARMA-GARCH 
    # ESTIMAR ARMA SEM MÉDIA E SEM SAZONALIDADE (RETORNOS SEMPRE MÉDIA 0)
    # USAR ORDEM SELECIONADA PARA ESTIMAR GARCH 
      # MODELO VARIÂNCIA É STANDARD GARCH
      # MODELO MÉDIA USA ORDENS DO ARMA ESTIMADO
      # INOVAÇÕES SEGUEM T-SUDENT SKEWED
    # RETORNA OBJETO GARCH

arma_garch <- function(y) {
  # ESTIMATE ARMA MODEL
  arima_model <- forecast::auto.arima(y = y,
                                      stationary = TRUE,
                                      seasonal = FALSE,
                                      allowmean = FALSE,
                                      allowdrift = FALSE
  )
  
  arima_order <- forecast::arimaorder(arima_model)[c(1,3)]
  
  # ESTIMATE GARCH MODEL
  garch_specs <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH",       # STANDARD GARCH
                          garchOrder = c(1,1),    # c(0,1) FOR ARCH
                          submodel = NULL,        # VALID FOR model = 'fGARCH
                          variance.targeting = T  # VARIANCE TARGETING FOR VAR INTERCEPT OMEGA (CARE WITH  fGARCH)
    ),
    mean.model = list(armaOrder = arima_order,    # ORDER FOR ARIMA (PREVIOUSLY CALCULATED)
                      include.mean = F,           # RETURNS MEAN EQUAL TO ZERO
                      archm = F,                  # ARCH VOLATILITY ON MEAN (VALID FOR include.mean = T)
                      archpow = 1                 # SD OR VAR FOR MEAN REGRESSION (VALID FOR include.mean = T)
    ),
    distribution.model = 'sstd'                   # INNOVATIONS (SKEWED STUDENT (GIVEN STYLIZED FACTS))
  )
  
  garch_model <- rugarch::ugarchfit(spec = garch_specs,
                                    data = y,
                                    solver = "solnp",
                                    solver.control = list(n.restarts = 2),
                                    out.sample = 0  # CAN KEEP OUT-OF-SAMPLE TO EVALUATE RESULTS
  )
  return(garch_model)
}