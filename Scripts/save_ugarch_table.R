# showMethods(show)
# getMethod("show", "uGARCHfit")
# getAnywhere(".weightedBoxTest")
# getAnywhere("Weighted.Box.test")
attach(loadNamespace("rugarch"), name = ".weightedBoxTest")
attach(loadNamespace("rugarch"), name = "Weighted.Box.test")

save_ugarch_table <- function(garch_object, pu_object, col_result = "Value") {
  model = garch_object@model
  modelinc = garch_object@model$modelinc
  stdresid = garch_object@fit$residuals / garch_object@fit$sigma
  
  # coletar parâmetros
  df_parametros <- round(garch_object@fit$robust.matcoef, 4) %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(resultado = paste(` Estimate`, " ", 
                                    case_when(`Pr(>|t|)` <= 0.01 ~ "***",
                                              `Pr(>|t|)` <= 0.05 ~ "**",
                                              `Pr(>|t|)` <= 0.10 ~ "*",
                                              TRUE ~ ""),
                                    # " \n (", ` Std. Error`, ")", 
                                    sep = "")) %>%
    select(Parameter = rowname, resultado)
  
  colnames(df_parametros)[2] <- col_result
  
  # ljung-box (h0: ausência autocorrelação)
  lb_test_lvl = .weightedBoxTest(stdresid, p = 1, df = sum(modelinc[2:3]))
  lb_test_sq = .weightedBoxTest(stdresid, p = 2, df = sum(modelinc[8:9]))
  
  df_lb_test = data.frame(Parameter = c("Q(15)", "Q²(15)"),
                       resultado = as.character(round(c(lb_test_lvl[3,2], lb_test_sq[3,2]), 4)))
  
  colnames(df_lb_test)[2] <- col_result
  
  # k-s test (h0: segue a mesma distribuição)
  ks_test <- ks.test(pu_object, "punif", 0 , 1)
  df_ks_test <- data.frame(Parameter = "K-S Test",
                           resultado = as.character(round(ks_test$p.value, 4)))
  
  colnames(df_ks_test)[2] <- col_result
  
  # criar output
  tabela_final <- dplyr::bind_rows(list(df_parametros, df_lb_test, df_ks_test))
return(tabela_final)    
}
