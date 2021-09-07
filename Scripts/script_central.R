setwd('/mnt/2892419C92416F7E/aMestrado Estatística/Disciplinas/T5 - Econometria III/Trabalho 2/')
# funcoes definidas pelo usuário
source("arma_garch.R")
source("pseudo_uniform_marginal.R")
source("copulae_fit.R")
source("simulate_returns.R")
source("generate_portfolio_weights.R")
source("returns_evaluation.R")

# pacotes
library(fPortfolio)      # CUIDAR ORDEM CHAMADA DESSE PACOTE
library(PerformanceAnalytics)
library(tidyverse)
library(lubridate)
library(moments)
library(forecast)
library(rugarch)
library(copula)


# BASE DE DADOS 
df <- data.table::fread('B3_stocks_long_2010-01-01_2021-04-09.csv')
df <- df %>%
  dplyr::mutate(ticker = gsub(pattern = ".SA", replacement = "", ticker)) %>%
  dplyr::filter(ticker %in% c("LREN3","RENT3","MGLU3","WEGE3","BBAS3","EMBR3")) %>%
  dplyr::arrange(ref.date) %>%
  dplyr::group_by(ticker) %>%
  dplyr::mutate(
    price.dif = price.close - lag(price.close, n = 1L),
    log.return = log(price.close / lag(price.close, n = 1L))
  ) %>%
  dplyr::mutate(
    nome_acao = case_when(ticker == "LREN3" ~ "Lojas Renner",
                          ticker == "RENT3" ~ "Localiza", 
                          ticker == "MGLU3" ~ "Magazine Luiza",
                          ticker == "WEGE3" ~ "WEG",
                          ticker == "BBAS3" ~ "Banco do Brasil",
                          ticker == "EMBR3" ~ "EMBRAER"
    )
  ) %>%
  dplyr::filter(year(ref.date) >= 2016 & year(ref.date) <= 2020) 

# DF LOG-RETORNOS DIÁRIOS
df_returns <- df %>%
  dplyr::select(ref.date, ticker, log.return) %>%
  tidyr::pivot_wider(id_cols = "ref.date", names_from = "ticker", values_from = "log.return") %>%
  tibble::column_to_rownames("ref.date") 


# SIMULAR CENÁRIOS ----
  # # SIMULAR CENÁRIOS: SELECIONAR PRIMEIRO DIA DE CADA SEMANA PRA OTIMIZAÇÃO ----
optim_days <- df_returns %>%
  tibble::rownames_to_column(var = "ref.date") %>%
  dplyr::filter(year(ref.date) %in% c(2019, 2020)) #%>% 
  # dplyr::mutate(semana = week(ref.date)) %>% dplyr::arrange(ref.date) %>%
  # dplyr::group_by(semana) %>%  dplyr::slice(1) %>% dplyr::ungroup() %>% dplyr::select(ref.date, semana)

    # # SIMULAR CENÁRIOS: LISTA COM SIMULAÇÃO RETORNOS T+1 ----
index_simulation <- which(row.names(df_returns) %in% optim_days$ref.date)
next_day_returns <- vector("list", length(index_simulation))
for (w in seq_along(index_simulation)) {
  x <- df_returns[seq(index_simulation[w] - 1), ]
  next_day_returns[[w]] <- simulate_returns(df_assets = x, n_simulations = 1000)
  if (w %% 50 == 0) {
    print(w)
    print(Sys.time())
  }
}
names(next_day_returns) <- rownames(df_returns)[index_simulation - 0]

# save.image(file = "ls_apos_retornos_diarios.RData") ##########################################################################################

    # # SIMULAR CENÁRIOS: GERAR RESULTADOS PORTFÓLIOS ----
real_returns <- df_returns[seq(index_simulation[1], nrow(df_returns)), ]   # ínice [1] porque está ordenado
real_returns <- exp(real_returns) - 1                                      # converter para retornos simples
  
    # # SIMULAR CENÁRIOS: OTIMIZAR PORTFÓLIOS (3 ALVOS) ----
        # # # CÓPULAS 1% ALVO
weights_copula_01 <- lapply(next_day_returns, function(x) { generate_portfolio_weights(x, target = 1.01 ** (1/252) - 1) }) %>% 
  bind_rows(.id = "ref.date") %>%
  tibble::column_to_rownames("ref.date")
        # # # CÓPULAS 3% ALVO
weights_copula_03 <- lapply(next_day_returns, function(x) { generate_portfolio_weights(x, target = 1.03 ** (1/252) - 1) }) %>% 
  bind_rows(.id = "ref.date") %>%
  tibble::column_to_rownames("ref.date")
        # # # CÓPULAS 5% ALVO
weights_copula_05 <- lapply(next_day_returns, function(x) { generate_portfolio_weights(x, target = 1.05 ** (1/252) - 1) }) %>% 
  bind_rows(.id = "ref.date") %>%
  tibble::column_to_rownames("ref.date")
        # # # 1/N COM AJUSTES MENSAIS
first_day_month <- data.frame(dt = rownames(weights_copula_01)) %>%
  mutate(mes = month(dt), ano = year(dt)) %>% group_by(ano, mes) %>% slice(1) %>% ungroup()
index_first_day <- which(row.names(weights_copula_01) %in% first_day_month$dt)

weights_1n_mes <- weights_copula_01[index_first_day,]
weights_1n_mes[seq(nrow(weights_1n_mes)),] <- 1 / ncol(weights_1n_mes)
        # # # 1/N COM AJUSTES DIÁRIOS
weights_1n_dia <- weights_copula_01
weights_1n_dia[seq(nrow(weights_copula_01)),] <- 1 / ncol(weights_copula_01)



    # # SIMULAR CENÁRIOS: GERAR MÉTRICAS DE DESEMPENHO ----
portfolio_copula_01 <- returns_evaluation(weights_portfolio = weights_copula_01,
                                          actual_returns = real_returns)
portfolio_copula_03 <- returns_evaluation(weights_portfolio = weights_copula_03,
                                          actual_returns = real_returns)
portfolio_copula_05 <- returns_evaluation(weights_portfolio = weights_copula_05,
                                          actual_returns = real_returns)
portfolio_1n_mes <- returns_evaluation(weights_portfolio = weights_1n_mes,
                                       actual_returns = real_returns)
portfolio_1n_dia <- returns_evaluation(weights_portfolio = weights_1n_dia,
                                       actual_returns = real_returns)

# COMPARAR RESULTADOS ----
    # # COMPARAR RESULTADOS: RETORNOS ----
list({portfolio_copula_01$metrics %>% rename('Copula 1%' = wealth)}, 
     {portfolio_copula_03$metrics %>% rename('Copula 3%' = wealth)}, 
     {portfolio_copula_05$metrics %>% rename('Copula 5%' = wealth)}, 
     {portfolio_1n_dia$metrics %>% rename('1/N Diário' = wealth)}, 
     {portfolio_1n_mes$metrics %>% rename('1/N Mensal' = wealth)}) %>%
  purrr::reduce(full_join, by = "metric") %>%
  knitr::kable(., "latex", digits = 4)


    # # COMPARAR RESULTADOS: PLOT RIQUEZA ----
list(data.frame(Estrategia = "Copulas 1%", portfolio_copula_01$wealth) %>% tibble::rownames_to_column("ref.date"),
     data.frame(Estrategia = "Copulas 3%", portfolio_copula_03$wealth) %>% tibble::rownames_to_column("ref.date"),
     data.frame(Estrategia = "Copulas 5%", portfolio_copula_05$wealth) %>% tibble::rownames_to_column("ref.date"),
     data.frame(Estrategia = "1/N Mensal", portfolio_1n_mes$wealth) %>% tibble::rownames_to_column("ref.date"),
     data.frame(Estrategia = "1/N Diário", portfolio_1n_dia$wealth) %>% tibble::rownames_to_column("ref.date")
     ) %>%
  bind_rows() %>%
  mutate(ref.date = ymd(ref.date)) %>%
  ggplot(., aes(x = ref.date, y = wealth, colour = Estrategia)) +
    geom_line(alpha = 0.4) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(y = "Riqueza (base 1)", x = "Data de Referência", title = "Evolução Riqueza por Portfólio") +
    theme_bw()
