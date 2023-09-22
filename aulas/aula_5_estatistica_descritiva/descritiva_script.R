# Carrega pacotes necessários
library(tidyverse); library(haven); library(dplyr)

# Importa o banco de dados do tipo .sav, utilizado pelo programa SPSS
saq_df <- haven::read_sav(file = "aulas/aula_5_estatistica_descritiva/data/SAQ_mod.sav")

# Visualiza o banco
glimpse(saq_df)

# Carrega o pacote
library(sjlabelled)

# Mostra a descrição de cada variável do banco, se houver
sjlabelled::get_label(saq_df)

# Mostra os tipos de respostas para cada variável, se houver (note que aqui a função é diferente, pois labels está no plural)
# Selecionei apenas variáveis de interesse para reduzir o tamanho do output
saq_df |> 
  dplyr::select(sexo, raca_etnia, q01) |> 
  sjlabelled::get_labels()

# Podemos também visualizar os valores numéricos que estão representando os tipos de resposta para cada variável
saq_df |> 
  dplyr::select(sexo, raca_etnia, q01) |> 
  sjlabelled::get_values()


# Como exemplo, vamos utilizar as duas funções juntas para a variável sexo,
saq_df |> 
  dplyr::select(sexo) |> 
  sjlabelled::get_labels()

saq_df |> 
  dplyr::select(raca_etnia)

# Acessando diretamente o atributo "labels" do objeto, conseguimos visualizar os valores numéricos e os tipos de resposta simultaneamente.
saq_df |> 
  pull(sexo) |> 
  attr(which = "labels")

saq_df |> 
  dplyr::select(q01) |>
  pull() |> 
  attr(which = "labels")

# Checa se há dados faltantes no banco de dados em cada coluna do questionário
saq_df |> 
  dplyr::select(q01:q23) |> 
  filter(if_any(everything(), ~ .x == 9)) |> 
  dim()


# Checa se há dados faltantes no banco de dados em cada coluna do questionário

saq_df |> 
  summarise(across(q01:q23, ~ 9 %in% .x)) |> 
  glimpse()

factor_saq <- saq_df |> 
  mutate(
    across(c(sexo, raca_etnia, q01:q23), haven::as_factor), 
    across(c(q01:q23), forcats::fct_rev),
    across(c(q01:q23), ordered)
  ) 

glimpse(factor_saq)

factor_saq |> 
  pull(q01) |> 
  get_labels()

# Convertendo as variáveis do questionário em números e visualizando

factor_saq |> 
  mutate(sexo = as.numeric(sexo)) |> 
  pull(sexo) |> 
  unique()

# Calculando a média de altura nos indivíduos do banco de dados
factor_saq |> 
  dplyr::summarise(mean = mean(altura))

# Calculando média de altura por sexo
factor_saq |> 
  dplyr::group_by(sexo) |> 
  dplyr::summarise(media = mean(altura), mediana = median(altura))

average_heights <- factor_saq |> 
  group_by(sexo) |> 
  summarise(mean = mean(altura)) |> 
  pull()

# Histograma sem grupo
factor_saq |> 
  ggplot(aes(x = altura)) +
  geom_histogram() +
  geom_vline(xintercept = average_heights, linetype = "dashed") +
  xlab("Altura") +
  ylab("Frequência") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x.bottom = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 10, colour = "black"))

# Histograma por sexo
factor_saq |> 
  ggplot(aes(x = altura, fill = sexo)) +
  geom_histogram() +
  geom_vline(xintercept = average_heights, linetype = "dashed") +
  labs(fill = "Sexo") +
  xlab("Altura") +
  ylab("Frequência") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x.bottom = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 10, colour = "black"))

## Outras medidas de posição

# Cálculo de exemplo do primeiro decil, primeiro quartil e percentil 99
factor_saq |> 
  dplyr::group_by(sexo) |> 
  dplyr::summarise(primeiro_decil = quantile(altura, prob = .1), primeiro_quartil =quantile(altura, prob = .25), percentil_99 = quantile(altura, prob = .99))

# Vamos calcular as métricas acima para o nosso a altura dos indivíduos em nosso banco de dados

factor_saq |> 
  dplyr::group_by(sexo) |> 
  dplyr::summarise(variancia = var(altura), desvio_padrao = sd(altura), amplitude = max(altura) - min(altura), distancia_interquartil = IQR(altura))

sd_h <- factor_saq |> 
  group_by(sexo) |> 
  summarise(sd = sd(altura)) |> 
  pull()

factor_saq |> 
  ggplot(aes(x = altura, fill = sexo)) +
  geom_density() +
  geom_vline(xintercept = average_heights) +
  geom_vline(xintercept = c(average_heights-sd_h, average_heights+sd_h), linetype = "dashed") +
  labs(fill = "Sexo") +
  xlab("Altura") +
  ylab("Densidade") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x.bottom = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 10, colour = "black"))

# Descrevendo variáveis categóricas

## Tabela de contingência e gráfico em barras

# Cria tabela de contingência para a variável raca_etnia
factor_saq |> 
  dplyr::group_by(sexo) |> 
  dplyr::pull(raca_etnia) |> 
  base::table() 

# Cria gráfico de barras por sexo
factor_saq |> 
  mutate(raca_etnia = forcats::fct_relevel(raca_etnia, "Parda", "Branca", "Preta")) |> 
  ggplot(aes(x = raca_etnia, fill = sexo)) +
  geom_bar() +
  labs(fill = "Sexo") +
  ylab("Frequência") +
  xlab("Raça/Etnia") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x.bottom = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 10, colour = "black"))

library(gridExtra)

# Gera gráfico de barras para todas as variáveis do questionário

quest_items <- names(factor_saq)[7:29]

plots <- list()

for(var in quest_items) {
  plot <- factor_saq |> 
    ggplot(aes(x = .data[[var]], fill = .data[[var]])) +
    geom_bar() +
    xlab(get_label(saq_df[[var]])) +
    ylab("Frequência") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text.y = element_text(size = 10, colour = "black"),
          axis.text.x.bottom = element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x = element_text(size = 10, colour = "black"))
  
  plots[[var]] <- plot
}

# Cria graficos em um unico canvas
grid.arrange(
  plots$q01, 
  plots$q02,
  plots$q03,
  plots$q04,
  plots$q05,
  plots$q06,
  plots$q07,
  plots$q08,
  plots$q09,
  plots$q10,
  plots$q11,
  plots$q12,
  ncol = 4, 
  nrow = 3
)

grid.arrange(
  plots$q13,
  plots$q14,
  plots$q15,
  plots$q16,
  plots$q17,
  plots$q18,
  plots$q19,
  plots$q20,
  plots$q21,
  plots$q22,
  plots$q23,
  ncol = 4, 
  nrow = 3
)

# Cria nova coluna representando pontuação para o questionário
factor_saq <- factor_saq |> 
  mutate(across(q01:q23, as.numeric)) |> 
  rowwise() |> 
  mutate(ansiedade_r = sum(c(q01,q02,q03,q04,q05,q07,q07,q08,q09,q10,q11,q12,q13,q14,q15,q16,q17,q18,q19,q20,q21,q22,q23)))


# Estatísticas descritivas para a pontuação do questionário
factor_saq |> 
  group_by() |> 
  dplyr::summarise(media = mean(ansiedade_r), desvio_padrao = sd(ansiedade_r), maior_pontuação = max(ansiedade_r), menor_pontuação = min(ansiedade_r), distancia_interquartil = IQR(ansiedade_r))

# Cria novo gráfico de densidade para a variável ansiedade_r
factor_saq |> 
  ggplot(aes(x = ansiedade_r)) +
  geom_density(fill = "pink") +
  geom_density(aes(x=rnorm(2571, mean = 75.9, sd = 9.15)), linetype = "dashed") +
  geom_vline(xintercept = mean(factor_saq$ansiedade_r), linetype = "dashed") +
  xlab("Pontuação no questionário de ansiedade em relação ao R") +
  ylab("Densidade") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x.bottom = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 10, colour = "black"))



