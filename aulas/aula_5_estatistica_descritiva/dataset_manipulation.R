library(tidyverse); library(haven); library(sjlabelled); library(httr); library(jsonlite)

df <- haven::read_sav("aulas/aula_5_estatistica_descritiva/data/SAQ.sav") |> 
  mutate(id = row_number(), .before = q01) 

# Consome API de nomes do IBGE
res_masc = httr::GET("servicodados.ibge.gov.br/api/v2/censos/nomes?sexo=M&qtd=1000")

res_fem = httr::GET("servicodados.ibge.gov.br/api/v2/censos/nomes?sexo=F&qtd=1000")

resp_list <- map(list(res_masc$content, res_fem$content), ~ jsonlite::fromJSON(rawToChar(.x)))

names_df <- bind_rows(resp_list[[1]], resp_list[[2]]) |> 
  select(-regiao) |> 
  arrange(desc(freq)) |> 
  group_by(sexo) |> 
  mutate(prob_freq = freq/sum(freq)) |> 
  ungroup()

fem_quantity = 1314

male_quantity = 1257

fem_names <- filter(names_df, sexo == "F")

masc_names <- filter(names_df, sexo == "M")

fem_names_sample <- fem_names |>  
  slice_sample(n = fem_quantity, replace = T, weight_by = prob_freq)

masc_names_sample <- masc_names |> 
  slice_sample(n = male_quantity, replace = T, weight_by = prob_freq)

names_sample <- bind_rows(fem_names_sample, masc_names_sample)

female_heights = rnorm(fem_quantity, mean = 1.62, sd = 0.06)
male_heights = rnorm(male_quantity, mean = 1.75, sd = 0.07)

heights = c(female_heights, male_heights) |> round(digits = 2)

names_sample_df <- names_sample |> 
  select(nome, sexo) |> 
  mutate(
    nome = str_to_title(nome),
    sexo = if_else(sexo == "M", 0,1),
    altura = heights
  )

names_sample_df <- names_sample_df[sample(nrow(names_sample_df)),]

column_labels <- c(
  "Identificação",
  "Primeiro nome",
  "Idade em anos",
  "Sexo",
  "Raça/Etnia",
  "Altura em metros",
  "A estatística me faz chorar",
  "Meus amigos vão pensar que sou estúpido por não conseguir lidar com o R",
  "Desvios padrão me animam",
  "Sonho que Pearson está me atacando com coeficientes de correlação",
  "Eu não entendo estatística",
  "Tenho pouca experiência com computadores",
  "Todos os computadores me odeiam",
  "Nunca fui bom em matemática",
  "Meus amigos são melhores em estatística do que eu",
  "Computadores são úteis apenas para jogar jogos",
  "Fui mal em matemática na escola",
  "As pessoas tentam dizer que o R torna a estatística mais fácil de entender, mas não torna",
  "Tenho medo de causar danos irreparáveis por causa da minha incompetência com computadores",
  "Computadores têm mente própria e deliberadamente dão errado sempre que os uso",
  "Computadores estão atrás de mim",
  "Choro abertamente ao ouvir falar de tendência central",
  "Entro em coma sempre que vejo uma equação",
  "O R sempre trava quando tento usá-lo",
  "Todo mundo olha para mim quando uso o R",
  "Não consigo dormir pensando em vetores próprios",
  "Acordo debaixo do meu edredom pensando que estou preso sob uma distribuição normal",
  "Meus amigos são melhores no R do que eu",
  "Se eu for bom em estatística, meus amigos vão pensar que sou um nerd"
)

raca_etnia <- c("Branca", "Preta", "Parda")

prob_raca <- c(0.428, 0.106, 0.466)

gender_vars <- c(0,1) |> setNames(c("Masculino", "Feminino"))

ethnicity_vars <- c(1,2,3) |> setNames(raca_etnia)

question_labels <- c("Concordo totalmente" = 1, "Concordo" = 2, "Não concordo nem discordo" = 3, "Discordo" = 4, "Discordo totalmente" = 5, "Não respondida" = 9)

# Criar novas variáveis fictícias e fazer um merge dos bancos
altered_df <- df |> 
  mutate(
    nome = names_sample_df$nome,
    idade = round(rnorm(n = 2571, mean = 21, sd = 3)),
    sexo = names_sample_df$sexo,
    raca_etnia = sample(c(1,2,3), size = 2571, replace = T, prob = prob_raca),
    altura = names_sample_df$altura
  ) |> 
  relocate(c("nome",
             "idade",
             "sexo",
             "raca_etnia",
             "altura"), .before = q01) |> 
  set_label(column_labels) |> 
  mutate(
    sexo = labelled(sexo, label = "Sexo",labels = gender_vars),
    raca_etnia = labelled(raca_etnia, label = "Raça/Etnia",labels = ethnicity_vars),
    across(q01:q23, ~ labelled(.x, label = get_label(.x), labels = question_labels))
  )
  

write_sav(altered_df, path = "aulas/aula_5_estatistica_descritiva/data/SAQ_mod.sav")



