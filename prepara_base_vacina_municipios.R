# r packages
library(readr)
library(dplyr)
library(basedosdados)

# prepara a tabela dos municipios -----------------------------------------

# importa a tabela do datalake da base dos dados -----------------------------------------

set_billing_id("live-base-dos-dados")

tabela_vacinacao <- read_sql("SELECT id_municipio_estabelecimento, dose_vacina, COUNT(*) AS dose_sum 
FROM basedosdados.br_ms_vacinacao_covid19.microdados
GROUP BY id_municipio_estabelecimento, dose_vacina
ORDER BY id_municipio_estabelecimento")


# prepara a tabela -----------------------------------------
janitor::tabyl(tabela_vacina_municipios080921, dose_vacina) # apenas par verificar como a variavel dose se comporta


tabela_vacina_municipios <- tabela_vacinacao |> 
  pivot_wider(names_from = dose_vacina,
              values_from = dose_sum) |>
  janitor::clean_names() |>
  mutate(dose_inicial = replace_na(dose_inicial, 0),
         x3a_dose = replace_na(x3a_dose, 0),
         dose_adicional = replace_na(dose_adicional, 0),
         reforco = replace_na(reforco, 0),
         x1o_reforco = replace_na(x1o_reforco, 0),
         x2a_dose_revacinacao = replace_na(x2a_dose_revacinacao,0), 
         primeira_dose = (x1a_dose + dose_inicial),
         imune = (x2a_dose+x3a_dose+dose_adicional+reforco+x2a_dose_revacinacao+x1o_reforco),) |> 
  rename(id_municipio = id_municipio_estabelecimento) |> 
  mutate(id_municipio = as.double(id_municipio)) |> 
  select(id_municipio, primeira_dose, imune)
  
# importa tabelas auxiliares (lista rm e populacao dos municpios) ---------

# lista dos municipios metropolitanos
library(readxl)
lista_metro_adaptada <- read_excel("C:/Users/jucia/Google Drive/Dossie_Covid/dados_espaciais/lista_rm.xlsx") |> 
  select(cod_mun, cod_mun6, nome, nome_rm, territorio) |> 
  rename(nome_metro=nome)
glimpse(lista_metro_adaptada)

# importa a tabela da populacao dos municipios

query5 <- "SELECT m.id_municipio, d.nome, populacao, 
FROM basedosdados.br_ibge_populacao.municipio AS m
LEFT JOIN basedosdados.br_bd_diretorios_brasil.municipio AS d
ON m.id_municipio=d.id_municipio
WHERE ano=2020"

pop_mun2020 <- read_sql(query5) |> 
  mutate(id_municipio = as.double(id_municipio),
         id_municipio6 = as.double(str_sub(id_municipio, end = 6))) |> 
  relocate(id_municipio, id_municipio6)


# tabela vacinacao municipios final
tabela_vacina_municpios_final <- tabela_vacina_municipios |> 
  left_join(pop_mun2020) |> 
  left_join(lista_metro_adaptada, by = c("id_municipio6" = "cod_mun6")) |> 
  relocate(id_municipio, 
           id_municipio6,
           nome,
           nome_metro,
           nome_rm,
           primeira_dose,
           imune,
           populacao
  ) |> 
  mutate(per_1dose = primeira_dose/populacao,
         per_imuni = imune/populacao) |> 
  select(-cod_mun) |> 
  as_tibble()




# exporta a tabela para a pasta do projeto no meu computador

write_csv(tabela_vacina_municpios_final, "C:/Users/jucia/Google Drive/Dossie_Covid/dashboard_vacina_rm/tabela_vacinacao_municipios.csv")

