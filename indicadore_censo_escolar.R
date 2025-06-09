#Paacotes necessários
library(tidyverse)
library(readxl)
library(writexl)

#Tentando reconhecer o encoding do arquivo
guess_encoding("microdados_ed_basica_2024.csv")

#Importando dados

base = read_csv2('microdados_ed_basica_2024.csv', 
                 locale = locale(encoding = "ISO-8859-1"),
                 na = c("88888"))


# Filtrando variáveis necessárias 
baserj = base |> 
  dplyr::filter(SG_UF == 'RJ')
baserj_indicadores = baserj |> 
  select(NO_MUNICIPIO,TP_DEPENDENCIA, IN_ESGOTO_INEXISTENTE,IN_INTERNET,
         IN_ACESSIBILIDADE_INEXISTENTE, IN_COMPUTADOR)


# Recodificando a variáveis IN_ESGOTO_INEXISTENTE, IN_INTERNET, IN_ACESSIBILIDADE_INEXISTENTE e 
#IN_COMPUTADOR : 0 = Não, 1 = Sim. E auterando variável TP_DEPENDENCIA: 1 = Federal, 2 = Estadual
# 3 = Municipal e 4 = Privada
baserj_indicadores = baserj_indicadores |> 
  mutate(across(
    c(
      IN_ESGOTO_INEXISTENTE,
      IN_INTERNET,
      IN_ACESSIBILIDADE_INEXISTENTE,
      IN_COMPUTADOR
    ),
    ~ case_when(
      .x == 0 ~ "Não",
      .x == 1 ~ "Sim"
    )
  )) |> 
  mutate(TP_DEPENDENCIA = case_when(
    TP_DEPENDENCIA == 1 ~ "Federal",
    TP_DEPENDENCIA == 2 ~ "Estadual",
    TP_DEPENDENCIA == 3 ~ "Municipal",
    TP_DEPENDENCIA == 4 ~ "Privada"
  )) 

#Contando o númaros de dependencias para cada munucípio
cotagemTP_DEPENDENCIA = baserj_indicadores |> 
  group_by(NO_MUNICIPIO, TP_DEPENDENCIA)  |> 
  summarise(
    n = n(),            
    .groups = "drop"    
  )
#Modificando colunas da tabela
tabela_TP_DEPENDENCIA = cotagemTP_DEPENDENCIA  |> 
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = n,
    values_fill = 0
  )


#Adicionando colunas na tabela de dependencias, total de escolas e a proporção de cada tipo de dependência  
tabela_TP_DEPENDENCIA = tabela_TP_DEPENDENCIA |> 
  mutate('Total_Escolas' = Estadual + Federal + Municipal + Privada) |> 
  mutate('Prop_EPu'= ((Estadual + Federal + Municipal) / Total_Escolas)*100 ) |> 
  mutate('Prop_Epr'= (Privada / (Estadual + Federal + Municipal))*100 )


# Filtrando por escolas municipais
municipais_rj = baserj_indicadores |> 
  filter(TP_DEPENDENCIA == "Municipal")

# Adicionando proporção de escola municipais com acesso a internet
tabela_internet_municipal = municipais_rj |> 
  group_by(NO_MUNICIPIO) |> 
  summarise(
    Total_Escolas = n(),
    Com_Internet = sum(IN_INTERNET == "Sim", na.rm = TRUE),
    Prop_Internet = round((Com_Internet / Total_Escolas) * 100, 2),
    .groups = "drop") |> 
  rename(Cidades = NO_MUNICIPIO)

# Montando tabela com proporção de escolas com pelo menos algum tipo de acessibilidade 
tabela_acessibilidade = baserj_indicadores |>  
  group_by(NO_MUNICIPIO) |> 
  summarise( Total_escola = n(),
             Com_Ac = sum(IN_ACESSIBILIDADE_INEXISTENTE == "Não", na.rm = TRUE),
             Prop_Ac = round((Com_Ac/Total_escola)*100,2),
             .groups = "drop") |> 
  rename(Municípios = NO_MUNICIPIO)

# Montando tabela com proporção de escolas com pelo menos algum tipo de esgoto
tabela_esgoto = baserj_indicadores |> 
  group_by(NO_MUNICIPIO) |> 
  summarise(Total_escola = n(),
            Com_Esg = sum(IN_ESGOTO_INEXISTENTE == "Não", na.rm = TRUE),
            Prop_Esg = round((Com_Esg/Total_escola)*100,2),
            .groups = "drop") |> 
  rename(Localidade = NO_MUNICIPIO)


#Juntando todas as tabelas de indicadores criadas, de internet, de dependencias, de acessibilidade e
# de esgoto
tabela_Indicadores = tabela_TP_DEPENDENCIA |>
  left_join(tabela_internet_municipal, by = c("NO_MUNICIPIO" = "Cidades")) |>
  left_join(tabela_acessibilidade, by = c("NO_MUNICIPIO" = "Municípios")) |>
  left_join(tabela_esgoto, by = c("NO_MUNICIPIO" = "Localidade"))


#planilha com os indicadores criados
write_xlsx(tabela_Indicadores,"Indicadores_Censo_escolar_2024.xlsx")

