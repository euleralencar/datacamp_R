library(tidyverse)

# Tabela de referência
referencia <- tribble(
  ~variavel, ~valor_tabela, ~descricao,
  "sexo", "F", "feminino",
  "sexo", "M", "masculino",
  "escola", "1", "publica",
  "escola", "2", "particular",
  "escola", "3", "outra"
)

# Tabela de interesse
df <- tribble(
  ~id, ~sexo, ~escola,
  1, "F", 1,
  2, "M", 1,
  3, "F", 2,
  4, "F", 3,
  5, "M", 1
)

by <- setNames("valor_tabela", "sexo")

tab_join <- referencia %>% filter(variavel == 'sexo') %>% select(-variavel)

# Como eu usaria
df %>% left_join(tab_join, by = c('sexo' == 'valor_tabela'))
#> by
#sexo 
#"valor_tabela"  
df %>% left_join(tab_join, by)

# Função para substituir uma coluna pela referência
substituir_referencia <- function(tab, col, tab_ref) {
  
  # Filtrar só os valores de referência correta
  ref_col <- tab_ref %>%
    filter(variavel == col) %>%
    select(-variavel)
  
  # Quais colunas juntar
  by <- setNames("valor_tabela", col)
  
  # Juntar a tabela com a referência e fazer a descricao virar a coluna
  tab %>%
    mutate(across(all_of(col), as.character)) %>%
    left_join(ref_col, by) %>%
    select(-{{col}}) %>%
    rename({{col}} := descricao)
}



df %>%
  substituir_referencia("sexo", referencia) %>%
  substituir_referencia("escola", referencia)
#> # A tibble: 5 × 3
#>      id sexo      escola    
#>   <dbl> <chr>     <chr>     
#> 1     1 feminino  publica   
#> 2     2 masculino publica   
#> 3     3 feminino  particular
#> 4     4 feminino  outra     
#> 5     5 masculino publica
#> 


# Se você estiver procurando um desafio, tente descobrir como funciona o comando abaixo, que aplica todas as substituições em uma linha só:
  
# Usando reduce()
reduce(c("sexo", "escola"), substituir_referencia, referencia, .init = df)
#> # A tibble: 5 × 3
#>      id sexo      escola    
#>   <dbl> <chr>     <chr>     
#> 1     1 feminino  publica   
#> 2     2 masculino publica   
#> 3     3 feminino  particular
#> 4     4 feminino  outra     
#> 5     5 masculino publica