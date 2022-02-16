# Exploraty to use in Rmarkdown

# Explanatory


# Load the ggplot2 package
require(ggplot2)
require(tidyverse)

# Explore the mtcars data frame with str()
str(mtcars)

# Execute the following command
ggplot(mtcars, aes(cyl, mpg)) +
  geom_point()

head(mtcars)

# 3 aesthetics: qsec vs. mpg, colored by fcyl
mtcars %>% 
  ggplot(aes(qsec, mpg, color = fcyl)) +
  geom_point()

mtcars_mod <- 
  mtcars %>% 
  mutate(fam = factor(am))

levels(mtcars_mod$fam) <- c("automatic", "manual")

mtcars_mod %>% 
  group_by(carb, fam) %>% 
  summarise(n = n())

# Plot 0 vs. mpg
ggplot(mtcars, aes(x=mpg, y=0)) +
  # Add jitter 
  geom_point(position = 'jitter') + 
  ylim(-1,1)

str(mtcars_mod)

# ---- 

base_decisoes <- readr::read_delim("base_decisoes.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
str(base_decisoes)

base_decisoes <- 
base_decisoes %>% 
  mutate(ano = lubridate::year(data_decisao), .after = data_decisao)

base_decisoes %>% 
  group_by(ano) %>% 
  summarise(n = sum(qtd_decisoes))

rm(base_decisoes)

# ----

link = 'https://assets.datacamp.com/production/repositories/5171/datasets/fd66a8c2408f8cccc24df8ce2668e0e195519532/fish.RData'
link2 = 'https://assets.datacamp.com/production/repositories/5171/datasets/98df1f3a8e599cbabd16ea2a21d8f74c0d02290d/recess.RData'

load(url(link))
load(url(link2))


str(recess)

require(car)

str(Vocab)

rm(recess)

# Plot vocabulary vs. education
ggplot(Vocab, aes(x =education , y = vocabulary)) +
  # Add a point layer
  geom_point()

mtcars



require(gapminder)

str(gapminder)

gm2007_complete <- gapminder %>% 
  filter(year == 2007) %>% 
  select(country, lifeExp, continent)

gm2017 <- sample_n(gm2007_complete, size = 30, replace = F)


require(ggthemes)
