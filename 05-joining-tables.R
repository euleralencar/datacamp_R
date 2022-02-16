# IMPORT PACKAGES ---------------------------------------------------------


require(tidyverse)


# IMPORTA DATA ------------------------------------------------------------



link1 = 'https://assets.datacamp.com/production/repositories/5284/datasets/cb649926d41ce73490a9bb710e1501a273061723/parts.rds'
link2 = 'https://assets.datacamp.com/production/repositories/5284/datasets/30fc459770c89e46cce9cce99752ca95fb1d06fe/part_categories.rds'
link3 = 'https://assets.datacamp.com/production/repositories/5284/datasets/a49d7bf17fc35fdd1331c01a7f36573800e93cb4/inventory_parts.rds'
link4 = 'https://assets.datacamp.com/production/repositories/5284/datasets/2b509dd7a49493ab990580be1845f21f36c46ca0/inventories.rds'
link5 = 'https://assets.datacamp.com/production/repositories/5284/datasets/2e7cb938873ba685957efd822867c86f46dc6b78/sets.rds'
link6 = 'https://assets.datacamp.com/production/repositories/5284/datasets/aeeda0eaafe6b04c1e42da71a4e9fed7299d096e/colors.rds'
link7 = 'https://assets.datacamp.com/production/repositories/5284/datasets/267bcb026359fb2104bf4b717ae166d0bd99c5e6/themes.rds'


source('01-funcoes/f_importar_rds.R')
parts <- importar_rds(link1)
part_categories <- importar_rds(link2)
inventory_parts <- importar_rds(link3)
inventories <- importar_rds(link4)
sets <- importar_rds(link5)
colors <- importar_rds(link6)
themes <- importar_rds(link7)


# INICIAL JOIN ------------------------------------------------------------


# Combine the parts and inventory_parts tables
parts %>%
  inner_join(inventory_parts, 
             by = c('part_num'))

# An inner_join works the same way with either table in either position.
# Combine the parts and inventory_parts tables

inventory_parts %>%
  inner_join(parts, 
             by = c('part_num'))


# Add the correct verb, table, and joining column
parts %>% 
  inner_join(part_categories, 
             by = c('part_cat_id' = 'id'),
             suffix = c('_part', '_category'))
# observe que como tem os mesmos nomes de variáveis nas duas colunas ele 
# chama nome.x e nome.y para diferenciar a origem do dado


# Joining with a one-to-many relationship


# This two has the same number of data.


# JOIN WITH 3 TABLES ------------------------------------------------------

sets %>%
  # Add inventories using an inner join 
  inner_join(inventories, by = 'set_num') %>%
  # Add inventory_parts using an inner join 
  inner_join(inventory_parts, by = c('id' = 'inventory_id')) %>% 
  # Add color name
  inner_join(colors, by = c('color_id'= 'id'), suffix = c('_set','_color'))


# Count the name color and sort the result

sets %>%
  # Add inventories using an inner join 
  inner_join(inventories, by = 'set_num') %>%
  # Add inventory_parts using an inner join 
  inner_join(inventory_parts, by = c('id' = 'inventory_id')) %>% 
  # Add color name
  inner_join(colors, by = c('color_id'= 'id'), suffix = c('_set','_color')) %>% 
  count(name_color, sort = TRUE)


# LEFT JOIN SCHEMA --------------------------------------------------------

# Create inventory_parts_joined!
inventory_parts_joined <- 
  inventories %>%  
  inner_join(inventory_parts, by = c('id' = 'inventory_id')) %>% 
  select(-id, -version) %>% 
  arrange(desc(quantity))

# Create millennium falcom sets!
millennium_falcon <- inventory_parts_joined %>%
  filter(set_num == "7965-1")
# Create millennium star_destroyer sets!
star_destroyer <- inventory_parts_joined %>%
  filter(set_num == "75190-1")

# Combine the star_destroyer and millennium_falcon tables
millennium_falcon %>%
  left_join(star_destroyer, by = c('part_num', 'color_id'),
             suffix = c('_falcon', '_star_destroyer'))

# Aggregate Millennium Falcon for the total quantity in each color
millennium_falcon_colors <- millennium_falcon %>%
  group_by(color_id) %>%
  summarize(total_quantity = sum(quantity))

# Aggregate Star Destroyer for the total quantity in each part
star_destroyer_colors <- star_destroyer %>%
  group_by(color_id) %>%
  summarize(total_quantity = sum(quantity))

# Left join the Millennium Falcon colors to the Star Destroyer colors
millennium_falcon_colors %>%
  left_join(star_destroyer_colors, by = 'color_id',
            suffix = c('_falcon', '_star_destroyer'))

# --

# Left joins are really great for testing your assumptions about a data set and ensuring your data has integrity.

# For example, the inventories table has a version column, for when a LEGO kit gets some kind of change or upgrade. It would be fair to assume that all sets (which joins well with inventories) would have at least a version 1. But let's test this assumption out in the following exercise.

# Filter version 1 of invetories
inventory_version_1 <- inventories %>%
  filter(version == 1)

# Join versions to sets
# If some data from inventory_version_1 dont have in set will complete with NA.
sets %>%
  left_join(inventory_version_1, by = c('set_num')) %>%
  # Filter for where version is na
  filter(is.na(version))

# you'll learn another way to find observations like this: anti_join

# See using this kind of join!!! 
sets %>%
  anti_join(inventory_version_1, by = c('set_num'))



# RIGHT JOIN SCHEMA - MIRROR OF LEFT JOIN!!!--------------------------------



parts %>%
  # Count the part_cat_id
  count(part_cat_id) %>%
  # Right join part_categories
  right_join(part_categories, by = c('part_cat_id' = 'id')) %>% 
  # Filter for NA
  filter(is.na(n))

# It's important to understand which entries would be impacted by replace_na(), so that we know which entries we would be omitting by using that function.

parts %>%
  count(part_cat_id) %>%
  right_join(part_categories, by = c("part_cat_id" = "id")) %>%
  # Use replace_na to replace missing values in the n column
  replace_na(list(n=0))

# Some examples how replace na works ____________________________->

# Replace NAs in a data frame
d1 <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
d1 %>% replace_na(list(x = 0, y = "unknown"))
# In a list
d1$x %>% replace_na(0)


batman_parts %>%
  # Combine the star_wars_parts table 
  full_join(star_wars_parts, 
            by = c('part_num', 'color_id'),
            suffix = c('_batman','_star_wars')) %>%
  # Replace NAs with 0s in the n_batman and n_star_wars columns 
  replace_na(list(n_batman = 0, n_star_wars = 0))

# _______________________________________________________________->

# JOIN TABLE TO THEMSELVES - HIERARQUICAL TABLE ---------------------------

# Look at the table. We have id, how it is id of name and that name has its parent_id. Imagine you have two tables:

# table 1: id, name
# table 2: name, parent_id

# If I call right(table1 [by id]) and left(table2 [by parentid]), we are link sons (name) with its fathers (parent_id).

# If I call right(table2 [by parent_id]) and left(table2 [by id]), we are link parents with its sons (id).

themes

# Call the first case
themes %>% 
  inner_join(themes, 
             by = c('parent_id' = 'id'), 
             suffix = c('_child', '_parent'))

# Looking for parents of 'The Lord of the Rings'
themes %>% 
  inner_join(themes, 
             by = c('parent_id' = 'id'), 
             suffix = c('_child', '_parent')) %>% 
  filter(name_child == 'The Lord of the Rings')

# Looking for sons of 'The Lord of the Rings'
themes %>% 
  inner_join(themes, 
             by = c('parent_id' = 'id'), 
             suffix = c('_child', '_parent')) %>% 
  filter(name_parent == 'The Lord of the Rings')

# --- Lets try in another way with Harry Potter

# Call the second case
themes %>% 
  # Inner join the themes table
  inner_join(themes, 
             by = c('id' = 'parent_id'), 
             suffix = c('_parent', '_child')) %>%
  # Filter for the "Harry Potter" parent name 
  filter(name_parent == 'Harry Potter')


# Join themes to itself again to find the grandchild relationships
table_with_granchild <- 
themes %>% 
  inner_join(themes, 
             by = c("id" = "parent_id"), 
             suffix = c("_parent", "_child")) %>%
  # We need to link the id_child with id_parent to find grandson!
  inner_join(themes, 
             by = c('id_child' = 'parent_id'), 
             suffix = c('_parent','_grandchild')) %>% 
  rename(name_grandchild = name)


# Left-joining a table to itself
# some themes might not have any children at all, which means they won't be included in the inner join. As you've learned in this chapter, you can identify those with a left_join and a filter()


# 1 option: child -> father
# Here we see sons with your fathers
themes %>% 
  # Left join the themes table to its own children
  left_join(themes, 
            by = c('parent_id' = 'id'), 
            suffix = c('_child', '_parent')) %>%
  # Filter for themes that have no child themes
  filter(is.na(name_child))


# 2 option: fhater -> son
# Here we see fathers with your sons
themes %>% 
  # Left join the themes table to its own children
  left_join(themes, 
            by = c("id" = "parent_id"), 
            suffix = c("_parent", "_child")) %>%
  # Filter for themes that have no child themes
  filter(is.na(name_child))


# FULL JOIN SCHEMA --------------------------------------------------------

# Preparation of data
inventory_parts_joined <- 
  inventories %>%  
  inner_join(inventory_parts, by = c('id' = 'inventory_id')) %>% 
  select(-id, -version) 

# Create batmobile DF
batmobile <- inventory_parts_joined %>% 
  filter(set_num == '7784-1') %>% 
  select(-set_num)

# Create batwin DF
batwing <- inventory_parts_joined %>% 
  filter(set_num == '70916-1') %>% 
  select(-set_num)

#left-join &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
batmobile %>% 
  left_join(batwing, 
            by = c('part_num', 'color_id'),
            suffix = c('_batmobile', '_batwing'))

batmobile %>% nrow() #-> Perfeito

#right-join &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# Há dados repetidos em batmobile que precisam ser agrupados, caso contrário teremos duplicação na base com join -> duplicated(batmobile[,1:2], fromLast = T)
batmobile <- 
batmobile %>% 
  arrange(part_num, color_id) %>% 
  group_by(part_num, color_id) %>% 
  summarise(quantity2 = sum(quantity))

batmobile %>% 
  right_join(batwing, 
            by = c('part_num', 'color_id'),
            suffix = c('_batmobile', '_batwing'))


batwing %>% nrow() #-> Perfeito

#full-join &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
batmobile %>% 
  full_join(batwing, 
            by = c('part_num', 'color_id'),
            suffix = c('_batmobile', '_batwing'))

# dados em comum para tirar prova
inner <- batmobile %>% 
  inner_join(batwing, 
            by = c('part_num', 'color_id'),
            suffix = c('_batmobile', '_batwing')) %>% nrow()

batwin %>% nrow() + batmobile %>% nrow() - inner #-> Perfeito

# -- Exercícios

# Start with inventory_parts_joined table
inventory_parts_joined %>%
  # Combine with the sets table 
  inner_join(sets, c('set_num')) %>%
  # Combine with the themes table 
  inner_join(themes, c('theme_id'='id'), suffix = c('_set','_theme'))


# Create batman and starwars base
inventory_sets_themes <- inventory_parts_joined %>%
  inner_join(sets, by = "set_num") %>%
  inner_join(themes, by = c("theme_id" = "id"), suffix = c("_set", "_theme"))

batman <- inventory_sets_themes %>%
  filter(name_theme == "Batman")

star_wars <- inventory_sets_themes %>%
  filter(name_theme == "Star Wars")

# Batman and starwars Parts
batman_parts <- batman %>%
  count(part_num, color_id, wt = quantity)

star_wars_parts <- star_wars %>%
  count(part_num, color_id, wt = quantity)

# Full join two bases
batman_parts %>%
  # Combine the star_wars_parts table 
  full_join(star_wars_parts, 
            by = c('part_num', 'color_id'),
            suffix = c('_batman','_star_wars')) %>%
  # Replace NAs with 0s in the n_batman and n_star_wars columns 
  replace_na(list(n_batman = 0, n_star_wars = 0))

# ANTI E SEMI-JOIN SCHEMA ------------------------------------------------

# Filter the batwing set for parts that are also in the batmobile set
batwing %>%
  semi_join(batmobile, by = 'part_num')

# Filter the batwing set for parts that aren't in the batmobile set
batwing %>%
  anti_join(batmobile, by = 'part_num')


# Aggregating sets and look their differences

# Data preparation
inventory_parts_themes <- inventories %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  arrange(desc(quantity)) %>%
  select(-id, -version) %>%
  inner_join(sets, by = "set_num") %>%
  inner_join(themes, by = c("theme_id" = "id"), suffix = c("_set", "_theme"))

# Exercise
# How many color in Batman theme set and add a fraction column
batman_colors <- inventory_parts_themes %>%
  # Filter the inventory_parts_themes table for the Batman theme
  filter(name_theme == 'Batman') %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity)) %>%
  # Add a fraction column of the total divided by the sum of the total 
  mutate(fraction = total/sum(total))

# Filter and aggregate the Star Wars set data; add a fraction column
star_wars_colors <- inventory_parts_themes %>%
  filter(name_theme == 'Star Wars') %>% 
  group_by(color_id) %>% 
  summarize(total = sum(quantity)) %>%
  # Add a fraction column of the total divided by the sum of the total 
  mutate(fraction = total/sum(total))


# Join two bases above
batman_colors %>%
  # Join the Batman and Star Wars colors
  full_join(star_wars_colors, by = "color_id", suffix = c("_batman", "_star_wars")) %>%
  # Replace NAs in the total_batman and total_star_wars columns
  replace_na(list(total_batman =0, total_star_wars = 0)) %>%
  inner_join(colors, by = c("color_id" = "id")) 

# Join and Include difference and total variables
batman_colors %>%
  full_join(star_wars_colors, by = "color_id", suffix = c("_batman", "_star_wars")) %>%
  replace_na(list(total_batman = 0, total_star_wars = 0)) %>%
  inner_join(colors, by = c("color_id" = "id")) %>%
  # Create the difference and total columns
  mutate(difference = fraction_batman - fraction_star_wars,
         total = total_batman + total_star_wars) %>%
  # Filter for totals greater than 200
  filter(total > 200)

# Making a ggplot
colors_joined <- batman_colors %>%
  full_join(star_wars_colors, by = "color_id", suffix = c("_batman", "_star_wars")) %>%
  replace_na(list(total_batman = 0, total_star_wars = 0)) %>%
  inner_join(colors, by = c("color_id" = "id")) %>%
  mutate(difference = fraction_batman - fraction_star_wars,
         total = total_batman + total_star_wars) %>%
  filter(total >= 200) %>%
  mutate(name = fct_reorder(name, difference)) 

# Create a pallet
color_palette <- setNames(colors_joined$rgb, colors_joined$name)

# Create a bar plot using colors_joined and the name and difference columns
colors_joined %>% 
ggplot(aes(x = name, y = difference, fill = name)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = color_palette, guide = FALSE) +
  labs(y = "Difference: Batman - Star Wars")


# FINAL PROJECT -----------------------------------------------------------

linka <- 'https://assets.datacamp.com/production/repositories/5284/datasets/89d5a716b4f41dbe4fcda1a7a1190f24f58f0e47/questions.rds'
linkb <- 'https://assets.datacamp.com/production/repositories/5284/datasets/207c31b235786e73496fd7e58e416779911a9d98/tags.rds'
linkc <- 'https://assets.datacamp.com/production/repositories/5284/datasets/966938d665c69bffd87393b345ea2837a94bab97/question_tags.rds'
linkd <- 'https://assets.datacamp.com/production/repositories/5284/datasets/6cb9c039aa8326d98de37afefa32e1c458764638/answers.rds'
  
source('01-funcoes/f_importar_rds.R')
questions <- importar_rds(linka)
tags <- importar_rds(linkb)
question_tags <- importar_rds(linkc)
answers <- importar_rds(linkd)

# Join the questions and question_tags tables
questions_with_tags <- 
questions %>%
  left_join(question_tags, by = c('id' = 'question_id')) %>% 
  left_join(tags, by = c('tag_id' = 'id')) %>% 
  replace_na(list(tag_name = 'only-r')) 

# Comparing scores across tags

questions_with_tags %>%
  # Group by tag_name
  group_by(tag_name) %>%
  # Get mean score and num_questions
  summarize(score = mean(score),
            num_questions = n()) %>%
  # Sort num_questions in descending order
  arrange(desc(num_questions))

#questions_with_tags %>% group_by(score) %>% summarise(n = n()) %>% arrange(desc(score))


# Using a join, filter for tags that are never on an R question
tags %>%
  anti_join(question_tags, by = c('id'='tag_id')) %>% 
  summarise(total.count = n())


#Finding gaps between questions and answers
questions %>%
  # Inner join questions and answers with proper suffixes
  inner_join(answers, 
             by =c('id' = 'question_id'),
             suffix = c('_question', '_answer')) %>% 
  # Subtract creation_date_question from creation_date_answer to create gap
  mutate(gap = as.integer(creation_date_answer - creation_date_question))

# Joining question and answer counts
# Count and sort the question id column in the answers table
answer_counts <- 
  answers %>% 
  count(question_id) %>% 
  arrange(desc(n))
  
# Combine the answer_counts and questions tables
question_answer_counts <- 
  questions %>%
  left_join(answer_counts, by = c('id' = 'question_id')) %>% 
  # Replace the NAs in the n column
  replace_na(list(n = 0))


# Joining questions, answers, and tags
tagged_answers <- 
  question_answer_counts %>%
  # Join the question_tags tables
  inner_join(question_tags, by = c('id' = 'question_id')) %>% 
  # Join the tags table
  inner_join(tags, by = c('tag_id'= 'id'))


# Average answers by question
tagged_answers %>%
  # Aggregate by tag_name
  group_by(tag_name) %>% 
  # Summarize questions and average_answers
  summarize(questions = sum(n),
            average_answers = mean(n)) %>%
  # Sort the questions in descending order
  arrange(desc(questions))


# Inner join the question_tags and tags tables with the questions table
questions_with_tags <- 
questions %>%
  inner_join(question_tags, by = c('id'='question_id')) %>%
  inner_join(tags, by = c('tag_id'='id'))

# Inner join the question_tags and tags tables with the answers table
answers_with_tags <- 
answers %>%
  inner_join(question_tags, by = 'question_id') %>%
  inner_join(tags, by = c('tag_id'='id'))

# Combine the two tables into posts_with_tags
posts_with_tags <- 
  bind_rows(
    questions_with_tags %>% 
      mutate(type = "question"),
    answers_with_tags %>% 
      mutate(type = "answer")
    )

# Add a year column, then count by type, year, and tag_name
by_type_year_tag  <- 
posts_with_tags %>%
  mutate(year = lubridate::year(creation_date)) %>% 
  count(type, year, tag_name)

# Filter for the 'dplyr' and 'ggplot2' tag names 
by_type_year_tag_filtered <- by_type_year_tag %>%
  filter(tag_name %in% c('dplyr', 'ggplot2'))

# Create a line plot faceted by the tag name 
by_type_year_tag_filtered %>% 
  ggplot(aes(x = year, y = n, color = type)) +
  geom_line() +
  facet_wrap(~ tag_name)
