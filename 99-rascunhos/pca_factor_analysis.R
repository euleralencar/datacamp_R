# Download data -------------------------------------------------
link = 'https://assets.datacamp.com/production/repositories/2136/datasets/869615371e66021e97829feb7e19e38037ed0c14/GCBS_data.rds'
source('01-funcoes/f_importar_rds.R')
gcbs <- importar_rds(link)

# Vamos restringir o tamanho dos dados
gcbs_simp <- gcbs[1:10,1:3]

# Calculate the correlation matrix first 
bfi_EFA_cor <- cor(gcbs_simp, use = 'pairwise.complete.obs')

# Then use that correlation matrix to create the scree plot
scree(bfi_EFA_cor, factors = TRUE)

# Run the EFA with six factors (as indicated by your scree plot)
EFA_model <- fa(gcbs_simp, nfactors = 3)

names(EFA_model)

EFA_model$loadings

EFA_model$communalities

cargas_fatoriais_factor <- EFA_model$loadings

# ---- PCA

EFA_pca <- prcomp(na.omit(gcbs_simp), scale = TRUE)

eigenvalue = EFA_pca$sdev^2

#Extraindo as Cargas Fatoriais
k <- sum((EFA_pca$sdev ^ 2) > 1)
k

cargas_fatoriais_acp <- EFA_pca$rotation[, 1:k] %*% diag(EFA_pca$sdev[1:k])

# Visualizando as cargas fatoriais
data.frame(cargas_fatoriais_acp) %>%
  # rename(F1 = X1,
  #        F2 = X2) %>%
  knitr::kable() %>%
  kableExtra::kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Visualizando as Comunalidades
data.frame(rowSums(cargas_fatoriais_acp ^ 2)) %>%
  rename(comunalidades = 1) %>%
  knitr::kable() %>%
  kableExtra::kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Scores Fatoriais
scores_fatoriais <- t(EFA_pca$rotation)/EFA_pca$sdev 
colnames(scores_fatoriais) <- colnames(gcbs_simp)

scores_fatoriais

scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2) %>%
  #select(PC1, PC2) %>%
  knitr::kable() %>%
  kableExtra::kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Calculando a variância compartilhada
var_compartilhada <- (EFA_pca$sdev ^ 2/sum(EFA_pca$sdev ^ 2))
var_compartilhada

scores_fatoriais

#Calcula score fatorial
score_D <- scores_fatoriais[var,]

# ------

EFA_pca <- prcomp(na.omit(bfi_EFA))
names(EFA_pca)

# Sumarizando resultado:
data.frame(eigenvalue = EFA_pca$sdev^2,
           var_compartilhada = summary(EFA_pca)$importance[2,],
           var_cumulativa = summary(EFA_pca)$importance[3,]) -> relatorio

var_compartilhada = summary(EFA_pca)$importance[2,] # OU
var_compartilhada = (EFA_pca$sdev ^ 2/sum(EFA_pca$sdev ^ 2))



# relatorio %>% 
#   kable() %>%
#   kable_styling(bootstrap_options = "striped", 
#                 full_width = T, 
#                 font_size = 12)

EFA_pca$scores
EFA_pca$sdev
EFA_pca$loadings

prcomp(bfi_EFA[,1:15])
summary(prcomp)