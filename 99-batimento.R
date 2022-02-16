# Dados -------

head(iris)

# Vamos retirar a coluna 5 que é quantitativa.
df <- iris[,-5]
#df <- prcomp(df.scaled)


# Rodar ACP com PRcomp ------

#df.pca <- prcomp(df.scaled, scale = TRUE); df.pca #original
df.pca <- prcomp(df.scaled, scale. = TRUE); df.pca

# Podemos normalizar nosso dataframe. A função [`scale(x)`](http://www.leg.ufpr.br/~walmes/cursoR/guia_rapido_R.pdf) padroniza um vetor/matriz subtraindo os valores de sua média e dividindo pelo desvio padrão.

#df.scaled <- scale(df, center = TRUE, scale = TRUE)

# Componentes
e.prcomp <- df.pca$rotation; e.prcomp

# Autovalores
lambda.prcomp <- df.pca$sdev; lambda.prcomp

# Calculo da variancia explicada por cada componente no PRCOMP
var.prcomp <- df.pca$sdev^2 / sum(df.pca$sdev^2); var.prcomp


# Rodar ACP manualmente -----
res.cor <- cor(df)

# Autovetores e autovetores
res.eig <- eigen(res.cor)

# Autovetor
e.manual = res.eig$vectors; e.manual

# Autovalor
lambda.manual = res.eig$values; lambda.manual

# Variancia total
var.manual <- res.eig$values/sum(res.eig$values); var.manual

# Obterndo a matriz de Loadings
k = 2
# Obtendo L
L.hat = t(t(res.eig$vectors[,1:2]) * sqrt(res.eig$values[1:2])); L.hat

# Criando a matriz de lambdas
D <- matrix(0, k, k)
diag(D) <- res.eig$values[1:2]

# Criando a matriz de loadings
L.hat2 = as.matrix(res.eig$vectors[,1:2]) %*% sqrt(D); L.hat2

# Criando as comunalidades
h2 <- rowSums(L^2); h2

# Criando uniques
#S <- cov(df)
#u2 <- diag(S) - h2
u2 <- 1 - h2



# Rodar Analise Fatorial
afe <- psych::principal(df, rotate = 'none', nfactors = 2, covar = TRUE); afe

# Comparação
afe$loadings
round(L.hat,2)
round(var.manual,2)


names(afe)

afe$values

round(afe$residual,6)

# O que são esses pesos
afe$weights

#The Weight by PCA operator generates attribute weights of the given ExampleSet using a component created by the PCA. The component is specified by the component number parameter. If the normalize weights parameter is not set to true, exact values of the selected component are used as attribute weights. The normalize weights parameter is usually set to true to spread the weights between 0 and 1. The attribute weights reflect the relevance of the attributes with respect to the class attribute. The higher the weight of an attribute, the more relevant it is considered.

#---




# Agora precisamos obter a matriz $\Psi$.
# 
# 
# Vamos obter a matriz L L'
# 
# ```{r}
# L.hat %*% t(L.hat)
# ```
# 
# Vamos obter a matriz R
# 
# 
# Matriz R
# ```{r}
# e1 = res.eig$vectors[,1]
# e2 = res.eig$vectors[,2]
# e3 = res.eig$vectors[,3]
# e4 = res.eig$vectors[,4]
# 
# v1 <- e1 %*% t(e1) * res.eig$values[1]
# v2 <- e2 %*% t(e2) * res.eig$values[2]
# v3 <- e3 %*% t(e3) * res.eig$values[3]
# v4 <- e4 %*% t(e4) * res.eig$values[4]
# 
# Rpp = v1+ v2 + v3 + v4
# ```
# 
# 
# 
# 
# ```{r}
# #Calculate eigenvectors/eigenvalues
# res.eig <- eigen(res.cor)
# res.eig
# ```
# 
# Perceba que é igual ao gerado pelo PRCOMP(), exceto que acima é mostrado a a variância a partir dos autovalores e embaixo a variância deles.
# 
# ```{r}
# df.pca
# ```
# 
# Para obter os loadings, precisamos utilizar $\hat{L_{pm}} = [\sqrt(\hat{\lambda_1})\hat{e1} + \sqrt(\hat{\lambda_2})\hat{e2} + ... + \sqrt(\hat{\lambda_m})\hat{em}]$.
# 
# ```{r}
# 
# desv.values <- sqrt(res.eig$values)
# desv.values.matrix <- matrix(NA, 4,4)
# 
# for (i in 1:4){
#   desv.values.matrix[i,] <- desv.values 
# }
# 
# desv.values.matrix
# ```
# 
# 
# Obtendo a matriz L
# 
# ```{r}
# # Obtendo L
# L = t(t(res.eig$vectors) * sqrt(res.eig$values))
# L
# ```
# 
# Considerando K = 2, seria:
# 
# ```{r}
# k = 2
# # Obtendo L
# L.hat = t(t(res.eig$vectors[,1:2]) * sqrt(res.eig$values[1:2]))
# L.hat
# ```
# OU
# 
# ```{r}
# # Criando a matriz de lambdas
# D <- matrix(0, k, k)
# diag(D) <- res.eig$values[1:2]
# 
# # Criando a matriz de loadings
# L = as.matrix(res.eig$vectors[,1:2]) %*% sqrt(D)
# L
# ```
# 
# 
# 
# Vamos obter o MRES:
# ```{r}
# # MRES = Rpp - L.hat * t(L.hat)
# MRES = Rpp - L.hat %*% t(L.hat)
# round(MRES,3)
# ```
# 
# Vamos obter as comunalidades
# ```{r}
# h2 <- rowSums(L^2); h2
# ```
# 
# Vamos obter u
# ```{r}
# S <- cov(df.scaled)
# u2 <- diag(S) - h2
# u2
# ```
# 


# ---

fa.none <- psych::fa(df, nfactors = 2, rotate = "none")
fa.varimax <- psych::fa(df, nfactors = 2, rotate = "varimax")
fa.promax <- psych::fa(df, nfactors = 2, rotate = "promax")

par(mfrow = c(1,3))
plot(fa.none$loadings[,1], 
     fa.none$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)

plot(fa.varimax$loadings[,1], 
     fa.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")

text(fa.varimax$loadings[,1]-0.08, 
     fa.varimax$loadings[,2]+0.08,
     colnames(food),
     col="blue")
abline(h = 0, v = 0)

plot(fa.promax$loadings[,1], 
     fa.promax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax rotation")
abline(h = 0, v = 0)

