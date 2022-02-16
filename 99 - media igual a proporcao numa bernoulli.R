# Geração da amostra
possibilidades <- c(0,1)
amostra <- sample(possibilidades, 1000, replace = T)

sum_p <- sum(amostra); n <- length(amostra); 
p <- sum_p/n; p
q <- 1 - p

# Bernouli
# Média
p
# Variancia
p*q

# Estatísticas
# média
mean(amostra) 
# variancia
varp <- function(x) mean((x-mean(x))^2)
varp(amostra)
