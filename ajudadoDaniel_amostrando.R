require(dplyr)

var1 <- rnorm(n = 100, mean = 10, sd = 1)
var2 <- rnorm(n = 100, mean = 10, sd = 1)
var3 <- rnorm(n = 100, mean = 10, sd = 1)

df <- data.frame(var1,var2,var3) %>%
  mutate(quantile = ntile(var1, 4))

#agora tem uma coluna com o quartil
df 

quart1 <- filter(df, quantile == 1)
quart2 <- filter(df, quantile == 2)
quart3 <- filter(df, quantile == 3)
quart4 <- filter(df, quantile == 4)

#para amostrar o quartil (e.x. 70%)

amostra.70.quart1 <- quart1[sample(x = nrow(quart1), 
                                   size = nrow(quart1)*0.7,
                                   replace = F),]

amostra.70.quart2 <- quart2[sample(x = nrow(quart2), 
                                   size = nrow(quart2)*0.7,
                                   replace = F),]

amostra.70.quart3 <- quart3[sample(x = nrow(quart3), 
                                   size = nrow(quart3)*0.7,
                                   replace = F),]
amostra.70.quart4 <- quart4[sample(x = nrow(quart4), 
                                   size = nrow(quart4)*0.7,
                                   replace = F),]
