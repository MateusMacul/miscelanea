library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)


#tentativa de fazer uma regressão com as principais componentes
pcat<-prcomp(as.data.frame(dados[c(4:22,24:31,36:65)]),center = T, scale = T)#todas as celulas
pcaag<-prcomp(agrupada[c(4:22,24:31,36:65,67:68)],center = T, scale = T) #amostras nozero agrupadas
pca<-prcomp(as.data.frame(dados.amost4[c(4:22,24:31,36:65)]),center = T, scale = T) #só as amostradas
pcax <- as.data.frame(pca$x)
pcax$incre_1417 <- dados.amost4$incre_1417

# plotando a PCA
ggbiplot(pca)
ggbiplot(pca, labels = dados.amost4$id)
ggbiplot(pcat, labels = dados.amost4$id)
ggbiplot(pcat, labels = dados$id, groups = dados$incre_1417)
ggbiplot(pcaag, groups = agrupada$`ntile(nozero.df$y1, 6)`)
ggbiplot(pcaag, groups = agrupada$`ntile(nozero.df$y1, 6)`, ellipse = T, obs.scale = 1, var.scale = 1)
ggbiplot(pcaag, groups = agrupada$`ntile(nozero.df$y1, 6)`, ellipse = T, choices = c(1,32)) #ecolhendo qual PC plotar, no caso, a única q entrou no modelo pelo stepwise


# rodando o modelo com as PCs
fmla_p <- as.formula(paste("incre_1417 ~ ", paste(c(names(pcax)[1:57]), collapse= "+")))
reg_p = lm(fmla_p, data = pcax)
stepRegression_p = step (reg_p, scale = 1, direction= "both")
summary(stepRegression_p)
summary(reg_p)

