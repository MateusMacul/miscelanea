## ahp mais#####
unidade = "�ndice de Valoriza��o da Terra"
titulo = "AHP mais"
variavel = "ahpmais"
v = dados$ahp
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_ahpmais.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,1,5,1))
choroLayer(spdf = dados, var = variavel, breaks = bks1, col = carto.pal("red.pal",10), border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
## ahp menos
unidade = "�ndice de Valoriza��o da Terra"
titulo = "AHP menos"
variavel = "ahpmenos"
v = dados$ahp
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_ahpmenos.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,1,5,1))
choroLayer(spdf = dados, var = variavel, breaks = bks1, col = carto.pal("red.pal",10), border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
# sensibilidade 25####
unidade = "q3 -q1"
titulo = "Sensibilidade do mapa de terras 25"
variavel = "sense"
v = dados.ahp$sense
choroLayer(spdf = dados.ahp, var = variavel, method = metodo, nclass = classes, col = rev(carto.pal("red.pal",10)), border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 8)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = carto.pal("red.pal",10), x,xlab = unidade, ylim = c(0,2000),ylab = "Frequ�ncia", main = titulo, freq = T)

plot(incremento, add = T)

# Vari�vel dependente ####
unidade = "incremento/floresta"
titulo = "Vari�vel dependente"
variavel = "y1"
v = dados$y1
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, frame = T, theme = tema)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)

plot(incremento, add = T)
# Vari�vel dependente alterada####
unidade = "incremento/floresta"
titulo = "Vari�vel dependente alterada"
variavel = "i_y1"
v = dados$i_y1
choroLayer(spdf = dados, var = variavel, breaks = c(0,0.0001, 0.03, 0.12, 0.21, 0.33, 0.5, 0.74, 1.04, 1.42, 1.82, 2), col = carto.pal("blue.pal",1,"orange.pal",10), border = FALSE, legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, frame = T, theme = tema)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)

plot(v,dados$y1, xlab = titulo, ylab = "Vari�vel dependente (incremente/floresta)")


# Dist BR 163 ####
unidade = "metros"
titulo = "Dist�ncia das C�lulas a BR-163"
variavel = "dis_mn_br"
v = dados$dis_mn_br
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

br163 = readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014/estradas_22765_2014_BR163.shp")
plot(br163, add = T)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)

# Estradas vicinais ####
unidade = "Metros"
titulo = "Dist�ncia das C�lulas a Estradas Vicinais"
variavel = "dis_mn_est"
v = dados$dis_mn_est
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

estradas = readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014/estradas_22765_2014.shp")
vicinais = estradas[c(6:length(estradas)),]
plot(vicinais, add = T)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
# Area Urbana ####
unidade = "Metros"
titulo = "Dist�ncia das C�lulas a �rea Urbana"
variavel = "dis_mn_au"
v = dados$dis_mn_au
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)

# �rea dos imoveis ####
unidade = "km²"
titulo = "Média da �rea dos Im�veis nas C�lulas"
variavel = "areakm_mea"
v = dados$areakm_mea
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
# Declividade####
unidade = "%"
titulo = "Mediana da Declividade nas C�lulas"
variavel = "med_decliv"
v = dados$med_decliv
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
# RIOS ####
unidade = "Metros"
titulo = "Mediana da Dist�ncia das C�lulas a Rios"
variavel = "dis_mn_r5"
v = dados$dis_mn_r5
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
# Assentamento ####
unidade = "Propor��o"
titulo = "Propor��o de Assentamento nas C�lulas"
variavel = "area_ass"
v = dados$area_ass
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
# Embargo ####
unidade = "Porcentagem"
titulo = "Porcentagem de Embargo nas C�lulas"
variavel = "area_embar"
v = dados$area_embar
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
# Floresta ####
unidade = "Propor��o"
titulo = "Propor��o de Floresta nas C�lulas"
variavel = "area_flore"
v = dados$area_flore
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)

# UC ####
unidade = "Propor��o"
titulo = "Propor��o de Unidades de Conserva��o nas C�lulas"
variavel = "area_uc"
v = dados$area_uc
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)

# TI ####
unidade = "Propor��o"
titulo = "Propor��o de Terra Ind�gena nas C�lulas"
variavel = "area_ti"
v = dados$area_ti
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)

# M�dulos Fiscais ####
unidade = "Módulos Fiscais"
titulo = "N� M�dulos Fiscais nas C�lulas"
variavel = "modulos_imv"
v = dados$modulos_imv
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = T, tabtitle = T)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Densidade", main = titulo)

# Módulos Fiscais Transformada ####
unidade = "�ndice"
titulo = "�ndice de M�dulos Fiscais nas C�lulas"
variavel = "class_imv"
v = dados$class_imv
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = T, tabtitle = T)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Densidade", main = titulo)

plot(v,dados$modulos_imv, xlab = titulo, ylab = "N� M�dulos Fiscais nas C�lulas")
# Assentamento Distancia ####
unidade = "Metros"
titulo = "Dist�ncia das C�lulas a Assentamentos"
variavel = "dis_mn_ass"
v = dados$dis_mn_ass
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = T, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�cia", main = titulo, freq = T)
# UC Dist ####
unidade = "Metros"
titulo = "Dist�ncia das C�lulas a Unidades de Conserva��o"
variavel = "dis_mn_uc"
v = dados$dis_mn_uc
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2,  legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
# Degrada��o Dist ####
unidade = "Metros"
titulo = "Dist�ncia das C�lulas a Ind�cios de Degrada��o"
variavel = "dis_mn_deg"
v = nozero$dis_mn_deg
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
abline(v = quantile(v, probs  = 0.75))
# Degrada��o ####
unidade = "Propor��o"
titulo = "Propor��o de Ind�cios de Degrada��o nas C�lulas"
variavel = "area_deg"
v = dados$area_deg
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
# Imoveis Certificados Dist ####
unidade = "Metros"
titulo = "Dist�ncia das C�lulas a Im�veis Certificados"
variavel = "dis_mn_crt"
v = dados$dis_mn_crt
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
abline(v = quantile(nozero$dis_mn_crt, probs= 0.75))
# Imoveis Conflituosos Dist ####
unidade = "Metros"
titulo = "Dist�ncia das C�lulas a Im�veis Conflituosos"
variavel = "dis_mn_cnf"
v = dados$dis_mn_cnf
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
abline(v = 20000)
# Imoveis Conflituosos ####
unidade = "Propor��o"
titulo = "Propor��o de Im�veis Conflituosos nas C�lulas"
variavel = "area_confl"
v = dados$area_confl
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
hist(nozero$area_confl, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
# Distancia de �rea desmatameta at� 2013 -----
unidade = "Metros"
titulo = "Dist�ncia das C�lulas a �rea Desmatada at� 2013"
variavel = "dis_mn_d13"
v = dados$dis_mn_d13
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
abline(v = 10000)
hist(nozero$dis_mn_d13, col = pal, xlab = unidade, ylab = "Frequ�ncia", main = titulo, freq = T)
abline(v = quantile(nozero$dis_mn_d13, probs = 0.75))
