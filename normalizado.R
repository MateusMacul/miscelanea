
colunas_selecionar <- c(indices[grep(c(paste(c("med_decliv", "area_confl", "area_deg", "area_tlsgf", "area_ass", 
                                               "area_uc", "area_ti", "area_embar", "area_abert", "area_vegse", 
                                               "area_retir", "incre_1417", "flor_14_p", "areakm_mea", "num_imov",
                                               "areakm_med", "dis_mn_au", "dis_mn_br", "dis_mn_est", "dis_mn_r5",
                                               "flor_17_p", "dis_mn_ass", "dis_mn_cnf", "dis_mn_ti", "dis_mn_deg",
                                               "dis_mn_crt", "dis_mn_uc", "flor_13_p", "ahp"),collapse = "|")),rownames(indices), ignore.case = T)])
# aplicar subset -------------------------------------------------------
dados.amost4.maior = subset(dados.amost4, dados.amost4$incre_1417>= 0.2)
# Selecionar as colunas ------------------------------------------------
selecionados = dados.amost4[,colunas_selecionar]
selecionados = dados.amost4.maior[,colunas_selecionar]
selecionados = nozero[,colunas_selecionar]
selecionados = nozero.maior02[,colunas_selecionar]

# Construindo a matriz com os dados normalizados ------------------------
normalizados = data.frame(matrix(ncol = length(names(selecionados@data))))
colnames(normalizados) <-(names(selecionados@data))
View(normalizados)

for (g in 1:ncol(selecionados)){
  for(i in 1:length(selecionados)){
    normalizados[i,g] = (selecionados@data[i,g] - min(selecionados@data[,g]))/diff(range(selecionados@data[,g]))}
}
# plotando o boxplot dos dados ---------------------------------------------
boxplot(normalizados, las = 2, main = "Células com Incremento Total >= 0.2", col = "blue")
boxplot(amostra@data[,-c(1:16,44:45)], las = 2, main = "amostras", col = "red")
str(amostra@data)
ncol(amostra@data)
#plotando mapa das selcionadas ---------------------------------------------

selecionados.mapa = dados[,c(inic:fim,93)]
for(i in 1:ncol(selecionados.mapa)){
  jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/selecionados/mapa_",paste0(names(selecionados.mapa)[i]),".jpeg"))
  titulo = paste0(names(selecionados.mapa[i]))
  variavel = paste0(names(selecionados.mapa[i]))
  choroLayer(spdf = selecionados.mapa, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.pos = "topleft",legend.values.rnd = 2)
  layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
  plot(incremento, add = T)
  dev.off()
}
