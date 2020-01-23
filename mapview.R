library("mapview")
### explorando o mapview ####
incremento = readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014/Prodes_2014/PDigital2017_PA__pol_fix_WGS84_21s_fix_clip_dissolve_arrumado_fix_incre1417.shp")
desmatamento13 = readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014/Prodes_2014/PDigital2017_PA__pol_fix_WGS84_21s_fix_clip_dissolve_arrumado_fix_ate_d2013_fix_diss.shp")
incremento$areakm = incremento$areameters/1000000
sub.incremento = subset(incremento,incremento$areakm>=3)
incremento.cortado = readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014/Prodes_2014/Intersect_incre1417_celulas.shp")


mapview(incremento, col.regions = "black", map.types = "OpenTopoMap")+ mapview(sub.incremento) 

mapview(dados, col.regions = "black", map.types = "OpenTopoMap")+ mapview(dados.dif_incre,zcol="dif_incre")
mapview(dados,zcol = "ahp")
mapview(dados,zcol = "i_aberta")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "i_vegse")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "i_ti_d")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "i_uc_d")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "dis_mn_deg")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "i_deg_d")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "dis_mn_crt")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "i_cert_d")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "dis_mn_cnf")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "i_conf_d")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "i_conf_p")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "dis_mn_d13")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados,zcol = "i_d13_d")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados.amost4,zcol = "abs_erros")
mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados.amost5,zcol = "erros", 
                                                                                col.regions = colorRampPalette(
                                                                                  colors = c("red2","white", "yellow")))

mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(dados.amost5,zcol = "fitted", 
                                                                                col.regions = colorRampPalette(
                                                                                  colors = c("red2","white", "yellow")))


mapview(incremento.cortado, zcol = "prop", col.regions = colorRampPalette(colors = c("black","grey","white")), map.types = "OpenTopoMap") + 
  mapview(dados.amost4,zcol = "erros", col.regions = colorRampPalette(colors = c("red2","white", "yellow")))


mapviewColors(x, zcol = NULL,
              colors = mapviewGetOption("vector.palette"), at = NULL,
              na.color = mapviewGetOption("na.color"))
mapviewPalette(name = "mapviewVectorColors")

mapViewPalette(name)
