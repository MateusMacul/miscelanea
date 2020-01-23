########## exportando os dados para csv ######

export.tudo = as.data.frame(dados.amost5@data)
export.tudo = as.data.frame(dados.amost2[,c("id","area_abert","area_vegse","dis_mn_br","dis_mn_est",
                                           "dis_mn_au","areakm_mea","area_tlsgf","med_decliv","dis_mn_r5",
                                           "area_ass","area_embar","area_uc","area_ti", "ass_dist",
                                           "ti_dist","uc_dist","deg_dist","area_deg","tl_sgf_dis","confl_dis",
                                           "area_confl","areakm_tot","incre_1417","incre_14","incre_15","incre_16",
                                           "incre_17", "flor_14_p","flor_17_p","i_aberta",
                                           "i_vegse","i_br","i_est","i_au","i_frag","i_cert","i_decliv",
                                           "i_rios5","i_ass_p","i_emb","i_flor","i_uc","i_ti","ahp","i_ass_d",
                                           "i_ti_d","i_uc_d","i_deg_d","i_deg_p","i_cert_d","i_conf_d","i_conf_p")])

export.i = as.data.frame(dados.amost[,c("id","area_incre","area_flore","y1","i_aberta",
                                           "i_vegse","i_br","i_est","i_au","i_frag","i_cert","i_decliv",
                                           "i_rios3","i_ass_p","i_emb","i_flor","i_uc","i_ti","ahp","i_ass_d",
                                           "i_ti_d","i_uc_d","i_deg_d","i_deg_p","i_cert_d","i_conf_d","i_conf_p")])

write.csv(export.tudo,file = "C:/Users/Padrao/OneDrive - inpe.br/Tabelas/dados_amostrados_arrumados.csv")
write.csv(export.i,file = "C:/Users/Padrao/OneDrive - inpe.br/Tabelas/dados_amostrados_indices.csv")
