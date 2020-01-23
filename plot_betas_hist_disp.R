
# histograma dos betas ----------------------------------------------------
# inc-d
beta_inc_d = -38.35 + (12.75*amostra$i_emb)+ (30.08*amostra$i_vegse)
h_inc_d <- hist(beta_inc_d, plot = F)
h_inc_d$counts=h_inc_d$counts/sum(h_inc_d$counts)
png("C:/Users/Padrao/OneDrive - inpe.br/graficos/histograma_beta_INC-d.png", width = f_dim[1], height = f_dim[2])
par(mar = c(5,5,5,5))
plot(h_inc_d,xlab = "beta para INC-d",ylab = "Frequência relativa",
     main = "Histograma do beta para INC-d", freq = T,  cex.lab=2.2, 
     cex.axis=2.2, cex.main=2.2)
dev.off()

# MPAR
beta_MPAR = -43.11+(70.78*amostra$i_decliv)
h_MPAR <- hist(beta_MPAR, plot = F)
h_MPAR$counts=h_MPAR$counts/sum(h_MPAR$counts)
png("C:/Users/Padrao/OneDrive - inpe.br/graficos/histograma_beta_MPAR.png", width = f_dim[1], height = f_dim[2])
par(mar = c(5,5,5,5))
plot(h_MPAR,xlab = "beta para MPAR",ylab = "Frequência relativa",
     main = "Histograma do beta para MPAR", freq = T,  cex.lab=2.2, 
     cex.axis=2.2, cex.main=2.2)
dev.off()

# D13-d
beta_d13_d = 6.163 - (2.898*amostra$i_cert_d) - (5.538*amostra$i_rios5) - (0.00001069*amostra$TAOBIA_0)
h_d13_d <- hist(beta_d13_d, plot = F)
h_d13_d$counts=h_d13_d$counts/sum(h_d13_d$counts)
png("C:/Users/Padrao/OneDrive - inpe.br/graficos/histograma_beta_D13-d.png", width = f_dim[1], height = f_dim[2])
par(mar = c(5,5,5,5))
plot(h_d13_d,xlab = "beta para D13-d",ylab = "Frequência relativa",
     main = "Histograma do beta para D13-d", freq = T,  cex.lab=2.2, 
     cex.axis=2.2, cex.main=2.2)
dev.off()


# dispersão das variáveis nos betas ---------------------------------------
plot(amostra$i_emb, amostra$i_vegse)
# i_emb x i_vegse
png(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/disp_emb_vegse.png"))
ggplot(data = amostra@data, aes(y =i_emb , x =i_vegse))+
  geom_point()+
  ylab("EMB-p_inv")+
  xlab("VS-p_inv")+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=18,face="bold"))
dev.off()
# i_cert_d x i_rio5
png(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/disp_cert-d_rio5.png"))
ggplot(data = amostra@data, aes(y =i_cert_d , x =i_rios5))+
  geom_point()+
  ylab("CRT-d")+
  xlab("RIO-d_inv")+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=18,face="bold"))
dev.off()
# i_cert_d x TAOBIA_0
png(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/disp_cert-d_TAOBIA.png"))
ggplot(data = amostra@data, aes(y =i_cert_d , x =TAOBIA_0))+
  geom_point()+
  ylab("CRT-d")+
  xlab("TAOBIA")+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=18,face="bold"))
dev.off()
# i_rio5 x TAOBIA_0
png(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/disp_rio5_TAOBIA.png"))
ggplot(data = amostra@data, aes(y =i_rios5 , x =TAOBIA_0))+
  geom_point()+
  ylab("RIO-d_inv")+
  xlab("TAOBIA")+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=18,face="bold"))
dev.off()


