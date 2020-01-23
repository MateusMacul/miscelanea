# -*- coding: utf-8 -*-
"""
Created on Thu Dec 13 19:24:58 2018

@author: Mateus
"""
#script para plotar utilizando o geopandas e matplotlib

import geopandas as gpd
import matplotlib.pyplot as plt
#abrindo os shapes
assentamento = gpd.read_file("Assentamentos_Oficial_Clip_wgs84_21s.shp")
area_estudo = gpd.read_file("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/227_65_wgs84_21s_prodes.shp")
#Verificando se estão no mesmo sistema de referencia de coordenadas
assentamento.crs
area_estudo.crs
#criando o plot
fig, ax = plt.subplots()
ax.set_aspect('equal')
area_estudo.plot(ax =ax , color = "white", edgecolor = "red")
assentamento.plot(ax=ax)
plt.show()