# -*- coding: utf-8 -*-
"""
Spyder Editor

Este é um arquivo de script temporário.
"""
import sys

try:
    from osgeo import gdal, ogr, osr
except:
    sys.exit("ERRO: módulo GDAL/OGR não encontrado!")

# importar constantes
from gdalconst import *

# informar o uso de exceções
gdal.UseExceptions()

# versão da biblioteca GDAL/OGR
print(gdal.VersionInfo("VERSION"))
from rasterio import features
import numpy as np
import geopandas as gpd
import os
import glob
import matplotlib.pyplot as plt
#from shapely.geometry import shape
#from shapely.geometry import mapping

#definindo o diretorio
#caminho para o pc no inpe: caminho_diretorio = "C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014/rasterizacao"

#caminho no meu pc pessoal: 
caminho_diretorio = "D:/OneDrive - inpe.br/Banco de dados/2014/rasterizacao"
print (caminho_diretorio)
os.chdir(caminho_diretorio) #mundando o diretorio para a pasta com os arquivos e onde os arquivos serão salvos

#Abrindo o arquivo da área de estudo para extrair o bounding box
#caminho pc inpe: area_estudo = "C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/227_65_wgs84_21s_prodes.shp"
#caminho pc pesssoal
area_estudo = "D:/OneDrive - inpe.br/Banco de dados/227_65_wgs84_21s_prodes.shp"
datasource_estudo = ogr.Open(area_estudo)
layer_estudo = datasource_estudo.GetLayer()

#definindo a resolução do raster com base no arquivo da área de estudo

pixel_size = 30
NoData_value = -9999
x_min, x_max, y_min, y_max = layer_estudo.GetExtent()
print ("x_min = {}, x_max = {}, y_min = {}, y_max = {}".format(x_min, x_max, y_min, y_max))
x_res = int((x_max - x_min) / pixel_size)
y_res = int((y_max - y_min) / pixel_size)
print (x_res, y_res)

geoTransform = (x_min, pixel_size, 0, y_max, 0, -pixel_size)

#determinando o sistema de projeção
srs = osr.SpatialReference()
srs.ImportFromEPSG(32721)
proj = srs.ExportToWkt()

# definindo a função para criar o raster GTiff
def writeTif(filename, geot, array, proj, dtype, fmt='GTiff'):
    '''
    Escreve uma array em um GeoTiff


    argumentos:

    - filename - (string) caminho para se escrever o tiff

    - geot - sequência com as valores do geotransform:
        [0] x coordinate of the upper left cell in raster
        [1] width of the elements in the raster
        [2] element rotation in x, is set to 0 if a north up raster
        [3] y coordinate of the upper left cell in raster
        [4] element rotation in y, is set to 0 if a north up raster
        [5] height of the elements in the raster (negative)

    - array - numpy array a ser escrita

    - proj - projeção cartográfica (ex: osr.SRS_WKT_WGS84)

    - dtype - tipo dos valores das arrays no formato do gdal (ex: gdal.GDT_Int16)

    - fmt - tipo do arquivo, GeoTiff por dafault ('GTiff')


    '''

    driver = gdal.GetDriverByName(fmt)
	
    raster = driver.Create(filename, array.shape[1], array.shape[0], 1, dtype)
	
    raster.SetGeoTransform(geot)
    	
    raster.SetProjection(proj)
    	
    band = raster.GetRasterBand(1)
    	
    band.WriteArray(array, 0, 0)
    	
    raster.FlushCache()
    		
    raster = None
    del raster, band

#lista de arquivos a ser rasterizados
lista_raster = glob.glob("*.shp")
lista_raster.sort()
for i in lista_raster: print (i)
#criando a array a partir dos polígonos
#for i in lista_raster:
shape = "floresta_diss.shp"    
shp = gpd.read_file(shape)

shplist = [(g, 1) for g in shp.geometry]

mask = features.rasterize(
    shplist, 
    out_shape=(x_res,y_res), 
    transform=geoTransform, 
    fill=0,
    all_touched=True, 
    dtype=np.uint16)
plt.imshow(mask)

#criando o raster
writeTif(str(shape.replace(".shp", ".tif")),geoTransform, mask, proj, gdal.GDT_UInt16)
