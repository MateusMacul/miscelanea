# importar biblioteca
# importar constantes
from osgeo import gdal
from gdalconst import *

import numpy as np
import scipy
import matplotlib.pyplot as plt
# informar o uso de exceções
gdal.UseExceptions()

# mostrar versão instalada
print (gdal.__version__)

# criar o dataset abrindo o arquivo para leitura

filename = "C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/imagens_LiSS/LC08_L1TP_227065_20130824_20170502/LC08_L1TP_227065_20130824_20170502_01_T1_B4.tif"
#dataset = gdal.Open(filename, GA_ReadOnly)

# fechar o dataset e liberar memória
#dataset = None

# biblioteca de funções relacionadas ao sistema
# sys: System-specific parameters and functions



filename_erro = " teste/" + filename
print("Tentar abrir" + filename_erro)

try:
    dataset = gdal.Open(filename_erro, GA_ReadOnly)
    print("Arquivo aberto com sucesso!")
except:
    print("Erro na abertura do arquivo!")

print(" Tentar abrir" + filename)
try:
    dataset = gdal.Open(filename, GA_ReadOnly)
    print("Arquivo aberto com sucesso!")
except:
    print("Erro na abertura do arquivo!")

geotransform = dataset.GetGeoTransform()
print (geotransform)

# número de linhas e colunas
linhas = dataset.RasterYSize
colunas = dataset.RasterXSize

print ("Número de linhas:", linhas)
print ("Número de colunas:", colunas)

# quantidade de bandas
bandas = dataset.RasterCount

print ("Número de bandas:", bandas)

# no caso da imagem RapidEye, as bandas 5
# e 3 correspondem às bandas NIR e RED
banda_nir = dataset.GetRasterBand(1)
#banda_red = dataset.GetRasterBand(3)

print ("Tipos de dados:")
print (" - banda NIR:", gdal.GetDataTypeName(banda_nir.DataType))

dsarray = dataset.ReadAsArray()

array_med =scipy.ndimage.filters.median_filter(dsarray, )

f, ax = plt.subplots(2,1)
ax[0].imshow(array_med)
ax[1].imshow(dsarray)