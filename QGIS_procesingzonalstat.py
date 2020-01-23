indice  = "DEGRADI"
#data = "20190724" #t1
#t =  "1"
data = "20190809" #t2
t =  "2"

imagem = "SR_" + indice + "_LC08_227065_" + data
prefixo = indice +"t" + t + "_"
print(imagem)
print(prefixo)

raster = QgsProject.instance().mapLayersByName(imagem)[0]
#vetor = QgsProject.instance().mapLayersByName("amostras")
vetor = iface.activeLayer()

processing.run('qgis:zonalstatistics', {'INPUT_RASTER': raster, 'RASTER_BAND': 1, 'INPUT_VECTOR': vetor, 'COLUMN_PREFIX': prefixo, 'STATS':[0,1,2,3,4,5,6,7], 'OUTPUT': "memory:comparando_imgs"})



