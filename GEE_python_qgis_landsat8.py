import ee

from ee_plugin import Map


grade_tm = ee.FeatureCollection("users/maculmateus/Area_corte_Mateus_tapajos");

Map.centerObject(grade_tm, 10);

#************* Sentinel 2 ***************

image1 = ee.ImageCollection('COPERNICUS/S2') \
 .filterBounds(grade_tm)\
 .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))\
 .filterDate('2015-07-01','2016-08-31').median() \
 .divide(10000).visualize(**{'bands': ['B4', 'B3', 'B2'], 'min': 0.05, 'max': 0.2})
 
#image2 = ee.ImageCollection('COPERNICUS/S2') \
# .filterBounds(grade_tm)\
# .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))\
# .filterDate('2018-09-01', '2018-09-19').median() \
# .divide(10000).visualize(**{'bands': ['B12', 'B8', 'B4'], 'min': 0.05, 'max': 0.4})
#
mosaico_s2_1 = image1.clip(grade_tm)
#mosaico_s2_2 = image2.clip(grade_tm)
#
Map.addLayer(mosaico_s2_1, {}, 'Imagem Sentinel-2 Atual')
#
#Map.addLayer(mosaico_s2_2, {}, 'Imagem Sentinel-2 Anterior')
#
## ********* Referencia imagem Landsat 8  ***********
##LANDSAT/LC08/C01/T1_TOA/LC08_231068_20190125
#
#img_atual = ee.Image('LANDSAT/LC08/C01/T1_TOA/LC08_225069_20180808')
#img_ant = ee.Image('LANDSAT/LC08/C01/T1_TOA/LC08_225069_20180723')
#
#************* Mosaico Landsat 8 ***************

satl8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_TOA')\
 .filterDate('2015-07-01','2016-08-31')\
 .filterMetadata('CLOUD_COVER_LAND','less_than',20);
#
sat_median = satl8.median()

#imagem_l8_atual = img_atual.clip(grade_tm)
imagem_l8 = sat_median.clip(grade_tm)

Map.addLayer(imagem_l8, {"bands": ['B6', 'B5', 'B4'], "max": 0.6}, 'Mos Imagem L8')
#Map.addLayer(imagem_l8_atual, {"bands": ['B7', 'B5', 'B4'], "max": 0.5}, 'Imagem L8 Atual ')