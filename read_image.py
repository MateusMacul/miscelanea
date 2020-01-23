# -*- coding: utf-8 -*-

from osgeo import gdal

filename = u'/home/vitorgalazzo/Documents/PDI/tmp/sjc-wv2-3200x2400-regiao1'
dataset = gdal.Open(filename,GA_ReadOnly)
ds = dataset.ReadAsArray()
