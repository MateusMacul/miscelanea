import netCDF4 as nc
import numpy as np
from osgeo import gdal, osr


def write_nc(filename, var_names, var_arrays):
	'''
	função para escrever netcdf
	
	argumentos:
	-filename -> string (ex. 'nc_file.nc')
	-var_names -> sequência com as keys do dicionário de variáveis
	-var_arrays -> dicionário, contendo as arrays das variáveis
	e das dimenções, sendo que, as arrays de latitude e longitude
	devem ter como keys 'lat' e 'lon', respectivamente.

	ex:

	v_names = ['lon','lat','propriedade1','propriedade2']
	arrays_dict = {'lon':lon_array,'lat':lat_array,'propriedade1':prop1_array,'propriedade2':prop2_array}
	
	write_nc('name.nc',v_names,arrays_dict)
	''' 
	if filename.endswith('.nc') == True:
		fn = unicode(filename)
	else:
		fn = unicode(filename+'.nc')
	
	ncfile = nc.Dataset(fn, 'w', 'NETCDF4')
	dt = unicode(var_arrays['lon'].dtype.str[1:])

	if len(var_arrays['lon'].shape) == 1:
		ncfile.createDimension(u'lat', var_arrays['lat'].shape[0])
		ncfile.createDimension(u'lon', var_arrays['lon'].shape[0])
		ncfile.createVariable(u'lat',dt,(u"lat",))
		ncfile.createVariable(u'lon',dt,(u"lon",))
	else:
		ncfile.createDimension(u'lat', var_arrays['lat'].shape[0])
		ncfile.createDimension(u'lon', var_arrays['lon'].shape[1])
		ncfile.createVariable(u'lat',dt,(u"lat",u"lon"))
		ncfile.createVariable(u'lon',dt,(u"lat",u"lon"))
	
	for name in var_names:
		if name != 'lat' and name != 'lon':
			n=unicode(name)
			dtv = unicode(var_arrays[n].dtype.str[1:])
			ncfile.createVariable(n,dtv,(u"lat",u"lon"))
			ncfile.variables[n][:] = var_arrays[n]
	
	ncfile.close()


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

