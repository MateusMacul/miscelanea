--------------------------------------------------------------
--          This file contains a Fill Cell Script           --
--               Compatible with LuccME 3.1                 --
--       Generated with Fill Cell Script Configurator       --
--               19/11/2018 at 18:35:19                     --
--------------------------------------------------------------

local x = os.clock()
import("gis")

local projFile = File("t3mp.tview")
if(projFile:exists()) then
	projFile:delete()
end

-- CREATING PROJECT --
print("-- Creating Project --\n")

proj = Project {
	file = "t3mp.tview",
	clean = true
}

-- ADDING LAYERS --
print("-- Adding Layers to the Project --")

l1 = Layer{
	project = proj,
	name = "limit",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\cell_2km_prodes.shp"
}
print("Added Cellular Spaced: cell_2km_prodes.shp")

l2 = Layer{
	project = proj,
	name = "layer2",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\Prodes_2014\\Desmatamento_2014_wgs84_21s.shp"
}
print("Added Layer2: Desmatamento_2014_wgs84_21s.shp")

l3 = Layer{
	project = proj,
	name = "layer3",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\degradacao\\deg2013novo.shp"
}
print("Added Layer3: deg2013novo.shp")

l4 = Layer{
	project = proj,
	name = "layer4",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\Area_urbana_2014_wgs84_21s.shp"
}
print("Added Layer4: Area_urbana_2014_wgs84_21s.shp")

l5 = Layer{
	project = proj,
	name = "layer5",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\Assentamentos_Oficial_Clip_wgs84_21s.shp"
}
print("Added Layer5: Assentamentos_Oficial_Clip_wgs84_21s.shp")

l6 = Layer{
	project = proj,
	name = "layer6",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\uctodas_wgs84_21s.shp"
}
print("Added Layer6: uctodas_wgs84_21s.shp")

l7 = Layer{
	project = proj,
	name = "layer7",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2004_e_2014\\ti_wgs84_21s.shp"
}
print("Added Layer7: ti_wgs84_21s.shp")

l8 = Layer{
	project = proj,
	name = "layer8",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2004_e_2014\\Hidrografia 250000_wgs84_21s.shp"
}
print("Added Layer8: Hidrografia 250000_wgs84_21s.shp")

-- Checking EPSGs --
print("\n-- Checking EPSGs--")
local epsgVector = {l1.epsg, l2.epsg, l3.epsg, l4.epsg, l5.epsg, l6.epsg, l7.epsg, l8.epsg}
local fileVector = {"cell_2km_prodes.shp", "Desmatamento_2014_wgs84_21s.shp", "deg2013novo.shp", "Area_urbana_2014_wgs84_21s.shp", "Assentamentos_Oficial_Clip_wgs84_21s.shp", "uctodas_wgs84_21s.shp", "ti_wgs84_21s.shp", "Hidrografia 250000_wgs84_21s.shp"}
local checkEPSG = true

for i = 1, #epsgVector, 1 do
	if (epsgVector[i] ~= l1.epsg) then
		print("Error: EPSG does not math - limit : "..l1.epsg.." "..fileVector[i].." : "..epsgVector[i])
		checkEPSG = false
	end

	if checkEPSG then print("EPSG - limit : "..l1.epsg.."\t"..fileVector[i]..": "..epsgVector[i]) end
end

if not checkEPSG then os.exit() end

-- OPENING CELLULAR SPACE --
print("\n-- Openning Cellular Space -- \n")
local cs = Layer{
	project = proj,
	name = l1.name,
}

-- FILLING CELLULAR SPACE --
print("Filling Desmatamento_2014_wgs84_21s.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer2",
	operation = "area",
	attribute = "desm14_are",
	dataType = "polygon",
}

print("Filling deg2013novo.shp into Cellular Space using distance operation")
cs:fill{
	layer = "layer3",
	operation = "distance",
	attribute = "deg_dist",
	dataType = "polygon",
}

print("Filling Area_urbana_2014_wgs84_21s.shp into Cellular Space using distance operation")
cs:fill{
	layer = "layer4",
	operation = "distance",
	attribute = "au_dist",
	dataType = "polygon",
}

print("Filling Assentamentos_Oficial_Clip_wgs84_21s.shp into Cellular Space using distance operation")
cs:fill{
	layer = "layer5",
	operation = "distance",
	attribute = "ass_dist",
	dataType = "polygon",
}

print("Filling uctodas_wgs84_21s.shp into Cellular Space using distance operation")
cs:fill{
	layer = "layer6",
	operation = "distance",
	attribute = "uc_dist",
	dataType = "polygon",
}

print("Filling ti_wgs84_21s.shp into Cellular Space using distance operation")
cs:fill{
	layer = "layer7",
	operation = "distance",
	attribute = "ti_dist",
	dataType = "polygon",
}

print("Filling Hidrografia 250000_wgs84_21s.shp into Cellular Space using distance operation")
cs:fill{
	layer = "layer8",
	operation = "distance",
	attribute = "hidro_dist",
	dataType = "line",
}

projFile = File("t3mp.tview")
if(projFile:exists()) then
	projFile:delete()
end
print(string.format("\nElapsed time : %.2fs\n", os.clock() - x))
print("\nEnd of Script")

