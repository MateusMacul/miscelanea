--------------------------------------------------------------
--          This file contains a Fill Cell Script           --
--               Compatible with LuccME 3.1                 --
--       Generated with Fill Cell Script Configurator       --
--               26/02/2019 at 15:22:14                     --
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
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\celulas_problema.shp"
}
print("Added Cellular Spaced: celulas_problema.shp")

l2 = Layer{
	project = proj,
	name = "layer2",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\degradacao\\degTodas_2010a2013_fix.shp"
}
print("Added Layer2: degTodas_2010a2013_fix.shp")

l3 = Layer{
	project = proj,
	name = "layer3",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\TerraLegal_SigefParticular_wg84_21s_fix.shp"
}
print("Added Layer3: TerraLegal_SigefParticular_wg84_21s_fix.shp")

l4 = Layer{
	project = proj,
	name = "layer4",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\CAR_Conflitos_UC_AST_Sobrp.shp"
}
print("Added Layer4: CAR_Conflitos_UC_AST_Sobrp.shp")

l5 = Layer{
	project = proj,
	name = "layer5",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\Assentamentos_Oficial_Clip_wgs84_21s.shp"
}
print("Added Layer5: Assentamentos_Oficial_Clip_wgs84_21s.shp")

l6 = Layer{
	project = proj,
	name = "layer6",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\uc_wgs84_21s_fix.shp"
}
print("Added Layer6: uc_wgs84_21s_fix.shp")

l7 = Layer{
	project = proj,
	name = "layer7",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\embargo_2005a2014_wgs84_21s_fix.shp"
}
print("Added Layer7: embargo_2005a2014_wgs84_21s_fix.shp")

l8 = Layer{
	project = proj,
	name = "layer8",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\TerraClass\\AMZ_2014_areaaberta.shp"
}
print("Added Layer8: AMZ_2014_areaaberta.shp")

l9 = Layer{
	project = proj,
	name = "layer9",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\TerraClass\\AMZ_2014_floresta.shp"
}
print("Added Layer9: AMZ_2014_floresta.shp")

l10 = Layer{
	project = proj,
	name = "layer10",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\TerraClass\\AMZ_2014_vegsec.shp"
}
print("Added Layer10: AMZ_2014_vegsec.shp")

l11 = Layer{
	project = proj,
	name = "layer11",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\Prodes_2014\\NaoFloresta_Hidrografia_2017.shp"
}
print("Added Layer11: NaoFloresta_Hidrografia_2017.shp")

-- Checking EPSGs --
print("\n-- Checking EPSGs--")
local epsgVector = {l1.epsg, l2.epsg, l3.epsg, l4.epsg, l5.epsg, l6.epsg, l7.epsg, l8.epsg, l9.epsg, l10.epsg, l11.epsg}
local fileVector = {"celulas_problema.shp", "degTodas_2010a2013_fix.shp", "TerraLegal_SigefParticular_wg84_21s_fix.shp", "CAR_Conflitos_UC_AST_Sobrp.shp", "Assentamentos_Oficial_Clip_wgs84_21s.shp", "uc_wgs84_21s_fix.shp", "embargo_2005a2014_wgs84_21s_fix.shp", "AMZ_2014_areaaberta.shp", "AMZ_2014_floresta.shp", "AMZ_2014_vegsec.shp", "NaoFloresta_Hidrografia_2017.shp"}
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
print("Filling degTodas_2010a2013_fix.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer2",
	operation = "area",
	attribute = "area_deg",
	dataType = "polygon",
}

print("Filling TerraLegal_SigefParticular_wg84_21s_fix.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer3",
	operation = "area",
	attribute = "area_tlsgf",
	dataType = "polygon",
}

print("Filling CAR_Conflitos_UC_AST_Sobrp.shp into Cellular Space using distance operation")
cs:fill{
	layer = "layer4",
	operation = "distance",
	attribute = "confl_dis",
	dataType = "polygon",
}

print("Filling Assentamentos_Oficial_Clip_wgs84_21s.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer5",
	operation = "area",
	attribute = "area_ass",
	dataType = "polygon",
}

print("Filling uc_wgs84_21s_fix.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer6",
	operation = "area",
	attribute = "area_uc",
	dataType = "polygon",
}

print("Filling embargo_2005a2014_wgs84_21s_fix.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer7",
	operation = "area",
	attribute = "area_embar",
	dataType = "polygon",
}

print("Filling AMZ_2014_areaaberta.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer8",
	operation = "area",
	attribute = "area_abert",
	dataType = "polygon",
}

print("Filling AMZ_2014_floresta.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer9",
	operation = "area",
	attribute = "area_flor",
	dataType = "polygon",
}

print("Filling AMZ_2014_vegsec.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer10",
	operation = "area",
	attribute = "area_vegse",
	dataType = "polygon",
}

print("Filling NaoFloresta_Hidrografia_2017.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer11",
	operation = "area",
	attribute = "area_retir",
	dataType = "polygon",
}

projFile = File("t3mp.tview")
if(projFile:exists()) then
	projFile:delete()
end
print(string.format("\nElapsed time : %.2fs\n", os.clock() - x))
print("\nEnd of Script")

