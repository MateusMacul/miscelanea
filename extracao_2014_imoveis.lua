--------------------------------------------------------------
--          This file contains a Fill Cell Script           --
--               Compatible with LuccME 3.1                 --
--       Generated with Fill Cell Script Configurator       --
--               20/11/2018 at 16:32:45                     --
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
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\interseao_CAR_Campo_fix.shp"
}
print("Added Layer2: interseao_CAR_Campo_fix.shp")

l3 = Layer{
	project = proj,
	name = "layer3",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\interseao_CAR_UC_wgs84_21s_fix.shp"
}
print("Added Layer3: interseao_CAR_UC_wgs84_21s_fix.shp")

l4 = Layer{
	project = proj,
	name = "layer4",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\TerraLegal_SigefParticular_wg84_21s_fix.shp"
}
print("Added Layer4: TerraLegal_SigefParticular_wg84_21s_fix.shp")

-- Checking EPSGs --
print("\n-- Checking EPSGs--")
local epsgVector = {l1.epsg, l2.epsg, l3.epsg, l4.epsg}
local fileVector = {"cell_2km_prodes.shp", "interseao_CAR_Campo_fix.shp", "interseao_CAR_UC_wgs84_21s_fix.shp", "TerraLegal_SigefParticular_wg84_21s_fix.shp"}
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
print("Filling interseao_CAR_Campo_fix.shp into Cellular Space using distance operation")
cs:fill{
	layer = "layer2",
	operation = "distance",
	attribute = "car_dist",
	dataType = "polygon",
}

print("Filling interseao_CAR_UC_wgs84_21s_fix.shp into Cellular Space using distance operation")
cs:fill{
	layer = "layer3",
	operation = "distance",
	attribute = "car_uc_dis",
	dataType = "polygon",
}

print("Filling TerraLegal_SigefParticular_wg84_21s_fix.shp into Cellular Space using distance operation")
cs:fill{
	layer = "layer4",
	operation = "distance",
	attribute = "tl_sgf_dis",
	dataType = "polygon",
}

projFile = File("t3mp.tview")
if(projFile:exists()) then
	projFile:delete()
end
print(string.format("\nElapsed time : %.2fs\n", os.clock() - x))
print("\nEnd of Script")

