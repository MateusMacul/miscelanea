--------------------------------------------------------------
--          This file contains a Fill Cell Script           --
--               Compatible with LuccME 3.1                 --
--       Generated with Fill Cell Script Configurator       --
--               25/04/2019 at 19:40:16                     --
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
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\celulas_extraida2.shp"
}
print("Added Cellular Spaced: celulas_extraida2.shp")

l2 = Layer{
	project = proj,
	name = "layer2",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\Prodes_2014\\PDigital2017_PA__WGS84_fix_clip_FLORESTA_fix_dissolve_subdividido3.shp"
}
print("Added Layer2: PDigital2017_PA__WGS84_fix_clip_FLORESTA_fix_dissolve_subdividido3.shp")

-- Checking EPSGs --
print("\n-- Checking EPSGs--")
local epsgVector = {l1.epsg, l2.epsg}
local fileVector = {"celulas_extraida2.shp", "PDigital2017_PA__WGS84_fix_clip_FLORESTA_fix_dissolve_subdividido3.shp"}
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
print("Filling PDigital2017_PA__WGS84_fix_clip_FLORESTA_fix_dissolve_subdividido3.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer2",
	operation = "area",
	attribute = "flor_17_p3",
	dataType = "polygon",
}

projFile = File("t3mp.tview")
if(projFile:exists()) then
	projFile:delete()
end
print(string.format("\nElapsed time : %.2fs\n", os.clock() - x))
print("\nEnd of Script")

