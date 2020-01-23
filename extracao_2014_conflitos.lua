--------------------------------------------------------------
--          This file contains a Fill Cell Script           --
--               Compatible with LuccME 3.1                 --
--       Generated with Fill Cell Script Configurator       --
--               15/12/2018 at 18:52:54                     --
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
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\cell_2km_prodes_declividade2.shp"
}
print("Added Cellular Spaced: cell_2km_prodes_declividade2.shp")

l2 = Layer{
	project = proj,
	name = "layer2",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2014\\CAR_Conflitos_UC_AST_Sobrp.shp"
}
print("Added Layer2: CAR_Conflitos_UC_AST_Sobrp.shp")

-- Checking EPSGs --
print("\n-- Checking EPSGs--")
local epsgVector = {l1.epsg, l2.epsg}
local fileVector = {"cell_2km_prodes_declividade2.shp", "CAR_Conflitos_UC_AST_Sobrp.shp"}
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
print("Filling CAR_Conflitos_UC_AST_Sobrp.shp into Cellular Space using area operation")
cs:fill{
	layer = "layer2",
	operation = "area",
	attribute = "area_confl",
	dataType = "polygon",
}

projFile = File("t3mp.tview")
if(projFile:exists()) then
	projFile:delete()
end
print(string.format("\nElapsed time : %.2fs\n", os.clock() - x))
print("\nEnd of Script")

