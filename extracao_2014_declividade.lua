--------------------------------------------------------------
--          This file contains a Fill Cell Script           --
--               Compatible with LuccME 3.1                 --
--       Generated with Fill Cell Script Configurator       --
--               20/11/2018 at 17:45:53                     --
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
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\cell_2km_prodes_4326.shp"
}
print("Added Cellular Spaced: cell_2km_prodes_4326.shp")

l2 = Layer{
	project = proj,
	name = "layer2",
	file = "C:\\Users\\Padrao\\Documents\\fillcell\\2004_e_2014\\topodata\\Mosaico_SN.tif"
}
print("Added Layer2: Mosaico_SN.tif")

-- Checking EPSGs --
print("\n-- Checking EPSGs--")
local epsgVector = {l1.epsg, l2.epsg}
local fileVector = {"cell_2km_prodes_4326.shp", "Mosaico_SN.tif"}
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
print("Filling Mosaico_SN.tif into Cellular Space using average operation")
cs:fill{
	layer = "layer2",
	operation = "average",
	attribute = "decl_med",
	pixel = "overlap"
}

projFile = File("t3mp.tview")
if(projFile:exists()) then
	projFile:delete()
end
print(string.format("\nElapsed time : %.2fs\n", os.clock() - x))
print("\nEnd of Script")

