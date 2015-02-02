
--[[
	Cache our ModPath variable so that we don't accidentally call a different mod's ModPath when the
	below hook is called.
]]
local this_mod_path = ModPath

--[[
	We load our localization files after the ingame localization manager has been setup.
]]
Hooks:Add("LocalizationManagerPostInit", "LocalizationManagerPostInit_LocExample", function(loc)
	
	--[[
		We can either add localization strings directly through lua by passing a key-value table to
		LocalizationManager:add_localized_strings, where the key is the localization key, and the value
		the string we want to assign it to.
	]]
	LocalizationManager:add_localized_strings({
		["loc_example_test_string"] = "This is our localization test string!",
		["loc_example_test_string2"] = "This is another localization test string!",
	})

	--[[
		We can also load JSON formatted files in the same way, which will automatically load our localized
		strings for us.
	]]
	LocalizationManager:load_localization_file( this_mod_path .. "loc.txt" )

end)
