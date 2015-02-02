
Hooks:Add("LocalizationManagerPostInit", "Base_LocalizationManagerPostInit", function(loc)
	loc:load_localization_file( LuaModManager._base_path .. "loc/en.txt")
end)
