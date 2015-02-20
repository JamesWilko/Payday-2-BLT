
LuaModManager.Constants._lua_mods_menu_id = "base_lua_mods_menu"
LuaModManager.Constants._lua_mod_options_menu_id = "lua_mod_options_menu"
local lua_mods_menu_id = LuaModManager.Constants._lua_mods_menu_id
local lua_mod_options_menu_id = LuaModManager.Constants._lua_mod_options_menu_id

-- Lua Mods Menu
Hooks:Add("MenuManager_Base_SetupModsMenu", "Base_SetupModsMenu", function( menu_manager, nodes )
	MenuHelper:NewMenu( lua_mods_menu_id )
end)

Hooks:Add("MenuManager_Base_BuildModsMenu", "Base_BuildModsMenu", function( menu_manager, nodes )

	-- Add mods menu
	local mods_menu = MenuHelper:BuildMenu( lua_mods_menu_id )
	mods_menu._parameters.name = lua_mods_menu_id	
	nodes[lua_mods_menu_id] = mods_menu
	MenuHelper:AddMenuItem( nodes.options, lua_mods_menu_id, "base_options_menu_lua_mods", "base_options_menu_lua_mods_desc", "mods", "after" )

end)

-- Mod Options Menu
Hooks:Add("MenuManager_Base_SetupModOptionsMenu", "Base_SetupModOptionsMenu", function( menu_manager, nodes )
	MenuHelper:NewMenu( lua_mod_options_menu_id )
end)

Hooks:Add("MenuManager_Base_BuildModOptionsMenu", "Base_BuildModOptionsMenu", function( menu_manager, nodes )

	-- Add mod options menu
	nodes[lua_mod_options_menu_id] = MenuHelper:BuildMenu( lua_mod_options_menu_id )
	MenuHelper:AddMenuItem( nodes.options, lua_mod_options_menu_id, "base_options_menu_lua_mod_options", "base_options_menu_lua_mod_options_desc", lua_mods_menu_id, "after" )

end)
