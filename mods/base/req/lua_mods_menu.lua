
LuaModManager.Constants._lua_mods_menu_id = "base_lua_mods_menu"
local lua_mods_menu_id = LuaModManager.Constants._lua_mods_menu_id

-- Lua Mods Menu
Hooks:Add("MenuManager_Base_SetupModsMenu", "Base_SetupModsMenu", function( menu_manager, nodes )
	MenuHelper:NewMenu( lua_mods_menu_id )
end)

Hooks:Add("MenuManager_Base_BuildModsMenu", "Base_BuildModsMenu", function( menu_manager, nodes )
	local mods_menu = MenuHelper:BuildMenu( lua_mods_menu_id )
	mods_menu._parameters.name = lua_mods_menu_id	
	nodes[lua_mods_menu_id] = mods_menu
	MenuHelper:AddMenuItem( nodes.options, lua_mods_menu_id, "base_options_menu_lua_mods", "base_options_menu_lua_mods_desc", 6 )
end)
