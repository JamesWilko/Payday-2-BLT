
local C = LuaModManager.Constants
LuaModManager.Constants._keybinds_menu_id = "base_keybinds_menu"
local keybinds_menu_id = C._keybinds_menu_id

local set_keybind_time = 0
local keybind_set_delay = 0.5

-- Keybinds Menu
Hooks:Add("MenuManager_Base_SetupKeybindsMenu", "Base_SetupKeybindsMenu", function( menu_manager, nodes )
	MenuHelper:NewMenu( keybinds_menu_id )
end)

Hooks:Add("MenuManager_Base_PopulateKeybindsMenu", "Base_PopulateKeybindsMenu", function( menu_manager, nodes )
	
	for k, v in pairs( LuaModManager:Keybinds() ) do

		local keybind_id = v[ C.mod_keybind_id_key ]
		local keybind_name = v[ C.mod_keybind_name_key ]
		local keybind_desc = v[ C.mod_keybind_desc_key ]
		local keybind_script = v[ C.mod_keybind_script_key ]
		local key = LuaModManager:GetPlayerKeybind( keybind_id ) or ""

		MenuHelper:AddKeybinding({
			id = keybind_id,
			title = keybind_name,
			desc = keybind_desc,
			connection_name = keybind_id,
			button = key,
			binding = key,
			menu_id = keybinds_menu_id,
		})

	end

end)

Hooks:Add("MenuManager_Base_BuildKeybindsMenu", "Base_BuildKeybindsMenu", function( menu_manager, nodes )
	nodes[keybinds_menu_id] = MenuHelper:BuildMenu( keybinds_menu_id )
	MenuHelper:AddMenuItem( nodes.options, keybinds_menu_id, "mod keybinds", "mod keybinds menu", 7 )
end)

Hooks:Add("CustomizeControllerOnKeySet", "Base_Keybinds_CustomizeControllerOnKeySet", function( item )
	LuaModManager:SetPlayerKeybind( item._name, item._input_name_list[1] )
	set_keybind_time = Application:time()
end)

Hooks:Add("MenuUpdate", "Base_Keybinds_MenuUpdate", function(t, dt)
	LuaModManager:UpdateBindings("MENU")
end)

Hooks:Add("GameSetupUpdate", "Base_Keybinds_GameStateUpdate", function(t, dt)
	LuaModManager:UpdateBindings("GAME")
end)

function LuaModManager:UpdateBindings( state )

	if not self._input then
		self._input = Input:keyboard()
	end
	if managers and managers.hud and managers.hud:chat_focus() then
		return
	end
	
	for keybind_id, key in pairs( LuaModManager:PlayerKeybinds() ) do
		if not string.is_nil_or_empty( key ) then

			local keybind = LuaModManager:Keybinds()[ keybind_id ]
			local key_pressed = self._input:pressed( Idstring(key) )
			local should_run = keybind[ state == "MENU" and C.mod_keybind_scope_menu_key or C.mod_keybind_scope_game_key ] or true

			if should_run and key_pressed then
				if Application:time() - set_keybind_time >= keybind_set_delay then
					local script = keybind[ C.mod_keybind_script_key ]
					dofile( script )
				end
			end

		end
	end

end
