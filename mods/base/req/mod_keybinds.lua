
local C = LuaModManager.Constants
LuaModManager.Constants._keybinds_menu_id = "base_keybinds_menu"
local keybinds_menu_id = C._keybinds_menu_id

local display_keybinds_menu = false
local set_keybind_time = 0
local keybind_set_delay = 0.5

-- Keybinds Menu
Hooks:Add("MenuManager_Base_SetupModOptionsMenu", "Base_SetupKeybindsMenu", function( menu_manager, nodes )
	display_keybinds_menu = LuaModManager:GetNumberOfJsonKeybinds() > 0
	if display_keybinds_menu then
		MenuHelper:NewMenu( keybinds_menu_id )
	end
end)

Hooks:Add("MenuManager_Base_PopulateModOptionsMenu", "Base_PopulateKeybindsMenu", function( menu_manager, nodes )
	
	if display_keybinds_menu then

		for k, v in pairs( LuaModManager:Keybinds() ) do

			local keybind_id = v[ C.mod_keybind_id_key ]
			local keybind_name = v[ C.mod_keybind_name_key ]
			local keybind_desc = v[ C.mod_keybind_desc_key ]
			local keybind_script = v[ C.mod_keybind_script_key ]
			local keybind_localized = v[ C.mod_keybind_localize_key ]
			local key = LuaModManager:GetPlayerKeybind( keybind_id ) or ""

			MenuHelper:AddKeybinding({
				id = keybind_id,
				title = keybind_name,
				desc = keybind_desc,
				connection_name = keybind_id,
				button = key,
				binding = key,
				menu_id = keybinds_menu_id,
				localized = keybind_localized,
			})

		end

	end

end)

Hooks:Add("MenuManager_Base_BuildModOptionsMenu", "Base_BuildKeybindsMenu", function( menu_manager, nodes )
	if display_keybinds_menu then
		nodes[keybinds_menu_id] = MenuHelper:BuildMenu( keybinds_menu_id )
		MenuHelper:AddMenuItem( nodes.options, keybinds_menu_id, "base_options_menu_keybinds", "base_options_menu_keybinds_desc", "lua_mod_options_menu", "after" )
	end
end)

Hooks:Add("CustomizeControllerOnKeySet", "Base_Keybinds_CustomizeControllerOnKeySet", function( keybind, key )
	LuaModManager:SetPlayerKeybind( keybind, key )
	set_keybind_time = Application:time()
end)

Hooks:Add("MenuUpdate", "Base_Keybinds_MenuUpdate", function(t, dt)
	LuaModManager:UpdateBindings("MENU")
end)

Hooks:Add("GameSetupUpdate", "Base_Keybinds_GameStateUpdate", function(t, dt)
	LuaModManager:UpdateBindings("GAME")
end)

function LuaModManager:UpdateBindings( state )

	if not self._input_keyboard then
		self._input_keyboard = Input:keyboard()
	end
	if not self._input_mouse then
		self._input_mouse = Input:mouse()
	end
	if managers and managers.hud and managers.hud:chat_focus() then
		return
	end
	
	for keybind_id, key in pairs( LuaModManager:PlayerKeybinds() ) do
		if not string.is_nil_or_empty( key ) then

			local keybind = LuaModManager:Keybinds()[ keybind_id ]
			if keybind then

				local key_pressed = nil
				if string.find(key, "mouse ") ~= nil then
					key_pressed = self._input_mouse:pressed( key:sub(7) )
				else
					key_pressed = self._input_keyboard:pressed( Idstring(key) )
				end
				if key_pressed and Application:time() - set_keybind_time >= keybind_set_delay then
					self:AttemptRunKeybind( keybind, state )
				end

			end

		end
	end

end

function LuaModManager:AttemptRunKeybind( keybind, state )

	local keybind_type = type(keybind)
	if keybind_type == "table" then
		local should_run = keybind[ state == "MENU" and C.mod_keybind_scope_menu_key or C.mod_keybind_scope_game_key ]
		if should_run then
			local script = keybind[ C.mod_keybind_script_key ]
			dofile( script )
		end
	end

	if keybind_type == "function" then
		keybind()
	end

end
