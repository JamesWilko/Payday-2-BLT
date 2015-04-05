
function LuaModUpdates:ShowMultiUpdateAvailableMessage( mods )

	local mod_names = ""
	for k, v in pairs( mods ) do
		local mod_definition = LuaModManager:GetMod( v.mod ).definition
		local name = v.display_name or mod_definition[ LuaModManager.Constants.mod_name_key ]
		mod_names = mod_names .. "        " .. name .. "\n"
	end

	local loc_table = { ["mods"] = mod_names }
	local menu_title = managers.localization:text("base_mod_updates_show_multiple_update_available", loc_table)
	local menu_message = managers.localization:text("base_mod_updates_show_multiple_update_available_message", loc_table)
	local menu_options = {
		-- [1] = {
		-- 	text = managers.localization:text("base_mod_updates_update_all_now"),
		-- 	callback = LuaModUpdates.DoUpdateAllModsNow,
		-- },
		[1] = {
			text = managers.localization:text("base_mod_updates_open_update_manager"),
			callback = LuaModUpdates.OpenUpdateManagerNode,
		},
		[2] = {
			text = managers.localization:text("base_mod_updates_update_later"),
			is_cancel_button = true,
		},
	}
	QuickMenu:new( menu_title, menu_message, menu_options, true )

end

function LuaModUpdates:ShowUpdateAvailableMessage( mod_tbl )

	local loc_table = { ["mod_name"] = self:GetModFriendlyName(mod_tbl.identifier) }
	local menu_title = managers.localization:text("base_mod_updates_show_update_available", loc_table)
	local menu_message = managers.localization:text("base_mod_updates_show_update_available_message", loc_table)
	local menu_options = {
		[1] = {
			text = managers.localization:text("base_mod_updates_update_mod_now", loc_table),
			callback = function()
				LuaModUpdates:OpenUpdateManagerNode()
				LuaModUpdates.ForceDownloadAndInstallMod(mod_tbl.identifier)
			end,
		},
		[2] = {
			text = managers.localization:text("base_mod_updates_open_update_manager"),
			callback = LuaModUpdates.OpenUpdateManagerNode,
		},
		[3] = {
			text = managers.localization:text("base_mod_updates_open_update_notes"),
			callback = LuaModUpdates.ShowModPatchNotes,
			data = mod_tbl.identifier
		},
		[4] = {
			text = managers.localization:text("base_mod_updates_update_later"),
			is_cancel_button = true,
		},
	}
	QuickMenu:new( menu_title, menu_message, menu_options, true )

end

-- Updates menu
local mod_updates_menu = "base_lua_mod_updates_menu"
Hooks:Add("MenuManagerSetupCustomMenus", "Base_ModUpdatesMenu_SetupCustomMenus", function( menu_manager, nodes )
	MenuHelper:NewMenu( mod_updates_menu )
end)

Hooks:Add("MenuManagerPopulateCustomMenus", "Base_ModUpdatesMenu_PopulateCustomMenus", function( menu_manager, nodes )

	MenuCallbackHandler.mod_updates_update_all = function(self, item)
		log("Update all mods")
	end

	MenuCallbackHandler.mod_updates_toggle_mod = function(self, item)
		if item and item._parameters then
			local mod_path = item._parameters.text_id:gsub("toggle_lua_auto_updates_", "")
			if mod_path then
				LuaModManager:SetModUpdatesState( mod_path, item:value() == "on" and true or false )
			end
		end
	end

	MenuCallbackHandler.mod_updates_check_mod = function(self, item)
		if item and item._parameters then
			local mod_path = item._parameters.text_id:gsub("button_check_for_updates_", "")
			if mod_path then

				LuaModUpdates:CheckForUpdates( function(updater, mods)
					LuaModUpdates:CheckModForUpdateAndShowOptions( mod_path )
				end )

			end
		end
	end

	local priority = #LuaModManager:UpdateChecks() * 3
	local toggle_updates_loc_str = "toggle_lua_auto_updates_{0}"
	local check_for_updates_loc_str = "button_check_for_updates_{0}"

	--[[
	MenuHelper:AddButton({
		id = "lua_mods_update_all",
		title = "base_mod_updates_update_all",
		desc = "base_mod_updates_update_all_desc",
		callback = "mod_updates_update_all",
		menu_id = mod_updates_menu,
		priority = priority + 3,
	})

	MenuHelper:AddDivider({
		id = "lua_mods_update_divider_1",
		size = 16,
		menu_id = mod_updates_menu,
		priority = priority + 1,
	})
	]]

	for k, v in ipairs( LuaModManager:UpdateChecks() ) do

		local mod_definition = LuaModManager:GetMod( v.mod ).definition
		local mod_name = v.display_name or mod_definition[ LuaModManager.Constants.mod_name_key ]
		local mod_name_table = { ["mod_name"] = mod_name }
		local loc_toggle = toggle_updates_loc_str:gsub("{0}", v.identifier)
		local loc_button = check_for_updates_loc_str:gsub("{0}", v.identifier)
		LocalizationManager:add_localized_strings({
			[loc_toggle] = managers.localization:text("base_mod_automically_check_for_updates", mod_name_table),
			[loc_toggle .. "_desc"] = managers.localization:text("base_mod_automically_check_for_updates_desc", mod_name_table),
			[loc_button] = managers.localization:text("base_mod_check_for_updates_now", mod_name_table),
			[loc_button .. "_desc"] = managers.localization:text("base_mod_check_for_updates_now_desc", mod_name_table),
		})

		local toggle = MenuHelper:AddToggle({
			id = "toggle_updates_" .. v.identifier,
			title = loc_toggle,
			desc = loc_toggle .. "_desc",
			callback = "mod_updates_toggle_mod",
			value = LuaModManager:AreModUpdatesEnable( v.identifier ),
			menu_id = mod_updates_menu,
			priority = priority,
		})

		MenuHelper:AddButton({
			id = "button_check_for_updates_" .. v.identifier,
			title = loc_button,
			desc = loc_button .. "_desc",
			callback = "mod_updates_check_mod",
			menu_id = mod_updates_menu,
			priority = priority - 1,
		})

		MenuHelper:AddDivider({
			id = "divider_updates_" .. v.identifier,
			size = 8,
			menu_id = mod_updates_menu,
			priority = priority - 2,
		})

		priority = priority - 3

	end

end)

Hooks:Add("MenuManagerBuildCustomMenus", "Base_ModUpdatesMenu_BuildCustomMenus", function( menu_manager, nodes )
	nodes[mod_updates_menu] = MenuHelper:BuildMenu( mod_updates_menu )
end)

-- Info Boxes
function LuaModUpdates:CheckModForUpdateAndShowOptions( mod_id )

	log("[Updates] Checking for updates for mod: " .. mod_id)
	for k, v in pairs( LuaModManager:UpdateChecks() ) do
		if v.identifier == mod_id then

			if v.update_required then
				self:ShowModRequiresUpdate( mod_id )
			else
				self:ShowModUpToDate( mod_id )
			end
			
		end
	end

end

function LuaModUpdates:ShowModUpToDate( mod_id )

	local message_id = self:VerifyModIsDownloadable( mod_id ) and "base_mod_update_no_update_available" or "base_mod_update_no_update_available_no_data"

	local mod_name_tbl = { ["mod_name"] = self:GetModFriendlyName( mod_id ) }
	local title = managers.localization:text("base_mod_update_no_update_available_title", mod_name_tbl)
	local message = managers.localization:text(message_id, mod_name_tbl)
	local options = {}

	if self:VerifyModIsDownloadable( mod_id ) then
		local download_btn = {
			text = managers.localization:text("base_mod_update_no_update_available_force"),
			callback = LuaModUpdates.ForceDownloadAndInstallMod,
			data = mod_id
		}
		table.insert( options, download_btn )
	end
	
	local cancel_btn = {
		text = managers.localization:text("dialog_ok"),
		is_cancel_button = true
	}
	table.insert( options, cancel_btn )

	QuickMenu:new( title, message, options, true )

end

function LuaModUpdates:ShowModRequiresUpdate( mod_id )

	local mod_name_tbl = { ["mod_name"] = self:GetModFriendlyName( mod_id ) }
	local title = managers.localization:text("base_mod_updates_update_mod_now", mod_name_tbl)
	local message = managers.localization:text("base_mod_update_update_available", mod_name_tbl)
	local options = {
		[1] = {
			text = managers.localization:text("base_mod_updates_update_now"),
			callback = LuaModUpdates.ForceDownloadAndInstallMod,
			data = mod_id
		},
		[2] = {
			text = managers.localization:text("base_mod_updates_open_update_notes"),
			callback = LuaModUpdates.ShowModPatchNotes,
			data = mod_id
		},
		[3] = {
			text = managers.localization:text("base_mod_updates_update_later"),
			is_cancel_button = true
		}
	}
	QuickMenu:new( title, message, options, true )

end

function LuaModUpdates.ForceDownloadAndInstallMod( data )
	LuaModUpdates:DownloadAndStoreMod( data )
end

function LuaModUpdates.ShowModPatchNotes( data )
	local mod_table = LuaModUpdates:GetModTable( mod_id ) or {}

	local url = mod_table.notes_url or LuaModUpdates._updates_notes_url:gsub("{1}", data)
	Steam:overlay_activate("url", url)
	LuaModUpdates:ShowModRequiresUpdate( data )
end
