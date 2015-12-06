
CloneClass( MenuManager )
CloneClass( MenuCallbackHandler )
CloneClass( ModMenuCreator )
CloneClass( MenuModInfoGui )

Hooks:RegisterHook( "MenuManagerInitialize" )
Hooks:RegisterHook( "MenuManagerPostInitialize" )
function MenuManager.init( self, ... )
	self.orig.init( self, ... )
	Hooks:Call( "MenuManagerInitialize", self )
	Hooks:Call( "MenuManagerPostInitialize", self )
end

Hooks:RegisterHook( "MenuManagerOnOpenMenu" )
function MenuManager.open_menu( self, menu_name, position )
	self.orig.open_menu( self, menu_name, position )
	Hooks:Call( "MenuManagerOnOpenMenu", self, menu_name, position )
end

function MenuManager.open_node( self, node_name, parameter_list )
	self.orig.open_node( self, node_name, parameter_list )
end

function MenuManager:show_download_progress( mod_name )

	local dialog_data = {}
	dialog_data.title = managers.localization:text("base_mod_download_downloading_mod", { ["mod_name"] = mod_name })
	dialog_data.mod_name = mod_name or "No Mod Name"

	local ok_button = {}
	ok_button.cancel_button = true
	ok_button.text = managers.localization:text("dialog_ok")

	dialog_data.focus_button = 1
	dialog_data.button_list = {
		ok_button
	}

	managers.system_menu:show_download_progress( dialog_data )

end

-- Add menus
Hooks:Add( "MenuManagerPostInitialize", "MenuManagerPostInitialize_Base", function( menu_manager )

	local success, err = pcall(function()

		-- Setup lua mods menu
		menu_manager:_base_process_menu(
			{"menu_main"},
			"mods_options",
			"options",
			"MenuManager_Base_SetupModsMenu",
			"MenuManager_Base_PopulateModsMenu",
			"MenuManager_Base_BuildModsMenu"
		)

		-- Setup mod options/keybinds menu
		menu_manager:_base_process_menu(
			{"menu_main", "menu_pause"},
			"video",
			"options",
			"MenuManager_Base_SetupModOptionsMenu",
			"MenuManager_Base_PopulateModOptionsMenu",
			"MenuManager_Base_BuildModOptionsMenu"
		)

		-- Allow custom menus on the main menu (and lobby) and the pause menu 
		menu_manager:_base_process_menu( {"menu_main"} )
		menu_manager:_base_process_menu( {"menu_pause"} )

	end)
	if not success then
		log("[Error] " .. tostring(err))
	end

end )

function MenuManager._base_process_menu( menu_manager, menu_names, parent_menu_name, parent_menu_button, setup_hook, populate_hook, build_hook )

	for k, v in pairs( menu_names ) do

		local menu = menu_manager._registered_menus[ v ]
		if menu then

			local nodes = menu.logic._data._nodes
			local hook_id_setup = setup_hook or "MenuManagerSetupCustomMenus"
			local hook_id_populate = populate_hook or "MenuManagerPopulateCustomMenus"
			local hook_id_build = build_hook or "MenuManagerBuildCustomMenus"

			MenuHelper:SetupMenu( nodes, parent_menu_name or "video" )
			MenuHelper:SetupMenuButton( nodes, parent_menu_button or "options" )

			Hooks:RegisterHook( hook_id_setup )
			Hooks:RegisterHook( hook_id_populate )
			Hooks:RegisterHook( hook_id_build )

			Hooks:Call( hook_id_setup, menu_manager, nodes )
			Hooks:Call( hook_id_populate, menu_manager, nodes )
			Hooks:Call( hook_id_build, menu_manager, nodes )

		end

	end

end

-- Add lua mods menu
function ModMenuCreator.modify_node(self, original_node, data)

	ModMenuCreator._mod_menu_modifies = {
		[ LuaModManager.Constants._lua_mods_menu_id ] = "create_lua_mods_menu"
	}

	local node_name = original_node._parameters.name
	if self._mod_menu_modifies then
		if self._mod_menu_modifies[node_name] then

			local func = self._mod_menu_modifies[node_name]
			local node = original_node
			self[func](self, node, data)

			return node

		end
	end

	return self.orig.modify_node(self, original_node, data)

end

function ModMenuCreator.create_lua_mods_menu(self, node)

	node:clean_items()
	
	local C = LuaModManager.Constants
	local sorted_mods = {}
	local mods = {}
	local conflicted_content = {}
	local modded_content = {}

	local text = function(str)
		return managers.localization:text(str)
	end

	local add_hooks_list = function( content_table, hooks_table, title )
		local _hooks = {}
		local hooks_str = ""
		if type(hooks_table) == "table" then
			for x, y in pairs( hooks_table ) do
				local hook = y[ C.mod_hook_id_key ]
				if not _hooks[ hook ] then
					hooks_str = hooks_str .. "    " .. tostring(hook) .. "\n"
					_hooks[ hook ] = true
				end
			end
		end
		if not string.is_nil_or_empty(hooks_str) then
			table.insert( content_table, text(title) .. ":\n" .. hooks_str )
		end
	end

	local add_persist_scripts_list = function( content_table, persist_table, title )
		local str = ""
		if type( persist_table ) == "table" then
			local pattern = "    [{1}] = {2}\n"
			for k, v in pairs( persist_table ) do
				str = str .. pattern
				str = str:gsub("{1}", v[C.mod_persists_global_key])
				str = str:gsub("{2}", v[C.mod_script_path_key])
			end
		end
		if not string.is_nil_or_empty(str) then
			table.insert( content_table, text(title) .. ":\n" .. str )
		end
	end

	local add_keybinds_list = function( content_table, keybinds_table, title )
		local str = ""
		if type( keybinds_table ) == "table" then
			local pattern = "    [{1}] {2}"
			for id, keybind in pairs( keybinds_table ) do
				
				local keybind_id = keybind[ C.mod_keybind_id_key ]
				local keybind_name = keybind[ C.mod_keybind_name_key ]
				local key = LuaModManager:GetPlayerKeybind( keybind_id ) or "Not Set"
				
				str = str .. pattern
				str = str:gsub("{1}", key)
				str = str:gsub("{2}", keybind_name)

			end
		end
		if not string.is_nil_or_empty(str) then
			table.insert( content_table, text(title) .. ":\n" .. str )
		end
	end

	for k, v in pairs( LuaModManager.Mods ) do

		local mod_disabled = not LuaModManager:WasModEnabledAtLoadTime( v.path )
		local mod_flagged = LuaModManager:IsFlaggedForEnabledChange( v.path )
		local path = v.path
		local info = v.definition
		local mod_tag = " [{0}]"
		local mod_id = info[ C.mod_name_key ] or text("base_mod_info_no_name")
		local mod_name = mod_id
		local mod_desc = info[ C.mod_desc_key ] or text("base_mod_info_no_desc")
		local mod_version = info[ C.mod_version_key ] or nil
		local mod_author = info[ C.mod_author_key ] or text("base_mod_info_no_author")
		local mod_contact = info[ C.mod_contact_key ] or text("base_mod_info_no_contact")
		local mod_hooks = info[ C.mod_hooks_key ] or text("base_mod_info_no_hooks")
		local mod_prehooks = info[ C.mod_prehooks_key ] or text("base_mod_info_no_prehooks")
		local mod_persist_scripts = info[ C.mod_persists_key ] or text("base_mod_info_no_persist_scripts")
		local mod_keybinds = info[ C.mod_keybinds_key ] or text("base_mod_info_no_keybinds")

		local name_iterator = 0
		while mods[mod_id] ~= nil do
			name_iterator = name_iterator + 1
			mod_id = string.format("%s_%i", mod_name, name_iterator)
		end

		table.insert(sorted_mods, mod_id)
		mods[mod_id] = {
			content = {},
			conflicted = {},
			title = nil
		}

		local title_str = mod_name
		if mod_flagged then
			title_str = title_str .. mod_tag:gsub( "{0}", mod_disabled and text("base_mod_info_to_be_enabled") or text("base_mod_info_to_be_disabled") )
		else
			title_str = title_str .. ( mod_disabled and mod_tag:gsub( "{0}", text("base_mod_info_disabled") ) or "" )
		end
		mods[mod_id].title = title_str

		local content = mods[mod_id].content
		table.insert( content, mod_desc )
		if mod_version then
			table.insert( content, text("base_mod_info_version") .. ": " .. mod_version )
		end
		table.insert( content, text("base_mod_info_author") .. ": " .. mod_author )
		table.insert( content, text("base_mod_info_contact") .. ": " .. mod_contact )
		table.insert( content, text("base_mod_info_path") .. ": " .. path )
		add_keybinds_list( content, mod_keybinds, "base_mod_info_keybinds" )
		add_hooks_list( content, mod_prehooks, "base_mod_info_prehooks" )
		add_hooks_list( content, mod_hooks, "base_mod_info_hooks" )
		add_persist_scripts_list( content, mod_persist_scripts, "base_mod_info_persist" )

		MenuCallbackHandler.base_toggle_lua_mod = function(self, item)
			if item and item._parameters.mod_path then

				local path_id = item._parameters.mod_path
				local text_id = item._parameters.text_id
				LuaModManager:ToggleModState( path_id )

				local disabled = item:value() == "off" and true or false
				local node_title = ""
				if C.always_active_mods[path_id] then
					item:set_value("on")
					node_title = text_id .. mod_tag:gsub( "{0}", text("base_mod_info_disabled_impossible") )
				else
					node_title = text_id .. mod_tag:gsub( "{0}", disabled and text("base_mod_info_to_be_disabled") or text("base_mod_info_to_be_enabled") )
				end
				node._parameters.mods[text_id].title = node_title

				local gui = MenuCallbackHandler._current_mod_info_gui
				if gui then
					gui:set_mod_info(item)
				end

			end
		end

		local params = {
			name = mod_id,
			text_id = mod_name,
			mod_path = path,
			localize = false,
			enabled = true,
			callback = "base_toggle_lua_mod",
			hightlight_color = mod_disabled and tweak_data.menu.default_disabled_text_color,
			row_item_color = mod_disabled and tweak_data.menu.default_disabled_text_color,
		}
		local toggle = self:create_toggle(node, params)
		toggle:set_value( mod_disabled and "off" or "on" )
		if mod_flagged then
			toggle:set_value( mod_disabled and "on" or "off" )
		end

	end

	self:add_back_button(node)

	node:parameters().mods = mods
	node:parameters().sorted_mods = sorted_mods
	node:parameters().conflicted_content = conflicted_content
	node:parameters().modded_content = modded_content


end

function MenuModInfoGui.set_mod_info(self, item)

	-- This is ugly as hell, but at least it gets us an easily usable reference to the mod gui
	MenuCallbackHandler._current_mod_info_gui = self

	self.mod_info_panel:clear()

	if alive(self._scroll_bar_panel) then
		self.safe_rect_panel:remove(self._scroll_bar_panel)
	end

	self._scroll_bar_panel = nil
	if self._scroll_up_box then
		self._scroll_up_box:close()
		self._scroll_up_box = nil
	end

	if self._scroll_down_box then
		self._scroll_down_box:close()
		self._scroll_down_box = nil
	end

	if self.safe_rect_panel:child("info_title") then
		self.safe_rect_panel:remove(self.safe_rect_panel:child("info_title"))
	end

	local params = item:parameters() or {}
	if params.back or params.pd2_corner then
		return
	end

	local mods = self.node:parameters().mods
	local modded_content = self.node:parameters().modded_content
	local mod_name = params.name
	local mod_data = mods and mods[mod_name]
	local conflicted_panel = self.mod_info_panel:panel({name = "conflicted", y = 10})
	local modded_panel = self.mod_info_panel:panel({name = "modded"})
	local title = self.safe_rect_panel:text({
		name = "info_title",
		text = managers.localization:to_upper_text("menu_mods_info_title", {mod = mod_name}),
		font = self.medium_font,
		font_size = self.medium_font_size,
		layer = 1
	})

	self.make_fine_text(title)

	if mod_data then

		local text = conflicted_panel:text({
			text = managers.localization:to_upper_text("menu_mods_conflict_title"),
			font = self.medium_font,
			font_size = self.medium_font_size,
			layer = 1,
			x = 10,
			y = 0,
			w = conflicted_panel:w() - 20
		})

		local _, _, _, h = text:text_rect()
		text:set_h(h)
		local cy = h
		local conflict_text_title = text
		conflict_text_title:hide()

		local text = modded_panel:text({
			text = mod_data.title or managers.localization:to_upper_text("menu_mods_modded_title"),
			font = self.medium_font,
			font_size = self.medium_font_size,
			layer = 1,
			x = 10,
			y = 0,
			w = conflicted_panel:w() - 20
		})

		local _, _, _, h = text:text_rect()
		text:set_h(h)
		local my = h
		local mod_text_title = text
		mod_text_title:hide()
		local conflicted_mods = {}

		for _, path in ipairs(mod_data.content) do

			if mod_data.conflicted[Idstring(path):key()] then

				for _, conflict_mod in ipairs(mod_data.conflicted[Idstring(path):key()]) do
					if conflict_mod ~= mod_name then
						conflicted_mods[conflict_mod] = conflicted_mods[conflict_mod] or {}
						table.insert(conflicted_mods[conflict_mod], path)
					end
				end

				conflict_text_title:show()

			else

				text = modded_panel:text({
					text = path,
					font = self.small_font,
					font_size = self.small_font_size,
					layer = 1,
					x = 20,
					y = my,
					w = modded_panel:w() - 30,
					wrap = true
				})
				_, _, _, h = text:text_rect()
				text:set_h(h)
				text:set_color(tweak_data.screen_colors.text)
				my = my + math.ceil(h)
				mod_text_title:show()

			end

		end

		local sorted_conflicts = {}

		for mod, conflicts in pairs(conflicted_mods) do
			table.insert(sorted_conflicts, mod)
		end

		table.sort(sorted_conflicts)

		for _, mod in ipairs(sorted_conflicts) do

			text = conflicted_panel:text({
				text = utf8.to_upper(mod) .. ":",
				font = self.small_font,
				font_size = self.small_font_size,
				layer = 1,
				x = 20,
				y = cy,
				w = conflicted_panel:w() - 30,
				wrap = true
			})
			_, _, _, h = text:text_rect()
			text:set_h(h)
			cy = cy + math.ceil(h)

			for _, path in ipairs(conflicted_mods[mod]) do

				text = conflicted_panel:text({
					text = path,
					font = self.small_font,
					font_size = self.small_font_size,
					layer = 1,
					x = 25,
					y = cy,
					w = conflicted_panel:w() - 35,
					wrap = true
				})
				_, _, _, h = text:text_rect()
				text:set_h(h)
				text:set_color(tweak_data.screen_colors.important_1)
				cy = cy + math.ceil(h)

			end

			cy = cy + 10

		end

		conflicted_panel:set_h(cy)
		modded_panel:set_y(conflict_text_title:visible() and conflicted_panel:bottom() or 10)
		modded_panel:set_h(my)
		self.mod_info_panel:set_y(0)
		self.mod_info_panel:set_h(modded_panel:bottom() + 10)

		if self.mod_info_panel:h() > self._mod_main_panel:h() then

			self._scroll_up_box = BoxGuiObject:new(self._mod_main_panel, {
				sides = {
					0,
					0,
					2,
					0
				}
			})

			self._scroll_down_box = BoxGuiObject:new(self._mod_main_panel, {
				sides = {
					0,
					0,
					0,
					2
				}
			})

			self._scroll_up_box:hide()
			self._scroll_down_box:show()
			self._scroll_bar_panel = self.safe_rect_panel:panel({
				name = "scroll_bar_panel",
				w = 20,
				h = self._mod_main_panel:h()
			})

			self._scroll_bar_panel:set_world_left(self._mod_main_panel:world_right())
			self._scroll_bar_panel:set_world_top(self._mod_main_panel:world_top())

			local texture, rect = tweak_data.hud_icons:get_icon_data("scrollbar_arrow")
			local scroll_up_indicator_arrow = self._scroll_bar_panel:bitmap({
				name = "scroll_up_indicator_arrow",
				texture = texture,
				texture_rect = rect,
				layer = 2,
				color = Color.white,
				blend_mode = "add"
			})

			scroll_up_indicator_arrow:set_center_x(self._scroll_bar_panel:w() / 2)

			local texture, rect = tweak_data.hud_icons:get_icon_data("scrollbar_arrow")
			local scroll_down_indicator_arrow = self._scroll_bar_panel:bitmap({
				name = "scroll_down_indicator_arrow",
				texture = texture,
				texture_rect = rect,
				layer = 2,
				color = Color.white,
				rotation = 180,
				blend_mode = "add"
			})

			scroll_down_indicator_arrow:set_bottom(self._scroll_bar_panel:h())
			scroll_down_indicator_arrow:set_center_x(self._scroll_bar_panel:w() / 2)

			local bar_h = scroll_down_indicator_arrow:top() - scroll_up_indicator_arrow:bottom()
			self._scroll_bar_panel:rect({
				color = Color.black,
				alpha = 0.05,
				y = scroll_up_indicator_arrow:bottom(),
				h = bar_h,
				w = 4
			}):set_center_x(self._scroll_bar_panel:w() / 2)
			bar_h = scroll_down_indicator_arrow:bottom() - scroll_up_indicator_arrow:top()

			local scroll_bar = self._scroll_bar_panel:panel({
				name = "scroll_bar",
				layer = 2,
				h = bar_h
			})

			local scroll_bar_box_panel = scroll_bar:panel({
				name = "scroll_bar_box_panel",
				w = 4,
				halign = "scale",
				valign = "scale"
			})

			self._scroll_bar_box_class = BoxGuiObject:new(scroll_bar_box_panel, {
				sides = {
					2,
					2,
					0,
					0
				}
			})

			self._scroll_bar_box_class:set_aligns("scale", "scale")
			self._scroll_bar_box_class:set_blend_mode("add")
			scroll_bar_box_panel:set_w(8)
			scroll_bar_box_panel:set_center_x(scroll_bar:w() / 2)
			scroll_bar:set_top(scroll_up_indicator_arrow:top())
			scroll_bar:set_center_x(scroll_up_indicator_arrow:center_x())
			self:set_scroll_indicators(0)

		end

	end

end

-- Create this function if it doesn't exist
function MenuCallbackHandler.can_toggle_chat( self )
	if managers and managers.menu then
		local input = managers.menu:active_menu() and managers.menu:active_menu().input
		return not input or input.can_toggle_chat and input:can_toggle_chat()
	else
		return true
	end
end
