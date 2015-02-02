
_G.MenuHelper = _G.MenuHelper or {}
local Menu = _G.MenuHelper

function Menu:SetupMenu( menu, id )
	if menu[id] == nil then
		log("[Error] Could not find '" .. id .. "' in menu!")
		return
	end
	self.menu_to_clone = menu[id]
end

function Menu:SetupMenuButton( menu, id, button_id )
	if menu[id] == nil then
		log("[Error] Could not find '" .. id .. "' in menu!")
		return
	end
	if button_id == nil then
		button_id = 1
	end
	self.menubutton_to_clone = menu[id]:items()[button_id]
end

function Menu:NewMenu( menu_id )

	self.menus = self.menus or {}

	local new_menu = deep_clone( self.menu_to_clone )
	new_menu._items = {}
	self.menus[menu_id] = new_menu

	return new_menu

end

function Menu:GetMenu( menu_id )
	local menu = self.menus[menu_id]
	if menu == nil then
		log("[Error] Could not find menu with id '" .. tostring(menu_id) .. "'!")
	end
	return menu
end

function Menu:AddBackButton( menu_id )
	local menu = self:GetMenu( menu_id )
	MenuManager:add_back_button( menu )
end

function Menu:AddButton( button_data )

	local data = {
		type = "CoreMenuItem.Item",
	}

	local params = {
		name = button_data.id,
		text_id = button_data.title,
		help_id = button_data.desc,
		callback = button_data.callback,
		back_callback = button_data.back_callback,
		disabled_color = Color(0.25, 1, 1, 1),
		next_node = button_data.next_node,
	}

	local menu = self:GetMenu( button_data.menu_id )
	local item = menu:create_item(data, params)
	item._priority = button_data.priority or 0

	menu._items_list = menu._items_list or {}
	table.insert( menu._items_list, item )

end

function Menu:AddDivider( divider_data )

	local data = {
		type = "MenuItemDivider",
		size = divider_data.size or 8,
		no_text = divider_data.no_text or true,
	}

	local params = {
		name = divider_data.id,
	}

	local menu = self:GetMenu( divider_data.menu_id )
	local item = menu:create_item( data, params )
	item._priority = divider_data.priority or 0
	menu._items_list = menu._items_list or {}
	table.insert( menu._items_list, item )

end

function Menu:AddToggle( toggle_data )

	local data = {
		type = "CoreMenuItemToggle.ItemToggle",
		{
			_meta = "option",
			icon = "guis/textures/menu_tickbox",
			value = "on",
			x = 24,
			y = 0,
			w = 24,
			h = 24,
			s_icon = "guis/textures/menu_tickbox",
			s_x = 24,
			s_y = 24,
			s_w = 24,
			s_h = 24
		},
		{
			_meta = "option",
			icon = "guis/textures/menu_tickbox",
			value = "off",
			x = 0,
			y = 0,
			w = 24,
			h = 24,
			s_icon = "guis/textures/menu_tickbox",
			s_x = 0,
			s_y = 24,
			s_w = 24,
			s_h = 24
		}
	}

	local params = {
		name = toggle_data.id,
		text_id = toggle_data.title,
		help_id = toggle_data.desc,
		callback = toggle_data.callback,
		disabled_color = toggle_data.disabled_color or Color( 0.25, 1, 1, 1 ),
		icon_by_text = toggle_data.icon_by_text or false
	}

	local menu = self:GetMenu( toggle_data.menu_id )
	local item = menu:create_item( data, params )
	item:set_value( toggle_data.value and "on" or "off" )
	item._priority = toggle_data.priority or 0
	menu._items_list = menu._items_list or {}
	table.insert( menu._items_list, item )

end

function Menu:AddSlider( slider_data )

	local data = {
		type = "CoreMenuItemSlider.ItemSlider",
		min = slider_data.min or 0,
		max = slider_data.max or 10,
		step = slider_data.step or 1,
		show_value = slider_data.show_value or false
	}

	local params = {
		name = slider_data.id,
		text_id = slider_data.title,
		help_id = slider_data.desc,
		callback = slider_data.callback,
		disabled_color = slider_data.disabled_color or Color( 0.25, 1, 1, 1 ),
	}

	local menu = self:GetMenu( slider_data.menu_id )
	local item = menu:create_item(data, params)
	item:set_value( math.clamp(slider_data.value, data.min, data.max) or data.min )
	item._priority = slider_data.priority or 0

	if slider_data.disabled then
		item:set_enabled( not slider_data.disabled )
	end

	menu._items_list = menu._items_list or {}
	table.insert( menu._items_list, item )

end

function Menu:AddMultipleChoice( multi_data )

	local data = {
		type = "MenuItemMultiChoice"
	}
	for k, v in ipairs( multi_data.items or {} ) do
		table.insert( data, { _meta = "option", text_id = v, value = k } )
	end
	
	local params = {
		name = multi_data.id,
		text_id = multi_data.title,
		help_id = multi_data.desc,
		callback = multi_data.callback,
		filter = true
	}
	
	local menu = self:GetMenu( multi_data.menu_id )
	local item = menu:create_item(data, params)
	item._priority = multi_data.priority or 0
	item:set_value( multi_data.value or 1 )

	menu._items_list = menu._items_list or {}
	table.insert( menu._items_list, item )

end

function Menu:AddKeybinding( button_data )

	local data = {
		type = "MenuItemCustomizeController",
	}

	local params = {
		name = button_data.id,
		text_id = managers.localization:text(button_data.title),
		connection_name = button_data.connection_name,
		binding = button_data.binding,
		localize = "false",
		button = button_data.button,
		callback = button_data.callback,
	}

	local menu = self:GetMenu( button_data.menu_id )
	local item = menu:create_item(data, params)
	item._priority = button_data.priority or 0

	menu._items_list = menu._items_list or {}
	table.insert( menu._items_list, item )

end


function Menu:BuildMenu( menu_id, data )

	-- Check menu exists
	local menu = self.menus[menu_id]
	if menu == nil then
		log("[Error] Attempting to build menu '" .. menu_id .."' which doesn't exist!")
		return
	end

	-- Check items exist for this menu
	if menu._items_list ~= nil then

		local priority_items = {}
		local nonpriority_items = {}
		for k, v in pairs( menu._items_list ) do
			if v._priority ~= nil and v._priority > 0 then
				table.insert( priority_items, v )
			else
				table.insert( nonpriority_items, v )
			end
		end

		-- Sort table by priority, higher priority first
		table.sort( priority_items, function(a, b)
			return a._priority > b._priority
		end)

		-- Sort non-priority items alphabetically
		table.sort( nonpriority_items, function(a, b)
			return managers.localization:text(a._parameters.text_id or "") < managers.localization:text(b._parameters.text_id or "")
		end)

		-- Add items to menu
		for k, item in pairs( priority_items ) do
			menu:add_item( item )
		end
		for k, item in pairs( nonpriority_items ) do
			menu:add_item( item )
		end

		-- Slider dirty callback fix
		for k, item in pairs( menu._items ) do
			if item._type == "slider" or item._parameters.type == "CoreMenuItemSlider.ItemSlider" then
				item.dirty_callback = nil
			end
		end

		-- Back callback
		if data then
			
			if data.back_callback then

				if not menu._parameters.back_callback then
					menu._parameters.back_callback = {}
				end

				if type(data.back_callback) == "table" then
					for k, v in pairs( data.back_callback ) do
						table.insert( menu._parameters.back_callback, v )
					end
				else
					table.insert( menu._parameters.back_callback, data.back_callback )
				end

			end

			if data.area_bg then
				menu._parameters.area_bg = data.area_bg
			end

		end

	end

	-- Add back button to menu
	self:AddBackButton( menu_id )

	-- Build menu data
	self.menus[menu_id] = menu

	return self.menus[menu_id]

end

function Menu:AddMenuItem( parent_menu, child_menu, name, desc, menu_position, subposition )

	if parent_menu == nil then
		log( string.gsub("[Menus][Warning] Parent menu for child '{1}' is null, ignoring...", "{1}", child_menu) )
		return
	end

	-- Put at end of menu
	if menu_position == nil then
		menu_position = #parent_menu._items + 1
	end

	-- Get menu position from string
	if type( menu_position ) == "string" then
		for k, v in pairs( parent_menu._items ) do
			if menu_position == v["_parameters"]["name"] then

				if subposition == nil then
					subposition = "after"
				end

				if subposition == "after" then
					menu_position = k + 1
				else
					menu_position = k
				end

				break

			end
		end
	end

	-- Insert in menu
	local button = deep_clone( self.menubutton_to_clone )
	button._parameters.name = name
	button._parameters.text_id = name
	button._parameters.help_id = desc
	button._parameters.next_node = child_menu
	table.insert( parent_menu._items, menu_position, button )

end

function MenuHelper:LoadFromJsonFile( file_path, parent_class, data_table )

	local file = io.open( file_path, "r" )
	if file then

		local file_content = file:read("*all")
		file:close()

		local content = json.decode( file_content )
		local menu_id = content.menu_id
		local parent_menu = content.parent_menu_id
		local menu_name = content.title
		local menu_desc = content.description
		local items = content.items

		Hooks:Add("MenuManagerSetupCustomMenus", "Base_SetupCustomMenus_Json_" .. menu_id, function( menu_manager, nodes )
			if nodes[parent_menu] then
				MenuHelper:NewMenu( menu_id )
			else
				log("[Error] Could not build menu from json, no node with id: " .. parent_menu)
			end
		end)

		Hooks:Add("MenuManagerBuildCustomMenus", "Base_BuildCustomMenus_Json_" .. menu_id, function( menu_manager, nodes )
			if nodes[parent_menu] then
				nodes[menu_id] = MenuHelper:BuildMenu( menu_id )
				MenuHelper:AddMenuItem( nodes[parent_menu], menu_id, menu_name, menu_desc )
			end
		end)

		Hooks:Add("MenuManagerPopulateCustomMenus", "Base_PopulateCustomMenus_Json_" .. menu_id, function( menu_manager, nodes )

			if not nodes[parent_menu] then
				return
			end

			for k, item in pairs( items ) do

				local type = item.type
				local id = item.id
				local title = item.title
				local desc = item.description
				local callback = item.callback
				local priority = #items - k
				local value = item.default_value
				if data_table then
					value = data_table[item.value] or item.default_value
				end

				if type == "button" then
					MenuHelper:AddButton({
						id = id,
						title = title,
						desc = desc,
						callback = callback,
						next_node = item.next_menu or nil,
						menu_id = menu_id,
						priority = priority,
					})
				end

				if type == "toggle" then
					MenuHelper:AddToggle({
						id = id,
						title = title,
						desc = desc,
						callback = callback,
						value = value,
						menu_id = menu_id,
						priority = priority,
					})
				end

				if type == "slider" then
					MenuHelper:AddSlider({
						id = id,
						title = title,
						desc = desc,
						callback = callback,
						value = value,
						min = item.min or 0,
						max = item.max or 1,
						step = item.step or 0.1,
						show_value = true,
						menu_id = menu_id,
						priority = priority
					})
				end

				if type == "divider" then
					MenuHelper:AddDivider({
						id = "divider_" .. menu_id .. "_" .. tostring(priority),
						size = item.size,
						menu_id = menu_id,
						priority = priority,
					})
				end

				if type == "keybind" then

					local key = ""
					if item.keybind_id then
						key = LuaModManager:GetPlayerKeybind( item.keybind_id ) or ""
					end

					MenuHelper:AddKeybinding({
						id = id,
						title = title,
						desc = desc,
						connection_name = item.keybind_id,
						button = key,
						binding = key,
						menu_id = menu_id,
						priority = priority,
					})

					LuaModManager:AddKeybinding( item.keybind_id, parent_class[item.func] )

				end

				if type == "multiple_choice" then
					MenuHelper:AddMultipleChoice({
						id = id,
						title = title,
						desc = desc,
						callback = callback,
						items = item.items,
						value = value,
						menu_id = menu_id,
						priority = priority,
					})
				end

			end

		end)

	else
		log("[Error] Could not load file: " .. file_path)
	end

end
