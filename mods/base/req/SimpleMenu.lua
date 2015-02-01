
-- SimpleMenu
-- by Harfatus

if not SimpleMenu then

	SimpleMenu = class()

	function SimpleMenu:New(title, message, options)
		return self:Init(title, message, options)
	end

	function SimpleMenu:Init(title, message, options)

		self.dialog_data = { title = title, text = message, button_list = {}, id = tostring(math.random(0,0xFFFFFFFF)) }
		self.visible = false

		for _, opt in ipairs(options) do

			local elem = {}
			elem.text = opt.text
			opt.data = opt.data or nil
			opt.callback = opt.callback or nil
			elem.callback_func = callback( self, self, "_do_callback", { data = opt.data, callback = opt.callback } )
			elem.cancel_button = opt.is_cancel_button or false

			if opt.is_focused_button then
				self.dialog_data.focus_button = #self.dialog_data.button_list+1
			end

			table.insert(self.dialog_data.button_list, elem)

		end

		return self

	end

	function SimpleMenu:_do_callback(info)

		if info.callback then
			if info.data then
				info.callback(info.data)
			else
				info.callback()
			end
		end

		self.visible = false

	end

	function SimpleMenu:Show()

		if self.visible then
			return
		end

		self.visible = true
		managers.system_menu:show(self.dialog_data)

	end

	function SimpleMenu:Hide()

		if self.visible then
			managers.system_menu:close(self.dialog_data.id)
			self.visible = false
			return
		end
		
	end

end

patched_update_input = patched_update_input or function (self, t, dt )

	if self._data.no_buttons then
		return
	end
	
	local dir, move_time
	local move = self._controller:get_input_axis( "menu_move" )

	if( self._controller:get_input_bool( "menu_down" )) then
		dir = 1
	elseif( self._controller:get_input_bool( "menu_up" )) then
		dir = -1
	end
	
	if dir == nil then
		if move.y > self.MOVE_AXIS_LIMIT then
			dir = 1
		elseif move.y < -self.MOVE_AXIS_LIMIT then
			dir = -1
		end
	end

	if dir ~= nil then
		if( ( self._move_button_dir == dir ) and self._move_button_time and ( t < self._move_button_time + self.MOVE_AXIS_DELAY ) ) then
			move_time = self._move_button_time or t
		else
			self._panel_script:change_focus_button( dir )
			move_time = t
		end
	end

	self._move_button_dir = dir
	self._move_button_time = move_time
	
	local scroll = self._controller:get_input_axis( "menu_scroll" )
	-- local sdir
	if( scroll.y > self.MOVE_AXIS_LIMIT ) then
		self._panel_script:scroll_up()
		-- sdir = 1
	elseif( scroll.y < -self.MOVE_AXIS_LIMIT ) then
		self._panel_script:scroll_down()
		-- sdir = -1
	end

end

Hooks:Add( "MenuManagerInitialize", "MenuManagerInitialize_InitSimpleMenu", function( menu_manager )
	managers.system_menu.DIALOG_CLASS.update_input = patched_update_input
	managers.system_menu.GENERIC_DIALOG_CLASS.update_input = patched_update_input
end )
