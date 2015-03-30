
QuickMenu = QuickMenu or class()
QuickMenu._menu_id_key = "quick_menu_id_"
QuickMenu._menu_id_index = 0

function QuickMenu:new( ... )
	return self:init( ... )
end

function QuickMenu:init( title, text, options, show_immediately )

	QuickMenu._menu_id_index = QuickMenu._menu_id_index + 1
	self.dialog_data = {
		id = QuickMenu._menu_id_key .. tostring( QuickMenu._menu_id_index ),
		title = title,
		text = text,
		button_list = {},
	}
	self.visible = false

	local add_default = false
	if (not options) or (options ~= nil and type(options) ~= "table") or (options ~= nil and type(options) == "table" and #options == 0) then
		add_default = true
	end
	if add_default then
		local tbl = {
			text = "OK",
			is_cancel_button = true,
		}
		table.insert( options, tbl )
	end

	for k, option in ipairs( options ) do

		option.data = option.data
		option.callback = option.callback

		local button = {}
		local callback_data = {
			data = option.data,
			callback = option.callback
		}
		button.text = option.text
		button.callback_func = callback( self, self, "_callback", callback_data )
		button.cancel_button = option.is_cancel_button or false

		if option.is_focused_button then
			self.dialog_data.focus_button = #self.dialog_data.button_list + 1
		end

		table.insert( self.dialog_data.button_list, button )

	end

	if show_immediately then
		self:show()
	end

	return self

end

function QuickMenu:_callback( callback_data )

	if callback_data.callback then
		callback_data.callback( callback_data.data )
	end

	self.visible = false

end

function QuickMenu:Show()

	if not self.visible then
		self.visible = true
		managers.system_menu:show( self.dialog_data )
	end

end

function QuickMenu:show()
	self:Show()
end

function QuickMenu:Hide()

	if self.visible then
		managers.system_menu:close( self.dialog_data.id )
		self.visible = false
	end

end

function QuickMenu:hide()
	self:Hide()
end
