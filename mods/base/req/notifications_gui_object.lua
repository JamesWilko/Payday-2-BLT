
NotificationsGuiObject = NotificationsGuiObject or class()
NotificationsGuiObject._edge_padding = 2

local HIGHLIGHT_VISIBLE = 0.3
local HIGHLIGHT_INVISIBLE = 0
local VISIBILITY_THRESHOLD = 0.001
local CHANGE_NOTIF_THRESHOLD = 0.15
local HIGHLIGHT_PADDING = 2

function NotificationsGuiObject:init(ws)

	local panel = ws:panel():panel()
	managers.menu_component:close_contract_gui()

	local next_level_data = managers.experience:next_level_data() or {}
	local font = tweak_data.menu.pd2_small_font
	local font_size = tweak_data.menu.pd2_small_font_size
	local max_left_len = 0
	local max_right_len = 0
	local extra_w = font_size * 4
	local icon_size = 16

	local highlight_rect = panel:rect({
		name = "highlight",
		color = tweak_data.screen_colors.button_stage_3,
		alpha = HIGHLIGHT_INVISIBLE,
		blend_mode = "add",
		layer = 0,
	})

	local highlight_left_rect = panel:rect({
		name = "highlight_left",
		color = tweak_data.screen_colors.button_stage_3,
		alpha = HIGHLIGHT_INVISIBLE,
		blend_mode = "add",
		layer = 0,
	})

	local highlight_right_rect = panel:rect({
		name = "highlight_right",
		color = tweak_data.screen_colors.button_stage_3,
		alpha = HIGHLIGHT_INVISIBLE,
		blend_mode = "add",
		layer = 0,
	})

	local update_icon = panel:bitmap({
		texture = "guis/textures/pd2/blackmarket/inv_newdrop",
		w = icon_size,
		h = icon_size,
		x = 10,
		y = 10,
		color = Color.white:with_alpha(0),
		blend_mode = "add",
		layer = 2
	})
	extra_w = extra_w - icon_size

	local heat_glow = panel:bitmap({
		texture = "guis/textures/pd2/hot_cold_glow",
		layer = 1,
		w = 32,
		h = 32,
		blend_mode = "add",
		color = Color.yellow:with_alpha(0),
	})
	heat_glow:set_center(10 + icon_size / 2 - 4, 10 + icon_size / 2)

	local focus = ws:panel():bitmap({
		name = "focus",
		texture = "guis/textures/crimenet_map_circle",
		layer = 10,
		color = Color.white:with_alpha(0),
		blend_mode = "add",
		w = 0,
		h = 0,
	})
	focus:set_center(10 + icon_size / 2 - 4, 10 + icon_size / 2)

	local notification_title_text = panel:text({
		font = font,
		font_size = font_size,
		text = managers.localization:text("base_mod_notifications_none"),
		y = 10,
		color = tweak_data.screen_colors.text
	})
	self:_make_fine_text(notification_title_text)
	notification_title_text:set_left(math.round(update_icon:right()))
	max_left_len = math.max(max_left_len, notification_title_text:w())

	local total_money_text = panel:text({
		text = self:get_text("hud_offshore_account") .. ": " .. managers.experience:cash_string(managers.money:offshore()),
		font_size = font_size,
		font = font,
		color = tweak_data.screen_colors.text:with_alpha(0)
	})
	self:_make_fine_text(total_money_text)
	total_money_text:set_left(math.round(update_icon:right()))
	total_money_text:set_top(math.round(notification_title_text:bottom()))
	total_money_text:set_height(0)
	max_left_len = math.max(max_left_len, total_money_text:w())

	local notification_message_text = panel:text({
		text = "",
		font_size = font_size,
		font = font,
		color = tweak_data.screen_colors.text
	})
	self:_make_fine_text(notification_message_text)
	notification_message_text:set_left(math.round(update_icon:right()))
	notification_message_text:set_top(math.round(total_money_text:bottom()))
	max_left_len = math.max(max_left_len, notification_message_text:w())

	local font_scale = 1
	local mastermind_ponts, num_skills = managers.skilltree:get_tree_progress("mastermind")
	mastermind_ponts = string.format("%02d", mastermind_ponts)
	local mastermind_text = panel:text({
		text = self:get_text("menu_profession_progress", {
			profession = self:get_text("st_menu_mastermind"),
			progress = mastermind_ponts,
			num_skills = num_skills
		}),
		font_size = font_size * font_scale,
		font = font,
		color = tweak_data.screen_colors.text:with_alpha(0),
		y = 10
	})
	self:_make_fine_text(mastermind_text)
	max_right_len = math.max(max_right_len, mastermind_text:w()) + extra_w

	local prev_notification_text = panel:text({
		font = font,
		font_size = font_size,
		text = managers.localization:text("base_mod_notifications_prev"),
		x = 10,
		y = 10,
		color = tweak_data.screen_colors.text:with_alpha( 0.3 )
	})
	self:_make_fine_text(prev_notification_text)

	local next_notification_text = panel:text({
		font = font,
		font_size = font_size,
		text = managers.localization:text("base_mod_notifications_next"),
		x = 10,
		y = 10,
		color = tweak_data.screen_colors.text:with_alpha( 0.3 )
	})
	self:_make_fine_text(next_notification_text)

	local notification_count_text = panel:text({
		font = font,
		font_size = font_size * 0.8,
		text = managers.localization:text("base_mod_notifications_count", {["current"] = 1, ["max"] = 1}),
		x = 10,
		y = 10,
		color = tweak_data.screen_colors.text:with_alpha( 0.3 )
	})
	self:_make_fine_text(notification_count_text)

	self._panel = panel
	self._panel:set_size(update_icon:w() + max_left_len + 15 + max_right_len + 10, math.max(notification_message_text:bottom(), mastermind_text:bottom()) + 8)
	self._panel:set_bottom(self._panel:parent():h() - 200)
	self._panel_box = BoxGuiObject:new(self._panel, {
		sides = {
			1,
			1,
			1,
			1
		}
	})

	mastermind_text:set_right(self._panel:w() - 10)
	update_icon:move(-5, 0)
	self:_rec_round_object(panel)

	self._highlight_rect = highlight_rect
	self._highlight_left_rect = highlight_left_rect
	self._highlight_right_rect = highlight_right_rect
	self._heat_glow = heat_glow
	self._focus = focus
	self._update_icon = update_icon
	self._notification_title_text = notification_title_text
	self._notification_message_text = notification_message_text
	self._total_money_text = total_money_text
	self._mastermind_text = mastermind_text
	self._max_left_len = max_left_len
	self._max_right_len = max_right_len

	self._prev_notification_text = prev_notification_text
	self._next_notification_text = next_notification_text
	self._notification_count_text = notification_count_text

	self._hovering_on_notification = false

	self:LoadNotifications()

end

function NotificationsGuiObject:_rec_round_object(object)
	local x, y, w, h = object:shape()
	object:set_shape(math.round(x), math.round(y), math.round(w), math.round(h))
	if object.children then
		for i, d in ipairs(object:children()) do
			self:_rec_round_object(d)
		end
	end
end

function NotificationsGuiObject:get_text(text, macros)
	return utf8.to_upper(managers.localization:text(text, macros))
end

function NotificationsGuiObject:_make_fine_text(text)
	local x, y, w, h = text:text_rect()
	text:set_size(w, h)
	text:set_position(math.round(text:x()), math.round(text:y()))
end

function NotificationsGuiObject:close()

	if self._panel and alive(self._panel) then

		if self._focus and alive(self._focus) then
			self._panel:parent():remove( self._focus )
			self._focus = nil
		end

		self._panel:parent():remove(self._panel)
		self._panel = nil

	end

end

function NotificationsGuiObject:LoadNotifications()

	local notifs = NotificationsManager:GetNotifications()
	if #notifs < 1 then

		self._notification_title_text:set_text( managers.localization:text("base_mod_notifications_none") )
		self._notification_message_text:set_text( "" )
		self:update()

	else

		local notif = NotificationsManager:GetCurrentNotification()
		if self._panel and alive(self._panel) then

			self._notification_title_text:set_text( notif.title )
			self._notification_message_text:set_text( notif.message )

			self._update_icon:set_color( Color.white:with_alpha(notif.read and 0 or 1) )
			self._heat_glow:set_color( Color.yellow:with_alpha(notif.read and 0 or 0.7) )

			self:update()

			DelayedCalls:Add("MarkNotificationAsRead", 0.2, function()
				NotificationsManager:MarkNotificationAsRead( notif.id )
			end)

		end

	end

end

function NotificationsGuiObject:update()

	local update_icon = self._update_icon
	local notification_title_text = self._notification_title_text
	local notification_message_text = self._notification_message_text
	local total_money_text = self._total_money_text
	local mastermind_text = self._mastermind_text

	if alive(update_icon) and alive(notification_title_text) and alive(notification_message_text) and alive(total_money_text) and alive(mastermind_text) then

		self:_make_fine_text(notification_title_text)
		notification_title_text:set_left(math.round(update_icon:right()))
		self._max_left_len = math.max(self._max_left_len, notification_title_text:w())

		self:_make_fine_text(notification_message_text)
		notification_message_text:set_left(math.round(update_icon:right()))
		notification_message_text:set_top(math.round(total_money_text:bottom()))
		self._max_left_len = math.max(self._max_left_len, notification_message_text:w())

		self._panel:set_size(self._panel:w(), math.max(notification_message_text:bottom(), mastermind_text:bottom()) + 8)
		self._panel:set_bottom(self._panel:parent():h() - 200)

		self._panel_box:close()
		self._panel_box = BoxGuiObject:new(self._panel, {
			sides = {
				1,
				1,
				1,
				1
			}
		})

	end

	local prev_notification_text = self._prev_notification_text
	local next_notification_text = self._next_notification_text
	local notification_count_text = self._notification_count_text

	if alive(prev_notification_text) and alive(next_notification_text) and alive(notification_count_text) then

		local padding = self._edge_padding
		local alpha = #NotificationsManager:GetNotifications() > 1 and 1 or 0

		prev_notification_text:set_left( padding )
		prev_notification_text:set_top( self._panel:h() / 2 - prev_notification_text:h() / 2 )
		prev_notification_text:set_alpha( alpha )

		next_notification_text:set_right( self._panel:w() - padding )
		next_notification_text:set_top( self._panel:h() / 2 - next_notification_text:h() / 2 )
		next_notification_text:set_alpha( alpha )

		notification_count_text:set_left( padding )
		notification_count_text:set_bottom( self._panel:h() - padding )
		notification_count_text:set_alpha( alpha )

		local current = NotificationsManager:GetCurrentNotificationIndex()
		local num_notifs = #NotificationsManager:GetNotifications()
		notification_count_text:set_text( managers.localization:text("base_mod_notifications_count", {["current"] = current, ["max"] = num_notifs}) )

	end

	local highlight_rect = self._highlight_rect
	local highlight_left_rect = self._highlight_left_rect
	local highlight_right_rect = self._highlight_right_rect

	if alive(highlight_rect) and alive(highlight_left_rect) and alive(highlight_right_rect) then
		
		local panel = self._panel

		highlight_rect:set_h( panel:h() - HIGHLIGHT_PADDING * 2 )
		highlight_rect:set_w( panel:w() - HIGHLIGHT_PADDING * 2 )
		highlight_rect:set_top( HIGHLIGHT_PADDING )
		highlight_rect:set_left( HIGHLIGHT_PADDING )

		highlight_left_rect:set_h( panel:h() - HIGHLIGHT_PADDING * 2 )
		highlight_left_rect:set_w( panel:w() * CHANGE_NOTIF_THRESHOLD - HIGHLIGHT_PADDING * 2 )
		highlight_left_rect:set_top( HIGHLIGHT_PADDING )
		highlight_left_rect:set_left( HIGHLIGHT_PADDING )

		highlight_right_rect:set_h( panel:h() - HIGHLIGHT_PADDING * 2 )
		highlight_right_rect:set_w( panel:w() * CHANGE_NOTIF_THRESHOLD - HIGHLIGHT_PADDING * 2 )
		highlight_right_rect:set_top( HIGHLIGHT_PADDING )
		highlight_right_rect:set_right( panel:right() - HIGHLIGHT_PADDING )

	end

end

function NotificationsGuiObject:SetHighlightVisibility( highlight, visible )

	if visible then
		if highlight:alpha() < HIGHLIGHT_VISIBLE - VISIBILITY_THRESHOLD then
			highlight:set_alpha( HIGHLIGHT_VISIBLE )
			managers.menu_component:post_event("highlight")
		end
	else
		if highlight:alpha() > HIGHLIGHT_INVISIBLE + VISIBILITY_THRESHOLD then
			highlight:set_alpha( HIGHLIGHT_INVISIBLE )
		end
	end

end

Hooks:Add("MenuComponentManagerOnMousePressed", "Base_ModUpdates_MenuComponentManagerOnMousePressed", function( menu, o, button, x, y )

	if menu._notifications_gui and menu._notifications_gui._panel and menu._notifications_gui._panel:inside(x, y) then

		local x_percent = ( x - menu._notifications_gui._panel:x() ) / menu._notifications_gui._panel:w()
		local num_notifs = #NotificationsManager:GetNotifications()

		if num_notifs > 1 then
			if x_percent < CHANGE_NOTIF_THRESHOLD then
				NotificationsManager:ShowPreviousNotification()
				menu._notifications_gui:LoadNotifications()
				return true
			end
			if x_percent > (1 - CHANGE_NOTIF_THRESHOLD) then
				NotificationsManager:ShowNextNotification()
				menu._notifications_gui:LoadNotifications()
				return true
			end
		end

		NotificationsManager:ClickNotification()
		return true

	end

end)

Hooks:Add("MenuComponentManagerOnMouseMoved", "Base_ModUpdates_MenuComponentManagerOnMouseMoved", function( menu, o, x, y )

	if menu._notifications_gui then

		local highlighted = false
		local multiple_notifs = #NotificationsManager:GetNotifications() > 1

		if multiple_notifs then

			-- Next notification highlight
			local highlight_right_rect = menu._notifications_gui._highlight_right_rect
			if alive( highlight_right_rect ) then
				local highlight_visible = highlight_right_rect:inside( x, y )
				highlighted = highlighted or highlight_visible
				menu._notifications_gui:SetHighlightVisibility( highlight_right_rect, highlight_visible )
			end

			-- Previous notification highlight
			local highlight_left_rect = menu._notifications_gui._highlight_left_rect
			if alive( highlight_left_rect ) then
				local highlight_visible = highlight_left_rect:inside( x, y ) 
				highlighted = highlighted or highlight_visible
				menu._notifications_gui:SetHighlightVisibility( highlight_left_rect, highlight_visible )
			end

		end

		-- Clickable area highlight
		local highlight_rect = menu._notifications_gui._highlight_rect
		if alive( highlight_rect ) then

			local current_notif = NotificationsManager:GetCurrentNotification()
			local x_percent = ( x - menu._notifications_gui._panel:x() ) / menu._notifications_gui._panel:w()
			local highlight_visible = false
			if multiple_notifs then
				highlight_visible = highlight_rect:inside( x, y ) and x_percent > CHANGE_NOTIF_THRESHOLD and x_percent < (1 - CHANGE_NOTIF_THRESHOLD)
			else
				highlight_visible = highlight_rect:inside( x, y )
			end
			if current_notif and not current_notif.callback then
				highlight_visible = false
			end

			menu._notifications_gui:SetHighlightVisibility( highlight_rect, highlight_visible )
			highlighted = highlighted or highlight_visible

		end

		menu._notifications_gui._hovering_on_notification = highlighted
		if highlighted then
			return true, "link"
		else
			if alive(highlight_rect) and highlight_rect:inside( x, y ) then
				return true, "arrow"
			end
		end

	end

end)

Hooks:Add("LogicOnSelectNode", "Base_ModUpdates_LogicOnSelectNode", function()

	local node = managers.menu:active_menu().logic:selected_node()
	if node and node._default_item_name and node._default_item_name ~= "crimenet" then

		if managers.menu_component and managers.menu_component._notifications_gui then
			managers.menu_component._notifications_gui:close()
		end

	end

end)

Hooks:Add("NotificationManagerOnNotificationsUpdated", "NotificationManagerOnNotificationsUpdated_NotificationsGUI", function(notify, notifications)
	if managers.menu_component and managers.menu_component._notifications_gui then
		managers.menu_component._notifications_gui:LoadNotifications()
	end
end)
