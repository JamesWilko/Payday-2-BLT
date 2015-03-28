
NotificationsManager = NotificationsManager or {}
local Notify = NotificationsManager
Notify._notifications = {}
Notify._current_notification = 1
Notify._NOTIFICATION_TIME = 6.5
Notify._time_to_next_notification = Notify._NOTIFICATION_TIME

Hooks:RegisterHook("NotificationManagerOnNotificationsUpdated")

function Notify:GetNotifications()
	return self._notifications
end

function Notify:GetCurrentNotification()
	return self._notifications[ self._current_notification ]
end

function Notify:GetCurrentNotificationIndex()
	return self._current_notification
end

function Notify:AddNotification( id, title, message, priority, callback )

	if not id then
		log("[Error] Attempting to add notification with no id!")
		return false
	end

	for k, v in ipairs( self._notifications ) do
		if v.id == id then
			local error_str = ("[Error] Notification already has a notification with id '{1}'! Can not add duplicate id's!"):gsub("{1}", id)
			log( error_str )
			return false
		end
	end

	local tbl = {
		id = id,
		title = title or "",
		message = message or "",
		priority = priority or 0,
		callback = callback or nil,
		read = false
	}

	table.insert( self._notifications, tbl )
	table.sort( self._notifications, function(a, b)
		return a.priority > b.priority
	end)

	self:_OnUpdated()
	return true

end

function Notify:UpdateNotification( id, new_title, new_message, new_priority, new_callback )

	if not id then
		log("[Error] Attempting to update notification with no id!")
		return false
	end
	
	local updated = false
	for k, v in ipairs( self._notifications ) do

		if v.id == id then

			v.title = new_title or v.title
			v.message = new_message or v.message
			v.priority = new_priority or v.priority
			v.callback = new_callback or v.callback
			v.read = false

			updated = true

		end

	end

	if not updated then
		local error_str = ("[Warning] Could not find notification with id '{1}', it has not been updated!"):gsub("{1}", id)
		log( error_str )
		return false
	end

	self:_OnUpdated()
	return true

end

function Notify:RemoveNotification( id )

	if not id then
		log("[Error] Attempting to remove notification with no id!")
		return false
	end

	local tbl = {}

	for k, v in ipairs( self._notifications ) do
		if v.id ~= id then
			table.insert( tbl, v )
		end
	end

	self._notifications = tbl
	self:_OnUpdated()
	return true

end

function Notify:ClearNotifications()
	self._notifications = {}
	self:_OnUpdated()
end

function Notify:NotificationExists( id )

	for k, v in ipairs( self._notifications ) do
		if v.id == id then
			return true
		end
	end

	return false

end

function Notify:ShowNextNotification( suppress_sound )

	self._current_notification = self._current_notification + 1
	if self._current_notification > #self._notifications then
		self._current_notification = 1
	end
	if not suppress_sound then
		managers.menu_component:post_event("highlight")
	end
	self:_OnUpdated()
	self:_ResetTimeToNextNotification()

end

function Notify:ShowPreviousNotification( suppress_sound )

	self._current_notification = self._current_notification - 1
	if self._current_notification < 1 then
		self._current_notification = #self._notifications
	end
	if not suppress_sound then
		managers.menu_component:post_event("highlight")
	end
	self:_OnUpdated()
	self:_ResetTimeToNextNotification()

end

function Notify:ClickNotification( suppress_sound )

	local notif = self:GetCurrentNotification()
	if notif and notif.callback then
		notif.callback()
		if not suppress_sound then
			managers.menu_component:post_event("menu_enter")
		end
	end

end

function Notify:MarkNotificationAsRead( id )

	if not id then
		log("[Error] Attempting to mark notification with no id!")
		return false
	end

	for k, v in ipairs( self._notifications ) do
		if v.id == id then
			v.read = true
			return true
		end
	end

	return false

end

function Notify:_OnUpdated()
	if not self:GetCurrentNotification().read then
		managers.menu_component:post_event("job_appear")
	end
	Hooks:Call("NotificationManagerOnNotificationsUpdated", self, self._notifications)
end

function Notify:_ResetTimeToNextNotification()
	NotificationsManager._time_to_next_notification = NotificationsManager._NOTIFICATION_TIME
end

-- Auto-scroll notifications
Hooks:Add("MenuUpdate", "Base_Notifications_MenuUpdate", function(t, dt)

	if #NotificationsManager:GetNotifications() > 1 then

		local hovering = false
		if managers.menu_component._notifications_gui then
			hovering = managers.menu_component._notifications_gui._hovering_on_notification
		end

		if not hovering then

			NotificationsManager._time_to_next_notification = NotificationsManager._time_to_next_notification - dt
			if NotificationsManager._time_to_next_notification <= 0 then
				NotificationsManager:ShowNextNotification( true )
				NotificationsManager:_ResetTimeToNextNotification()
			end

		end

	end

end)

-- Add notifications GUI to main menu
Hooks:Add("MenuComponentManagerInitialize", "Base_Notifications_MenuComponentManagerInitialize", function(menu)

	menu._create_notifications_gui = function( self )
		self:create_notifications_gui()
	end

	menu.create_notifications_gui = function( self )
		self:close_notifications_gui()
		self._notifications_gui = NotificationsGuiObject:new( self._ws )
	end

	menu.refresh_notifications_gui = function( self )
		if self._notifications_gui then
			self:create_notifications_gui()
		end
	end

	menu.close_notifications_gui = function( self )
		if self._notifications_gui then
			self._notifications_gui:close()
			self._notifications_gui = nil
		end
	end

	menu._active_components.lua_mod_notifications = {
		create = callback(menu, menu, "create_notifications_gui"),
		close = callback(menu, menu, "close_notifications_gui")
	}

end)

Hooks:Add("MenuComponentManagerPreSetActiveComponents", "Base_Notifications_MenuComponentManagerPreSetActiveComponents", function(menu, components, node)

	if node then
		local node_name = node._parameters.name
		if node._default_item_name and node_name == "main" then
			table.insert( components, "lua_mod_notifications" )
		end
	end

end)
