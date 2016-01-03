
LuaModUpdates = LuaModUpdates or {}
LuaModUpdates._updates_api_path = "http://api.paydaymods.com/updates/retrieve/?"
LuaModUpdates._updates_api_mod = "mod[{1}]={2}"
LuaModUpdates._updates_download_url = "http://download.paydaymods.com/download/latest/{1}"
LuaModUpdates._updates_notes_url = "http://download.paydaymods.com/download/patchnotes/{1}"
LuaModUpdates._notification_id = "lua_mod_updates_notif"
LuaModUpdates.__required_notification_id = "lua_mod_require_notif"

LuaModUpdates._currently_downloading = {}
LuaModUpdates._current_download_dialog = nil

Hooks:Add("MenuManagerOnOpenMenu", "Base_ModUpdates_MenuManagerOnOpenMenu", function( menu_manager, menu, position )

	-- Check for updates after going to the main menu
	if menu == "menu_main" then

		LuaModUpdates:ShowUpdatesAvailableNotification({})

		if not LuaNetworking:IsMultiplayer() then
			LuaModUpdates:CheckForUpdates( LuaModUpdates.ShowUpdatesAvailableCallback )
		end

		-- Remove temporary hook dll
		LuaModUpdates:RemoveTemporaryDLL()
        
        local required = LuaModManager:Required()
    
        local required_count = table.size(required)
        
        local initial_mod
        
        for _, mod in pairs(required) do
            initial_mod = mod
            break
        end
        
        if required_count == 1 then
            LuaModUpdates:ShowModRequiredMessage(initial_mod)
        elseif required_count > 1 then
            LuaModUpdates:ShowMultiRequiredAvailableMessage( required )
        end
        
        LuaModUpdates:ShowRequiredModsNotification( required )
        
	end
	
end)

function LuaModUpdates:RemoveTemporaryDLL()
	log("[Updates] Attempting to remove temporary hook dll...")
	local hook_result, hook_error = os.remove( LuaModManager.Constants.hook_dll_temp_name )
	if not hook_result then
		log("[Warning] Could not remove hook dll: " .. tostring(hook_error))
	end
end

Hooks:RegisterHook("ModUpdates_CheckedModForUpdates")
function LuaModUpdates:CheckForUpdates( callback )

	local url_path = LuaModUpdates._updates_api_path
	local i = 0

	for k, v in pairs( LuaModManager:UpdateChecks() ) do
		if LuaModManager:AreModUpdatesEnable( v.mod ) then

			if i > 0 then
				url_path = url_path .. "&"
			end

			url_path = url_path .. LuaModUpdates._updates_api_mod:gsub("{1}", i)
			url_path = url_path:gsub("{2}", v.identifier)
			i = i + 1

		end
	end

	if i > 0 then
		LuaModUpdates:FetchUpdatesFromAPI( url_path, callback )
	end

end

function LuaModUpdates:FetchUpdatesFromAPI( path, callback )

	-- Escape characters for URL
	local url = path:gsub(" ", "%%20")
	url = url:gsub("!", "%%21")
	url = url:gsub("#", "%%23")
	url = url:gsub("-", "%%2D")

	-- Get data from API
	dohttpreq( url, function( data, id )
		
		if data:is_nil_or_empty() then
			log("[Error] Could not connect to PaydayMods.com API!")
			return
		end

		local server_data = json.decode( data )
		if server_data then

			for k, v in pairs( server_data ) do
				log(string.format("[Updates] Received update data for '%s', server revision: %i", k, v.revision))
			end

			local mods_needing_updates = {}
            local mods_required = {}
			for k, v in pairs( LuaModManager:UpdateChecks() ) do

				local mod_data = server_data[v.identifier]
				if mod_data then

					local local_version = tonumber( v.revision ) or -1
					local server_version = tonumber( mod_data.revision ) or -1

					v.server_revision = server_version
					v.update_required = local_version < server_version
					if local_version < server_version then
						table.insert( mods_needing_updates, v )
                    end

				else
					log( ("[Updates] Received no update data for '{1}'"):gsub("{1}", v.identifier) ) 
				end

			end
            
			LuaModUpdates:ShowUpdatesAvailableNotification( mods_needing_updates )

			if callback then
				callback( self, mods_needing_updates )
			end
            

		else
			log("[Error] Could not decode server updates data!")
		end

	end )

end

function LuaModUpdates:ShowUpdatesAvailableCallback( mods, required )
	if #mods == 1 then
        LuaModUpdates:ShowUpdateAvailableMessage( mods[1] )
	elseif #mods > 1 then
		LuaModUpdates:ShowMultiUpdateAvailableMessage( mods )
	end
end

function LuaModUpdates.DoUpdateAllModsNow()
	LuaModUpdates.OpenUpdateManagerNode()
	log("[Updates] Updating all mods now...")
end

function LuaModUpdates:ShowUpdatesAvailableNotification( mods_to_update )

	local count = 0
	local message = ""
	for k, v in pairs( mods_to_update ) do
		local loc_table = {
			["mod"] = v.display_name or v.mod_table[ LuaModManager.Constants.mod_name_key ]
		}
		message = message .. managers.localization:text("base_mod_updates_updates_required_row", loc_table) .. "\n"
		count = count + 1
	end
	message = message .. managers.localization:text("base_mod_updates_click_manager")

	local loc_table = {
		["count"] = count,
		["s"] = count > 1 and "s" or "",
	}
	local title = count < 1 and managers.localization:text("base_mod_updates_all_up_to_date") or managers.localization:text("base_mod_updates_updates_required", loc_table)
	local prio = count < 1 and 101 or 1001

	if NotificationsManager:NotificationExists( LuaModUpdates._notification_id ) then
		NotificationsManager:UpdateNotification( LuaModUpdates._notification_id, title, message, prio, LuaModUpdates.NotificationClickCallback )
	else
		NotificationsManager:AddNotification( LuaModUpdates._notification_id, title, message, prio, LuaModUpdates.NotificationClickCallback )
	end

end

function LuaModUpdates:ShowRequiredModsNotification( mods_required )
    if table.size(mods_required) == 0 then
        return
    end
    
	local count = 0
	local message = ""
	for k, v in pairs( mods_required ) do
		local loc_table = {
			["mod"] = v.display_name
		}
		message = message .. managers.localization:text("base_mod_updates_updates_required_row", loc_table) .. "\n"
		count = count + 1
	end
	message = message .. managers.localization:text("base_mod_updates_click_manager")

	local loc_table = {
		["count"] = count,
		["s"] = count > 1 and "s" or "",
	}
	local title = count < 1 and managers.localization:text("base_mod_updates_all_up_to_date") or managers.localization:text("base_mod_updates_mod_required", loc_table)
	local prio = count < 1 and 101 or 1001
    
	if NotificationsManager:NotificationExists( LuaModUpdates.__required_notification_id ) then
		NotificationsManager:UpdateNotification( LuaModUpdates.__required_notification_id, title, message, prio, LuaModUpdates.NotificationClickCallback )
	else
		NotificationsManager:AddNotification( LuaModUpdates.__required_notification_id, title, message, prio, LuaModUpdates.NotificationClickCallback )
	end

end

function LuaModUpdates.NotificationClickCallback()
	LuaModUpdates.OpenUpdateManagerNode()
	return true
end

function LuaModUpdates.OpenUpdateManagerNode()

	managers.menu:open_node("base_lua_mod_updates_menu")

	if managers.menu_component and managers.menu_component._notifications_gui then
		managers.menu_component._notifications_gui:close()
	end

end

function LuaModUpdates:VerifyModIsDownloadable( mod_id )
	for k, v in pairs( LuaModManager:UpdateChecks() ) do
		if mod_id == v.identifier then
			return v.server_revision and v.server_revision >= 0 or false
		end
	end
	return false
end

function LuaModUpdates:DownloadAndStoreMod( mod_id )

	if not self:VerifyModIsDownloadable( mod_id ) then
		log("[Updates][Warning] Attempted to download a mod which is not on the server, halting...")
	end

	local url = self._updates_download_url:gsub("{1}", mod_id)
	log( ("[Updates] Downloading mod data for {1}"):gsub("{1}", mod_id) )

	local http_id = dohttpreq( url, LuaModUpdates.ModDownloadFinished, LuaModUpdates.UpdateDownloadDialog )
	log("[Updates] Started http download: " .. tostring(http_id))

	local mod_name = LuaModUpdates:GetModFriendlyName( mod_id )
	managers.menu:show_download_progress( mod_name )

	LuaModUpdates._currently_downloading[http_id] = mod_id

end

function LuaModUpdates.ModDownloadFinished( data, http_id )
	
	local self = LuaModUpdates
	local psuccess, perror = pcall(function()

	log("[Updates] Finished http download: " .. tostring(http_id))

	local mod_id = LuaModUpdates._currently_downloading[http_id]
	local mod_table = LuaModUpdates:GetModTable( mod_id )
	local mod_path = mod_table.mod

	log( ("[Updates] Finished downloading mod data for {1}"):gsub("{1}", tostring(mod_id)) )
	LuaModUpdates:SetDownloadDialogKey( "mod_download_complete", true )

	if data:is_nil_or_empty() then
		log("[Updates] Update failed, no data received!")
		LuaModUpdates:SetDownloadDialogKey( "mod_download_failed", true )
		return
	end

	local C = LuaModManager.Constants
	local download_path = C.mods_directory .. C.downloads_directory
	local file_path = download_path .. tostring(mod_id) .. ".zip"
	log("[Updates] Saving mod to file path: " .. file_path)

	local file = io.open( file_path, "wb+" )
	if file then

		file:write( data )
		file:close()

		local install_dir = mod_table.install_dir or C.mods_directory
		if not mod_table.install_dir and not mod_table.required then
			io.remove_directory_and_files( mod_path )
		else

			local install_path = mod_table.install_dir
			if mod_table.install_folder then
				install_path = install_path .. tostring(mod_table.install_folder) .. "/"
				io.remove_directory_and_files( install_path )
			end

			-- Special case for hook dll
			if mod_id == C.hook_dll_id or mod_id == "testmod" or mod_id == "payday2bltdll_test" then

				local hook_result, hook_error = os.rename( C.hook_dll_name, C.hook_dll_temp_name )
				if not hook_result then
					log( "[Error] Could not update hook DLL! " .. tostring(hook_error) )
					return
				end

				if mod_table.revision_path and mod_table.server_revision then

					local revision_file, file_err = io.open( mod_table.revision_path, "w+" )
					if revision_file then
						revision_file:write( tostring(mod_table.server_revision) )
						revision_file:close()
					end

				else
					log( "[Error] No revision path or server revision found in mod table, aborting update of DLL" )
					return
				end

			end

		end

		unzip( file_path, install_dir )
		LuaModUpdates:SetDownloadDialogKey( "mod_extraction_complete", true )

	end

	LuaModUpdates._currently_downloading[http_id] = nil
	self._current_download_dialog = nil

	end)
	if not psuccess then
		log("[Error] " .. perror)
	end

end

function LuaModUpdates.UpdateDownloadDialog( id, bytes, total_bytes )
	LuaModUpdates:SetDownloadDialogKey( "bytes_downloaded", bytes or 0 )
	LuaModUpdates:SetDownloadDialogKey( "bytes_total", total_bytes or 0 )
end

function LuaModUpdates:RegisterDownloadDialog( dialog )
	self._current_download_dialog = dialog
end

function LuaModUpdates:SetDownloadDialogKey( key, data )

	if self._current_download_dialog then
		local dialog = self._current_download_dialog
		dialog._panel_script._anim_data[ key ] = data
	end

end

function LuaModUpdates:GetModFriendlyName( mod_id )

	for k, v in ipairs( LuaModManager:UpdateChecks() ) do
		if v.identifier == mod_id then
			local mod_definition = LuaModManager:GetMod( v.mod ).definition
			return v.display_name or mod_definition[ LuaModManager.Constants.mod_name_key ]
		end
	end
    
    for k, v in pairs( LuaModManager:Required() ) do
		if v.identifier == mod_id then
			return v.display_name
		end
	end
	
	return mod_id

end

function LuaModUpdates:GetModTable( mod_id )

	for k, v in ipairs( LuaModManager:UpdateChecks() ) do
		if v.identifier == mod_id then
			return v
		end
	end
    
    for k, v in pairs( LuaModManager:Required() ) do
		if v.identifier == mod_id then
			return v
		end
	end
    
end
