
LuaModUpdates = LuaModUpdates or {}
LuaModUpdates._updates_api_path = "http://api.paydaymods.com/updates/retrieve/?"
LuaModUpdates._updates_api_mod = "mod[{1}]={2}"
LuaModUpdates._updates_download_url = "http://download.paydaymods.com/download/latest/{1}"
LuaModUpdates._updates_notes_url = "http://download.paydaymods.com/download/patchnotes/{1}"
LuaModUpdates._notification_id = "lua_mod_updates_notif"

LuaModUpdates._currently_downloading = {}
LuaModUpdates._current_download_dialog = nil

Hooks:Add("MenuManagerOnOpenMenu", "Base_ModUpdates_MenuManagerOnOpenMenu", function( menu_manager, menu, position )

	-- Check for updates after going to the main menu
	if menu == "menu_main" then

		if not LuaNetworking:IsMultiplayer() then
			LuaModUpdates:ShowUpdatesAvailableNotification({})
			LuaModUpdates:CheckForUpdates()
		end

		-- Remove temporary hook dll
		LuaModUpdates:RemoveTemporaryDLL()

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
function LuaModUpdates:CheckForUpdates()

	local path_str = LuaModUpdates._updates_api_path
	local i = 0

	for k, v in pairs( LuaModManager:UpdateChecks() ) do
		if LuaModManager:AreModUpdatesEnable( v.mod ) then

			if i > 0 then
				path_str = path_str .. "&"
			end

			path_str = path_str .. LuaModUpdates._updates_api_mod:gsub("{1}", i)
			path_str = path_str:gsub("{2}", v.identifier)
			i = i + 1

		end
	end

	if i > 0 then
		dohttpreq( path_str, LuaModUpdates.RetrievedModsFromServer )
	end

end

function LuaModUpdates.RetrievedModsFromServer( data, id )

	if data:is_nil_or_empty() then
		log("[Error] Could not connect to PaydayMods.com API!")
		return
	end

	local server_data = json.decode( data )
	if server_data then

		for k, v in pairs( server_data ) do
			log( ("[Updates] Received update data for '{1}', server revision: {2}"):gsub("{1}", k):gsub("{2}", v.revision) )
		end

		local mods_needing_updates = {}
		for k, v in pairs( LuaModManager:UpdateChecks() ) do

			local mod_data = server_data[v.identifier]
			local local_version = tonumber( v.revision ) or 1
			local server_version = tonumber( mod_data.revision ) or 1

			v.update_required = local_version < server_version
			if local_version < server_version then
				table.insert( mods_needing_updates, v )
			end

		end

		LuaModUpdates:ShowUpdatesAvailableNotification( mods_needing_updates )
		if #mods_needing_updates == 1 then
			LuaModUpdates:ShowUpdateAvailableMessage( mods_needing_updates[1] )
		elseif #mods_needing_updates > 1 then
			LuaModUpdates:ShowMultiUpdateAvailableMessage( mods_needing_updates )
		end

	else
		log("[Error] Could not decode server updates data!")
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

	if NotificationsManager:NotificationExists( LuaModUpdates._notification_id ) then
		NotificationsManager:UpdateNotification( LuaModUpdates._notification_id, title, message, 1001, LuaModUpdates.NotificationClickCallback )
	else
		NotificationsManager:AddNotification( LuaModUpdates._notification_id, title, message, 1001, LuaModUpdates.NotificationClickCallback )
	end

end

function LuaModUpdates.NotificationClickCallback()

	local node = managers.menu:active_menu().logic:selected_node()
	if node and node._default_item_name and node._default_item_name == "crimenet" then

		LuaModUpdates.OpenUpdateManagerNode()
		return true

	end

end

function LuaModUpdates.OpenUpdateManagerNode()

	managers.menu:open_node("base_lua_mod_updates_menu")

	if managers.menu_component and managers.menu_component._notifications_gui then
		managers.menu_component._notifications_gui:close()
	end

end

function LuaModUpdates:DownloadAndStoreMod( mod_id )

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
	local download_path = C.mods_directory .. C.lua_base_directory .. C.downloads_directory
	local file_path = download_path .. tostring(mod_id) .. ".zip"
	log("[Updates] Saving mod to file path: " .. file_path)

	local file = io.open( file_path, "wb+" )
	if file then

		file:write( data )
		file:close()

		local install_dir = mod_table.install_dir or C.mods_directory
		if not mod_table.install_dir then
			io.remove_directory_and_files( mod_path )
		else

			local install_path = mod_table.install_dir
			if mod_table.install_folder then
				install_path = install_path .. tostring(mod_table.install_folder) .. "/"
				io.remove_directory_and_files( install_path )
			end

			-- Special case for hook dll
			if mod_id == C.hook_dll_id then
				local hook_result, hook_error = os.rename( C.hook_dll_name, C.hook_dll_temp_name )
				if not hook_result then
					log( "[Error] Could not update hook DLL! " .. tostring(hook_error) )
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
	
	return mod_id

end

function LuaModUpdates:GetModTable( mod_id )

	for k, v in ipairs( LuaModManager:UpdateChecks() ) do
		if v.identifier == mod_id then
			return v
		end
	end

end
