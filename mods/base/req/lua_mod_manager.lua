
if not _G["LuaModManager"] then
	declare( "LuaModManager", {} )
end
LuaModManager = LuaModManager or {}
LuaModManager.Constants = LuaModManager.Constants or {}

local C = LuaModManager.Constants
C.mods_directory = "mods/"
C.lua_base_directory = "base/"
C.logs_directory = "logs/"
C.downloads_directory = "downloads/"
C.saves_directory = "saves/"
C.json_module = "req/json.lua"
C.mod_manager_file = "mod_manager.txt"
C.mod_keybinds_file = "mod_keybinds.txt"
C.mod_updates_file = "mod_updates.txt"
C.mod_required_file = "mod_required.txt"

C.excluded_mods_directories = {
	["logs"] = true,
	["saves"] = true,
	["downloads"] = true,
}

C.always_active_mods = {
	["mods/base/"] = true,
	["mods/logs/"] = true,
	["mods/saves/"] = true,
}

C.required_script_global = "RequiredScript"
C.mod_path_global = "ModPath"
C.logs_path_global = "LogsPath"
C.save_path_global = "SavePath"

C.mod_definition_file = "mod.txt"
C.mod_name_key = "name"
C.mod_desc_key = "description"
C.mod_version_key = "version"
C.mod_author_key = "author"
C.mod_contact_key = "contact"
C.mod_hooks_key = "hooks"
C.mod_prehooks_key = "pre_hooks"
C.mod_persists_key = "persist_scripts"
C.mod_hook_id_key = "hook_id"
C.mod_script_path_key = "script_path"
C.mod_persists_global_key = "global"
C.mod_persists_path_key = "path"
C.mod_hook_wildcard_key = "*"

C.mod_keybinds_key = "keybinds"
C.mod_keybind_id_key = "keybind_id"
C.mod_keybind_name_key = "name"
C.mod_keybind_desc_key = "description"
C.mod_keybind_script_key = "script_path"
C.mod_keybind_path_key = "path"
C.mod_keybind_scope_menu_key = "run_in_menu"
C.mod_keybind_scope_game_key = "run_in_game"
C.mod_keybind_callback_key = "callback"
C.mod_keybind_localize_key = "localized"

C.mod_update_key = "updates"
C.mod_update_revision_key = "revision"
C.mod_update_identifier_key = "identifier"
C.mod_update_install_key = "install_dir"
C.mod_update_install_folder_key = "install_folder"
C.mod_update_name_key = "display_name"

C.mod_libs_key = "libraries"
C.mod_libs_identifier_key = "identifier"
C.mod_libs_display_name_key = "display_name"
C.mod_libs_optional_key = "optional"

C.hook_dll_id = "payday2bltdll"
C.hook_dll_name = "IPHLPAPI.dll"
C.hook_dll_temp_name = "IPHLPAPI_temp.dll"

LuaModManager._persist_scripts = LuaModManager._persist_scripts or {}

LuaModManager._base_path = C.mods_directory .. C.lua_base_directory
LuaModManager._save_path = C.mods_directory .. C.saves_directory

LuaModManager._enabled_mods = LuaModManager._enabled_mods or {}
LuaModManager._mod_manager_file_path = LuaModManager._save_path .. C.mod_manager_file

LuaModManager._keybinds = LuaModManager._keybinds or {}
LuaModManager._player_keybinds = LuaModManager._player_keybinds or {}
LuaModManager._mod_keybinds_file_path = LuaModManager._save_path .. C.mod_keybinds_file

LuaModManager._updates = LuaModManager._updates or {}
LuaModManager._required = LuaModManager._required or {}
LuaModManager._updates_enabled = LuaModManager._updates_enabled or {}
LuaModManager._mod_updates_file_path = LuaModManager._save_path .. C.mod_updates_file

LuaModManager._required_enabled = LuaModManager._required_enabled or {}
LuaModManager._mod_required_file_path = LuaModManager._save_path .. C.mod_required_file

local function clone( o )
	local res = {}
	for k, v in pairs( o ) do
		res[k] = v
	end
	setmetatable( res, getmetatable(o) )
	return res
end

function LuaModManager:GetMod( mod_path )

	if self.Mods then
		for k, v in pairs( self.Mods ) do
			if v.path == mod_path then
				return v
			end
		end
	end

end

function LuaModManager:WasModEnabledAtLoadTime( mod_name )
	-- Use a separate table for checking if mods are enabled, so that we don't end up showing that mods
	-- are "enabled" when somebody flags a mod for being enabled and then reloads the mods menu
	if not self._enabled_mods_on_load then
		self._enabled_mods_on_load = clone( self._enabled_mods )
	end
	return (self._enabled_mods_on_load[mod_name] == nil or self._enabled_mods_on_load[mod_name] == true)
end

function LuaModManager:IsModEnabled( mod_name )
	return (self._enabled_mods[mod_name] == nil or self._enabled_mods[mod_name] == true)
end

function LuaModManager:SetModEnabledState( mod_name, state )
	self._enabled_mods[mod_name] = state
	self:Save()
end

function LuaModManager:HasModFromIdentifier(identifier)
    for k, v in pairs(_mods) do
        local updates = v.definition[C.mod_update_key]
        if updates then
            for i, update in pairs(updates) do
                if update[C.mod_update_identifier_key] == identifier then
                    return true
                end
            end
        end
    end
    return false
end

function LuaModManager:HasRequiredMod(mod)
    local libs = mod.definition[C.mod_libs_key]
    local has_any_required = false
    if libs then
        for k, lib in pairs(libs) do
            if not self:HasModFromIdentifier(lib[C.mod_libs_identifier_key]) then
                if not lib[C.mod_libs_optional_key] == "true" then
                    has_any_required = true
                end
                self:AddRequireCheck( lib[C.mod_libs_display_name_key], lib[C.mod_libs_identifier_key], mod.definition[C.mod_name_key], (lib[C.mod_libs_optional_key] == "true") )
            end
        end
    end
    
    return not has_any_required
end

function LuaModManager:ToggleModState( mod_name )
	if not C.always_active_mods[mod_name] then
		if self._enabled_mods[mod_name] == nil then
			self._enabled_mods[mod_name] = false
		else
			self._enabled_mods[mod_name] = not self._enabled_mods[mod_name]
		end
		self:Save()
	end
	return self._enabled_mods[mod_name]
end

function LuaModManager:EnableMod( mod_name )
	self:SetModEnabledState( mod_name, true )
	self:Save()
end

function LuaModManager:DisableMod( mod_name )
	self:SetModEnabledState( mod_name, false )
	self:Save()
end

function LuaModManager:IsFlaggedForEnabledChange( mod_name )
	return self:IsModEnabled( mod_name ) ~= self:WasModEnabledAtLoadTime( mod_name )
end

function LuaModManager:PersistScripts()	
	return self._persist_scripts
end

function LuaModManager:AddPersistScript( persist, path )
	local tbl = clone( persist )
	tbl[ C.mod_script_path_key ] = path .. tbl[ C.mod_script_path_key ]
	tbl[ C.mod_persists_path_key ] = path
	declare( persist[C.mod_persists_global_key], false )
	table.insert( self._persist_scripts, tbl )
end

function LuaModManager:Keybinds()
	return self._keybinds
end

function LuaModManager:GetNumberOfJsonKeybinds()
	local i = 0
	for k, v in pairs( self._keybinds ) do
		if v._is_json then
			i = i + 1
		end
	end
	return i
end

function LuaModManager:PlayerKeybinds()
	return self._player_keybinds
end

function LuaModManager:AddJsonKeybinding( keybind, path )

	local tbl = clone( keybind )
	tbl._is_json = true
	tbl[ C.mod_keybind_path_key ] = path
	tbl[ C.mod_keybind_script_key ] = path .. tbl[ C.mod_keybind_script_key ]

	local keybind_id = tbl[ C.mod_keybind_id_key ]
	self._keybinds[ keybind_id ] = tbl

end

function LuaModManager:AddKeybinding( keybind_id, callback )
	self._keybinds[ keybind_id ] = callback
end

function LuaModManager:GetPlayerKeybind( keybind_id )
	return self._player_keybinds[ keybind_id ]
end

function LuaModManager:SetPlayerKeybind( keybind_id, key )
	self._player_keybinds[ keybind_id ] = key
	self:Save()
end

function LuaModManager:UpdateChecks()
	return self._updates
end

function LuaModManager:Required()
	return self._required
end

function LuaModManager:AddUpdateCheck( mod_table, mod_id, update_tbl )

	local tbl = {
		mod = mod_id,
		mod_table = mod_table,
		revision = update_tbl[ C.mod_update_revision_key ],
		identifier = update_tbl[ C.mod_update_identifier_key ],
		install_dir = update_tbl[ C.mod_update_install_key ] or nil,
		install_folder = update_tbl[ C.mod_update_install_folder_key ] or nil,
		display_name = update_tbl[ C.mod_update_name_key ] or nil,
	}

	if tbl.identifier == nil then
		log("[Error] Could not add automatic update for " .. tostring(mod_id) .. " as it has no identifier key!")
		return
	end

	if tbl.revision == nil then
		log("[Error] Could not add automatic update for " .. tostring(tbl.display_name or mod_id) .. " as it has no revision key!")
		return
	end

	-- Load revision from file if necessary
	if type(tbl.revision) == "string" then

		local path = tbl.revision
		if tbl.revision:sub(1, 2) == "./" then
			path = tbl.revision:sub( 3, tbl.revision:len() )
			tbl.revision_path = path
		else
			path = tbl.install_dir .. tbl.install_folder .. "/" .. tbl.revision
		end

		local file = io.open( path, "r" )
		if file then
			local data = file:read("*all")
			data = tonumber(data)
			if data then
				tbl.revision = data
			else
				tbl.revision = nil
			end
		else
			tbl.revision = nil
		end

		if tbl.revision == nil then
			log("[Error] Could not load revision file for " .. tostring(tbl.display_name or mod_id) .. "!")
			return
		end

	end

	table.insert( self._updates, tbl )

end

function LuaModManager:AddRequireCheck( mod_name, identifier, required_by, optional )
    self._required[identifier] = self._required[identifier] or {}
    self._required[identifier].required_by = self._required[identifier].required_by or {}
    self._required[identifier].optional = self._required[identifier].optional == nil and optional or not optional and optional or self._required[identifier].optional
    self._required[identifier].display_name = mod_name
    self._required[identifier].identifier = identifier
    
    table.insert(self._required[identifier].required_by, required_by)
end

function LuaModManager:AreModUpdatesEnable( mod_id )
	return (self._updates_enabled[mod_id] == nil or self._updates_enabled[mod_id] == true)
end

function LuaModManager:SetModUpdatesState( mod_id, state )
	log( ("[Updates] Setting automatic updates for mod '{1}' to: {2}"):gsub("{1}", mod_id):gsub("{2}", tostring(state)) )
	self._updates_enabled[mod_id] = state
	self:Save()
end

function LuaModManager:Save()
	self:SaveTableToJson( self._enabled_mods, self._mod_manager_file_path )
	self:SaveTableToJson( self._player_keybinds, self._mod_keybinds_file_path )
	self:SaveTableToJson( self._updates_enabled, self._mod_updates_file_path )
end

function LuaModManager:SaveTableToJson( tbl, file_path )

	local count = 0
	for k, v in pairs( tbl ) do
		count = count + 1
	end

	if tbl and count > 0 then

		local file = io.open(file_path, "w+")
		if file then
			file:write( json.encode( tbl ) )
			file:close()
		else
			log("[Error] Could not save to file '" .. file_path .. "', data may be lost!")
		end

	else
		log("[Warning] Attempting to save empty data table '" .. file_path .. "', skipping...")
	end

end

function LuaModManager:Load()
	self._enabled_mods = self:LoadJsonFileToTable( self._mod_manager_file_path ) or self._enabled_mods
	self._player_keybinds = self:LoadJsonFileToTable( self._mod_keybinds_file_path ) or self._player_keybinds
	self._updates_enabled = self:LoadJsonFileToTable( self._mod_updates_file_path ) or self._updates_enabled
end

function LuaModManager:LoadJsonFileToTable( file_path )

	local file = io.open( file_path, "r" )
	if file then

		local file_contents = file:read("*all")
		local mod_manager_data = json.decode(file_contents)
		file:close()
		return mod_manager_data

	else
		log("[Warning] Could not load file '" .. file_path .. "', no data loaded...")
	end
	return nil

end
