
if not _G["LuaModManager"] then
	declare( "LuaModManager", {} )
end
LuaModManager = LuaModManager or {}
LuaModManager.Constants = LuaModManager.Constants or {}

local C = LuaModManager.Constants
C.mods_directory = "mods/"
C.lua_base_directory = "base/"
C.logs_directory = "logs/"
C.json_module = "req/json.lua"
C.mod_manager_file = "mod_manager.txt"
C.mod_keybinds_file = "mod_keybinds.txt"

C.excluded_mods_directories = {
	["logs"] = true,
}

C.always_active_mods = {
	["mods/base/"] = true,
	["mods/logs/"] = true,
}

C.required_script_global = "RequiredScript"
C.mod_path_global = "ModPath"
C.logs_path_global = "LogsPath"

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

C.mod_keybinds_key = "keybinds"
C.mod_keybind_id_key = "keybind_id"
C.mod_keybind_name_key = "name"
C.mod_keybind_desc_key = "description"
C.mod_keybind_script_key = "script_path"
C.mod_keybind_path_key = "path"
C.mod_keybind_scope_menu_key = "run_in_menu"
C.mod_keybind_scope_game_key = "run_in_game"

LuaModManager._persist_scripts = LuaModManager._persist_scripts or {}

LuaModManager._enabled_mods = LuaModManager._enabled_mods or {}
LuaModManager._base_path = C.mods_directory .. C.lua_base_directory
LuaModManager._mod_manager_file_path = LuaModManager._base_path .. C.mod_manager_file

LuaModManager._keybinds = LuaModManager._keybinds or {}
LuaModManager._player_keybinds = LuaModManager._player_keybinds or {}
LuaModManager._mod_keybinds_file_path = LuaModManager._base_path .. C.mod_keybinds_file

local function clone( o )
	local res = {}
	for k, v in pairs( o ) do
		res[k] = v
	end
	setmetatable( res, getmetatable(o) )
	return res
end

function LuaModManager:IsModEnabled( mod_name )
	return (self._enabled_mods[mod_name] == nil or self._enabled_mods[mod_name] == true)
end

function LuaModManager:SetModEnabledState( mod_name, state )
	self._enabled_mods[mod_name] = state
	self:Save()
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
end

function LuaModManager:EnableMod( mod_name )
	self:SetModEnabledState( mod_name, true )
	self:Save()
end

function LuaModManager:DisableMod( mod_name )
	self:SetModEnabledState( mod_name, false )
	self:Save()
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

function LuaModManager:GetNumberOfKeybinds()
	local i = 0
	for k, v in pairs( self._keybinds ) do
		i = i + 1
	end
	return i
end

function LuaModManager:PlayerKeybinds()
	return self._player_keybinds
end

function LuaModManager:AddKeybinding( keybind, path )

	local tbl = clone( keybind )
	tbl[ C.mod_keybind_path_key ] = path
	tbl[ C.mod_keybind_script_key ] = path .. tbl[ C.mod_keybind_script_key ]

	local keybind_id = tbl[ C.mod_keybind_id_key ]
	self._keybinds[ keybind_id ] = tbl

end

function LuaModManager:GetPlayerKeybind( keybind_id )
	return self._player_keybinds[ keybind_id ]
end

function LuaModManager:SetPlayerKeybind( keybind_id, key )
	self._player_keybinds[ keybind_id ] = key
	self:Save()
end

function LuaModManager:Save()
	self:SaveTableToJson( self._enabled_mods, self._mod_manager_file_path )
	self:SaveTableToJson( self._player_keybinds, self._mod_keybinds_file_path )
end

function LuaModManager:SaveTableToJson( tbl, file_path )

	local file = io.open(file_path, "w+")
	if file then
		file:write( json.encode( tbl ) )
		file:close()
	else
		log("[Error] Could not save to file '" .. file_path .. "', data may be lost!")
	end

end

function LuaModManager:Load()
	self._enabled_mods = self:LoadJsonFileToTable( self._mod_manager_file_path ) or self._enabled_mods
	self._player_keybinds = self:LoadJsonFileToTable( self._mod_keybinds_file_path ) or self._player_keybinds
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
