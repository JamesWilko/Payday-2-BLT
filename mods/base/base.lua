
-- Create our console
local console = console
console.CreateConsole()

-- Vars
if not _G then return end
local _G = _G
local io = io
local print = print
local getdir = getdir

if not declare then
	declare = function(var, val)
		rawset(_G, var, val or false)
	end
end

-- Constants
declare( "LuaModManager", {} )
LuaModManager.Constants = {}
local C = LuaModManager.Constants
C.mods_directory = "mods/"
C.lua_base_directory = "base/"
C.logs_directory = "logs/"
C.json_module = "req/json.lua"
C.mod_manager_file = "mod_manager.txt"
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

-- Mods and Hooks
_lua_reqs 		= _lua_reqs or {}
_mods_folders 	= _mods_folders or {}
_mods 			= _mods or {}
_prehooks 		= _prehooks or {}
_posthooks 		= _posthooks or {}

-- Load JSON
if not _loaded_json then
	dofile( C.mods_directory .. C.lua_base_directory .. C.json_module )
	_loaded_json = true
end

-- Set logs folder
if not _G[ C.logs_path_global ] then
	declare( C.logs_path_global, C.mods_directory .. C.logs_directory )
end

-- IO Helper
if io then

	io.file_is_readable = function( fname )
		local file = io.open(fname, "r" )
		if file ~= nil then
			io.close(file)
			return true
		end
		return false
	end

end

-- Override require()
if not _require_orig then

	local _require_orig = require

	call_require_hook = function( hooks_table, path )

		if hooks_table then
			if hooks_table[path] then
				for k, v in pairs( hooks_table[path] ) do
					declare( C.required_script_global, path )
					declare( C.mod_path_global, v.mod_path )
					dofile( v.script )
				end
			end
		end

	end

	require = function( path )

		local path_orig = path
		path = path:lower()

		call_require_hook( _prehooks, path )
		local res = _require_orig( path_orig )
		call_require_hook( _posthooks, path )

		return res

	end

end

-- Load mod manager
if not _loaded_mod_manager then

	LuaModManager._enabled_mods = {}
	LuaModManager._persist_scripts = {}
	LuaModManager._path = C.mods_directory .. C.lua_base_directory .. C.mod_manager_file

	LuaModManager._EnsureTablesExists = function( self )
		if not self._enabled_mods then self._enabled_mods = {} end
		if not self._persist_scripts then self._persist_scripts = {} end
	end

	LuaModManager.SaveModsStates = function( self )
		self:_EnsureTablesExists()
		local file = io.open(self._path, "w+")
		file:write( json.encode( self._enabled_mods ) )
		file:close()
	end

	LuaModManager.IsModEnabled = function( self, mod_name )
		self:_EnsureTablesExists()
		return (self._enabled_mods[mod_name] == nil or self._enabled_mods[mod_name] == true)
	end

	LuaModManager.SetModEnabledState = function( self, mod_name, state )
		self:_EnsureTablesExists()
		self._enabled_mods[mod_name] = state
		self:SaveModsStates()
	end

	LuaModManager.ToggleModState = function( self, mod_name )
		self:_EnsureTablesExists()
		if not C.always_active_mods[mod_name] then
			if self._enabled_mods[mod_name] == nil then
				self._enabled_mods[mod_name] = false
			else
				self._enabled_mods[mod_name] = not self._enabled_mods[mod_name]
			end
			self:SaveModsStates()
		end
	end

	LuaModManager.EnableMod = function( self, mod_name )
		self:_EnsureTablesExists()
		self:SetModEnabledState( mod_name, true )
		self:SaveModsStates()
	end

	LuaModManager.DisableMod = function( self, mod_name )
		self:_EnsureTablesExists()
		self:SetModEnabledState( mod_name, false )
		self:SaveModsStates()
	end

	LuaModManager.AddPersistScript = function( self, global, script, path )
		self:_EnsureTablesExists()
		local tbl = {
			[ C.mod_persists_global_key ] = global,
			[ C.mod_script_path_key ] = script,
			[ C.mod_persists_path_key ] = path,
		}
		table.insert( self._persist_scripts, tbl )
	end

	LuaModManager.PersistScripts = function( self )	
		self:_EnsureTablesExists()
		return self._persist_scripts
	end

	-- Load mod manager data
	local is_readable = io.file_is_readable and io.file_is_readable( LuaModManager._path ) or false
	if is_readable then

		local file = io.open( LuaModManager._path )
		if file then

			local file_contents = file:read("*all")
			local mod_manager_data = json.decode(file_contents)
			if mod_manager_data then
				LuaModManager._enabled_mods = mod_manager_data
			end
			file:close()

		end

	end

end

-- Load mod definition files
if not _loaded_mod_folders then

	local print = function(str)
		log("[Mods] " .. str)
	end

	print("Loading mods for state (" .. tostring(_G) .. ")")
	_mods_folders = getdir( C.mods_directory )

	for k, v in pairs( _mods_folders ) do

		if not C.excluded_mods_directories[v] then

			print("Loading mod: " .. tostring(v) .. "...")
			local mod_path = C.mods_directory .. v .. "/"
			local mod_def_file = mod_path .. C.mod_definition_file
			local is_readable = io.file_is_readable and io.file_is_readable(mod_def_file) or false
			if is_readable then

				local file = io.open(mod_def_file)
				if file then

					local file_contents = file:read("*all")
					local mod_content = json.decode(file_contents)
					if mod_content then
						local data = {
							path = mod_path,
							definition = mod_content,
							priority = mod_content.priority or 0,
						}
						table.insert( _mods, data )
					end
					file:close()

				end

			else
				print("Could not read or find " .. C.mod_definition_file .. " for modification: " .. v)
			end

		end

	end

	_loaded_mod_folders = true

end

-- Process mod definitions
if _loaded_mod_folders and _mods then

	local add_hooks_table = function( mod, table_key, destination_table )

		local hooks = mod.definition[table_key]
		if hooks then

			for i, hook in pairs( hooks ) do
				local hook_id = hook[ C.mod_hook_id_key ]
				local script = hook[ C.mod_script_path_key ]
				if hook_id and script then

					local tbl = {
						mod_path = mod.path,
						script = mod.path .. script
					}
					destination_table[ hook_id ] = destination_table[ hook_id ] or {}
					table.insert( destination_table[ hook_id ], tbl )

				end
			end

		end

	end

	local add_persist_scripts = function( mod )

		local persists = mod.definition[C.mod_persists_key]
		if persists then
			for k, v in pairs( persists ) do
				local global = v[C.mod_persists_global_key]
				local script = mod.path .. v[C.mod_script_path_key]
				declare( global, false )
				LuaModManager:AddPersistScript( global, script, mod.path )
			end
		end

	end

	-- Prioritize
	table.sort( _mods, function(a, b)
		return a.priority > b.priority
	end)

	-- Add mod hooks to tables
	for k, v in ipairs( _mods ) do

		if LuaModManager:IsModEnabled( v.path ) then

			-- Load pre- and post- hooks
			add_hooks_table( v, C.mod_hooks_key, _posthooks )
			add_hooks_table( v, C.mod_prehooks_key, _prehooks )

			-- Load persist scripts
			add_persist_scripts( v )

		else
			log("[Mods] Mod '" .. v.path .. "' is disabled!")
		end

	end

	LuaModManager.Mods = _mods

end
