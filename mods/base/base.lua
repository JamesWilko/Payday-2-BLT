
-- Create our console
if false then
	console.CreateConsole()
end

-- Vars
if not _G then return end
local _G = _G
local io = io
local print = print
local file = file

if not declare then
	declare = function(var, val)
		rawset(_G, var, val or false)
	end
end

-- Load Mod Manager
if not LuaModManager then
	local lmm_path = "mods/base/req/lua_mod_manager.lua"
	dofile( lmm_path )
end

local C = LuaModManager.Constants
_lua_reqs 		= _lua_reqs or {}
_mods_folders 	= _mods_folders or {}
_mods 			= _mods or {}
_prehooks 		= _prehooks or {}
_posthooks 		= _posthooks or {}
_wildcard_hooks	= _wildcard_hooks or {}

-- Load JSON
if not _loaded_json then
	dofile( C.mods_directory .. C.lua_base_directory .. C.json_module )
	_loaded_json = true
end

-- Set logs folder
if not _G[ C.logs_path_global ] then
	declare( C.logs_path_global, C.mods_directory .. C.logs_directory )
end

-- Set saves folder
if not _G[ C.save_path_global ] then
	declare( C.save_path_global, C.mods_directory .. C.saves_directory )
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

	io.remove_directory_and_files = function( path, do_log )
        if not path then
            log("[Error] paramater #1 to io.remove_directory_and_files, string expected, recieved " .. tostring(path))
            return false
        end
        
		if not file.DirectoryExists( path ) then
			log("[Error] Directory does not exist: " .. path)
			return false
		end

		local dirs = file.GetDirectories( path )
		if dirs then
			for k, v in pairs( dirs ) do
				local child_path = path .. v .. "/"
				if do_log then log("Removing directory: " .. child_path) end
				io.remove_directory_and_files( child_path, do_log )
				local r = file.RemoveDirectory( child_path )
				if not r then
					log("[Error] Could not remove directory: " .. child_path)
					return false
				end
			end
		end

		local files = file.GetFiles( path )
		if files then
			for k, v in pairs( files ) do
				local file_path = path .. v
				if do_log then log("Removing files: " .. file_path) end
				local r, error_str = os.remove( file_path )
				if not r then
					log("[Error] Could not remove file: " .. file_path .. ", " .. error_str)
					return false
				end
			end
		end

		if do_log then log("Removing directory: " .. path) end
		local r = file.RemoveDirectory( path )
		if not r then
			log("[Error] Could not remove directory: " .. path)
			return false
		end

		return true

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
		for k, v in ipairs( _wildcard_hooks ) do
			declare( C.required_script_global, path )
			declare( C.mod_path_global, v.mod_path )
			dofile( v.script )
		end

		return res

	end

end

-- Load saved mod data
if not _loaded_mod_manager then

	LuaModManager:Load()
	_loaded_mod_manager = true

end

-- Load mod definition files
if not _loaded_mod_folders then

	local print = function(str)
		log("[Mods] " .. str)
	end

	print("Loading mods for state (" .. tostring(_G) .. ")")
	_mods_folders = file.GetDirectories( C.mods_directory )

	if _mods_folders then

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
						local mod_content = nil
						local json_success = pcall(function()
							mod_content = json.decode(file_contents)
						end)

						if json_success and mod_content then
							local data = {
								path = mod_path,
								definition = mod_content,
								priority = tonumber(mod_content.priority) or 0,
							}
							table.insert( _mods, data )
						else
							print("An error occured while loading mod.txt from: " .. mod_path)
						end
						file:close()

					end

				else
					print("Could not read or find " .. C.mod_definition_file .. " for modification: " .. v)
				end

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

					hook_id = hook_id:lower()
					local tbl = {
						mod_path = mod.path,
						script = mod.path .. script
					}

					if hook_id ~= C.mod_hook_wildcard_key then
						destination_table[ hook_id ] = destination_table[ hook_id ] or {}
						table.insert( destination_table[ hook_id ], tbl )
					else
						table.insert( _wildcard_hooks, tbl )
					end

				end
			end

		end

	end

	local add_persist_scripts = function( mod )

		local persists = mod.definition[C.mod_persists_key]
		if persists then
			for k, v in pairs( persists ) do
				LuaModManager:AddPersistScript( v, mod.path )
			end
		end

	end

	local add_keybind_scripts = function( mod )

		local keybinds = mod.definition[C.mod_keybinds_key]
		if keybinds then
			for k, v in pairs( keybinds ) do
				LuaModManager:AddJsonKeybinding( v, mod.path )
			end
		end

	end

	local add_updates = function( mod )

		local updates = mod.definition[C.mod_update_key]
		if updates then
			for k, update in pairs( updates ) do
				LuaModManager:AddUpdateCheck( mod.definition, mod.path, update )
			end
		end

	end
    
	-- Prioritize
	table.sort( _mods, function(a, b)
		return a.priority > b.priority
	end)

	-- Add mod hooks to tables
	for k, v in ipairs( _mods ) do

		if LuaModManager:IsModEnabled( v.path ) and LuaModManager:HasRequiredMod(v) then

			-- Load pre- and post- hooks
			add_hooks_table( v, C.mod_hooks_key, _posthooks )
			add_hooks_table( v, C.mod_prehooks_key, _prehooks )

			-- Load persist scripts
			add_persist_scripts( v )

			-- Load keybinds
			add_keybind_scripts( v )

			-- Load updates
			add_updates( v )

		else
			log("[Mods] Mod '" .. v.path .. "' is disabled!")
		end

	end

	LuaModManager.Mods = _mods

end
