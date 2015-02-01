
local LMM = LuaModManager
local C = LuaModManager.Constants

Hooks:Add( "MenuUpdate", "MenuUpdate_Base_UpdatePersistScripts", function(t, dt)
	LMM:UpdatePersistScripts()
end )

Hooks:Add( "GameSetupUpdate", "GameSetupUpdate_Base_UpdatePersistScripts", function(t, dt)
	LMM:UpdatePersistScripts()
end )

function LMM:UpdatePersistScripts()

	for k, v in pairs( LMM:PersistScripts() ) do

		local global = v[ C.mod_persists_global_key ]
		local script = v[ C.mod_script_path_key ]
		local path = v[ C.mod_persists_path_key ]
		local exists = _G[global]

		if not exists then
			declare( "PersistScriptPath", path )
			dofile( script )
		end
		
	end
	declare( "PersistScriptPath", nil )

end
