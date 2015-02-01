
CloneClass( LocalizationManager )

function LocalizationManager.text(this, str, macros)

	-- log( "Localizer: " .. tostring(Localizer.__index) )
	-- log( "SystemInfo:language():key(): " )
	-- lang_mods[Idstring("german"):key()]
	-- lang_mods[Idstring("french"):key()]
	-- lang_mods[Idstring("italian"):key()]
	-- lang_mods[Idstring("spanish"):key()]
	-- lang_mods[Idstring("english"):key()]

	--[[
	if _G.GoonBase.Localization[str] then

		local return_str =_G.GoonBase.Localization[str]
		if macros and type(macros) == "table" then
			for k, v in pairs( macros ) do
				return_str = return_str:gsub( "$" .. k, v )
			end
		end
		return return_str

	end
	]]
	return this.orig.text(this, str, macros)

end
