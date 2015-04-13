
CloneClass( LocalizationManager )

LocalizationManager._custom_localizations = LocalizationManager._custom_localizations or {}

Hooks:RegisterHook("LocalizationManagerPostInit")
function LocalizationManager.init( self )
	self.orig.init( self )
	Hooks:Call( "LocalizationManagerPostInit", self )
end

function LocalizationManager.text( self, str, macros )

	-- log( "Localizer: " .. tostring(Localizer.__index) )
	-- log( "SystemInfo:language():key(): " )
	-- lang_mods[Idstring("german"):key()]
	-- lang_mods[Idstring("french"):key()]
	-- lang_mods[Idstring("italian"):key()]
	-- lang_mods[Idstring("spanish"):key()]
	-- lang_mods[Idstring("english"):key()]

	if self._custom_localizations[str] then

		local return_str = self._custom_localizations[str]
		if macros and type(macros) == "table" then
			for k, v in pairs( macros ) do
				return_str = return_str:gsub( "$" .. k, v )
			end
		end
		return return_str

	end
	return self.orig.text(self, str, macros)

end

function LocalizationManager:add_localized_strings( string_table, overwrite )

	-- Should we overwrite existing localization strings
	if overwrite == nil then
		overwrite = true
	end

	if type(string_table) == "table" then
		for k, v in pairs( string_table ) do
			if not self._custom_localizations[k] or (self._custom_localizations[k] and overwrite) then
				self._custom_localizations[k] = v
			end
		end
	end

end

function LocalizationManager:load_localization_file( file_path, overwrite )

	-- Should we overwrite existing localization strings
	if overwrite == nil then
		overwrite = true
	end

	local file = io.open( file_path, "r" )
	if file then

		local file_contents = file:read("*all")
		file:close()

		local contents = json.decode( file_contents )
		self:add_localized_strings( contents, overwrite )

	end

end
