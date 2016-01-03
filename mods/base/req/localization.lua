
LuaModManager._languages = {
	"en",
	"de",
	"fr",
	"ru",
	"tr",
	"id"
}
LuaModManager.Constants.default_language = "en"
LuaModManager.Constants.language_key = "language"

function LuaModManager:GetIndexOfDefaultLanguage()
	for k, v in pairs(LuaModManager._languages) do
		if v == LuaModManager.Constants.default_language then
			return k
		end
	end
	return 1
end

function LuaModManager:GetLanguageIndex()
	local key = LuaModManager.Constants.language_key
	return self._enabled_mods[key] or self:GetIndexOfDefaultLanguage()
end

function LuaModManager:GetLanguageFile()
	local lang = LuaModManager._languages[self:GetLanguageIndex()]
	lang = lang or LuaModManager._languages[self:GetIndexOfDefaultLanguage()]
	return string.format("%sloc/%s.txt", LuaModManager._base_path, lang)
end

function LuaModManager:SetActiveLanguage( index )
	local key = LuaModManager.Constants.language_key
	self._enabled_mods[key] = index
end

Hooks:Add("LocalizationManagerPostInit", "Base_LocalizationManagerPostInit", function(loc)
	-- Load english strings to use as backup
	loc:load_localization_file( string.format("%sloc/%s.txt", LuaModManager._base_path, "en") )
	loc:load_localization_file( LuaModManager:GetLanguageFile() )
end)

Hooks:Add("MenuManager_Base_BuildModOptionsMenu", "MenuManager_Base_SetupModOptionsMenu_Localization", function( menu_manager )

	local menu_id = LuaModManager.Constants._lua_mod_options_menu_id

	MenuCallbackHandler.blt_base_select_language = function(this, item)
		LuaModManager:SetActiveLanguage( tonumber(item:value()) )
		LuaModManager:Save()
	end

	local items = {}
	for k, v in ipairs( LuaModManager._languages ) do
		items[k] = "base_language_" .. v
	end

	MenuHelper:AddMultipleChoice({
		id = "base_language_select",
		title = "base_language_select",
		desc = "base_language_select_desc",
		callback = "blt_base_select_language",
		menu_id = menu_id,
		items = items,
		value = LuaModManager:GetLanguageIndex(),
		priority = 1001,
	})

	MenuHelper:AddDivider({
		size = 16,
		menu_id = menu_id,
		priority = 1000,
	})

end)
