
_G.GoonBase.Options = _G.GoonBase.Options or {}
local Options = _G.GoonBase.Options
Options.SaveFile = GoonBase.Path .. "options.ini"

local OptionsMenuID = "goonbase_options_menu"
Hooks:RegisterHook( "MenuManagerSetupGoonBaseMenu" )
Hooks:RegisterHook( "MenuManagerPostSetupGoonBaseMenu" )

Hooks:Add("MenuManagerSetupCustomMenus", "MenuManagerSetupCustomMenus_OptionsMenu", function(menu_manager, mainmenu_nodes)
	GoonBase.MenuHelper:NewMenu( OptionsMenuID )
end)

Hooks:Add("MenuManagerPopulateCustomMenus", "MenuManagerPopulateCustomMenus_OptionsMenu", function(menu_manager, mainmenu_nodes)
	Hooks:Call( "MenuManagerSetupGoonBaseMenu", menu_manager, mainmenu_nodes )
end)

Hooks:Add("MenuManagerBuildCustomMenus", "MenuManagerBuildCustomMenus_OptionsMenu", function(menu_manager, mainmenu_nodes)
	mainmenu_nodes[OptionsMenuID] = GoonBase.MenuHelper:BuildMenu( OptionsMenuID )
	GoonBase.MenuHelper:AddMenuItem( mainmenu_nodes.options, OptionsMenuID, "GoonBaseOptionsName", "GoonBaseOptionsDesc", 5 )
	Hooks:Call( "MenuManagerPostSetupGoonBaseMenu", menu_manager, mainmenu_nodes )
end)

function Options:GetSaveString()

	local contents = "";
	for k, v in pairs( Options ) do
		
		if type(v) == "table" then
			contents = string.format( "%s[%s]\n", contents, tostring(k) )
			for a, b in pairs( v ) do
				contents = string.format( "%s%s=%s\n", contents, tostring(a), tostring(b) )
			end
		end

	end

	return contents

end

function Options:UsingDefaults()
	if self._default then
		return true
	end
	return false
end

Hooks:RegisterHook("OptionsRegisterDefaultOptions")
function Options:LoadDefaults()
	self._default = nil
	Hooks:Call("OptionsRegisterDefaultOptions")
	self:Save()
end

function Options:Save(fileName)

	if fileName == nil then
		fileName = Options.SaveFile
	end

	local file = io.open(fileName, "w+")
	file:write( Options:GetSaveString() )
	file:close()

end

function Options:Load(fileName)

	if fileName == nil then
		fileName = Options.SaveFile
	end

	local file = io.open(fileName, 'r')
	local key

	if file == nil then
		log( "Could not open file (" .. fileName .. "), using default options..." )
		self._default = true
		return
	end

	for line in file:lines() do

		local loadKey = line:match('^%[([^%[%]]+)%]$')

		if loadKey then
			key = tonumber(loadKey) and tonumber(loadKey) or loadKey
			Options[key] = Options[key] or {}
		end

		local param, val = line:match('^([%w|_]+)%s-=%s-(.+)$')

		if param and val ~= nil then

			if tonumber(val) then
				val = tonumber(val)
			elseif val == "true" then
				val = true
			elseif val == "false" then
				val = false
			end

			if tonumber(param) then
				param = tonumber(param)
			end

			Options[key][param] = val

		end

	end

	file:close()

end
Options:Load()
