
--[[
	We setup the global table for our mod, along with some path variables, and a data table.
	We cache the ModPath directory, so that when our hooks are called, we aren't using the ModPath from a
		different mod.
]]
_G.JsonMenuExample = _G.JsonMenuExample or {}
JsonMenuExample._path = ModPath
JsonMenuExample._data_path = ModPath .. "saved_example_data.txt"
JsonMenuExample._data = {} 

--[[
	A simple save function that json encodes our _data table and saves it to a file.
]]
function JsonMenuExample:Save()
	local file = io.open( self._data_path, "w+" )
	if file then
		file:write( json.encode( self._data ) )
		file:close()
	end
end

--[[
	A simple load function that decodes the saved json _data table if it exists.
]]
function JsonMenuExample:Load()
	local file = io.open( self._data_path, "r" )
	if file then
		self._data = json.decode( file:read("*all") )
		file:close()
	end
end

--[[
	Load our localization keys for our menu, and menu items.
]]
Hooks:Add("LocalizationManagerPostInit", "LocalizationManagerPostInit_JsonMenuExample", function( loc )
	loc:load_localization_file( JsonMenuExample._path .. "en.txt")
end)

--[[
	Setup our menu callbacks, load our saved data, and build the menu from our json file.
]]
Hooks:Add( "MenuManagerInitialize", "MenuManagerInitialize_JsonMenuExample", function( menu_manager )

	--[[
		Setup our callbacks as defined in our item callback keys, and perform our logic on the data retrieved.
	]]
	MenuCallbackHandler.callback_test_toggle = function(self, item)
		JsonMenuExample._data.toggle_value = (item:value() == "on" and true or false)
		JsonMenuExample:Save()
		log("Toggle is: " .. item:value())
	end

	MenuCallbackHandler.callback_test_slider = function(self, item)
		JsonMenuExample._data.slider_value = item:value()
		JsonMenuExample:Save()
		log("Slider value: " .. item:value())
	end

	MenuCallbackHandler.callback_test_button = function(self, item)
		log("Pressed Button")
	end

	MenuCallbackHandler.callback_test_multi = function(self, item)
		JsonMenuExample._data.multi_value = item:value()
		JsonMenuExample:Save()
		log("Multiple-choice value: " .. item:value())
	end

	--[[
		Keybind function calls operate slightly differently, if it doesn't already exist we assign the function
		to our mod table. When set via the menu, keybinds will automatically saved and loaded, so no logic is required
		to save and load their assigned keys.
	]]
	JsonMenuExample.func_test_keybind = function(self)
		log("Pressed keybind from example_menu.txt")
	end

	--[[
		Load our previously saved data from our save file.
	]]
	JsonMenuExample:Load()

	--[[
		Load our menu json file and pass it to our MenuHelper so that it can build our in-game menu for us.
		We pass our parent mod table as the second argument so that any keybind functions can be found and called
			as necessary.
		We also pass our data table as the third argument so that our saved values can be loaded from it.
	]]
	MenuHelper:LoadFromJsonFile( JsonMenuExample._path .. "example_menu.txt", JsonMenuExample, JsonMenuExample._data )

end )
