
_G.JsonMenuExample = _G.JsonMenuExample or {}
JsonMenuExample._path = ModPath
JsonMenuExample._data = {} 
JsonMenuExample._data_path = ModPath .. "saved_example_data.txt"

function JsonMenuExample:Save()
	local file = io.open( self._data_path, "w+" )
	if file then
		file:write( json.encode( self._data ) )
		file:close()
	end
end

function JsonMenuExample:Load()
	local file = io.open( self._data_path, "r" )
	if file then
		self._data = json.decode( file:read("*all") )
		file:close()
	end
end


Hooks:Add("LocalizationManagerPostInit", "LocalizationManagerPostInit_JsonMenuExample", function( loc )
	loc:load_localization_file( JsonMenuExample._path .. "en.txt")
end)

Hooks:Add( "MenuManagerInitialize", "MenuManagerInitialize_JsonMenuExample", function( menu_manager )

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

	JsonMenuExample.func_test_keybind = function(self)
		log("Pressed keybind from example_menu.txt")
	end

	JsonMenuExample:Load()
	MenuHelper:LoadFromJsonFile( JsonMenuExample._path .. "example_menu.txt", JsonMenuExample, JsonMenuExample._data )

end )
