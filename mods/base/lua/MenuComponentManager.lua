
CloneClass( MenuComponentManager )

Hooks:RegisterHook("MenuComponentManagerInitialize")
function MenuComponentManager.init( self )
	self.orig.init( self )
	Hooks:Call( "MenuComponentManagerInitialize", self )
end

Hooks:RegisterHook("MenuComponentManagerUpdate")
function MenuComponentManager.update( self, t, dt )
	self.orig.update( self, t, dt )
	Hooks:Call( "MenuComponentManagerUpdate", self, t, dt )
end

Hooks:RegisterHook("MenuComponentManagerPreSetActiveComponents")
function MenuComponentManager.set_active_components( self, components, node )
	Hooks:Call( "MenuComponentManagerPreSetActiveComponents", self, components, node )
	self.orig.set_active_components(self, components, node)
end

Hooks:RegisterHook("MenuComponentManagerOnMousePressed")
function MenuComponentManager.mouse_pressed( self, o, button, x, y )
	local r = self.orig.mouse_pressed(self, o, button, x, y)
	local val = Hooks:ReturnCall("MenuComponentManagerOnMousePressed", self, o, button, x, y)
	if val then
		r = val
	end
	return r
end

Hooks:RegisterHook("MenuComponentManagerOnMouseMoved")
function MenuComponentManager.mouse_moved( self, o, x, y )
	local hover, pointer = self.orig.mouse_moved( self, o, x, y )
	local ohover, opointer = Hooks:ReturnCall("MenuComponentManagerOnMouseMoved", self, o, x, y)
	if ohover ~= nil then
		hover = ohover
		pointer = opointer
	end
	return hover, pointer
end
