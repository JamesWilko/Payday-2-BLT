
CloneClass( MenuSetup )

Hooks:RegisterHook("MenuUpdate")
function MenuSetup.update(self, t, dt)
	self.orig.update(self, t, dt)
	Hooks:Call("MenuUpdate", t, dt)
end

Hooks:RegisterHook("SetupOnQuit")
function MenuSetup.quit(self)
	Hooks:Call("SetupOnQuit", self)
	return self.orig.quit(self)
end
