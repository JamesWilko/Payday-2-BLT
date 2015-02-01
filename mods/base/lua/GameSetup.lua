
CloneClass( GameSetup )

Hooks:RegisterHook("GameSetupUpdate")
function GameSetup.update(this, t, dt)
	Hooks:Call("GameSetupUpdate", t, dt)
	return this.orig.update(this, t, dt)
end
