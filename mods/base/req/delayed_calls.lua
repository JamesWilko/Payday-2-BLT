
DelayedCalls = DelayedCalls or {}
DelayedCalls._calls = DelayedCalls._calls or {}

Hooks:Add("MenuUpdate", "MenuUpdate_Queue", function( t, dt )
	DelayedCalls:Update(t, dt)
end)

Hooks:Add("GameSetupUpdate", "GameSetupUpdate_Queue", function( t, dt )
	DelayedCalls:Update(t, dt)
end)

function DelayedCalls:Update( time, deltaTime )

	local t = {}

	for k, v in pairs( self._calls ) do

		if v ~= nil then
			
			v.currentTime = v.currentTime + deltaTime
			if v.currentTime > v.timeToWait then
				if v.functionCall then
					v.functionCall()
				end
				v = nil
			else
				table.insert(t, v)
			end

		end

	end

	self._calls = t

end

--[[
	DelayedCalls:Add( id, time, func )
		Adds a function to be automatically called after a set delay
	id, 	Unique identifier for this delayed call
	time, 	Time in seconds to call the specified function after
	func, 	Function call to call after the time runs out 
]]
function DelayedCalls:Add( id, time, func )
	local queuedFunc = {
		functionCall = func,
		timeToWait = time,
		currentTime = 0
	}
	self._calls[id] = queuedFunc
end

--[[
	DelayedCalls:Remove( id )
		Removes a scheduled call before it can be automatically called
	id, Unique identifier for the delayed call remove 
]]
function DelayedCalls:Remove( id )
	self._calls[id] = nil
end
