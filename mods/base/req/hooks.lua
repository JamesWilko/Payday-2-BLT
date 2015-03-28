
_G.Hooks = Hooks or {}
Hooks._registered_hooks = Hooks._registered_hooks or {}
Hooks._prehooks = Hooks._prehooks or {}
Hooks._posthooks = Hooks._posthooks or {}

--[[
	Hooks:Register( key )
		Registers a hook so that functions can be added to it, and later called 
	key, Unique identifier for the hook, so that hooked functions can be added to it
]]
function Hooks:RegisterHook( key )
	self._registered_hooks[key] = self._registered_hooks[key] or {}
end

--[[
	Hooks:Register( key )
		Functionaly the same as Hooks:RegisterHook
]]
function Hooks:Register( key )
	self:RegisterHook( key )
end

--[[
	Hooks:AddHook( key, id, func )
		Adds a function call to a hook, so that it will be called when the hook is
	key, 	The unique identifier of the hook to be called on
	id, 	A unique identifier for this specific function call
	func, 	The function to call with the hook 
]]
function Hooks:AddHook( key, id, func )
	self._registered_hooks[key] = self._registered_hooks[key] or {}
	-- Update existing hook
	for k, v in pairs( self._registered_hooks[key] ) do
		if v.id == id then
			v.func = func
			return
		end
	end
	-- Add new hook, if id doesn't exist
	local tbl = {
		id = id,
		func = func
	}
	table.insert( self._registered_hooks[key], tbl )
end

--[[
	Hooks:Add( key, id, func )
		Functionaly the same as Hooks:AddHook
]]
function Hooks:Add( key, id, func )
	self:AddHook( key, id, func )
end

--[[
	Hooks:UnregisterHook( key )
		Removes a hook, so that it will not call any functions
	key, The unique identifier of the hook to unregister
]]
function Hooks:UnregisterHook( key )
	self._registered_hooks[key] = nil
end

--[[
	Hooks:Unregister( key )
		Functionaly the same as Hooks:UnregisterHook
]]
function Hooks:Unregister( key )
	self:UnregisterHook( key )
end

--[[
	Hooks:Remove( id )
		Removes a hooked function call with the specified id to prevent it from being called
	id, Removes the function call and prevents it from being called
]]
function Hooks:Remove( id )

	for k, v in pairs(self._registered_hooks) do
		if type(v) == "table" then
			for x, y in pairs( v ) do
				if y.id == id then
					y = nil
				end
			end
		end
	end
	
end

--[[
	Hooks:Call( key, ... )
			Calls a specified hook, and all of its hooked functions
	key,	The unique identifier of the hook to call its hooked functions
	args,	The arguments to pass to the hooked functions 
]]
function Hooks:Call( key, ... )

	if not self._registered_hooks[key] then
		return
	end

	for k, v in pairs(self._registered_hooks[key]) do
		if v then
			if type(v.func) == "function" then
				v.func( ... )
			end
		end
	end

end

--[[
	Hooks:ReturnCall( key, ... )
		Calls a specified hook, and returns the first non-nil value returned by a hooked function
	key, 		The unique identifier of the hook to call its hooked functions
	args, 		The arguments to pass to the hooked functions
	returns, 	The first non-nil value returned by a hooked function
]]
function Hooks:ReturnCall( key, ... )

	if not self._registered_hooks[key] then
		return
	end

	for k, v in pairs(self._registered_hooks[key]) do
		if v then
			if type(v.func) == "function" then

				-- Holy hell would you look at this shit
				local r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32 = v.func( ... )
				if r1 ~= nil then
					return r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32
				end

			end
		end
	end

end

--[[
	Hooks:PreHook( object, func, id, pre_call )
		Automatically hooks a function to be called before the specified function on a specified object
	object, 	The object for the hooked function to be called on
	func, 		The name of the function (as a string) on the object for the hooked call to be ran before
	id, 		The unique identifier for this prehook
	pre_call, 	The function to be called before the func on object
]]
function Hooks:PreHook( object, func, id, pre_call )

	if not object then
		self:_PrePostHookError(func)
		return
	end

	if object and self._prehooks[object] == nil then
		self._prehooks[object] = {}
	end

	if object and self._prehooks[object][func] == nil then

		self._prehooks[object][func] = {
			original = object[func],
			overrides = {}
		}

		object[func] = function(...)

			local hooked_func = self._prehooks[object][func]
			local r, _r

			for k, v in ipairs(hooked_func.overrides) do
				if v.func then
					_r = v.func(...)
				end
				if _r then
					r = _r
				end
			end

			_r = hooked_func.original(...)
			if _r then
				r = _r
			end

			return r

		end

	end

	for k, v in pairs( self._prehooks[object][func].overrides ) do
		if v.id == id then
			return
		end
	end

	local func_tbl = {
		id = id,
		func = pre_call,
	}
	table.insert( self._prehooks[object][func].overrides, func_tbl )

end

--[[
	Hooks:RemovePreHook( id )
		Removes the prehook with id, and prevents it from being run
	id, The unique identifier of the prehook to remove
]]
function Hooks:RemovePreHook( id )

	for object_i, object in pairs( self._prehooks ) do
		for func_i, func in pairs( object ) do
			for override_i, override in ipairs( func.overrides ) do
				if override and override.id == id then
					table.remove( func.overrides, override_i )
				end
			end
		end
	end

end

--[[
	Hooks:PostHook( object, func, id, post_call )
		Automatically hooks a function to be called after the specified function on a specified object
	object, 	The object for the hooked function to be called on
	func, 		The name of the function (as a string) on the object for the hooked call to be ran after
	id, 		The unique identifier for this posthook
	post_call, 	The function to be called after the func on object
]]
function Hooks:PostHook( object, func, id, post_call )

	if not object then
		self:_PrePostHookError(func)
		return
	end

	if object and self._posthooks[object] == nil then
		self._posthooks[object] = {}
	end

	if object and self._posthooks[object][func] == nil then

		self._posthooks[object][func] = {
			original = object[func],
			overrides = {}
		}

		object[func] = function(...)

			local hooked_func = self._posthooks[object][func]
			local r, _r

			_r = hooked_func.original(...)
			if _r then
				r = _r
			end

			for k, v in ipairs(hooked_func.overrides) do
				if v.func then
					_r = v.func(...)
				end
				if _r then
					r = _r
				end
			end

			return r

		end

	end

	for k, v in pairs( self._posthooks[object][func].overrides ) do
		if v.id == id then
			return
		end
	end

	local func_tbl = {
		id = id,
		func = post_call,
	}
	table.insert( self._posthooks[object][func].overrides, func_tbl )

end

--[[
	Hooks:RemovePostHook( id )
		Removes the posthook with id, and prevents it from being run
	id, The unique identifier of the posthook to remove
]]
function Hooks:RemovePostHook( id )

	for object_i, object in pairs( self._posthooks ) do
		for func_i, func in pairs( object ) do
			for override_i, override in ipairs( func.overrides ) do
				if override and override.id == id then
					table.remove( func.overrides, override_i )
				end
			end
		end
	end

end

function Hooks:_PrePostHookError( func )
	log("[Hooks] Error: Could not hook function '", tostring(func), "'!")
end
