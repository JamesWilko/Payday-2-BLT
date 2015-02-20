
local mt = getmetatable(_G)
if mt == nil then
	mt = {}
	setmetatable(_G, mt)
end
mt.__declared = {}
function mt.__newindex(t, n, v)
	if not mt.__declared[n] then
		local info = debug.getinfo(2, "S")
		mt.__declared[n] = true
	end
	rawset(t, n, v)
end

function mt.__index(t, n)
	if not mt.__declared[n] then
		local info = debug.getinfo(2, "S")
		if info and info.what ~= "main" and info.what ~= "C" then
			return nil
		end
	end
end
