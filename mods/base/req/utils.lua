
_G.Utils = _G.Utils or {}

--[[
	CloneClass( class )
		Copies an existing class into an orig table, so that class functions can be overwritten and called again easily
	class, The class table to clone
]]
function _G.CloneClass( class )
	if not class.orig then
		class.orig = clone(class)
	end
end

--[[
	PrintTable( tbl )
		Prints the contents of a table to your console
		Warning, may cause game slowdown if the table is fairly large, only for debugging purposes
	tbl, The table to print to console
]]
function _G.PrintTable( tbl, cmp )
	cmp = cmp or {}
	if type(tbl) == "table" then
		for k, v in pairs (tbl) do
			if type(v) == "table" and not cmp[v] then
				cmp[v] = true
				log( string.format("[\"%s\"] = table", tostring(k)) );
				PrintTable (v, cmp)
			else
				log( string.format("\"%s\" = %s", tostring(k), tostring(v)) )
			end
		end
	else
		log(tbl)
	end
end

--[[
	SaveTable( tbl, file )
		Saves the contents of a table to the specified file
	tbl, 	The table to save to a file
	file, 	The path (relative to payday2_win32_release.exe) and file name to save the table to
]]
function _G.SaveTable( tbl, file )
	Utils.DoSaveTable( tbl, {}, file, nil, "" )
end

function Utils.DoSaveTable( tbl, cmp, fileName, fileIsOpen, preText )

	local file = nil
	if fileIsOpen == nil then
		file = io.open(fileName, "w")
	else
		file = fileIsOpen
	end

	cmp = cmp or {}
	if type(tbl) == "table" then
		for k, v in pairs(tbl) do
			if type(v) == "table" and not cmp[v] then
				cmp[v] = true
				file:write( preText .. string.format("[\"%s\"] -> table", tostring (k)) .. "\n" )
				Utils.DoSaveTable(v, cmp, fileName, file, preText .. "\t")
			else
				file:write( preText .. string.format( "\"%s\" -> %s", tostring(k), tostring(v) ) .. "\n" )
			end
		end
	else
		file:write( preText .. tbl .. "\n")
	end

	if fileIsOpen == nil then
		file:close()
	end

end

Vector3.StringFormat = "%08f,%08f,%08f"
Vector3.MatchFormat = "([-0-9.]+),([-0-9.]+),([-0-9.]+)"

--[[
	Vector3.ToString( v )
		Converts a Vector3 to a string, useful in conjunction with Networking
	v, 			The Vector3 to convert to a formatted string
	return, 	A formatted string containing the data of the Vector3
]]
function Vector3.ToString( v )
	return string.format( Vector3.StringFormat, v.x, v.y, v.z )
end

--[[
	string.ToVector3( string )
		Converts a formatted string to a Vector3, useful in conjunction with Networking
	string, 	The string to convert to a Vector3
	return, 	A Vector3 of the converted string or nil if no conversion could be made
]]
function string.ToVector3( string )
	local x, y, z = string:match( Vector3.MatchFormat )
	if x ~= nil and y ~= nil and z ~= nil then
		return Vector3( tonumber(x), tonumber(y), tonumber(z) )
	end
	return nil
end

--[[
	string.is_nil_or_empty( str )
		Returns if a string exists or not
	str, 		The string to check if it exists or is empty
	return, 	Returns false if the string is empty ("") or nil, true otherwise
]]
function string.is_nil_or_empty( str )
	return str == "" or str == nil
end

--[[
	math.round_with_precision( num, idp )
		Rounds a number to the specified precision (decimal places)
	num, 		The number to round
	idp, 		The number of decimal places to round to (0 default)
	return, 	The input number rounded to the input precision (or floored integer)
]]
function math.round_with_precision( num, idp )
	local mult = 10 ^ (idp or 0)
	return math.floor(num * mult + 0.5) / mult
end

--[[
	GetPlayerAimPos( player, maximum_range )
		Gets the point in the world, as a Vector3, where the player is aiming at
	player, 		The player to get the aiming position of
	maximum_range, 	The maximum distance to check for a point (default 100000, 1km)
	return, 		A Vector3 containing the location that the player is looking at, or false if the player was not looking at anything
			or was looking at something past the maximum_range
]]
function _G.GetPlayerAimPos( player, maximum_range )
	local ray = GetCrosshairRay(player:camera():position(), player:camera():position() + player:camera():forward() * (maximum_range or 100000))
	if not ray then
		return false
	end
	return ray.hit_position
end

--[[
	GetCrosshairRay( from, to, slot_mask )
		Gets a ray between two points and checks for a collision with the slot_mask along the ray
	from, 		The starting position of the ray, defaults to the player's head
	to, 		The ending position of the ray, defaults to 1m in from of the player's head
	slot_mask, 	The collision group to check against the ray, defaults to all objects the player can shoot
	return, 	A table containing the ray information
]]
function _G.GetCrosshairRay( from, to, slot_mask )

	slot_mask = slot_mask or "bullet_impact_targets"

	local player = managers.player:player_unit()
	local from = from or player:camera():position()
	local mvecTo = Vector3()

	mvector3.set( mvecTo, player:camera():forward() )
	mvector3.add(to, from)

	local colRay = World:raycast("ray", from, to, "slot_mask", managers.slot:get_mask(slot_mask))
	return colRay

end
