--[[---------------------------------------------
	AdvLib (C) Mijyuoon 2014-2020
	Contains various helper functions
-----------------------------------------------]]

adv = {
	-- Constants here
	HttpCache = {},
	Markup = {},
}

----- Config section ----------------------------
local Use_MoonScript	= true
local Use_PrintTable	= false
-------------------------------------------------

local MODLOAD = {}
adv._MODLOAD = MODLOAD


function loadmodule(name)
	if MODLOAD[name] then
		return MODLOAD[name]
	end

	local kname = "mods/base/" .. name:gsub("%.", "/") .. ".lua"
        local file = io.open(kname, "r")
        io.input(file)
        local s = io.read("*all")
        io.close(file)
	local func = loadstring(s)
   	if func then
		MODLOAD[name] = func() or true
		return MODLOAD[name]
	end
end


loadmodule("moonscript.lulpeg"):register(_G)
lpeg.re = loadmodule("moonscript.lpeg_re")

local trmv = table.remove
function adv.StrFormat(text, subst)
	if subst == nil then
		subst = text
		text = trmv(text, 1)
	end
	text = text:gsub("$([%w_]+)", function(s)
		local ns = tonumber(s) or s
		local subs = subst[ns]
		if subs == nil then return end
		return tostring(subs)
	end)
	return text
end

function adv.StrSet(text, pos, rep)
	pos = (pos < 0) and #text+pos+1 or pos
	if pos > #text or pos < 1 then return text end
	return text:sub(1, pos-1) .. rep .. text:sub(pos+1, -1)
end

function adv.StrSplit(str, sep)
	sep = lpeg.re.compile(sep)
	local elem = lpeg.C((1 - sep)^0)
	local gs = lpeg.Ct(elem * (sep * elem)^0)
	return gs:match(str)
end

adv.StrPatt  = lpeg.re.compile
adv.StrFind  = lpeg.re.find
adv.StrMatch = lpeg.re.match
adv.StrSubst = lpeg.re.gsub

function adv.TblMap(tbl, func)
	for ki, vi in pairs(tbl) do
		tbl[ki] = func(vi)
	end
	return tbl
end

function adv.TblMapN(tbl, func)
	local res = {}
	for ki, vi in pairs(tbl) do
		res[ki] = func(vi)
	end
	return res
end

function adv.TblFold(tbl, acc, func)
	local init = nil
	if func == nil then
		func = acc
		acc = tbl[1]
		init = acc
	end
	for _, vi in next, tbl, init do
		acc = func(acc, vi)
	end
	return acc
end

function adv.TblFilt(tbl, func)
	for ki, vi in pairs(tbl) do
		if not func(vi) then
			tbl[ki] = nil
		end
	end
	return tbl
end

function adv.TblFiltN(tbl, func)
	local res = {}
	for ki, vi in pairs(tbl) do
		if func(vi) then
			res[ki] = vi
		end
	end
	return res
end

function adv.TblSlice(tbl, from, to, step)
	local res = {}
	from = from or 1
	to = to or #tbl
	step = step or 1
	for i = from, to, step do
		res[#res+1] = tbl[i]
	end
	return res
end

function adv.TblAny(tbl, func)
	for ki, vi in pairs(tbl) do
		if func(vi) then
			return true
		end
	end
	return false
end

function adv.TblAll(tbl, func)
	for ki, vi in pairs(tbl) do
		if not func(vi) then
			return false
		end
	end
	return true
end

function adv.TblKeys(tbl)
	local res = {}
	for k in pairs(tbl) do
		res[#res+1] = k
	end
	return res
end

for _, kn in pairs{"K", "V", "KV"} do
	adv["TblWeak" .. kn] = function()
		return setmetatable({}, {
			__mode = kn:lower()
		})
	end
end

local function prefix(str)
	local stri = tostring(str)
	return (stri:gsub("^[a-z]+: ", ""))
end
local function do_printr(arg, spaces, passed)
	local ty = type(arg)
	if ty == "table" then
		passed[arg] = true
		Msg(adv.StrFormat{"(table) $v {\n",
			v = prefix(arg)})
		for k ,v in pairs(arg) do
			if not passed[v] then
				Msg(adv.StrFormat{"  $s($t) $k => ",
					s = spaces, t = type(k), k = k})
				do_printr(rawget(arg, k), spaces.."  ", passed)
			else
				Msg(adv.StrFormat{"  $s($t) $k => [RECURSIVE TABLE: $v]\n",
					s = spaces, t = type(k), k = k, v = prefix(v)})
			end
		end
		Msg(spaces .. "}\n")
	elseif ty == "function" then
		Msg(adv.StrFormat{"($t) $v\n",
			t = ty, v = prefix(arg)})
	elseif ty == "string" then
		Msg(adv.StrFormat{"($t) '$v'\n",
			t = ty, v = arg})
	elseif ty == "nil" then
		Msg(adv.StrFormat{"($t)\n",
			t = ty})
	else
		Msg(adv.StrFormat{"($t) $v\n",
			t = ty, v = arg})
	end
end

function adv.TblPrint(...)
	local arg = {...}
	for i = 1, #arg do
		do_printr(arg[i], "", {})
	end
end

if Use_MoonScript then
   moonscript = loadmodule "moonscript.base"
end
