
_G.LuaNetworking = _G.LuaNetworking or {}
local LNetwork = _G.LuaNetworking

LNetwork.HiddenChannel = 4
LNetwork.AllPeers = "GNAP"
LNetwork.AllPeersString = "{1}/{2}/{3}"
LNetwork.SinglePeer = "GNSP"
LNetwork.SinglePeerString = "{1}/{2}/{3}/{4}"
LNetwork.ExceptPeer = "GNEP"
LNetwork.ExceptPeerString = "{1}/{2}/{3}/{4}"
LNetwork.Split = "[/]"

function LNetwork:IsMultiplayer()
	if not managers.network then
		return false
	end
	return managers.network:session()
end

function LNetwork:IsHost()
	if not Network then
		return false
	end
	return not Network:is_client()
end

function LNetwork:IsClient()
	if not Network then
		return false
	end
	return Network:is_client()
end

function LNetwork:LocalPeerID()
	if managers.network == nil or managers.network:session() == nil or managers.network:session():local_peer() == nil then
		return 0
	end
	return managers.network:session():local_peer():id() or 0
end

function LNetwork:TableToString(tbl)
	local str = ""
	for k, v in pairs(tbl) do
		if str ~= "" then
			str = str .. ","
		end
		str = str .. ("{0}|{1}"):gsub("{0}", tostring(k)):gsub("{1}", tostring(v))
	end
	return str
end

function LNetwork:StringToTable(str)
	local tbl = {}
	local tblPairs = string.split( str, "[,]" )
	for k, v in pairs(tblPairs) do
		local pairData = string.split( v, "[|]" )
		tbl[ pairData[1] ] = pairData[2]
	end
	return tbl
end

function LNetwork:GetNameFromPeerID(id)

	if managers.network and managers.network:session() and managers.network:session():peers() then
		
		for k, v in pairs( managers.network:session():peers() ) do
			if k == id then
				return v:name()
			end
		end

	end

	return "No Name"
	
end

function LNetwork:GetPeers()
	return managers.network:session():peers()
end

function LNetwork:GetNumberOfPeers()
	local i = 0
	for k, v in pairs( managers.network:session():peers() ) do
		i = i + 1
	end
	return i
end

function LNetwork:SendToPeers(type_prm, data)
	local dataString = LNetwork.AllPeersString
	dataString = dataString:gsub("{1}", LNetwork.AllPeers)
	dataString = dataString:gsub("{2}", type_prm)
	dataString = dataString:gsub("{3}", data)
	LNetwork:SendStringThroughChat(dataString)
end

function LNetwork:SendToPeer(peer, type_prm, data)
	local dataString = LNetwork.SinglePeerString
	dataString = dataString:gsub("{1}", LNetwork.SinglePeer)
	dataString = dataString:gsub("{2}", peer)
	dataString = dataString:gsub("{3}", type_prm)
	dataString = dataString:gsub("{4}", data)
	LNetwork:SendStringThroughChat(dataString)
end

function LNetwork:SendToPeersExcept(peer, type_prm, data)
	local dataString = LNetwork.ExceptPeerString
	local peerStr = peer
	if type(peer) == "table" then
		peerStr = ""
		for k, v in pairs(peer) do
			if peerStr ~= "" then
				peerStr = peerStr .. ","
			end
			peerStr = peerStr .. tostring(v)
		end
	end

	dataString = dataString:gsub("{1}", LNetwork.ExceptPeer)
	dataString = dataString:gsub("{2}", peerStr)
	dataString = dataString:gsub("{3}", type_prm)
	dataString = dataString:gsub("{4}", data)
	LNetwork:SendStringThroughChat(dataString)
end

function LNetwork:SendStringThroughChat(message)
	if ChatManager._receivers == nil then
		ChatManager._receivers = {}
	end
	ChatManager:send_message( LNetwork.HiddenChannel, tostring(LNetwork:LocalPeerID()), message )
end

Hooks:Add("ChatManagerOnReceiveMessage", "ChatManagerOnReceiveMessage_Network", function(channel_id, name, message, color, icon)

	local s = "[{1}] {2}: {3}"
	s = s:gsub("{1}", channel_id)
	s = s:gsub("{2}", name)
	s = s:gsub("{3}", message)
	log(s)

	local senderID = nil
	if LNetwork:IsMultiplayer() then

		if name == managers.network:session():local_peer():name() then
			senderID = LNetwork:LocalPeerID()
		end

		for k, v in pairs( managers.network:session():peers() ) do
			if v:name() == name then
				senderID = k
			end
		end

	end

	if senderID == LNetwork:LocalPeerID() then return end

	if tonumber(channel_id) == LNetwork.HiddenChannel then
		LNetwork:ProcessChatString(senderID or name, message, color, icon)
	end

end)

Hooks:RegisterHook("NetworkReceivedData")
function LNetwork:ProcessChatString(sender, message, color, icon)

	local splitData = string.split( message, LNetwork.Split )
	local msgType = splitData[1]
	if msgType == LNetwork.AllPeers then
		LNetwork:ProcessAllPeers(sender, message, color, icon)
	end
	if msgType == LNetwork.SinglePeer then
		LNetwork:ProcessSinglePeer(sender, message, color, icon)
	end
	if msgType == LNetwork.ExceptPeer then
		LNetwork:ProcessExceptPeer(sender, message, color, icon)
	end
	
end

function LNetwork:ProcessAllPeers(sender, message, color, icon)
	local splitData = string.split( message, LNetwork.Split )
	Hooks:Call("NetworkReceivedData", sender, splitData[2], splitData[3])
end

function LNetwork:ProcessSinglePeer(sender, message, color, icon)

	local splitData = string.split( message, LNetwork.Split )
	local toPeer = tonumber( splitData[2] )

	if toPeer == LNetwork:LocalPeerID() then
		Hooks:Call("NetworkReceivedData", sender, splitData[3], splitData[4])
	end

end

function LNetwork:ProcessExceptPeer(sender, message, color, icon)
	
	local splitData = string.split( message, LNetwork.Split )
	local exceptedPeers = string.split( splitData[2], "[,]" )

	local excepted = false
	for k, v in pairs(exceptedPeers) do
		if tonumber(v) == LNetwork:LocalPeerID() then
			excepted = true
		end
	end

	if not excepted then
		Hooks:Call("NetworkReceivedData", sender, splitData[3], splitData[4])
	end

end

-- Extensions
LNetwork._networked_colour_string = "r:{1}|g:{2}|b:{3}|a:{4}"
function LNetwork:ColourToString(col)
	local dataString = LNetwork._networked_colour_string
	dataString = dataString:gsub("{1}", math.round_with_precision(col.r, 4))
	dataString = dataString:gsub("{2}", math.round_with_precision(col.g, 4))
	dataString = dataString:gsub("{3}", math.round_with_precision(col.b, 4))
	dataString = dataString:gsub("{4}", math.round_with_precision(col.a, 4))
	return dataString
end

function LNetwork:StringToColour(str)

	local data = string.split( str, "[|]" )
	if #data < 4 then
		return nil
	end
	
	local split_str = "[:]"
	local r = tonumber(string.split(data[1], split_str)[2])
	local g = tonumber(string.split(data[2], split_str)[2])
	local b = tonumber(string.split(data[3], split_str)[2])
	local a = tonumber(string.split(data[4], split_str)[2])

	return Color(a, r, g, b)

end
