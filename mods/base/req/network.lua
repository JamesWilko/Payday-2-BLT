
_G.GoonBase.Network = _G.GoonBase.Network or {}
local GNetwork = _G.GoonBase.Network

GNetwork.HiddenChannel = 4
GNetwork.AllPeers = "GNAP"
GNetwork.AllPeersString = "{1}/{2}/{3}"
GNetwork.SinglePeer = "GNSP"
GNetwork.SinglePeerString = "{1}/{2}/{3}/{4}"
GNetwork.ExceptPeer = "GNEP"
GNetwork.ExceptPeerString = "{1}/{2}/{3}/{4}"
GNetwork.Split = "[/]"

function GNetwork:IsMultiplayer()
	if managers.network == nil then
		return false
	end
	return managers.network:session()
end

function GNetwork:IsHost()
	if not Network then
		return false
	end
	return not Network:is_client()
end

function GNetwork:IsClient()
	if not Network then
		return false
	end
	return Network:is_client()
end

function GNetwork:LocalPeerID()
	if managers.network == nil or managers.network:session() == nil or managers.network:session():local_peer() == nil then
		return 0
	end
	return managers.network:session():local_peer():id() or 0
end

function GNetwork:TableToString(tbl)
	local str = ""
	for k, v in pairs(tbl) do
		if str ~= "" then
			str = str .. ","
		end
		str = str .. ("{0}|{1}"):gsub("{0}", tostring(k)):gsub("{1}", tostring(v))
	end
	return str
end

function GNetwork:StringToTable(str)
	local tbl = {}
	local tblPairs = string.split( str, "[,]" )
	for k, v in pairs(tblPairs) do
		local pairData = string.split( v, "[|]" )
		tbl[ pairData[1] ] = pairData[2]
	end
	return tbl
end

function GNetwork:GetNameFromPeerID(id)

	for k, v in pairs( managers.network:session():peers() ) do
		if k == id then
			return v:name()
		end
	end

	return "No Name"
	
end

function GNetwork:GetPeers()
	return managers.network:session():peers()
end

function GNetwork:GetNumberOfPeers()
	local i = 0
	for k, v in pairs( managers.network:session():peers() ) do
		i = i + 1
	end
	return i
end

function GNetwork:SendToPeers(type, data)
	local dataString = GNetwork.AllPeersString
	dataString = dataString:gsub("{1}", GNetwork.AllPeers)
	dataString = dataString:gsub("{2}", type)
	dataString = dataString:gsub("{3}", data)
	GNetwork:SendStringThroughChat(dataString)
end

function GNetwork:SendToPeer(peer, type, data)
	local dataString = GNetwork.SinglePeerString
	dataString = dataString:gsub("{1}", GNetwork.SinglePeer)
	dataString = dataString:gsub("{2}", peer)
	dataString = dataString:gsub("{3}", type)
	dataString = dataString:gsub("{4}", data)
	GNetwork:SendStringThroughChat(dataString)
end

function GNetwork:SendToPeersExcept(peer, type, data)
	local dataString = GNetwork.ExceptPeerString
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

	dataString = dataString:gsub("{1}", GNetwork.ExceptPeer)
	dataString = dataString:gsub("{2}", peerStr)
	dataString = dataString:gsub("{3}", type)
	dataString = dataString:gsub("{4}", data)
	GNetwork:SendStringThroughChat(dataString)
end

function GNetwork:SendStringThroughChat(message)
	if ChatManager._receivers == nil then
		ChatManager._receivers = {}
	end
	ChatManager:send_message( GNetwork.HiddenChannel, tostring(GNetwork:LocalPeerID()), message )
end

Hooks:Add("ChatManagerOnReceiveMessage", "ChatManagerOnReceiveMessage_Network", function(channel_id, name, message, color, icon)

	local s = "[{1}] {2}: {3}"
	s = s:gsub("{1}", channel_id)
	s = s:gsub("{2}", name)
	s = s:gsub("{3}", message)
	log(s)

	local senderID = nil
	if GNetwork:IsMultiplayer() then

		if name == managers.network:session():local_peer():name() then
			senderID = GNetwork:LocalPeerID()
		end

		for k, v in pairs( managers.network:session():peers() ) do
			if v:name() == name then
				senderID = k
			end
		end

	end

	if senderID == GNetwork:LocalPeerID() then return end

	if tonumber(channel_id) == GNetwork.HiddenChannel then
		GNetwork:ProcessChatString(senderID or name, message, color, icon)
	end

end)

Hooks:RegisterHook("NetworkReceivedData")
function GNetwork:ProcessChatString(sender, message, color, icon)

	local splitData = string.split( message, GNetwork.Split )
	local msgType = splitData[1]
	if msgType == GNetwork.AllPeers then
		GNetwork:ProcessAllPeers(sender, message, color, icon)
	end
	if msgType == GNetwork.SinglePeer then
		GNetwork:ProcessSinglePeer(sender, message, color, icon)
	end
	if msgType == GNetwork.ExceptPeer then
		GNetwork:ProcessExceptPeer(sender, message, color, icon)
	end
	
end

function GNetwork:ProcessAllPeers(sender, message, color, icon)
	local splitData = string.split( message, GNetwork.Split )
	Hooks:Call("NetworkReceivedData", sender, splitData[2], splitData[3])
end

function GNetwork:ProcessSinglePeer(sender, message, color, icon)

	local splitData = string.split( message, GNetwork.Split )
	local toPeer = tonumber( splitData[2] )

	if toPeer == GNetwork:LocalPeerID() then
		Hooks:Call("NetworkReceivedData", sender, splitData[3], splitData[4])
	end

end

function GNetwork:ProcessExceptPeer(sender, message, color, icon)
	
	local splitData = string.split( message, GNetwork.Split )
	local exceptedPeers = string.split( splitData[2], "[,]" )

	local excepted = false
	for k, v in pairs(exceptedPeers) do
		if tonumber(v) == GNetwork:LocalPeerID() then
			excepted = true
		end
	end

	if not excepted then
		Hooks:Call("NetworkReceivedData", sender, splitData[3], splitData[4])
	end

end

-- Extensions

GNetwork._networked_colour_string = "r:{1}|g:{2}|b:{3}|a:{4}"
function GNetwork:PrepareNetworkedColourString(col)
	local dataString = GNetwork._networked_colour_string
	dataString = dataString:gsub("{1}", math.round_with_precision(col.r, 4))
	dataString = dataString:gsub("{2}", math.round_with_precision(col.g, 4))
	dataString = dataString:gsub("{3}", math.round_with_precision(col.b, 4))
	dataString = dataString:gsub("{4}", math.round_with_precision(col.a, 4))
	return dataString
end

function GNetwork:NetworkedColourStringToColour(str)

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
