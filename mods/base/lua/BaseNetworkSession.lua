CloneClass(BaseNetworkSession)

Hooks:Register("BaseNetworkSessionOnEnteredLobby")
function BaseNetworkSession.on_entered_lobby(self)
	self.orig.on_entered_lobby(self)
	local local_peer = managers.network:session():local_peer()
	local id = local_peer:id()
	Hooks:Call("BaseNetworkSessionOnEnteredLobby", local_peer, id)
end

Hooks:Register("BaseNetworkSessionOnPeerEnteredLobby")
function BaseNetworkSession.on_peer_entered_lobby(self, peer)
	self.orig.on_peer_entered_lobby(self, peer)
	local peer_id = peer:id()
	Hooks:Call("BaseNetworkSessionOnPeerEnteredLobby", peer, peer_id)
end

Hooks:Register("BaseNetworkSessionOnPeerRemoved")
function BaseNetworkSession._on_peer_removed(self, peer, peer_id, reason)
	self.orig._on_peer_removed(self, peer, peer_id, reason)
	Hooks:Call("BaseNetworkSessionOnPeerRemoved", peer, peer_id, reason)
end

Hooks:Register("BaseNetworkSessionOnLoadComplete")
function BaseNetworkSession.on_load_complete(self, simulation)
	self.orig.on_load_complete(self, simulation)
	local local_peer = managers.network:session():local_peer()
	local id = local_peer:id()
	Hooks:Call("BaseNetworkSessionOnLoadComplete", local_peer, id)
end
