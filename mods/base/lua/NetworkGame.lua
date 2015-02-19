CloneClass(NetworkGame)

Hooks:Register("NetworkGameOnEnteredLobby")
function NetworkGame.on_entered_lobby(self)
	self.orig.on_entered_lobby(self)
	local local_peer = managers.network:session():local_peer()
	local id = local_peer:id()
	Hooks:Call("NetworkGameOnEnteredLobby", local_peer, id)
end

Hooks:Register("NetworkGameOnGameJoined")
function NetworkGame.on_game_joined(self)
	self.orig.on_game_joined(self)
	local local_peer = managers.network:session():local_peer()
	local id = local_peer:id()
	Hooks:Call("NetworkGameOnGameJoined", local_peer, id)
end

Hooks:Register("NetworkGameOnPeerEnteredLobby")
function NetworkGame.on_peer_entered_lobby(self, peer_id)
	self.orig.on_peer_entered_lobby(self, peer_id)
	local peer = managers.network:session():peer(peer_id)
	Hooks:Call("NetworkGameOnPeerEnteredLobby", peer, peer_id)
end

Hooks:Register("NetworkGameOnPeerAdded")
function NetworkGame.on_peer_added(self, peer, peer_id)
	self.orig.on_peer_added(self, peer, peer_id)
	Hooks:Call("NetworkGameOnPeerAdded", peer, peer_id)
end

Hooks:Register("NetworkGameOnPeerRemoved")
function NetworkGame.on_peer_removed(self, peer, peer_id, reason)
	self.orig.on_peer_removed(self, peer, peer_id, reason)
	Hooks:Call("NetworkGameOnPeerRemoved", peer, peer_id, reason)
end

Hooks:Register("NetworkGameOnLoadComplete")
function NetworkGame.on_load_complete(self)
	self.orig.on_load_complete(self)
	local local_peer = managers.network:session():local_peer()
	local id = local_peer:id()
	Hooks:Call("NetworkGameOnLoadComplete", local_peer, id)
end
