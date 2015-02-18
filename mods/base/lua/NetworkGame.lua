CloneClass(NetworkGame)

Hooks:Register("NetworkGame_lobby_entered")
function NetworkGame.on_entered_lobby(self)
	self.orig.on_entered_lobby(self)
	local local_peer = managers.network:session():local_peer()
	local id = local_peer:id()
	Hooks:Call("NetworkGame_lobby_entered", local_peer, id)
end

Hooks:Register("NetworkGame_joined_game")
function NetworkGame.on_game_joined(self)
	self.orig.on_game_joined(self)
	local local_peer = managers.network:session():local_peer()
	local id = local_peer:id()
	Hooks:Call("NetworkGame_joined_game", local_peer, id)
end

Hooks:Register("NetworkGame_peer_entered_lobby")
function NetworkGame.on_peer_entered_lobby(self, peer_id)
	self.orig.on_peer_entered_lobby(self, peer_id)
	local peer = managers.network:session():peer(peer_id)
	Hooks:Call("NetworkGame_peer_entered_lobby", peer, peer_id)
end

Hooks:Register("NetworkGame_peer_added")
function NetworkGame.on_peer_added(self, peer, peer_id)
	self.orig.on_peer_added(self, peer, peer_id)
	Hooks:Call("NetworkGame_peer_added", peer, peer_id)
end
