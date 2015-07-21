CloneClass(NetworkManager)

Hooks:Register("NetworkManagerOnPeerAdded")
function NetworkManager.on_peer_added(self, peer, peer_id)
	self.orig.on_peer_added(self, peer, peer_id)
	Hooks:Call("NetworkManagerOnPeerAdded", peer, peer_id)
end
