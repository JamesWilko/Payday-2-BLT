
local Hooks = Hooks
local CloneClass = CloneClass
core:module("CoreMenuLogic")

CloneClass( Logic )

Hooks:RegisterHook("LogicOnSelectNode")
function Logic.select_node(self, node_name, queue, ...)
	self.orig.select_node(self, node_name, queue, ...)
	Hooks:Call( "LogicOnSelectNode", self, node_name, queue, ... )
end
