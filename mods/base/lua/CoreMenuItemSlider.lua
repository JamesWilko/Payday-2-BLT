
core:module("CoreMenuItemSlider")
local CloneClass = _G.CloneClass
local tweak_data = _G.tweak_data

CloneClass( ItemSlider )

function ItemSlider.setup_gui(self, node, row_item)
	local r = self.orig.setup_gui(self, node, row_item)
	row_item.gui_slider_text:set_font_size( tweak_data.menu.stats_font_size )
	return r
end

function ItemSlider.set_value(self, value)
	self._value = math.min(math.max(self._min, value), self._max)
	self:dirty()
end

function ItemSlider.reload(self, row_item, node)
	local r = self.orig.reload(self, row_item, node)
	local value = self:show_value() and string.format("%.2f", math.round_with_precision(self:value(), 2)) or string.format("%.0f", self:percentage()) .. "%"
	row_item.gui_slider_text:set_text(value)
	return r
end
