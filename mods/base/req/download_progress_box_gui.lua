
DownloadProgressBoxGui = DownloadProgressBoxGui or class(TextBoxGui)
DownloadProgressBoxGui.TEXT = ""

function DownloadProgressBoxGui:init(...)
	local ws, title, text, content_data, config = ...
	config.forced_h = 100
	config.w = 600
	config.is_title_outside = true
	DownloadProgressBoxGui.super.init(self, ...)
end

local make_fine_text = function(text)
	local x, y, w, h = text:text_rect()
	text:set_size(w, h)
	text:set_position(math.round(text:x()), math.round(text:y()))
end

function DownloadProgressBoxGui:_create_text_box(ws, title, text, content_data, config)

	local panel = DownloadProgressBoxGui.super._create_text_box(self, ws, title, text, content_data, config)
	local small_text = {
		text = "",
		layer = 1,
		font = tweak_data.menu.pd2_small_font,
		font_size = tweak_data.menu.pd2_small_font_size,
		blend_mode = "add"
	}

	local medium_text = {
		text = "",
		layer = 1,
		font = tweak_data.menu.pd2_medium_font,
		font_size = tweak_data.menu.pd2_medium_font_size,
		blend_mode = "add"
	}

	local progress_text = self._scroll_panel:text(medium_text)
	progress_text:set_position(10, 30)
	progress_text:set_text( "000%" )
	make_fine_text(progress_text)

	local progress_bg = self._scroll_panel:rect({
		h = progress_text:h(),
		color = Color.black,
		alpha = 0.4,
		layer = 1
	})
	progress_bg:set_position(progress_text:right() + 4, progress_text:top())
	progress_bg:set_w(self._scroll_panel:w() - progress_bg:left() - 5)

	local progress_bar = self._scroll_panel:rect({
		color = Color.white,
		alpha = 1,
		layer = 2,
		blend_mode = "add"
	})
	progress_bar:set_shape(progress_bg:shape())
	progress_bar:set_w(0)
	progress_bar:grow(0, -4)
	progress_bar:move(2, 0)
	progress_bar:set_center_y(progress_bg:center_y())

	local progress_end = self._scroll_panel:rect({
		color = Color.white,
		alpha = 1,
		layer = 3,
		blend_mode = "add"
	})
	progress_end:set_shape(progress_bg:shape())
	progress_end:set_w(2)
	progress_end:grow(0, -4)
	progress_end:set_center_y(progress_bg:center_y())
	progress_end:set_right(progress_bg:right())

	local download_amt_text = self._scroll_panel:text(small_text)
	download_amt_text:set_text(managers.localization:to_upper_text("base_mod_download_download_progress"))
	make_fine_text(download_amt_text)
	download_amt_text:set_position(progress_bg:left(), progress_bg:bottom() + 2)

	self._panel:set_y(math.round(self._panel:y()))
	self._scroll_panel:set_y(math.round(self._scroll_panel:y()))
	self._anim_data = {
		progress_bar = progress_bar,
		progress_text = progress_text,
		download_amt_text = download_amt_text,

		start_progress_width = 0,
		progress_width = 0,
		end_progress_width = progress_end:right() - progress_bar:left(),

		bytes_downloaded = 0,
		bytes_total = 0,
	}


end

function DownloadProgressBoxGui:chk_close()
	return self._anim_data.download_complete
end

function DownloadProgressBoxGui._update(o, self)

	local init_done = false
	while not init_done do
		init_done = not not self._anim_data
		coroutine.yield()
	end
	wait(1)
	-- managers.menu_component:post_event("count_1")

	-- Download Progress
	while self._anim_data and not self._anim_data.mod_download_complete and not self._anim_data.mod_download_failed do
		
		coroutine.yield()

		local bytes_down = math.round(self._anim_data.bytes_downloaded / 1024)
		local bytes_total = math.round(self._anim_data.bytes_total / 1024)
		local bytes_tbl = {
			["downloaded"] = bytes_down,
			["total"] = bytes_total
		}
		local t = 0
		if self._anim_data.bytes_downloaded > 0 and self._anim_data.bytes_total > 0 then
			t = self._anim_data.bytes_downloaded / self._anim_data.bytes_total
		end

		self._anim_data.progress_width = math.lerp(self._anim_data.start_progress_width, self._anim_data.end_progress_width, t)
		self._anim_data.progress_bar:set_width(self._anim_data.progress_width)

		self._anim_data.progress_text:set_text( string.format("%000.f %%", t * 100) )
		self._anim_data.download_amt_text:set_text( managers.localization:to_upper_text("base_mod_download_download_progress", bytes_tbl) )

	end

	managers.menu_component:post_event("count_1_finished")

	-- Extract Progress
	self._anim_data.download_amt_text:set_text( managers.localization:to_upper_text("base_mod_download_download_progress_extract") )
	make_fine_text( self._anim_data.download_amt_text )

	while self._anim_data and not self._anim_data.mod_extraction_complete and not self._anim_data.mod_download_failed do
		coroutine.yield()
	end

	managers.menu_component:post_event("count_1_finished")

	-- Download Complete or Failed
	if not self._anim_data.mod_download_failed then
		-- Complete
		self._anim_data.download_amt_text:set_text( managers.localization:to_upper_text("base_mod_download_download_progress_complete") )
		make_fine_text( self._anim_data.download_amt_text )
	else
		-- Failed
		self._anim_data.download_amt_text:set_text( managers.localization:to_upper_text("base_mod_download_download_progress_failed") )
		make_fine_text( self._anim_data.download_amt_text )
	end

	self._anim_data.progress_bar:set_width( not self._anim_data.mod_download_failed and self._anim_data.end_progress_width or 0 )
	self._anim_data.download_complete = true

end
