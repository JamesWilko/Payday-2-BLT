
core:module("SystemMenuManager")
require("lib/managers/dialogs/SpecializationDialog")

GenericSystemMenuManager.GENERIC_DOWNLOAD_PROGRESS_CLASS = DownloadProgressDialog
GenericSystemMenuManager.DOWNLOAD_PROGRESS_CLASS = DownloadProgressDialog

function GenericSystemMenuManager:show_download_progress( data )
	local success = self:_show_class(data, self.GENERIC_DOWNLOAD_PROGRESS_CLASS, self.DOWNLOAD_PROGRESS_CLASS, data.force)
	self:_show_result(success, data)
end
