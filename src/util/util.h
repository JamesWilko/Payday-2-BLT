#ifndef __UTIL_HEADER__
#define __UTIL_HEADER__

#include <vector>
#include <string>

namespace Util {
	std::vector<std::string> GetDirectoryContents(std::string path, bool isDirs = false);
	std::string GetFileContents(std::string filename);
}


namespace Logging {

	enum LogType{
		LOGGING_LOG=1,
		LOGGING_LUA,
		LOGGING_WARN,
		LOGGING_ERROR
	};

	void Log(std::string msg, LogType msgType = LOGGING_LOG);
}

#endif // __UTIL_HEADER__