#ifndef __UTIL_HEADER__
#define __UTIL_HEADER__

#include <vector>
#include <string>

namespace Util {
	std::vector<std::string> GetDirectoryContents(char* path, bool isDirs = false);
	std::string GetFileContents(std::string filename);
}


namespace Logging {
	void Log(std::string msg);
}

#endif // __UTIL_HEADER__