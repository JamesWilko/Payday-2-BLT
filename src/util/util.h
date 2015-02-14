#ifndef __UTIL_HEADER__
#define __UTIL_HEADER__

#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <list>

namespace Util {
	std::vector<std::string> GetDirectoryContents(std::string path, bool isDirs = false);
	std::string GetFileContents(std::string filename);
	void EnsurePathWritable(std::string path);
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

class ByteStream {
public:
	ByteStream(std::string path);
	~ByteStream();

	template<typename T>
	T readType();
	std::string readString(int length);
private:

	std::ifstream mainStream;
};

struct ZIPFileData {
	std::string filepath;
	std::string compressedData;
	std::string decompressedData;
	int compressedSize;
	int uncompressedSize;
};



class ZIPArchive {
public:
	ZIPArchive(std::string path, std::string extractPath);
	~ZIPArchive();
	void ReadArchive();
private:

	bool ReadFile();
	bool WriteFile(ZIPFileData* data);
	void DecompressFile(ZIPFileData* fileToDecompress);

	ByteStream mainStream;
	std::list<ZIPFileData*> readFiles;
	std::string extractTo;
};

#endif // __UTIL_HEADER__