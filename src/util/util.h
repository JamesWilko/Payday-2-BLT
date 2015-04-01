#ifndef __UTIL_HEADER__
#define __UTIL_HEADER__

#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <list>

namespace Util {
	std::vector<std::string> GetDirectoryContents(std::string path, bool isDirs = false);
	std::string GetFileContents(std::string filename);
	void EnsurePathWritable(std::string path);
	bool RemoveEmptyDirectory(std::string dir);
	bool DirectoryExists(std::string dir);
	bool CreateDirectoryPath(std::string dir);
	// String split from http://stackoverflow.com/a/236803
	std::vector<std::string> &SplitString(const std::string &s, char delim, std::vector<std::string> &elems);
	std::vector<std::string> SplitString(const std::string &s, char delim);
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
