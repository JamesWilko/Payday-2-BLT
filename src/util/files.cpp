//WINDOWS ONLY FILE!
//Some other stuff might be aswell, but this is rather desperately windows only.

#include "util/util.h"
#include <Windows.h>
#include <tchar.h>
#include <stdio.h>
#include <strsafe.h>

#include <fstream>
#include <streambuf>

using namespace std;

namespace pd2hook
{
namespace Util{
IOException::IOException(const char *file, int line) : Exception(file, line)
{}

IOException::IOException(std::string msg, const char *file, int line) : Exception(std::move(msg), file, line)
{}

const char *IOException::exceptionName() const
{
	return "An IOException";
}

	vector<string> GetDirectoryContents(const std::string& path, bool dirs){
		vector<string> files;
		WIN32_FIND_DATA ffd;
		TCHAR szDir[MAX_PATH];
		TCHAR *fPath = (TCHAR*)path.c_str();
		//WideCharToMultiByte(CP_ACP, 0, ffd.cFileName, -1, fileName, MAX_PATH, &DefChar, NULL);

		size_t length_of_arg;
		HANDLE hFind = INVALID_HANDLE_VALUE;
		DWORD dwError = 0;
		StringCchLength(fPath, MAX_PATH, &length_of_arg);
		if (length_of_arg>MAX_PATH - 3){
			PD2HOOK_THROW_IO_MSG("Path too long");
		}
		StringCchCopy(szDir, MAX_PATH, fPath);
		StringCchCat(szDir, MAX_PATH, TEXT("\\*"));

		hFind = FindFirstFile(szDir, &ffd);
		if (hFind == INVALID_HANDLE_VALUE){
			PD2HOOK_THROW_IO_MSG("FindFirstFile() failed");
		}
		do{
			bool isDir = (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0;

			if ((dirs && isDir) || (!dirs && !isDir)){
				files.push_back(ffd.cFileName);
			}
		} while (FindNextFile(hFind, &ffd) != 0);

		dwError = GetLastError();
		if (dwError != ERROR_NO_MORE_FILES){
			PD2HOOK_THROW_IO_MSG("FindNextFile() failed");
		}
		FindClose(hFind);
		return files;
	}

	string GetFileContents(const string& filename){
		ifstream t(filename, std::ifstream::binary);
		string str;

		t.seekg(0, std::ios::end);
		str.reserve(static_cast<string::size_type>(t.tellg()));
		t.seekg(0, std::ios::beg);
		str.assign((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());

		return str;
	}

	bool DirectoryExists(const std::string& dir){
		DWORD ftyp = GetFileAttributes(dir.c_str());
		if (ftyp == INVALID_FILE_ATTRIBUTES) return false;
		if (ftyp & FILE_ATTRIBUTE_DIRECTORY) return true;
		return false;
	}

	void EnsurePathWritable(const std::string& path){
		int finalSlash = path.find_last_of('/');
		std::string finalPath = path.substr(0, finalSlash);
		if (DirectoryExists(finalPath)) return;
		CreateDirectoryPath(finalPath);
	}

	bool RemoveEmptyDirectory(const std::string& dir){
		return RemoveDirectory(dir.c_str()) != 0;
	}

	bool CreateDirectoryPath(const std::string& path){
		std::string newPath = "";
		std::vector<std::string> paths = Util::SplitString(path, '/');
		for (const auto& i : paths) {
			newPath = newPath + i + "/";
			CreateDirectory(newPath.c_str(), NULL);
		}
		return true;
	}

	void SplitString(const std::string &s, char delim, std::vector<std::string> &elems) {
		std::istringstream ss(s);
		std::string item;
		while (std::getline(ss, item, delim)) {
			if (!item.empty()){
				elems.push_back(item);
			}
		}
	}

	std::vector<std::string> SplitString(const std::string &s, char delim) {
		std::vector<std::string> elems;
		SplitString(s, delim, elems);
		return elems;
	}

}
}
