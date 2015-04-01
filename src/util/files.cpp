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

namespace Util{
	vector<string> GetDirectoryContents(std::string path, bool dirs){
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
			throw - 1;
		}
		StringCchCopy(szDir, MAX_PATH, fPath);
		StringCchCat(szDir, MAX_PATH, TEXT("\\*"));

		hFind = FindFirstFile(szDir, &ffd);
		if (hFind == INVALID_HANDLE_VALUE){
			throw - 1;
		}
		do{
			bool isDir = ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY;
			
			if ((dirs && isDir) || (!dirs && !isDir)){
				files.push_back(ffd.cFileName);
			}
		} while (FindNextFile(hFind, &ffd) != 0);

		dwError = GetLastError();
		if (dwError != ERROR_NO_MORE_FILES){
			throw - 1;
		}
		FindClose(hFind);
		return files;
	}

	string GetFileContents(string filename){
		ifstream t(filename);
		string str;

		t.seekg(0, std::ios::end);
		str.reserve(t.tellg());
		t.seekg(0, std::ios::beg);
		str.assign((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());
		
		return str;
	}

	bool DirectoryExists(std::string dir){
		DWORD ftyp = GetFileAttributes(dir.c_str());
		if (ftyp == INVALID_FILE_ATTRIBUTES) return false;
		if (ftyp & FILE_ATTRIBUTE_DIRECTORY) return true;
		return false;
	}

	void EnsurePathWritable(std::string path){
		int finalSlash = path.find_last_of('/');
		std::string finalPath = path.substr(0, finalSlash);
		if (DirectoryExists(finalPath)) return;
		CreateDirectoryPath(finalPath.c_str());
	}

	bool RemoveEmptyDirectory(std::string dir){
		return RemoveDirectory(dir.c_str());
	}

	bool CreateDirectoryPath(std::string path){
		std::string newPath = "";
		std::vector<std::string> paths = Util::SplitString(path.c_str(), '/');
		for (auto i : paths) {
			newPath = newPath + i + "/";
			CreateDirectory(newPath.c_str(), NULL);
		}
		return true;
	}

	std::vector<std::string> &SplitString(const std::string &s, char delim, std::vector<std::string> &elems) {
		std::stringstream ss(s);
		std::string item;
		while (std::getline(ss, item, delim)) {
			if (!item.empty()){
				elems.push_back(item);
			}
		}
		return elems;
	}

	std::vector<std::string> SplitString(const std::string &s, char delim) {
		std::vector<std::string> elems;
		SplitString(s, delim, elems);
		return elems;
	}

}
