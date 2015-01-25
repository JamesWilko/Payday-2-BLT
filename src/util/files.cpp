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
	vector<string> GetDirectoryContents(char* path){
		vector<string> files;
		WIN32_FIND_DATA ffd;
		TCHAR szDir[MAX_PATH];
		TCHAR *fPath = (TCHAR*)path;
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
			if (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY){

			}
			else {
				//char fileName[MAX_PATH];
				//char DefChar = ' ';
				//WideCharToMultiByte(CP_ACP, 0, ffd.cFileName, -1, fileName, MAX_PATH, &DefChar, NULL);
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
}