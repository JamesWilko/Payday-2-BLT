#include <iostream>
#include <Windows.h>
#include <tlhelp32.h>
#include <Psapi.h>

#include "signatures.h"
#include "functions.h"

MODULEINFO GetModuleInfo(char *szModule)
{
	MODULEINFO modinfo = { 0 };
	HMODULE hModule = GetModuleHandle(szModule);
	if (hModule == 0)
		return modinfo;
	GetModuleInformation(GetCurrentProcess(), hModule, &modinfo, sizeof(MODULEINFO));
	return modinfo;
}


unsigned long FindPattern(char *module, const char *pattern, const char *mask)
{
	MODULEINFO mInfo = GetModuleInfo(module);
	DWORD base = (DWORD)mInfo.lpBaseOfDll;
	DWORD size = (DWORD)mInfo.SizeOfImage;
	DWORD patternLength = (DWORD)strlen(mask);
	for (DWORD i = 0; i < size - patternLength; i++){
		bool found = true;
		for (DWORD j = 0; j < patternLength; j++){
			found &= mask[j] == '?' || pattern[j] == *(char*)(base + i + j);
		}
		if (found) {
			return base + i;
		}
	}
	return NULL;
}

std::vector<SignatureF> SignatureSearch::signatures;

SignatureSearch::SignatureSearch(void* target, std::string signature, std::string mask, int offset){
	SignatureF ins = { signature, mask, offset, NULL, target };
	signatures.push_back(ins);
}

void SignatureSearch::Search(){
	std::vector<SignatureF>::iterator it;
	for (it = signatures.begin(); it < signatures.end(); it++){
		it->address = (void*)(FindPattern("payday2_win32_release.exe", it->signature.c_str(), it->mask.c_str()) + it->offset);
	}
}