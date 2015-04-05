#include <iostream>
#include <Windows.h>
#include <tlhelp32.h>
#include <Psapi.h>
#include <detours.h>

#include "signatures.h"

MODULEINFO GetModuleInfo(std::string szModule)
{
	MODULEINFO modinfo = { 0 };
	HMODULE hModule = GetModuleHandle(szModule.c_str());
	if (hModule == 0)
		return modinfo;
	GetModuleInformation(GetCurrentProcess(), hModule, &modinfo, sizeof(MODULEINFO));
	return modinfo;
}


unsigned long FindPattern(char* module, const char* pattern, const char* mask)
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

std::vector<SignatureF> allSignatures;

SignatureSearch::SignatureSearch(void** adress, const char* signature, const char* mask, int offset){
	allSignatures.emplace_back(SignatureF{signature, mask, offset, adress});
}

void SignatureSearch::Search(){
	printf("Scanning for signatures.\n");
	for (auto&& it : allSignatures)
		*it.address = (void*)(FindPattern("payday2_win32_release.exe", it.signature, it.mask) + it.offset);
	printf("Signatures Found.\n");
	allSignatures.clear(); //We don't need it, clear it.
}


FuncDetour::FuncDetour(void** oldF, void* newF) : oldFunction(oldF), newFunction(newF){
	//DetourRestoreAfterWith();

	DetourTransactionBegin();
	DetourUpdateThread(GetCurrentThread());
	DetourAttach(oldF, newF);
	LONG result = DetourTransactionCommit();
}

FuncDetour::~FuncDetour(){
	DetourTransactionBegin();
	DetourUpdateThread(GetCurrentThread());
	DetourDetach(oldFunction, newFunction);
	LONG result = DetourTransactionCommit();
}