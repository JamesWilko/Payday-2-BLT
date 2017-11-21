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


unsigned long FindPattern(char* module, const char* funcname, const char* pattern, const char* mask)
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
//			printf("Found %s: 0x%p\n", funcname, base + i);
			return base + i;
		}
	}
	printf("Warning: Failed to locate function %s\n", funcname);
	return NULL;
}

std::vector<SignatureF>* allSignatures = NULL;

SignatureSearch::SignatureSearch(const char* funcname, void* adress, const char* signature, const char* mask, int offset){
	// lazy-init, container gets 'emptied' when initialized on compile.
	if (!allSignatures){
		allSignatures = new std::vector<SignatureF>();
	}

	SignatureF ins = { funcname, signature, mask, offset, adress };
	allSignatures->push_back(ins);
}

void SignatureSearch::Search(){
	// Find the name of the current EXE
	TCHAR processPath[MAX_PATH + 1];
	GetModuleFileName(NULL, processPath, MAX_PATH + 1); // Get the path
	TCHAR filename[MAX_PATH + 1];
	_splitpath_s( // Find the filename part of the path
		processPath, // Input
		NULL, 0, // Don't care about the drive letter
		NULL, 0, // Don't care about the directory
		filename, MAX_PATH, // Grab the filename
		NULL, 0 // Extension is always .exe
	);

	// Add the .exe back on
	strcat_s(filename, MAX_PATH, ".exe");

	printf("Scanning for signatures in %s.\n", filename);
	std::vector<SignatureF>::iterator it;
	for (it = allSignatures->begin(); it < allSignatures->end(); it++){
		*((void**)it->address) = (void*)(FindPattern(filename, it->funcname, it->signature, it->mask) + it->offset);
	}
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