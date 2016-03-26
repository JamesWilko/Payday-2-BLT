#include <iostream>
#include <Windows.h>
#include <tlhelp32.h>
#include <Psapi.h>
#include <detours.h>

#include "signatures.h"
#include "util/util.h"

#include <algorithm>
#include <vector>

namespace pd2hook
{
namespace
{
MODULEINFO GetModuleInfo(const std::string& szModule)
{
	PD2HOOK_TRACE_FUNC;
	MODULEINFO modinfo = { nullptr, 0, nullptr };
	HMODULE hModule = GetModuleHandle(szModule.c_str());
	if (hModule == 0)
		return modinfo;
	GetModuleInformation(GetCurrentProcess(), hModule, &modinfo, sizeof(MODULEINFO));
	return modinfo;
}

const MODULEINFO& GetPd2ModuleInfo()
{
	static const MODULEINFO modinfo = GetModuleInfo("payday2_win32_release.exe");
	return modinfo;
}

const char *FindPattern(const char *pattern, const char *mask)
{
	PD2HOOK_TRACE_FUNC;
	const auto& modInfo = GetPd2ModuleInfo();
	const char * const base = reinterpret_cast<const char *>(modInfo.lpBaseOfDll);
	const DWORD size = modInfo.SizeOfImage;
	decltype(size) patternLength = strlen(mask);
	for (std::remove_const<decltype(size)>::type i = 0; i < size - patternLength; ++i)
	{
		bool found = true;
		for (decltype(i) j = 0; j < patternLength && found; ++j)
		{
			found &= mask[j] == '?' || pattern[j] == base[i + j];
		}

		if (found)
		{
			return base + i;
		}
	}

	return nullptr;
}

bool FindUnassignedSignaturesPredicate(const SignatureF& s)
{
	return *s.address == nullptr;
}

std::vector<SignatureF> allSignatures;
}

SignatureSearch::SignatureSearch(const void** adress, const char* signature, const char* mask, int offset){
	SignatureF ins = { signature, mask, offset, adress };
	allSignatures.push_back(ins);
}

void SignatureSearch::Search(){
	PD2HOOK_TRACE_FUNC;
	PD2HOOK_LOG_LOG("Scanning for signatures.");

	std::for_each(allSignatures.begin(), allSignatures.end(), [](SignatureF& s) { *s.address = FindPattern(s.signature, s.mask) + s.offset; });

	const auto end = allSignatures.cend();
	auto it = std::find_if(allSignatures.cbegin(), end, FindUnassignedSignaturesPredicate);
	int unassigned_count = 0;
	while (it != end)
	{
		++unassigned_count;
		PD2HOOK_LOG_WARN("Didn't find signature with pattern: " << it->signature << ", and mask: " << it->mask);
		it = std::find_if(it, end, FindUnassignedSignaturesPredicate);
	}
	
	if (unassigned_count)
	{
		PD2HOOK_LOG_WARN("Total: " << unassigned_count << " signatures not found.");
	}

	PD2HOOK_LOG_LOG("Signatures Found.");
}


FuncDetour::FuncDetour(void** oldF, void* newF) : oldFunction(oldF), newFunction(newF){
	PD2HOOK_TRACE_FUNC;
	//DetourRestoreAfterWith();

#define PD2_DETOUR_CHK_PARAM(param) if(!param) { PD2HOOK_LOG_WARN(#param " is null"); }
	PD2_DETOUR_CHK_PARAM(oldF)
	PD2_DETOUR_CHK_PARAM(*oldF)
	PD2_DETOUR_CHK_PARAM(newF)

	LONG result;
#define PD2_DETOUR_CHK_FUNC(func) if((result = func) != ERROR_SUCCESS) { PD2HOOK_LOG_WARN(#func " returns " << result); }
	PD2_DETOUR_CHK_FUNC(DetourTransactionBegin())
	PD2_DETOUR_CHK_FUNC(DetourUpdateThread(GetCurrentThread()))
	PD2_DETOUR_CHK_FUNC(DetourAttach(oldF, newF))
	PD2_DETOUR_CHK_FUNC(DetourTransactionCommit())

#undef PD2_DETOUR_CHK_PARAM
#undef PD2_DETOUR_CHK_FUNC
}

FuncDetour::~FuncDetour(){
	PD2HOOK_TRACE_FUNC;
	DetourTransactionBegin();
	DetourUpdateThread(GetCurrentThread());
	DetourDetach(oldFunction, newFunction);
	LONG result = DetourTransactionCommit();
}

}