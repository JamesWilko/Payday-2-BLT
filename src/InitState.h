#ifndef __INIT_STATE__
#define __INIT_STATE__

#include <Windows.h>
#include <Shlwapi.h>
#include <string>
#pragma comment(lib, "shlwapi.lib")

extern bool IS_STANDALONE;
extern std::string moduleFile;

void InitiateStates();
void DestroyStates();

#endif