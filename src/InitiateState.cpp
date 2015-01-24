#include "InitState.h"
#include "Functions.h"

#include <iostream>
#include <fstream>

/*extern "C" {
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
}*/


class lua_State;
//#define luaL_loadfile ((int (*)(lua_State *L, const char *filename))0x7CDB70);
//#define lua_pcall ((int (*)(lua_State *L, int nargs, int nresults, int errfunc))0x7BC4A0);

typedef int(*luaL_loadfileptr)(lua_State*, const char*);
typedef int(*lua_pcallptr)(lua_State*, int, int, int);

DWORD StringJmpBack = 0;
DWORD LuaStatePtr;

__declspec(naked) void InterceptString(){
	__asm {
		PUSH ESI
		MOV ESI, [ESP+8]
		PUSH 0
		MOV LuaStatePtr, ESI
		JMP [StringJmpBack]
	}

}



void InitiateStates(){
	DWORD StringAddy = FindPattern("payday2_win32_release.exe",
		"\x8B\x74\x24\x08\x6A\x00\x68\x00\x00\x00\x00\x68\x00\x00\x00\x00\x56\xE8\x00\x00\x00\x00\x68\x00\x00\x00\x00\x6A\xFF\x56\xE8\x00\x00\x00\x00",
		"xxxxxxx????x????xx????x????xxxx????");

	StringAddy -= 1; // 1 byte behind the pattern.

	StringJmpBack = StringAddy + 0x7;

	PlaceJMP((BYTE*)StringAddy, (DWORD)InterceptString, 7);
}

DWORD WINAPI GatherThread(){
	for (;; Sleep(1000)){

		if (LuaStatePtr == 0) continue;

		std::ofstream mFile;
		mFile.open("stuff.txt", std::ios::out | std::ios::app);

		luaL_loadfileptr luaL_loadfile = (luaL_loadfileptr)0x7CDB70;
		lua_pcallptr lua_pcall = (lua_pcallptr)0x7BC4A0;
		
		mFile << "1\n";
		lua_State* L = (lua_State*)LuaStatePtr;
		mFile << "2\n";
		mFile << luaL_loadfile(L, "test.lua");
		mFile << "3\n";
		mFile << lua_pcall(L, 0, 0, 0);
		mFile << "4\n";

		mFile.close();
	}

}