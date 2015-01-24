#include "InitState.h"

#include <iostream>
#include <fstream>
#include <detours.h>


class lua_State;

typedef int(*lua_callptr)(lua_State*, int, int);
typedef int(*luaL_loadfileptr)(lua_State*, const char*);

lua_callptr lua_call = NULL;
luaL_loadfileptr luaL_loadfile = NULL;

typedef void* (__thiscall *do_game_updateptr)(void*, DWORD*, DWORD*);

do_game_updateptr do_game_update_old = NULL;

int updates = 0;

void* __fastcall do_game_update_new(void* thislol, int edx, DWORD* a, DWORD* b){
	lua_State* L = (lua_State*)*((void**)thislol);
	updates++;
	if (L != NULL && updates == 500){
		luaL_loadfile(L, "test.lua");
		lua_call(L, 0, 1);
	}
	return do_game_update_old(thislol, a, b);
}

/*class CMember {
public:
	int* do_game_update_old(int* a, int* b);
};

class CDetour {
public:
	int* do_game_update_new(int* a, int* b);
	static int* (CDetour::* Real_Target)(int* a, int* b);
};

int* (CDetour::* CDetour::Real_Target)(int*, int*) = (int* (CDetour::*)(int*, int*))&CMember::do_game_update_old;*/

void InitiateStates(){
	DetourRestoreAfterWith();

	lua_call = (lua_callptr)0x007BA570;
	luaL_loadfile = (luaL_loadfileptr)0x007CDB70;

	DetourTransactionBegin();
	DetourUpdateThread(GetCurrentThread());

	do_game_update_old = (do_game_updateptr)0x00767B60;
	DetourAttach(&(PVOID&)do_game_update_old, do_game_update_new);

	LONG result = DetourTransactionCommit();
}