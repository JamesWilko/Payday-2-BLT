#include "InitState.h"

#include <iostream>
#include <fstream>
#include <detours.h>

#include "signatures/signatures.h"

class lua_State;

CREATE_CALLABLE_SIGNATURE(lua_call, int, "\x8B\x44\x24\x08\x56\x8B\x74\x24\x08\x8B\x56\x08", "xxxxxxxxxxxx", 0, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(luaL_loadfile, int, "\x81\xEC\x01\x01\x01\x01\x55\x8B\xAC\x24\x01\x01\x01\x01\x56\x8B\xB4\x24\x01\x01\x01\x01\x57", "xx????xxxx????xxxx????x", 0, lua_State*, const char*)

typedef void* (__thiscall *do_game_updateptr)(void*, DWORD*, DWORD*);

do_game_updateptr do_game_update_old = NULL;

int updates = 0;

void* __fastcall do_game_update_new(void* thislol, int edx, DWORD* a, DWORD* b){
	lua_State* L = (lua_State*)*((void**)thislol);
	updates++;
	if (L != NULL && updates == 500){
		luaL_loadfile(L, "test.lua");
		lua_call(L, 0, 0);
	}
	return do_game_update_old(thislol, a, b);
}

void InitiateStates(){

	SignatureSearch::Search();

	DetourRestoreAfterWith();

	DetourTransactionBegin();
	DetourUpdateThread(GetCurrentThread());

	do_game_update_old = (do_game_updateptr)0x00767B60;
	DetourAttach(&(PVOID&)do_game_update_old, do_game_update_new);

	LONG result = DetourTransactionCommit();
	
}