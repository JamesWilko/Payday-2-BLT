#include "InitState.h"

#include <iostream>
#include <fstream>
#include <detours.h>

#include "signatures/signatures.h"

class lua_State;

CREATE_CALLABLE_SIGNATURE(lua_call, int, "\x8B\x44\x24\x08\x56\x8B\x74\x24\x08\x8B\x56\x08", "xxxxxxxxxxxx", 0, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(luaL_loadfile, int, "\x81\xEC\x01\x01\x01\x01\x55\x8B\xAC\x24\x01\x01\x01\x01\x56\x8B\xB4\x24\x01\x01\x01\x01\x57", "xx????xxxx????xxxx????x", 0, lua_State*, const char*)
CREATE_CALLABLE_CLASS_SIGNATURE(do_game_update, void*, "\x8B\x44\x24\x08\x56\x50\x8B\xF1\x8B\x0E", "xxxxxxxxxx", 0, int*, int*)

int updates = 0;

void* __fastcall do_game_update_new(void* thislol, int edx, int* a, int* b){
	lua_State* L = (lua_State*)*((void**)thislol);
	updates++;
	if (L != NULL && updates == 500){
		luaL_loadfile(L, "test.lua");
		lua_call(L, 0, 0);
	}
	return do_game_update(thislol, a, b);
}

void InitiateStates(){

	SignatureSearch::Search();

	FuncDetour* gameUpdateDetour = new FuncDetour((void**)&do_game_update, do_game_update_new);
}