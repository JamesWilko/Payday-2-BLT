#include "InitState.h"
#include <detours.h>

#include "signatures/signatures.h"
#include "util/util.h"
#include "addons.h"

class lua_State;

typedef const char * (*lua_Reader) (lua_State *L, void *ud, size_t *sz);

CREATE_CALLABLE_SIGNATURE(lua_call, int, "\x8B\x44\x24\x08\x56\x8B\x74\x24\x08\x8B\x56\x08", "xxxxxxxxxxxx", 0, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(luaL_loadfile, int, "\x81\xEC\x01\x01\x01\x01\x55\x8B\xAC\x24\x01\x01\x01\x01\x56\x8B\xB4\x24\x01\x01\x01\x01\x57", "xx????xxxx????xxxx????x", 0, lua_State*, const char*)
CREATE_CALLABLE_SIGNATURE(lua_load, int, "\x8B\x4C\x24\x10\x33\xD2\x83\xEC\x18\x3B\xCA", "xxxxxxxxxxx", 0, lua_State*, lua_Reader, void*, const char*)
CREATE_CALLABLE_CLASS_SIGNATURE(do_game_update, void*, "\x8B\x44\x24\x08\x56\x50\x8B\xF1\x8B\x0E", "xxxxxxxxxx", 0, int*, int*)

int updates = 0;

void* __fastcall do_game_update_new(void* thislol, int edx, int* a, int* b){
	//lua_State* L = (lua_State*)*((void**)thislol);
	if (updates == 0){
		InitializeAllAddons();
	}

	updates++;
	return do_game_update(thislol, a, b);
}

int lua_load_new(lua_State* L, lua_Reader reader, void* data, const char* chunkname){
	//Logging::Log(chunkname);

	int result = lua_load(L, reader, data, chunkname);
	//lua_call(L, 0, 0);

	RunFunctionHook(chunkname, L);
	return result;
}

void InitiateStates(){

	SignatureSearch::Search();

	FuncDetour* gameUpdateDetour = new FuncDetour((void**)&do_game_update, do_game_update_new);
	FuncDetour* loadFileDetour = new FuncDetour((void**)&lua_load, lua_load_new);
}


// needs captured functions, variables only here, I might extern them? eh
void PaydayAddon::RunScript(void* luaState){
	lua_State* L = (lua_State*)luaState;
	
	lua_call(L, 0, -1);
	luaL_loadfile(L, initScript.c_str());
	

	//Logging::Log(initScript);
	//Logging::Log(postLoad);
}