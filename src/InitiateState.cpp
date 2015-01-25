#include "InitState.h"
#include <detours.h>

#include "signatures/signatures.h"
#include "util/util.h"
#include "addons.h"

class lua_State;

typedef const char * (*lua_Reader) (lua_State *L, void *ud, size_t *sz);
typedef int(*lua_CFunction) (lua_State *L);

CREATE_CALLABLE_SIGNATURE(lua_call, int, "\x8B\x44\x24\x08\x56\x8B\x74\x24\x08\x8B\x56\x08", "xxxxxxxxxxxx", 0, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(lua_pcall, int, "\x8B\x4C\x24\x10\x83\xEC\x08\x56\x8B\x74\x24\x10", "xxxxxxxxxxxx", 0, lua_State*, int, int, int);
CREATE_CALLABLE_SIGNATURE(lua_gettop, int, "\x8B\x4C\x24\x04\x8B\x41\x08\x2B\x41\x0C", "xxxxxxxxxx", 0, lua_State*);
CREATE_CALLABLE_SIGNATURE(lua_tolstring, const char*, "\x56\x8B\x74\x24\x08\x57\x8B\x7C\x24\x10\x8B\xCF\x8B\xD6", "xxxxxxxxxxxxxx", 0, lua_State*, int, size_t*)
CREATE_CALLABLE_SIGNATURE(luaL_loadfile, int, "\x81\xEC\x01\x01\x01\x01\x55\x8B\xAC\x24\x01\x01\x01\x01\x56\x8B\xB4\x24\x01\x01\x01\x01\x57", "xx????xxxx????xxxx????x", 0, lua_State*, const char*)
CREATE_CALLABLE_SIGNATURE(lua_load, int, "\x8B\x4C\x24\x10\x33\xD2\x83\xEC\x18\x3B\xCA", "xxxxxxxxxxx", 0, lua_State*, lua_Reader, void*, const char*)
CREATE_CALLABLE_SIGNATURE(lua_pushcclosure, void, "\x8B\x50\x04\x8B\x02\x8B\x40\x0C\x8B\x7C\x24\x14\x50\x57\x56", "xxxxxxxxxxxxxxx", -60, lua_State*, lua_CFunction, int);
CREATE_CALLABLE_SIGNATURE(lua_setfield, void, "\x8B\x46\x08\x83\xE8\x08\x50\x8D\x4C\x24\x1C", "xxxxxxxxxxx", -53, lua_State*, int, const char*)
CREATE_CALLABLE_CLASS_SIGNATURE(do_game_update, void*, "\x8B\x44\x24\x08\x56\x50\x8B\xF1\x8B\x0E", "xxxxxxxxxx", 0, int*, int*)


// lua c-functions

#define LUA_GLOBALSINDEX	(-10002)

int luaF_pcall(lua_State* L){
	int args = lua_gettop(L);

	int result = lua_pcall(L, args - 1, -1, 0);
	if (result != 0) return 1;
	return 0;
}

int luaF_dofile(lua_State* L){
	size_t length = 0;
	const char* filename = lua_tolstring(L, 1, &length);
	luaL_loadfile(L, filename);
	lua_call(L, 0, -1);
	return 0;
}

int updates = 0;

void* __fastcall do_game_update_new(void* thislol, int edx, int* a, int* b){
	lua_State* L = (lua_State*)*((void**)thislol);
	if (updates == 0){
		InitializeAllAddons();
	}
	if (updates == 1){
		lua_pushcclosure(L, luaF_pcall, 0);
		lua_setfield(L, LUA_GLOBALSINDEX, "pcall");

		lua_pushcclosure(L, luaF_dofile, 0);
		lua_setfield(L, LUA_GLOBALSINDEX, "dofile");
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
void PaydayHook::RunHook(void* luaState){
	lua_State* L = (lua_State*)luaState;
	
	lua_call(L, 0, -1);
	std::string fPath = "addons/" + ownerAddon->GetIdentifer() + "/" + scriptPath;
	luaL_loadfile(L, fPath.c_str());
	

	//Logging::Log(initScript);
	//Logging::Log(postLoad);
}