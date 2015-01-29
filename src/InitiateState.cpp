#include "InitState.h"
#include <detours.h>

#include "signatures/signatures.h"
#include "util/util.h"
#include "addons.h"
#include "console/console.h"
#include "threading/queue.h"
#include "http/http.h"

#include <thread>

class lua_State;

typedef const char * (*lua_Reader) (lua_State *L, void *ud, size_t *sz);
typedef int(*lua_CFunction) (lua_State *L);
typedef struct luaL_Reg {
	const char* name;
	lua_CFunction func;
} luaL_Reg;

CREATE_CALLABLE_SIGNATURE(lua_call, int, "\x8B\x44\x24\x08\x56\x8B\x74\x24\x08\x8B\x56\x08", "xxxxxxxxxxxx", 0, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(lua_pcall, int, "\x8B\x4C\x24\x10\x83\xEC\x08\x56\x8B\x74\x24\x10", "xxxxxxxxxxxx", 0, lua_State*, int, int, int);
CREATE_CALLABLE_SIGNATURE(lua_gettop, int, "\x8B\x4C\x24\x04\x8B\x41\x08\x2B\x41\x0C", "xxxxxxxxxx", 0, lua_State*);
CREATE_CALLABLE_SIGNATURE(lua_tolstring, const char*, "\x56\x8B\x74\x24\x08\x57\x8B\x7C\x24\x10\x8B\xCF\x8B\xD6", "xxxxxxxxxxxxxx", 0, lua_State*, int, size_t*)
CREATE_CALLABLE_SIGNATURE(luaL_loadfile, int, "\x81\xEC\x01\x01\x01\x01\x55\x8B\xAC\x24\x01\x01\x01\x01\x56\x8B\xB4\x24\x01\x01\x01\x01\x57", "xx????xxxx????xxxx????x", 0, lua_State*, const char*)
CREATE_CALLABLE_SIGNATURE(lua_load, int, "\x8B\x4C\x24\x10\x33\xD2\x83\xEC\x18\x3B\xCA", "xxxxxxxxxxx", 0, lua_State*, lua_Reader, void*, const char*)
CREATE_CALLABLE_SIGNATURE(lua_pushcclosure, void, "\x8B\x50\x04\x8B\x02\x8B\x40\x0C\x8B\x7C\x24\x14\x50\x57\x56", "xxxxxxxxxxxxxxx", -60, lua_State*, lua_CFunction, int);
CREATE_CALLABLE_SIGNATURE(lua_setfield, void, "\x8B\x46\x08\x83\xE8\x08\x50\x8D\x4C\x24\x1C", "xxxxxxxxxxx", -53, lua_State*, int, const char*)
CREATE_CALLABLE_SIGNATURE(lua_pushlstring, void, "\x52\x50\x56\xE8\x00\x00\x00\x00\x83\xC4\x0C\x89\x07\xC7\x47\x04\x04\x00\x00\x00\x83\x46\x08\x08\x5F", "xxxx????xxxxxxxxx???xxxxx", -58, lua_State*, const char*, size_t)
CREATE_CALLABLE_SIGNATURE(lua_pushboolean, void, "\x8B\x44\x24\x04\x8B\x48\x08\x33", "xxxxxxxx", 0, lua_State*, bool)
CREATE_CALLABLE_SIGNATURE(lua_insert, void, "\x8B\x4C\x24\x08\x56\x8B\x74\x24\x08\x8B\xD6\xE8\x50\xFE", "xxxxxxxxxxxxxx", 0, lua_State*, int b)

CREATE_CALLABLE_SIGNATURE(luaI_openlib, void, "\x83\xEC\x08\x53\x8B\x5C\x24\x14\x55\x8B\x6C\x24\x1C\x56", "xxxxxxxxxxxxxx", 0, lua_State*, const char*, const luaL_Reg*, int)
CREATE_CALLABLE_SIGNATURE(luaL_ref, int, "\x53\x8B\x5C\x24\x0C\x8D\x83\x00\x00\x00\x00", "xxxxxxx????", 0, lua_State*, int);
CREATE_CALLABLE_SIGNATURE(lua_rawgeti, void, "\x8B\x4C\x24\x08\x56\x8B\x74\x24\x08\x8B\xD6\xE8\x00\x00\x00\x00\x8B\x4C\x24\x10", "xxxxxxxxxxxx????xxxx", 0, lua_State*, int, int);
CREATE_CALLABLE_SIGNATURE(luaL_unref, void, "\x53\x8B\x5C\x24\x10\x85\xDB\x7C\x74", "xxxxxxxxx", 0, lua_State*, int, int);
CREATE_CALLABLE_CLASS_SIGNATURE(do_game_update, void*, "\x8B\x44\x24\x08\x56\x50\x8B\xF1\x8B\x0E", "xxxxxxxxxx", 0, int*, int*)


// lua c-functions

#define LUA_REGISTRYINDEX	(-10000)
#define LUA_GLOBALSINDEX	(-10002)

int luaF_pcall(lua_State* L){
	int args = lua_gettop(L);

	int result = lua_pcall(L, args - 1, -1, 0);
	lua_pushboolean(L, result == 0);
	lua_insert(L, 1);

	return lua_gettop(L);
}

int luaF_dofile(lua_State* L){
	size_t length = 0;
	const char* filename = lua_tolstring(L, 1, &length);
	luaL_loadfile(L, filename);
	lua_call(L, 0, -1);
	return 0;
}

struct lua_http_data {
	int funcRef;
	lua_State* L;
};

void return_lua_http(void* data, std::string& urlcontents){
	lua_http_data* ourData = (lua_http_data*)data;
	lua_rawgeti(ourData->L, LUA_REGISTRYINDEX, ourData->funcRef);
	lua_pushlstring(ourData->L, urlcontents.c_str(), urlcontents.length());
	lua_pcall(ourData->L, 1, 0, 0);
	luaL_unref(ourData->L, LUA_REGISTRYINDEX, ourData->funcRef);
	delete ourData;
}

int luaF_dohttpreq(lua_State* L){
	Logging::Log("Incoming HTTP Request/Request");
	int functionReference = luaL_ref(L, LUA_REGISTRYINDEX);
	size_t len;
	const char* url_c = lua_tolstring(L, 1, &len);
	std::string url = std::string(url_c, len);

	Logging::Log(url);
	Logging::Log(std::to_string(functionReference));

	lua_http_data* ourData = new lua_http_data();
	ourData->funcRef = functionReference;
	ourData->L = L;

	HTTPItem* reqItem = new HTTPItem();
	reqItem->call = return_lua_http;
	reqItem->data = ourData;
	reqItem->url = url;

	HTTPManager::GetSingleton()->LaunchHTTPRequest(reqItem);
	return 0;
}

int updates = 0;
std::thread::id main_thread_id;

void* __fastcall do_game_update_new(void* thislol, int edx, int* a, int* b){

	// If someone has a better way of doing this, I'd like to know about it.
	// I could save the this pointer?
	// I'll check if it's even different at all later.
	if (std::this_thread::get_id() != main_thread_id){
		return do_game_update(thislol, a, b);
	}

	lua_State* L = (lua_State*)*((void**)thislol);
	if (updates == 0){
		new AddonManager();
		new EventQueueM();
		HTTPManager::GetSingleton()->init_locks();
	}
	if (updates == 1){
		lua_pushcclosure(L, luaF_pcall, 0);
		lua_setfield(L, LUA_GLOBALSINDEX, "pcall");

		lua_pushcclosure(L, luaF_dofile, 0);
		lua_setfield(L, LUA_GLOBALSINDEX, "dofile");

		lua_pushcclosure(L, luaF_dohttpreq, 0);
		lua_setfield(L, LUA_GLOBALSINDEX, "dohttpreq");
	}

	if (updates > 1){
		EventQueueM::GetSingleton()->ProcessEvents();
	}

	updates++;
	return do_game_update(thislol, a, b);
}

int lua_load_new(lua_State* L, lua_Reader reader, void* data, const char* chunkname){
	//Logging::Log(chunkname);

	int result = lua_load(L, reader, data, chunkname);
	//lua_call(L, 0, 0);

	AddonManager::GetSingleton()->RunFunctionHook(chunkname, L);
	return result;
}

CConsole* gbl_mConsole = NULL;
static HTTPManager mainManager;

void InitiateStates(){
	Configuration::LoadConfiguration();
	main_thread_id = std::this_thread::get_id();
	if (Configuration::IsDeveloperConsole()){
		gbl_mConsole = new CConsole();
	}

	SignatureSearch::Search();
	FuncDetour* gameUpdateDetour = new FuncDetour((void**)&do_game_update, do_game_update_new);
	FuncDetour* loadFileDetour = new FuncDetour((void**)&lua_load, lua_load_new);
}

void DestroyStates(){
	// Okay... let's not do that.
	// I don't want to keep this in memory, but it CRASHES THE SHIT OUT if you delete this after all is said and done.
	// if (gbl_mConsole) delete gbl_mConsole;
	delete AddonManager::GetSingleton();
}


// needs captured functions, variables only here, I might extern them? eh
void PaydayHook::RunHook(void* luaState){
	lua_State* L = (lua_State*)luaState;
	
	// This function is executed directly after a PAYDAY script is loaded.
	// Execute the PAYDAY Script (I'm assuming it returns nothing.)
	lua_call(L, 0, -1);
	std::string fPath = "addons/" + ownerAddon->GetIdentifer() + "/" + scriptPath;

	// Set the RequiredScript variable because Wilko is addicted to Olinub's methods.
	lua_pushlstring(L, hookID.c_str(), hookID.length());
	lua_setfield(L, LUA_GLOBALSINDEX, "RequiredScript");

	// Load our script
	luaL_loadfile(L, fPath.c_str());

	// Then PAYDAY will execute our script instead of it's one.
	// (This function should still work okay if ran multiple times after a script.)
}