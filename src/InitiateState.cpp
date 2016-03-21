#include "InitState.h"

#define WIN32_LEAN_AND_MEAN 1
#include <Windows.h>
#include <detours.h>

#include "signatures/signatures.h"
#include "util/util.h"
#include "console/console.h"
#include "threading/queue.h"
#include "http/http.h"

#include <thread>
#include <list>

namespace pd2hook
{
struct lua_State;

typedef const char * (*lua_Reader) (lua_State *L, void *ud, size_t *sz);
typedef int(*lua_CFunction) (lua_State *L);
typedef void * (*lua_Alloc) (void *ud, void *ptr, size_t osize, size_t nsize);
typedef struct luaL_Reg {
	const char* name;
	lua_CFunction func;
} luaL_Reg;

CREATE_CALLABLE_SIGNATURE(lua_call, void, "\x55\x8B\xEC\x83\xE4\xF8\x56\x8B\xF1\xB9\x00\x00\x00\x00\x66\xFF\x46\x34", "xxxxxxxxxx????xxxx", 0, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(lua_pcall, int, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x08\x56\x8B\xF2", "xxxxxxxxxxxx", 0, lua_State*, int, int, int)
//CREATE_CALLABLE_SIGNATURE(lua_gettop, int, "\x8B\x4C\x24\x04\x8B\x41\x08\x2B\x41\x0C", "xxxxxxxxxx", 0, lua_State*)
CREATE_CALLABLE_SIGNATURE(lua_settop, void, "\x85\xD2\x78\x2E\x8B\x41\x0C\xC1\xE2\x03\x03\xC2", "xxxxxxxxxxxx", 0, lua_State*, int)
CREATE_CALLABLE_SIGNATURE(lua_tolstring, const char*, "\x56\x57\x8B\xFA\x8B\xF1\xE8\x00\x00\x00\x00\x8B\xC8\x83\x79\x04\x04", "xxxxxxx????xxxxxx", 0, lua_State*, int, size_t*)
CREATE_CALLABLE_SIGNATURE(luaL_loadfile, int, "\x55\x8B\xEC\x83\xE4\xF8\x81\xEC\x00\x00\x00\x00\x53\x55\x56\x57\x8B\xEA\x8B\xF9\xC7\x44\x24\x00\x00\x00\x00\x00\x8B\x5F\x08\x2B\x5F\x0C", "xxxxxxxx????xxxxxxxxxxx?????xxxxxx", 0, lua_State*, const char*)
//CREATE_CALLABLE_SIGNATURE(lua_load, int, "\x8B\x4C\x24\x10\x33\xD2\x83\xEC\x18\x3B\xCA", "xxxxxxxxxxx", 0, lua_State*, lua_Reader, void*, const char*)
CREATE_CALLABLE_SIGNATURE(lua_setfield, void, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x0C\x53\x56\x57\x8B\xF9\xE8\x00\x00\x00\x00\x8B\x55\x08\x8B\xF2\x8B\xD8\x8D\x4E\x01\x8D\x49\x00\x8A\x06\x46\x84\xC0\x75\xF9\x2B\xF1\x56\x8B\xCF\xE8\x00\x00\x00\x00\x89\x44\x24\x14\x8B\x47\x08\x83\xE8\x08\x50", "xxxxxxxxxxxxxxx????xxxxxxxxxxxxxxxxxxxxxxxxxx????xxxxxxxxxxx", 0, lua_State*, int, const char*)
CREATE_CALLABLE_SIGNATURE(lua_createtable, void, "\x53\x56\x57\x8B\xF9\x8B\xDA\x8B\x4F\x10\x8B\x41\x48\x85\xC0\x74\x0B\x48\x89\x41\x48\x8B\xCF\xE8\x00\x00\x00\x00\x8B\x4F\x10\x8B\x41\x4C\x3B\x41\x40\x72\x07\x8B\xCF\xE8\x00\x00\x00\x00\xFF\x74\x24\x10\x8B\x77\x08\x8B\xD3\x8B\xCF\xE8\x00\x00\x00\x00\x83\xC4\x04\x89\x06\xC7\x46\x04\x05\x00\x00\x00", "xxxxxxxxxxxxxxxxxxxxxxxx????xxxxxxxxxxxxxx????xxxxxxxxxxxx????xxxxxxxxx???", 0, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(lua_insert, void, "\x53\x57\x8B\xD9\xE8\x00\x00\x00\x00\x8B\x53\x08\x8B\xF8\x3B\xD7", "xxxxx????xxxxxxx", 0, lua_State*, int)
//CREATE_CALLABLE_SIGNATURE(lua_newstate, lua_State*, "\x53\x55\x8B\x6C\x24\x0C\x56\x57\x8B\x7C\x24\x18\x68\x00\x00\x00\x00\x33\xDB", "xxxxxxxxxxxxx????xx", 0, lua_Alloc, void*)
CREATE_CALLABLE_SIGNATURE(lua_close, void, "\x55\x8B\xEC\x83\xE4\xF8\x51\x56\x8B\x71\x10\x8B\x76\x70", "xxxxxxxxxxxxxx", 0, lua_State*)

CREATE_CALLABLE_SIGNATURE(lua_rawset, void, "\x51\x53\x55\x56\x57\x8B\xF1\xE8\x00\x00\x00\x00", "xxxxxxxx????", 0, lua_State*, int)
//CREATE_CALLABLE_SIGNATURE(lua_settable, void, "\x8B\x4C\x24\x08\x56\x8B\x74\x24\x08\x8B\xD6\xE8\x00\x00\x00\x00\x8B\x4E\x08\x8D\x51\xF8", "xxxxxxxxxxxx????xxxxxx", 0, lua_State*, int)

//CREATE_CALLABLE_SIGNATURE(lua_pushnumber, void, "\x8B\x44\x24\x04\x8B\x48\x08\xF3\x0F\x10\x44\x24\x08", "xxxxxxxxxxxxx", 0, lua_State*, double)
//CREATE_CALLABLE_SIGNATURE(lua_pushinteger, void, "\x8B\x44\x24\x04\x8B\x48\x08\xF3\x0F\x2A\x44\x24\x08", "xxxxxxxxxxxxx", 0, lua_State*, ptrdiff_t)
//CREATE_CALLABLE_SIGNATURE(lua_pushboolean, void, "\x8B\x44\x24\x04\x8B\x48\x08\x33", "xxxxxxxx", 0, lua_State*, bool)
CREATE_CALLABLE_SIGNATURE(lua_pushcclosure, void, "\x83\xEC\x08\x53\x55\x56\x8B\xF1\x57\x8B\x4E\x10\x89\x54\x24\x14", "xxxxxxxxxxxxxxxx", 0, lua_State*, lua_CFunction, int);
CREATE_CALLABLE_SIGNATURE(lua_pushlstring, void, "\x53\x56\x57\x8B\xF9\x8B\xDA\x8B\x4F\x10\x8B\x41\x48\x85\xC0\x74\x0B\x48\x89\x41\x48\x8B\xCF\xE8\x00\x00\x00\x00\x8B\x4F\x10\x8B\x41\x4C\x3B\x41\x40\x72\x07\x8B\xCF\xE8\x00\x00\x00\x00\xFF\x74\x24\x10\x8B\x77\x08\x8B\xD3\x8B\xCF\xE8\x00\x00\x00\x00\x83\xC4\x04\x89\x06\xC7\x46\x04\x04\x00\x00\x00", "xxxxxxxxxxxxxxxxxxxxxxxx????xxxxxxxxxxxxxx????xxxxxxxxxxxx????xxxxxxxxx???", 0, lua_State*, const char*, size_t)

CREATE_CALLABLE_SIGNATURE(luaI_openlib, void, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x14\x53\x56\x8B\xDA\x57\x8B\xF1\x85\xDB\x0F\x84\x00\x00\x00\x00\x8B\x4D\x08", "xxxxxxxxxxxxxxxxxxxx????xxx", 0, lua_State*, const char*, const luaL_Reg*, int)
CREATE_CALLABLE_SIGNATURE(luaL_ref, int, "\x83\xEC\x0C\x56\x8B\xF1\x57\x8B\x46\x08\x83\xC0\xF8\x3D\x00\x00\x00\x00\x74\x12", "xxxxxxxxxxxxxx????xx", 0, lua_State*, int);
//CREATE_CALLABLE_SIGNATURE(lua_rawgeti, void, "\x8B\x4C\x24\x08\x56\x8B\x74\x24\x08\x8B\xD6\xE8\x00\x00\x00\x00\x8B\x4C\x24\x10", "xxxxxxxxxxxx????xxxx", 0, lua_State*, int, int);
CREATE_CALLABLE_SIGNATURE(luaL_unref, void, "\x56\x57\x8B\x7C\x24\x0C\x8B\xF1\x85\xFF\x78\x5B", "xxxxxxxxxxxx", 0, lua_State*, int, int);
CREATE_CALLABLE_CLASS_SIGNATURE(do_game_update, void*, "\x56\xFF\x74\x24\x0C\x8B\xF1\xBA\x00\x00\x00\x00\x8B\x0E", "xxxxxxxx????xx", 0, int*, int*)
CREATE_CALLABLE_CLASS_SIGNATURE(luaL_newstate, int, "\x51\x8B\x44\x24\x10\x53\x56\x57\x8B\xF9\x85\xC0", "xxxxxxxxxxxx", 0, char, char, int)


// lua c-functions

#define LUA_REGISTRYINDEX	(-10000)
#define LUA_GLOBALSINDEX	(-10002)

// more bloody lua shit
#define LUA_YIELD	1
#define LUA_ERRRUN	2
#define LUA_ERRSYNTAX	3
#define LUA_ERRMEM	4
#define LUA_ERRERR	5
#define LUA_ERRFILE     (LUA_ERRERR+1)

struct luastackState {
	char a[8];
	int top;
	int base;
};

union LValue {
	void* gc;
	void* p;
	float n;
	int b;
};

struct TValue {
	LValue value;
	int tt;
};

typedef TValue *StkId;

struct lua_State {
	char a[8];
	StkId top;
	StkId base;
};

class LTable;

CREATE_CALLABLE_SIGNATURE(index2adr, TValue*, "\x85\xD2\x7E\x13\x8B\x41\x0C\x4A\x8D\x14\xD0\x3B\x51\x08\xB8\x00\x00\x00\x00", "xxxxxxxxxxxxxxx????", 0, lua_State*, int)
CREATE_CALLABLE_SIGNATURE(luaV_settable, void, "\x83\xEC\x08\x53\x55\x56\x8B\xEA\x57\x89\x6C\x24\x10\x8B\xD9\xC7\x44\x24\x00\x00\x00\x00\x00\xEB\x07\x8D\xA4\x24\x00\x00\x00\x00", "xxxxxxxxxxxxxxxxxx?????xxxxx????", 0, lua_State*, TValue*, TValue*, StkId)
CREATE_CALLABLE_SIGNATURE(luaH_getnum, TValue*, "\x51\x57\x8B\xF9\x8D\x42\xFF\x3B\x47\x1C\x73\x0C", "xxxxxxxxxxxx", 0, LTable*, int)

void lua_rawgeti(lua_State* L, int idx, int n) {
	StkId o;
	o = index2adr(L, idx);
	LTable* ott = (LTable*)(o)->value.gc;
	TValue* o2 = luaH_getnum(ott, n);
	L->top->value = o2->value;
	L->top->tt = o2->tt;
	L->top++;
}

void lua_settable(lua_State* L, int idx) {
	StkId t;
	t = index2adr(L, idx);
	luaV_settable(L, t, L->top - 2, L->top - 1);
	_asm add esp, 8
	L->top -= 2;
}

int lua_gettop(lua_State* L) {
	return L->top - L->base;
}

void lua_pushboolean(lua_State* L, int b)
{
	TValue* i_o = L->top;
	i_o->value.b = (b != 0);
	i_o->tt = 1; // LUA_TBOOLEAN
	L->top++;
}

void lua_pushinteger(lua_State* L, int n)
{
	TValue* i_o = L->top;
	i_o->value.n = static_cast<float>(n);
	i_o->tt = 3; // LUA_TNUMBER
	L->top++;
}

std::list<lua_State*> activeStates;
void add_active_state(lua_State* L){
	activeStates.push_back(L);
}

void remove_active_state(lua_State* L){
	activeStates.remove(L);
}

bool check_active_state(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	std::list<lua_State*>::iterator it;
	for (it = activeStates.begin(); it != activeStates.end(); it++){
		if (*it == L) {
			return true;
		}
	}
	return false;
}

void __fastcall lua_newcall(lua_State* L, int args, int returns){
	PD2HOOK_TRACE_FUNC;
	int result = lua_pcall(L, args, returns, 0);
	_asm add esp, 8
	if (result != 0) {
		size_t len;
		// Beware! If the logging level of the Logger is higher than ERROR (shouldn't be possible) then this lua call would be elided and the asm below would corrupt the stack!
		// The safer option would be to save the string from the lua call to a variable then throw it into the logger.
		PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
		_asm add esp, 4
	}
	printf("lua call\n");
}

int luaH_getcontents(lua_State* L, bool files){
	PD2HOOK_TRACE_FUNC;
	size_t len;
	const char* dirc = lua_tolstring(L, 1, &len);
	_asm add esp, 4
	std::string dir(dirc, len);
	std::vector<std::string> directories;

	try {
		directories = Util::GetDirectoryContents(dir, files);
	}
	catch (const Util::IOException& e){
		PD2HOOK_LOG_EXCEPTION(e);
		lua_pushboolean(L, false);
		return 1;
	}

	lua_createtable(L, 0, 0);
	_asm add esp, 4

	std::vector<std::string>::iterator it;
	int index = 1;
	for (it = directories.begin(); it < directories.end(); it++){
		if (*it == "." || *it == "..") continue;
		lua_pushinteger(L, index);
		lua_pushlstring(L, it->c_str(), it->length());
		_asm add esp, 4
		lua_settable(L, -3);
		index++;
	}

	return 1;
}

int luaF_getdir(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	return luaH_getcontents(L, true);
}

int luaF_getfiles(lua_State* L){
	return luaH_getcontents(L, false);
}

int luaF_directoryExists(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	size_t len;
	const char* dirc = lua_tolstring(L, 1, &len);
	_asm add esp, 4
	bool doesExist = Util::DirectoryExists(dirc);
	lua_pushboolean(L, doesExist);
	return 1;
}

int luaF_unzipfile(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	size_t len;
	const char* archivePath = lua_tolstring(L, 1, &len);
	_asm add esp, 4
	const char* extractPath = lua_tolstring(L, 2, &len);
	_asm add esp, 4

	pd2hook::ExtractZIPArchive(archivePath, extractPath);
	return 0;
}

int luaF_removeDirectory(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	size_t len;
	const char* directory = lua_tolstring(L, 1, &len);
	bool success = Util::RemoveEmptyDirectory(directory);
	lua_pushboolean(L, success);
	return 1;
}

int luaF_pcall(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	int args = lua_gettop(L);

	int result = lua_pcall(L, args - 1, -1, 0);
	_asm add esp, 8
	if (result == LUA_ERRRUN){
		size_t len;
		PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
		_asm add esp, 4
		return 0;
	}
	lua_pushboolean(L, result == 0);
	lua_insert(L, 1);

	//if (result != 0) return 1;

	return lua_gettop(L);
}

int luaF_dofile(lua_State* L){
	PD2HOOK_TRACE_FUNC;

	int n = lua_gettop(L);

	size_t length = 0;
	const char* filename = lua_tolstring(L, 1, &length);
	__asm add esp, 4
	int error = luaL_loadfile(L, filename);
	if (error == LUA_ERRSYNTAX){
		size_t len;
		PD2HOOK_LOG_ERROR(filename << " - " << lua_tolstring(L, -1, &len));
		__asm add esp, 4
	}
	error = lua_pcall(L, 0, 0, 0);
	__asm add esp, 8
	if (error == LUA_ERRRUN){
		size_t len;
		PD2HOOK_LOG_ERROR(filename << " - " << lua_tolstring(L, -1, &len));
		__asm add esp, 4
	}
	return 0;
}

struct lua_http_data {
	int funcRef;
	int progressRef;
	int requestIdentifier;
	lua_State* L;
};

void return_lua_http(void* data, std::string& urlcontents){
	PD2HOOK_TRACE_FUNC;
	lua_http_data* ourData = (lua_http_data*)data;
	if (!check_active_state(ourData->L)) {
		delete ourData;
		return;
	}

	lua_rawgeti(ourData->L, LUA_REGISTRYINDEX, ourData->funcRef);
	lua_pushlstring(ourData->L, urlcontents.c_str(), urlcontents.length());
	_asm add esp, 4
	lua_pushinteger(ourData->L, ourData->requestIdentifier);
	lua_pcall(ourData->L, 2, 0, 0);
	_asm add esp, 8
	luaL_unref(ourData->L, LUA_REGISTRYINDEX, ourData->funcRef);
	_asm add esp, 4
	luaL_unref(ourData->L, LUA_REGISTRYINDEX, ourData->progressRef);
	_asm add esp, 4
	delete ourData;
}

void progress_lua_http(void* data, long progress, long total){
	PD2HOOK_TRACE_FUNC;
	lua_http_data* ourData = (lua_http_data*)data;

	if (!check_active_state(ourData->L)){
		return;
	}

	if (ourData->progressRef == 0) return;
	lua_rawgeti(ourData->L, LUA_REGISTRYINDEX, ourData->progressRef);
	lua_pushinteger(ourData->L, ourData->requestIdentifier);
	lua_pushinteger(ourData->L, progress);
	lua_pushinteger(ourData->L, total);
	lua_pcall(ourData->L, 3, 0, 0);
	_asm add esp, 8
}

static int HTTPReqIdent = 0;

int luaF_dohttpreq(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	PD2HOOK_LOG_LOG("Incoming HTTP Request/Request");

	int args = lua_gettop(L);
	int progressReference = 0;
	if (args >= 3){
		progressReference = luaL_ref(L, LUA_REGISTRYINDEX);
	}

	int functionReference = luaL_ref(L, LUA_REGISTRYINDEX);
	size_t len;
	const char* url_c = lua_tolstring(L, 1, &len);
	_asm add esp, 4
	std::string url = std::string(url_c, len);

	PD2HOOK_LOG_LOG(std::string(url_c, len) << " - " << functionReference);

	lua_http_data* ourData = new lua_http_data();
	ourData->funcRef = functionReference;
	ourData->progressRef = progressReference;
	ourData->L = L;

	HTTPReqIdent++;
	ourData->requestIdentifier = HTTPReqIdent;

	std::unique_ptr<HTTPItem> reqItem(new HTTPItem());
	reqItem->call = return_lua_http;
	reqItem->data = ourData;
	reqItem->url = url;

	if (progressReference != 0){
		reqItem->progress = progress_lua_http;
	}

	HTTPManager::GetSingleton()->LaunchHTTPRequest(std::move(reqItem));
	lua_pushinteger(L, HTTPReqIdent);
	return 1;
}

CConsole* gbl_mConsole = NULL;

int luaF_createconsole(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	if (gbl_mConsole) return 0;
	gbl_mConsole = new CConsole();
	return 0;
}

int luaF_destroyconsole(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	if (!gbl_mConsole) return 0;
	delete gbl_mConsole;
	gbl_mConsole = NULL;
	return 0;
}

int luaF_print(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	size_t len;
	const char* str = lua_tolstring(L, 1, &len);
	__asm add esp, 4
	PD2HOOK_LOG_LUA(str);
	//Logging::Log("aaaaaa", Logging::LOGGING_LUA);
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

	lua_State* L = reinterpret_cast<lua_State *>(thislol);
	if (updates == 0){
		HTTPManager::GetSingleton()->init_locks();
	}


	if (updates > 1){
		EventQueueMaster::GetSingleton().ProcessEvents();
	}

	updates++;
	return do_game_update(thislol, a, b);
}

// Random dude who wrote what's his face?
// I 'unno, I stole this method from the guy who wrote the 'underground-light-lua-hook'
// Mine worked fine, but this seems more elegant.
int __fastcall luaL_newstate_new(void* thislol, int edx, char no, char freakin, int clue){
	PD2HOOK_TRACE_FUNC;
	int ret = luaL_newstate(thislol, no, freakin, clue);

	lua_State* L = (lua_State*)*((void**)thislol);
	printf("Lua State: %p\n", (void*)L);
	if (!L) return ret;
	//int stack_size = lua_gettop(L);
	//printf("%d\n", stack_size);

	add_active_state(L);

	//CREATE_LUA_FUNCTION(luaF_print, "log")
	lua_pushcclosure(L, luaF_print, 0);
	_asm add esp, 4
	lua_setfield(L, LUA_GLOBALSINDEX, "log");
	_asm add esp, 4

	lua_pushcclosure(L, luaF_pcall, 0);
	_asm add esp, 4
	lua_setfield(L, LUA_GLOBALSINDEX, "pcall");
	_asm add esp, 4

	lua_pushcclosure(L, luaF_dofile, 0);
	_asm add esp, 4
	lua_setfield(L, LUA_GLOBALSINDEX, "dofile");
	_asm add esp, 4

	lua_pushcclosure(L, luaF_unzipfile, 0);
	_asm add esp, 4
	lua_setfield(L, LUA_GLOBALSINDEX, "unzip");
	_asm add esp, 4

	lua_pushcclosure(L, luaF_dohttpreq, 0);
	_asm add esp, 4
	lua_setfield(L, LUA_GLOBALSINDEX, "dohttpreq");
	_asm add esp, 4

	luaL_Reg consoleLib[] = { { "CreateConsole", luaF_createconsole }, { "DestroyConsole", luaF_destroyconsole }, { NULL, NULL } };
	luaI_openlib(L, "console", consoleLib, 0);
	_asm add esp, 8

	luaL_Reg fileLib[] = { { "GetDirectories", luaF_getdir }, { "GetFiles", luaF_getfiles }, { "RemoveDirectory", luaF_removeDirectory }, { "DirectoryExists", luaF_directoryExists }, { NULL, NULL } };
	luaI_openlib(L, "file", fileLib, 0);
	_asm add esp, 8

	//lua_settop(L, stack_size);
	int result;
	PD2HOOK_LOG_LOG("Initiating Hook");

	result = luaL_loadfile(L, "mods/base/base.lua");
	if (result == LUA_ERRSYNTAX){
		size_t len;
		PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
		_asm add esp, 4
		return ret;
	}
	result = lua_pcall(L, 0, 1, 0);
	_asm add esp, 8
	if (result == LUA_ERRRUN){
		size_t len;
		PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
		_asm add esp, 4
		return ret;
	}

	//CREATE_LUA_FUNCTION(luaF_pcall, "pcall")
	//CREATE_LUA_FUNCTION(luaF_dofile, "dofile")
	/*CREATE_LUA_FUNCTION(luaF_dohttpreq, "dohttpreq")

	CREATE_LUA_FUNCTION(luaF_unzipfile, "unzip")

	*/
	return ret;
}

void __fastcall luaF_close(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	remove_active_state(L);
	lua_close(L);
}

void InitiateStates(){
	PD2HOOK_TRACE_FUNC;

	main_thread_id = std::this_thread::get_id();

	SignatureSearch::Search();


	FuncDetour* gameUpdateDetour = new FuncDetour((void**)&do_game_update, do_game_update_new);
	FuncDetour* newStateDetour = new FuncDetour((void**)&luaL_newstate, luaL_newstate_new);
	//FuncDetour* luaCallDetour = new FuncDetour((void**)&lua_call, lua_newcall);
	FuncDetour* luaCloseDetour = new FuncDetour((void**)&lua_close, luaF_close);
}

void DestroyStates(){
	PD2HOOK_TRACE_FUNC;
	// Okay... let's not do that.
	// I don't want to keep this in memory, but it CRASHES THE SHIT OUT if you delete this after all is said and done.
	// if (gbl_mConsole) delete gbl_mConsole;
}
}
