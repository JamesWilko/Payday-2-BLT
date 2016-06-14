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
#include "bass.h"

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
CREATE_CALLABLE_SIGNATURE(lua_settop, void, "\x85\xD2\x78\x2E\x8B\x41\x0C\xC1\xE2\x03\x03\xC2", "xxxxxxxxxxxx", 0, lua_State*, int)
CREATE_CALLABLE_SIGNATURE(lua_tolstring, const char*, "\x56\x57\x8B\xFA\x8B\xF1\xE8\x00\x00\x00\x00\x8B\xC8\x83\x79\x04\x04", "xxxxxxx????xxxxxx", 0, lua_State*, int, size_t*)
CREATE_CALLABLE_SIGNATURE(luaL_loadfile, int, "\x55\x8B\xEC\x83\xE4\xF8\x81\xEC\x00\x00\x00\x00\x53\x55\x56\x57\x8B\xEA\x8B\xF9\xC7\x44\x24\x00\x00\x00\x00\x00\x8B\x5F\x08\x2B\x5F\x0C", "xxxxxxxx????xxxxxxxxxxx?????xxxxxx", 0, lua_State*, const char*)
CREATE_CALLABLE_SIGNATURE(lua_setfield, void, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x0C\x53\x56\x57\x8B\xF9\xE8\x00\x00\x00\x00\x8B\x55\x08\x8B\xF2\x8B\xD8\x8D\x4E\x01\x8D\x49\x00\x8A\x06\x46\x84\xC0\x75\xF9\x2B\xF1\x56\x8B\xCF\xE8\x00\x00\x00\x00\x89\x44\x24\x14\x8B\x47\x08\x83\xE8\x08\x50", "xxxxxxxxxxxxxxx????xxxxxxxxxxxxxxxxxxxxxxxxxx????xxxxxxxxxxx", 0, lua_State*, int, const char*)
CREATE_CALLABLE_SIGNATURE(lua_createtable, void, "\x53\x56\x57\x8B\xF9\x8B\xDA\x8B\x4F\x10\x8B\x41\x48\x85\xC0\x74\x0B\x48\x89\x41\x48\x8B\xCF\xE8\x00\x00\x00\x00\x8B\x4F\x10\x8B\x41\x4C\x3B\x41\x40\x72\x07\x8B\xCF\xE8\x00\x00\x00\x00\xFF\x74\x24\x10\x8B\x77\x08\x8B\xD3\x8B\xCF\xE8\x00\x00\x00\x00\x83\xC4\x04\x89\x06\xC7\x46\x04\x05\x00\x00\x00", "xxxxxxxxxxxxxxxxxxxxxxxx????xxxxxxxxxxxxxx????xxxxxxxxxxxx????xxxxxxxxx???", 0, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(lua_insert, void, "\x53\x57\x8B\xD9\xE8\x00\x00\x00\x00\x8B\x53\x08\x8B\xF8\x3B\xD7", "xxxxx????xxxxxxx", 0, lua_State*, int)
CREATE_CALLABLE_SIGNATURE(lua_close, void, "\x55\x8B\xEC\x83\xE4\xF8\x51\x56\x8B\x71\x10\x8B\x76\x70", "xxxxxxxxxxxxxx", 0, lua_State*)

CREATE_CALLABLE_SIGNATURE(lua_load, int, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x1C\x8B\x45\x0C", "xxxxxxxxxxxx", 0, lua_State*, lua_Reader, void*, const char*)
CREATE_CALLABLE_SIGNATURE(lua_newstate, lua_State*, "\x51\x8B\x44\x24\x10\x53\x56\x57", "xxxxxxxx", 0, lua_Alloc, void*)
CREATE_CALLABLE_SIGNATURE(lua_rawseti, void, "\x53\x56\x57\x8B\xF9\xE8\x00\x00\x00\x00\xFF\x74\x24\x10", "xxxxxx????xxxx", 0, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(lua_pushstring, void, "\x51\x56\x57\x8B\xF1\x85\xD2\x75\x0E", "xxxxxxxxx", 0, lua_State*, const char *)
CREATE_CALLABLE_SIGNATURE(lua_tonumber, double, "\x83\xEC\x0C\xE8\x00\x00\x00\x00\x8B\x48\x04", "xxxx????xxx", 0, lua_State*, int)

CREATE_CALLABLE_SIGNATURE(lua_rawset, void, "\x51\x53\x55\x56\x57\x8B\xF1\xE8\x00\x00\x00\x00", "xxxxxxxx????", 0, lua_State*, int)
CREATE_CALLABLE_SIGNATURE(lua_pushcclosure, void, "\x83\xEC\x08\x53\x55\x56\x8B\xF1\x57\x8B\x4E\x10\x89\x54\x24\x14", "xxxxxxxxxxxxxxxx", 0, lua_State*, lua_CFunction, int)
CREATE_CALLABLE_SIGNATURE(lua_pushlstring, void, "\x53\x56\x57\x8B\xF9\x8B\xDA\x8B\x4F\x10\x8B\x41\x48\x85\xC0\x74\x0B\x48\x89\x41\x48\x8B\xCF\xE8\x00\x00\x00\x00\x8B\x4F\x10\x8B\x41\x4C\x3B\x41\x40\x72\x07\x8B\xCF\xE8\x00\x00\x00\x00\xFF\x74\x24\x10\x8B\x77\x08\x8B\xD3\x8B\xCF\xE8\x00\x00\x00\x00\x83\xC4\x04\x89\x06\xC7\x46\x04\x04\x00\x00\x00", "xxxxxxxxxxxxxxxxxxxxxxxx????xxxxxxxxxxxxxx????xxxxxxxxxxxx????xxxxxxxxx???", 0, lua_State*, const char*, size_t)
CREATE_CALLABLE_SIGNATURE(luaI_openlib, void, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x14\x53\x56\x8B\xDA\x57\x8B\xF1\x85\xDB\x0F\x84\x00\x00\x00\x00\x8B\x4D\x08", "xxxxxxxxxxxxxxxxxxxx????xxx", 0, lua_State*, const char*, const luaL_Reg*, int)
CREATE_CALLABLE_SIGNATURE(luaL_ref, int, "\x83\xEC\x0C\x56\x8B\xF1\x57\x8B\x46\x08\x83\xC0\xF8\x3D\x00\x00\x00\x00\x74\x12", "xxxxxxxxxxxxxx????xx", 0, lua_State*, int)
CREATE_CALLABLE_SIGNATURE(luaL_unref, void, "\x56\x57\x8B\x7C\x24\x0C\x8B\xF1\x85\xFF\x78\x5B", "xxxxxxxxxxxx", 0, lua_State*, int, int)
CREATE_CALLABLE_CLASS_SIGNATURE(do_game_update, void*, "\x56\xFF\x74\x24\x0C\x8B\xF1\xBA\x00\x00\x00\x00\x8B\x0E", "xxxxxxxx????xx", 0, int*, int*)
CREATE_CALLABLE_CLASS_SIGNATURE(luaL_newstate, int, "\x51\x8B\x44\x24\x10\x53\x56\x57\x8B\xF9\x85\xC0", "xxxxxxxxxxxx", 0, char, char, int)

// lua c-functions

#define LUA_REGISTRYINDEX	(-10000)
#define LUA_GLOBALSINDEX	(-10002)

// lua types
#define LUA_TNONE               (-1)
#define LUA_TNIL                0
#define LUA_TBOOLEAN            1
#define LUA_TLIGHTUSERDATA      2
#define LUA_TNUMBER             3
#define LUA_TSTRING             4
#define LUA_TTABLE              5
#define LUA_TFUNCTION           6
#define LUA_TUSERDATA           7
#define LUA_TTHREAD             8


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
	L->top -= 2;
}

int lua_gettop(lua_State* L) {
	return L->top - L->base;
}

void lua_pushboolean(lua_State* L, int b)
{
	TValue* i_o = L->top;
	i_o->value.b = (b != 0);
	i_o->tt = LUA_TBOOLEAN;
	L->top++;
}

void lua_pushinteger(lua_State* L, int n)
{
	TValue* i_o = L->top;
	i_o->value.n = static_cast<float>(n);
	i_o->tt = LUA_TNUMBER;
	L->top++;
}

void lua_pushnumber(lua_State* L, float n)
{
	TValue* i_o = L->top;
	i_o->value.n = static_cast<float>(n);
	i_o->tt = LUA_TNUMBER;
	L->top++;
}

int lua_tointeger(lua_State * L, int idx)
{
	return ((int)lua_tonumber(L, idx));
}

int lua_toboolean(lua_State * L, int idx)
{
	const TValue * o = index2adr(L, idx);
	return !(o->tt == LUA_TNIL || (o->tt == LUA_TBOOLEAN && o->value.b == 0)); // l_isfalse
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
	if (result != 0) {
		size_t len;
		PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
	}
	PD2HOOK_LOG_LOG("lua call");
}

int luaH_getcontents(lua_State* L, bool files){
	PD2HOOK_TRACE_FUNC;
	size_t len;
	const char* dirc = lua_tolstring(L, 1, &len);
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

	std::vector<std::string>::iterator it;
	int index = 1;
	for (it = directories.begin(); it < directories.end(); it++){
		if (*it == "." || *it == "..") continue;
		lua_pushinteger(L, index);
		lua_pushlstring(L, it->c_str(), it->length());
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
	bool doesExist = Util::DirectoryExists(dirc);
	lua_pushboolean(L, doesExist);
	return 1;
}

int luaF_unzipfile(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	size_t len;
	const char* archivePath = lua_tolstring(L, 1, &len);
	const char* extractPath = lua_tolstring(L, 2, &len);

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
	if (result == LUA_ERRRUN){
		size_t len;
		PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
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
	int error = luaL_loadfile(L, filename);
	if (error == LUA_ERRSYNTAX){
		size_t len;
		PD2HOOK_LOG_ERROR(filename << " - " << lua_tolstring(L, -1, &len));
	}
	error = lua_pcall(L, 0, 0, 0);
	if (error == LUA_ERRRUN){
		size_t len;
		PD2HOOK_LOG_ERROR(filename << " - " << lua_tolstring(L, -1, &len));
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
	lua_pushinteger(ourData->L, ourData->requestIdentifier);
	lua_pcall(ourData->L, 2, 0, 0);
	luaL_unref(ourData->L, LUA_REGISTRYINDEX, ourData->funcRef);
	luaL_unref(ourData->L, LUA_REGISTRYINDEX, ourData->progressRef);
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

namespace
{
std::unique_ptr<CConsole> gbl_mConsole;
}

int luaF_createconsole(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	if (!gbl_mConsole)
	{
		gbl_mConsole.reset(new CConsole());
	}
	return 0;
}

int luaF_destroyconsole(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	gbl_mConsole.reset();
	return 0;
}

int luaF_print(lua_State* L){
	PD2HOOK_TRACE_FUNC;
	size_t len;
	const char* str = lua_tolstring(L, 1, &len);
	PD2HOOK_LOG_LUA(str);
	return 0;
}


HSTREAM bassHandles[1024];
int currentBassHandle = 0;

int luaF_Bass_Init(lua_State *L)
{
	int device = (int)lua_tonumber(L, 1);
	int freq = (int)lua_tonumber(L, 2);
	int flags = (int)lua_tonumber(L, 3);
	lua_pushboolean(L, BASS_Init(device, freq, flags, 0, 0));
	return 1;
}

int luaF_Bass_Free(lua_State *L)
{
	lua_pushboolean(L, BASS_Free());
	return 1;
}


int luaF_Bass_ErrorGetCode(lua_State *L)
{
	lua_pushnumber(L, BASS_ErrorGetCode());
	return 1;
}

int luaF_Bass_StreamCreateFile(lua_State *L)
{
	size_t len;
	const char* filename = lua_tolstring(L, 1, &len);
	PD2HOOK_LOG_LOG(filename);
	int offset = (int)lua_tonumber(L, 2);
	int length = (int)lua_tonumber(L, 3);
	int flags = (int)lua_tonumber(L, 4);
	bassHandles[currentBassHandle] = BASS_StreamCreateFile(false, filename, offset, length, BASS_SAMPLE_FLOAT);

	int errorcode = BASS_ErrorGetCode();
	if (errorcode > 0)
	{
		printf("[Error] Could not create stream! Error: %i", errorcode);
	}

	lua_pushinteger(L, currentBassHandle);
	currentBassHandle++;

	return 1;
}

int luaF_Bass_GetVolume(lua_State *L)
{
	lua_pushnumber(L, BASS_GetVolume());
	return 1;
}

int luaF_Bass_SetVolume(lua_State *L)
{
	float volume = lua_tonumber(L, 1);
	lua_pushboolean(L, BASS_SetVolume(volume));
	return 1;
}

int luaF_Bass_SetStreamVolume(lua_State *L)
{
	float volume = lua_tonumber(L, 1);
	lua_pushboolean(L, BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, volume));
	return 1;
}

int luaF_Bass_SetSampleVolume(lua_State *L)
{
	float volume = lua_tonumber(L, 1);
	lua_pushboolean(L, BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, volume));
	return 1;
}

int luaF_Bass_SetBufferSize(lua_State *L)
{
	double ms = lua_tonumber(L, 1);
	lua_pushboolean(L, BASS_SetConfig(BASS_CONFIG_BUFFER, ms));
	return 1;
}

int luaF_Bass_ChannelPlay(lua_State *L)
{
	int handleIndex = (int)lua_tonumber(L, 1);
	HSTREAM handle = bassHandles[handleIndex];
	bool restart = lua_toboolean(L, 2);
	lua_pushboolean(L, BASS_ChannelPlay(handle, restart));
	return 1;
}

int luaF_Bass_ChannelStop(lua_State *L)
{
	int handleIndex = (int)lua_tonumber(L, 1);
	HSTREAM handle = bassHandles[handleIndex];
	lua_pushboolean(L, BASS_ChannelStop(handle));
	return 1;
}

int luaF_Bass_ChannelPause(lua_State *L)
{
	int handleIndex = (int)lua_tonumber(L, 1);
	HSTREAM handle = bassHandles[handleIndex];
	lua_pushboolean(L, BASS_ChannelPause(handle));
	return 1;
}

int luaF_Bass_ChannelSetSync(lua_State *L)
{
	// lua_pushinteger(L, BASS_ChannelSetSync(lua_tointeger(L, 1), lua_tointeger(L, 2), lua_tointeger(L, 3), &SyncDispatcher, NULL));
	return 1;
}

int luaF_Bass_ChannelGetPosition(lua_State *L)
{
	int handleIndex = (int)lua_tonumber(L, 1);
	HSTREAM handle = bassHandles[handleIndex];
	int flags = (int)lua_tonumber(L, 2);
	lua_pushinteger(L, BASS_ChannelGetPosition(handle, flags));
	return 1;
}

int luaF_Bass_ChannelGetLength(lua_State *L)
{
	int handleIndex = (int)lua_tonumber(L, 1);
	HSTREAM handle = bassHandles[handleIndex];
	int flags = (int)lua_tonumber(L, 2);
	lua_pushinteger(L, BASS_ChannelGetLength(handle, BASS_POS_BYTE));
	return 1;
}

int luaF_Bass_ChannelBytes2Seconds(lua_State *L)
{
	int handleIndex = (int)lua_tonumber(L, 1);
	HSTREAM handle = bassHandles[handleIndex];
	double byteLength = (int)lua_tonumber(L, 2);
	lua_pushinteger(L, BASS_ChannelBytes2Seconds(handle, byteLength));
	return 1;
}

int luaF_Bass_ChannelSeconds2Bytes(lua_State *L)
{
	int handleIndex = (int)lua_tonumber(L, 1);
	HSTREAM handle = bassHandles[handleIndex];
	double pos = (int)lua_tonumber(L, 2);
	lua_pushinteger(L, BASS_ChannelSeconds2Bytes(handle, pos));
	return 1;
}

int luaF_Bass_ChannelSetPosition(lua_State *L)
{
	int handleIndex = (int)lua_tonumber(L, 1);
	HSTREAM handle = bassHandles[handleIndex];
	double pos = (int)lua_tonumber(L, 2);
	lua_pushinteger(L, BASS_ChannelSetPosition(handle, pos, BASS_POS_BYTE));
	return 1;
}

int luaF_Bass_ChannelSetAttribute(lua_State *L)
{
	int handleIndex = (int)lua_tonumber(L, 1);
	HSTREAM handle = bassHandles[handleIndex];
	double attribute = (int)lua_tonumber(L, 2);
	double value = lua_tonumber(L, 3);
	lua_pushinteger(L, BASS_ChannelSetAttribute(handle, attribute, value));
	return 1;
}

int luaF_Bass_ChannelGetData(lua_State *L)
{
	int handleIndex = (int)lua_tonumber(L, 1);
	HSTREAM handle = bassHandles[handleIndex];
	int arrayLength = (int)lua_tonumber(L, 2);
	int flags = (int)(BASS_DATA_FFT1024); //(int)lua_tonumber(L, 3);

	// 	printf("BASS_DATA_FFT1024 | BASS_DATA_FLOAT : %i\n", BASS_DATA_FFT1024 | BASS_DATA_FLOAT);
	// 	printf("flags : %i\n", flags);

	float * data = new float[arrayLength];
	int success = BASS_ChannelGetData(handle, data, BASS_DATA_FFT1024 | BASS_DATA_FLOAT);

	lua_createtable(L, arrayLength, 0);
	for (int i = 0; i < arrayLength; i++)
	{
		//lua_pushinteger(L, i + 1);
		//lua_pushnumber(L, data[i]);
		//lua_settable(L, -3);
		int num = (int)(data[i] * 1000000);
		lua_pushinteger(L, num);
		lua_rawseti(L, -2, i + 1);
	}

	free(data);
	return 1;
}

int luaF_Bass_Flags(lua_State *L)
{
	int NumFlags = lua_gettop(L);
	int FlagAccum = 0;
	//printf("%d\n", NumFlags);
	for (int i = 1; i <= NumFlags; i++) {
		FlagAccum = FlagAccum | lua_tointeger(L, i);
		//printf("%d %d\n", FlagAccum, lua_tointeger(L, i));
	}
	lua_pushinteger(L, FlagAccum);
	return 1;
}

#define LUA_ENUM(LUASTATE, ENUM) lua_pushstring(LUASTATE, #ENUM); lua_pushinteger(LUASTATE, ENUM); lua_settable(L, -3);
void Bass_CreateBassEnums(lua_State *L)
{
	lua_createtable(L, 0, 0);
	LUA_ENUM(L, BASS_OK);
	LUA_ENUM(L, BASS_ERROR_MEM);
	LUA_ENUM(L, BASS_ERROR_FILEOPEN);
	LUA_ENUM(L, BASS_ERROR_DRIVER);
	LUA_ENUM(L, BASS_ERROR_BUFLOST);
	LUA_ENUM(L, BASS_ERROR_HANDLE);
	LUA_ENUM(L, BASS_ERROR_FORMAT);
	LUA_ENUM(L, BASS_ERROR_POSITION);
	LUA_ENUM(L, BASS_ERROR_INIT);
	LUA_ENUM(L, BASS_ERROR_START);
	LUA_ENUM(L, BASS_ERROR_ALREADY);
	LUA_ENUM(L, BASS_ERROR_NOCHAN);
	LUA_ENUM(L, BASS_ERROR_ILLTYPE);
	LUA_ENUM(L, BASS_ERROR_ILLPARAM);
	LUA_ENUM(L, BASS_ERROR_NO3D);
	LUA_ENUM(L, BASS_ERROR_NOEAX);
	LUA_ENUM(L, BASS_ERROR_DEVICE);
	LUA_ENUM(L, BASS_ERROR_NOPLAY);
	LUA_ENUM(L, BASS_ERROR_FREQ);
	LUA_ENUM(L, BASS_ERROR_NOTFILE);
	LUA_ENUM(L, BASS_ERROR_NOHW);
	LUA_ENUM(L, BASS_ERROR_EMPTY);
	LUA_ENUM(L, BASS_ERROR_NONET);
	LUA_ENUM(L, BASS_ERROR_CREATE);
	LUA_ENUM(L, BASS_ERROR_NOFX);
	LUA_ENUM(L, BASS_ERROR_NOTAVAIL);
	LUA_ENUM(L, BASS_ERROR_DECODE);
	LUA_ENUM(L, BASS_ERROR_DX);
	LUA_ENUM(L, BASS_ERROR_TIMEOUT);
	LUA_ENUM(L, BASS_ERROR_FILEFORM);
	LUA_ENUM(L, BASS_ERROR_SPEAKER);
	LUA_ENUM(L, BASS_ERROR_VERSION);
	LUA_ENUM(L, BASS_ERROR_CODEC);
	LUA_ENUM(L, BASS_ERROR_ENDED);
	LUA_ENUM(L, BASS_ERROR_UNKNOWN);
	lua_setfield(L, -2, "Error");

	lua_createtable(L, 0, 0);
	LUA_ENUM(L, BASS_DEVICE_8BITS);
	LUA_ENUM(L, BASS_DEVICE_MONO);
	LUA_ENUM(L, BASS_DEVICE_3D);
	LUA_ENUM(L, BASS_DEVICE_LATENCY);
	LUA_ENUM(L, BASS_DEVICE_CPSPEAKERS);
	LUA_ENUM(L, BASS_DEVICE_SPEAKERS);
	LUA_ENUM(L, BASS_DEVICE_NOSPEAKER);
	lua_setfield(L, -2, "Device");

	lua_createtable(L, 0, 0);
	LUA_ENUM(L, BASS_STREAM_PRESCAN);
	LUA_ENUM(L, BASS_STREAM_AUTOFREE);
	LUA_ENUM(L, BASS_STREAM_DECODE);
	lua_setfield(L, -2, "Stream");

	lua_createtable(L, 0, 0);
	LUA_ENUM(L, BASS_SAMPLE_FLOAT);
	LUA_ENUM(L, BASS_SAMPLE_MONO);
	LUA_ENUM(L, BASS_SAMPLE_SOFTWARE);
	LUA_ENUM(L, BASS_SAMPLE_3D);
	LUA_ENUM(L, BASS_SAMPLE_LOOP);
	LUA_ENUM(L, BASS_SAMPLE_FX);
	lua_setfield(L, -2, "Sample");

	lua_createtable(L, 0, 0);
	LUA_ENUM(L, BASS_POS_BYTE);
	LUA_ENUM(L, BASS_POS_MUSIC_ORDER);
	LUA_ENUM(L, BASS_POS_DECODE);
	lua_setfield(L, -2, "POS");

	lua_createtable(L, 0, 0);
	LUA_ENUM(L, BASS_DATA_FLOAT);
	LUA_ENUM(L, BASS_DATA_FIXED);
	LUA_ENUM(L, BASS_DATA_FFT256);
	LUA_ENUM(L, BASS_DATA_FFT512);
	LUA_ENUM(L, BASS_DATA_FFT1024);
	LUA_ENUM(L, BASS_DATA_FFT2048);
	LUA_ENUM(L, BASS_DATA_FFT4096);
	LUA_ENUM(L, BASS_DATA_FFT8192);
	LUA_ENUM(L, BASS_DATA_FFT16384);
	LUA_ENUM(L, BASS_DATA_FFT_COMPLEX);
	LUA_ENUM(L, BASS_DATA_FFT_INDIVIDUAL);
	LUA_ENUM(L, BASS_DATA_FFT_NOWINDOW);
	LUA_ENUM(L, BASS_DATA_FFT_REMOVEDC);
	LUA_ENUM(L, BASS_DATA_AVAILABLE);
	lua_setfield(L, -2, "Data");

	lua_createtable(L, 0, 0);
	LUA_ENUM(L, BASS_ATTRIB_EAXMIX);
	LUA_ENUM(L, BASS_ATTRIB_FREQ);
	LUA_ENUM(L, BASS_ATTRIB_MUSIC_AMPLIFY);
	LUA_ENUM(L, BASS_ATTRIB_MUSIC_BPM);
	LUA_ENUM(L, BASS_ATTRIB_MUSIC_PANSEP);
	LUA_ENUM(L, BASS_ATTRIB_MUSIC_PSCALER);
	LUA_ENUM(L, BASS_ATTRIB_MUSIC_SPEED);
	LUA_ENUM(L, BASS_ATTRIB_MUSIC_VOL_CHAN);
	LUA_ENUM(L, BASS_ATTRIB_MUSIC_VOL_GLOBAL);
	LUA_ENUM(L, BASS_ATTRIB_MUSIC_VOL_INST);
	LUA_ENUM(L, BASS_ATTRIB_NET_RESUME);
	LUA_ENUM(L, BASS_ATTRIB_NOBUFFER);
	LUA_ENUM(L, BASS_ATTRIB_NORAMP);
	LUA_ENUM(L, BASS_ATTRIB_PAN);
	LUA_ENUM(L, BASS_ATTRIB_SRC);
	LUA_ENUM(L, BASS_ATTRIB_VOL);
	lua_setfield(L, -2, "Attributes");
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
	PD2HOOK_LOG_LUA("Lua State: " << L);
	if (!L) return ret;
	//int stack_size = lua_gettop(L);
	//printf("%d\n", stack_size);

	add_active_state(L);

	CREATE_LUA_FUNCTION(luaF_print, "log");
	CREATE_LUA_FUNCTION(luaF_pcall, "pcall");
	CREATE_LUA_FUNCTION(luaF_dofile, "dofile");
	CREATE_LUA_FUNCTION(luaF_unzipfile, "unzip");
	CREATE_LUA_FUNCTION(luaF_dohttpreq, "dohttpreq");

	// Console lib
	luaL_Reg consoleLib[] = {
		{ "CreateConsole", luaF_createconsole },
		{ "DestroyConsole", luaF_destroyconsole },
		{ NULL, NULL }
	};
	luaI_openlib(L, "console", consoleLib, 0);

	// File lib
	luaL_Reg fileLib[] = {
		{ "GetDirectories", luaF_getdir },
		{ "GetFiles", luaF_getfiles },
		{ "RemoveDirectory", luaF_removeDirectory },
		{ "DirectoryExists", luaF_directoryExists },
		{ NULL, NULL }
	};
	luaI_openlib(L, "file", fileLib, 0);

	// Bass lib
	luaL_Reg bassLib[] = {
		{ "Init", luaF_Bass_Init },
		{ "Free", luaF_Bass_Free },
		{ "ErrorGetCode", luaF_Bass_ErrorGetCode },
		{ "Flags", luaF_Bass_Flags },
		{ "GetVolume", luaF_Bass_GetVolume },
		{ "SetVolume", luaF_Bass_SetVolume },
		{ "SetBufferSize", luaF_Bass_SetBufferSize },
		{ "SetStreamVolume", luaF_Bass_SetStreamVolume },
		{ "SetSampleVolume", luaF_Bass_SetSampleVolume },
		{ "StreamCreateFile", luaF_Bass_StreamCreateFile },
		{ "ChannelPlay", luaF_Bass_ChannelPlay },
		{ "ChannelStop", luaF_Bass_ChannelStop },
		{ "ChannelPause", luaF_Bass_ChannelPause },
		{ "ChannelSetSync", luaF_Bass_ChannelSetSync },
		{ "ChannelGetPosition", luaF_Bass_ChannelGetPosition },
		{ "ChannelGetLength", luaF_Bass_ChannelGetLength },
		{ "ChannelBytes2Seconds", luaF_Bass_ChannelBytes2Seconds },
		{ "ChannelSeconds2Bytes", luaF_Bass_ChannelSeconds2Bytes },
		{ "ChannelSetPosition", luaF_Bass_ChannelSetPosition },
		{ "ChannelSetAttribute", luaF_Bass_ChannelSetAttribute },
		{ "ChannelGetData", luaF_Bass_ChannelGetData },
		{ NULL, NULL }
	};
	luaI_openlib(L, "Bass", bassLib, 0);

	// Add enumerators to bass library
	Bass_CreateBassEnums(L);

	//lua_settop(L, stack_size);
	int result;
	PD2HOOK_LOG_LOG("Initiating Hook");

	result = luaL_loadfile(L, "mods/base/base.lua");
	if (result == LUA_ERRSYNTAX){
		size_t len;
		PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
		return ret;
	}
	result = lua_pcall(L, 0, 1, 0);
	if (result == LUA_ERRRUN){
		size_t len;
		PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
		return ret;
	}

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
	gbl_mConsole.reset();
}
}
