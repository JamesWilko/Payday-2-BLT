#include "InitState.h"
#include <detours.h>

#include "signatures/signatures.h"
#include "util/util.h"
#include "console/console.h"
#include "threading/queue.h"
#include "http/http.h"

#include <thread>
#include <list>
#include "bass.h"

class lua_State;

typedef const char * (*lua_Reader) (lua_State *L, void *ud, size_t *sz);
typedef int(*lua_CFunction) (lua_State *L);
typedef void * (*lua_Alloc) (void *ud, void *ptr, size_t osize, size_t nsize);
typedef struct luaL_Reg {
	const char* name;
	lua_CFunction func;
} luaL_Reg;

CREATE_CALLABLE_SIGNATURE(lua_call, void, "\x8B\x44\x24\x08\x56\x8B\x74\x24\x08\x8B\x56\x08", "xxxxxxxxxxxx", 0, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(lua_pcall, int, "\x8B\x4C\x24\x10\x83\xEC\x08\x56\x8B\x74\x24\x10", "xxxxxxxxxxxx", 0, lua_State*, int, int, int)
CREATE_CALLABLE_SIGNATURE(lua_gettop, int, "\x8B\x4C\x24\x04\x8B\x41\x08\x2B\x41\x0C", "xxxxxxxxxx", 0, lua_State*)
CREATE_CALLABLE_SIGNATURE(lua_settop, void, "\x8B\x4C\x24\x08\x8B\x44\x24\x04\x85", "xxxxxxxxx", 0, lua_State*, int)
CREATE_CALLABLE_SIGNATURE(lua_tolstring, const char*, "\x56\x8B\x74\x24\x08\x57\x8B\x7C\x24\x10\x8B\xCF\x8B\xD6", "xxxxxxxxxxxxxx", 0, lua_State*, int, size_t*)
CREATE_CALLABLE_SIGNATURE(luaL_loadfile, int, "\x81\xEC\x01\x01\x01\x01\x55\x8B\xAC\x24\x01\x01\x01\x01\x56\x8B\xB4\x24\x01\x01\x01\x01\x57", "xx????xxxx????xxxx????x", 0, lua_State*, const char*)
CREATE_CALLABLE_SIGNATURE(lua_load, int, "\x8B\x4C\x24\x10\x33\xD2\x83\xEC\x18\x3B\xCA", "xxxxxxxxxxx", 0, lua_State*, lua_Reader, void*, const char*)
CREATE_CALLABLE_SIGNATURE(lua_setfield, void, "\x8B\x46\x08\x83\xE8\x08\x50\x8D\x4C\x24\x1C", "xxxxxxxxxxx", -53, lua_State*, int, const char*)
CREATE_CALLABLE_SIGNATURE(lua_createtable, void, "\x83\xC4\x0C\x89\x07\xC7\x47\x04\x05\x00\x00\x00\x83\x46\x08\x08\x5F", "xxxxxxxxx???xxxxx", -66, lua_State*, int, int)
CREATE_CALLABLE_SIGNATURE(lua_insert, void, "\x8B\x4C\x24\x08\x56\x8B\x74\x24\x08\x8B\xD6\xE8\x50\xFE", "xxxxxxxxxxxxxx", 0, lua_State*, int)
CREATE_CALLABLE_SIGNATURE(lua_newstate, lua_State*, "\x53\x55\x8B\x6C\x24\x0C\x56\x57\x8B\x7C\x24\x18\x68\x00\x00\x00\x00\x33\xDB", "xxxxxxxxxxxxx????xx", 0, lua_Alloc, void*)
CREATE_CALLABLE_SIGNATURE(lua_close, void, "\x8B\x44\x24\x04\x8B\x48\x10\x56\x8B\x71\x70", "xxxxxxxxxxx", 0, lua_State*)

CREATE_CALLABLE_SIGNATURE(lua_rawset, void, "\x8B\x4C\x24\x08\x53\x56\x8B\x74\x24\x0C\x57", "xxxxxxxxxxx", 0, lua_State*, int)
CREATE_CALLABLE_SIGNATURE(lua_settable, void, "\x8B\x4C\x24\x08\x56\x8B\x74\x24\x08\x8B\xD6\xE8\x00\x00\x00\x00\x8B\x4E\x08\x8D\x51\xF8", "xxxxxxxxxxxx????xxxxxx", 0, lua_State*, int)

CREATE_CALLABLE_SIGNATURE(lua_rawseti, void, "\x83\xEC\x08\x8B\x4C\x24\x10\x53\x55\x56\x8B\x74\x24\x18", "xxxxxxxxxxxxxx", 0, lua_State*, int, int)

CREATE_CALLABLE_SIGNATURE(lua_pushnumber, void, "\x8B\x44\x24\x04\x8B\x48\x08\xF3\x0F\x10\x44\x24\x08", "xxxxxxxxxxxxx", 0, lua_State*, double)
CREATE_CALLABLE_SIGNATURE(lua_pushinteger, void, "\x8B\x44\x24\x04\x8B\x48\x08\xF3\x0F\x2A\x44\x24\x08", "xxxxxxxxxxxxx", 0, lua_State*, ptrdiff_t)
CREATE_CALLABLE_SIGNATURE(lua_pushboolean, void, "\x8B\x44\x24\x04\x8B\x48\x08\x33", "xxxxxxxx", 0, lua_State*, bool)
CREATE_CALLABLE_SIGNATURE(lua_pushcclosure, void, "\x8B\x50\x04\x8B\x02\x8B\x40\x0C\x8B\x7C\x24\x14\x50\x57\x56", "xxxxxxxxxxxxxxx", -60, lua_State*, lua_CFunction, int);
CREATE_CALLABLE_SIGNATURE(lua_pushlstring, void, "\x52\x50\x56\xE8\x00\x00\x00\x00\x83\xC4\x0C\x89\x07\xC7\x47\x04\x04\x00\x00\x00\x83\x46\x08\x08\x5F", "xxxx????xxxxxxxxx???xxxxx", -58, lua_State*, const char*, size_t)
CREATE_CALLABLE_SIGNATURE(lua_pushstring, void, "\x8B\x54\x24\x08\x85\xD2\x75\x0F", "xxxxxxxx", 0, lua_State*, const char *s);

CREATE_CALLABLE_SIGNATURE(luaI_openlib, void, "\x83\xEC\x08\x53\x8B\x5C\x24\x14\x55\x8B\x6C\x24\x1C\x56", "xxxxxxxxxxxxxx", 0, lua_State*, const char*, const luaL_Reg*, int)
CREATE_CALLABLE_SIGNATURE(luaL_ref, int, "\x53\x8B\x5C\x24\x0C\x8D\x83\x00\x00\x00\x00", "xxxxxxx????", 0, lua_State*, int);
CREATE_CALLABLE_SIGNATURE(lua_rawgeti, void, "\x8B\x4C\x24\x08\x56\x8B\x74\x24\x08\x8B\xD6\xE8\x00\x00\x00\x00\x8B\x4C\x24\x10", "xxxxxxxxxxxx????xxxx", 0, lua_State*, int, int);
CREATE_CALLABLE_SIGNATURE(luaL_unref, void, "\x53\x8B\x5C\x24\x10\x85\xDB\x7C\x74", "xxxxxxxxx", 0, lua_State*, int, int);
CREATE_CALLABLE_CLASS_SIGNATURE(do_game_update, void*, "\x8B\x44\x24\x08\x56\x50\x8B\xF1\x8B\x0E", "xxxxxxxxxx", 0, int*, int*)
CREATE_CALLABLE_CLASS_SIGNATURE(luaL_newstate, int, "\x8B\x44\x24\x0C\x56\x8B\xF1\x85", "xxxxxxxx", 0, char, char, int)

CREATE_CALLABLE_SIGNATURE(lua_tonumber, double, "\x83\xF9\x03\x74\x30\x83\xF9\x04\x75\x31", "xxxxxxxxxx", -19, lua_State*, int);
CREATE_CALLABLE_SIGNATURE(lua_tointeger, ptrdiff_t, "\x83\xF9\x03\x74\x30\x83\xF9\x04\x75\x33", "xxxxxxxxxx", -19, lua_State*, int);
CREATE_CALLABLE_SIGNATURE(lua_toboolean, int, "\x8B\x4C\x24\x08\x8B\x54\x24\x04\xE8\x00\x00\x00\x00\x8B\x48\x04\x85\xC9", "xxxxxxxxx????xxxxx", 0, lua_State*, int);

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

std::list<lua_State*> activeStates;
void add_active_state(lua_State* L){
	activeStates.push_back(L);
}

void remove_active_state(lua_State* L){
	activeStates.remove(L);
}

bool check_active_state(lua_State* L){
	std::list<lua_State*>::iterator it;
	for (it = activeStates.begin(); it != activeStates.end(); it++){
		if (*it == L) {
			return true;
		}
	}
	return false;
}

void lua_newcall(lua_State* L, int args, int returns){
	int result = lua_pcall(L, args, returns, 0);
	if (result != 0) {
		size_t len;
		Logging::Log(lua_tolstring(L, -1, &len), Logging::LOGGING_ERROR);
	}
}

int luaH_getcontents(lua_State* L, bool files){
	size_t len;
	const char* dirc = lua_tolstring(L, 1, &len);
	std::string dir(dirc, len);
	std::vector<std::string> directories;

	try {
		directories = Util::GetDirectoryContents(dir, files);
	}
	catch (int e){
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
	return luaH_getcontents(L, true);
}

int luaF_getfiles(lua_State* L){
	return luaH_getcontents(L, false);
}

int luaF_directoryExists(lua_State* L){
	size_t len;
	const char* dirc = lua_tolstring(L, 1, &len);
	bool doesExist = Util::DirectoryExists(dirc);
	lua_pushboolean(L, doesExist);
	return 1;
}

int luaF_unzipfile(lua_State* L){
	size_t len;
	const char* archivePath = lua_tolstring(L, 1, &len);
	const char* extractPath = lua_tolstring(L, 2, &len);

	ZIPArchive* archive = new ZIPArchive(archivePath, extractPath);
	archive->ReadArchive();
	delete archive;
	return 0;
}

int luaF_removeDirectory(lua_State* L){
	size_t len;
	const char* directory = lua_tolstring(L, 1, &len);
	bool success = Util::RemoveEmptyDirectory(directory);
	lua_pushboolean(L, success);
	return 1;
}

int luaF_pcall(lua_State* L){
	int args = lua_gettop(L);

	int result = lua_pcall(L, args - 1, -1, 0);
	if (result == LUA_ERRRUN){
		size_t len;
		Logging::Log(lua_tolstring(L, -1, &len), Logging::LOGGING_ERROR);
		return 0;
	}
	lua_pushboolean(L, result == 0);
	lua_insert(L, 1);

	//if (result != 0) return 1;

	return lua_gettop(L);
}

int luaF_dofile(lua_State* L){

	int n = lua_gettop(L);

	size_t length = 0;
	const char* filename = lua_tolstring(L, 1, &length);
	int error = luaL_loadfile(L, filename);
	if (error == LUA_ERRSYNTAX){
		size_t len;
		Logging::Log(filename, Logging::LOGGING_ERROR);
		Logging::Log(lua_tolstring(L, -1, &len), Logging::LOGGING_ERROR);
	}
	error = lua_pcall(L, 0, 0, 0);
	if (error == LUA_ERRRUN){
		size_t len;
		Logging::Log(filename, Logging::LOGGING_ERROR);
		Logging::Log(lua_tolstring(L, -1, &len), Logging::LOGGING_ERROR);
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
	Logging::Log("Incoming HTTP Request/Request");

	int args = lua_gettop(L);
	int progressReference = 0;
	if (args >= 3){
		progressReference = luaL_ref(L, LUA_REGISTRYINDEX);
	}

	int functionReference = luaL_ref(L, LUA_REGISTRYINDEX);
	size_t len;
	const char* url_c = lua_tolstring(L, 1, &len);
	std::string url = std::string(url_c, len);

	Logging::Log(url);
	Logging::Log(std::to_string(functionReference));

	lua_http_data* ourData = new lua_http_data();
	ourData->funcRef = functionReference;
	ourData->progressRef = progressReference;
	ourData->L = L;

	HTTPReqIdent++;
	ourData->requestIdentifier = HTTPReqIdent;

	HTTPItem* reqItem = new HTTPItem();
	reqItem->call = return_lua_http;
	reqItem->data = ourData;
	reqItem->url = url;

	if (progressReference != 0){
		reqItem->progress = progress_lua_http;
	}

	HTTPManager::GetSingleton()->LaunchHTTPRequest(reqItem);
	lua_pushinteger(L, HTTPReqIdent);
	return 1;
}

CConsole* gbl_mConsole = NULL;

int luaF_createconsole(lua_State* L){
	if (gbl_mConsole) return 0;
	gbl_mConsole = new CConsole();
	return 0;
}

int luaF_destroyconsole(lua_State* L){
	if (!gbl_mConsole) return 0;
	delete gbl_mConsole;
	gbl_mConsole = NULL;
	return 0;
}

int luaF_print(lua_State* L){
	size_t len;
	const char* str = lua_tolstring(L, 1, &len);
	Logging::Log(str, Logging::LOGGING_LUA);
	return 0;
}

// bass functions
/*
static int GetDeviceCount(lua_State *L) {
	int a, count = 0;
	BASS_DEVICEINFO Info;
	for (a = 0; BASS_GetDeviceInfo(a, &Info); a++)
		if (Info.flags&BASS_DEVICE_ENABLED)
			count++;
	lua_pushinteger(L, count);
	return 1;
}
static int Flags(lua_State *L) {
	int NumFlags = lua_gettop(L);
	int FlagAccum = 0;
	printf("%d\n", NumFlags);
	for (int i = 1; i <= NumFlags; i++) {
		FlagAccum = FlagAccum | lua_tointeger(L, i);
		printf("%d %d\n", FlagAccum, lua_tointeger(L, i));
	}
	lua_pushinteger(L, FlagAccum);
	return 1;
}
static int GetSyncEventList(lua_State *L) {
	DWORD WaitResult = WaitForSingleObject(SyncMutex, INFINITE);
	lua_newtable(L); //return table
	for (int i = 0; i < NextEmptyEDLIndex; i++)
	{
		SyncEventData EventData = EventDataList[i];
		lua_pushinteger(L, i);
		lua_newtable(L); //EventData table
		M_FIELDSET(L, integer, -1, "handle", EventData.handle)
			M_FIELDSET(L, integer, -1, "channel", EventData.channel)
			M_FIELDSET(L, integer, -1, "data", EventData.data)
			lua_settable(L, -3);
	}
	NextEmptyEDLIndex = 0;
	ReleaseMutex(SyncMutex);
	return 1;
}
void CALLBACK SyncDispatcher(HSYNC handle, DWORD channel, DWORD data, void *user)
{
	DWORD WaitResult = WaitForSingleObject(SyncMutex, INFINITE);
	SyncEventData EventData = { handle, channel, data };
	EventDataList[NextEmptyEDLIndex] = EventData;
	NextEmptyEDLIndex++;
	ReleaseMutex(SyncMutex);
}
static int Init(lua_State *L) {
	lua_pushboolean(L, BASS_Init(lua_tointeger(L, 1), lua_tointeger(L, 2), lua_tointeger(L, 3), 0, 0));
	M_RETURN(L, 1)
}

static int GetDeviceInfo(lua_State *L) {
	DWORD Device = lua_tointeger(L, 1);
	BASS_DEVICEINFO Info;
	BOOL Success = BASS_GetDeviceInfo(Device, &Info);
	lua_pushboolean(L, Success);
	if (Success) {
		lua_newtable(L);
		M_FIELDSET(L, string, -2, "name", Info.name)
			M_FIELDSET(L, string, -2, "driver", Info.driver)
			M_FIELDSET(L, integer, -2, "flags", Info.flags)
	}
	else {
		lua_pushnil(L);
	}
	M_RETURN(L, 2)
}

static int Free(lua_State *L) {
	lua_pushboolean(L, BASS_Free());
	M_RETURN(L, 1)
}

static int GetDevice(lua_State *L) {
	lua_pushinteger(L, BASS_GetDevice());
	M_RETURN(L, 1)
}

static int SetDevice(lua_State *L) {
	lua_pushboolean(L, BASS_SetDevice(lua_tonumber(L, 1)));
	M_RETURN(L, 1)
}

static int GetInfo(lua_State *L) {
	BASS_INFO Info;
	BOOL Success = BASS_GetInfo(&Info);
	lua_pushboolean(L, Success);
	if (Success) {
		lua_newtable(L);
		M_FIELDSET(L, integer, -1, "flags", Info.flags)
			M_FIELDSET(L, integer, -1, "hwsize", Info.hwsize)
			M_FIELDSET(L, integer, -1, "hwfree", Info.hwfree)
			M_FIELDSET(L, integer, -1, "freesam", Info.freesam)
			M_FIELDSET(L, integer, -1, "free3d", Info.free3d)
			M_FIELDSET(L, integer, -1, "minrate", Info.minrate)
			M_FIELDSET(L, integer, -1, "maxrate", Info.maxrate)
			M_FIELDSET(L, boolean, -1, "eax", Info.eax)
			M_FIELDSET(L, integer, -1, "minbuf", Info.minbuf)
			M_FIELDSET(L, integer, -1, "dsver", Info.dsver)
			M_FIELDSET(L, integer, -1, "latency", Info.latency)
			M_FIELDSET(L, integer, -1, "initflags", Info.initflags)
			M_FIELDSET(L, integer, -1, "speakers", Info.speakers)
			M_FIELDSET(L, integer, -1, "freq", Info.freq)
	}
	else {
		lua_pushnil(L);
	}
	M_RETURN(L, 2)
}

static int GetVersion(lua_State *L) {
	lua_pushinteger(L, BASS_GetVersion());
	M_RETURN(L, 1)
}

static int GetVolume(lua_State *L) {
	lua_pushnumber(L, BASS_GetVolume());
	M_RETURN(L, 1)
}

static int Pause(lua_State *L) {
	lua_pushboolean(L, BASS_Pause());
	M_RETURN(L, 1)
}

static int SetVolume(lua_State *L) {
	lua_pushboolean(L, BASS_SetVolume(lua_tonumber(L, 1)));
	M_RETURN(L, 1)
}

static int Start(lua_State *L) {
	lua_pushboolean(L, BASS_Start());
	M_RETURN(L, 1)
}

static int Stop(lua_State *L) {
	lua_pushboolean(L, BASS_Stop());
	M_RETURN(L, 1)
}

static int Update(lua_State *L) {
	lua_pushboolean(L, BASS_Update(lua_tointeger(L, 1)));
	M_RETURN(L, 1)
}

static int GetCPU(lua_State *L) {
	lua_pushnumber(L, BASS_GetCPU());
	M_RETURN(L, 1)
}

static int StreamCreateFile(lua_State *L) {
	HSTREAM Handle = BASS_StreamCreateFile(false, lua_tostring(L, 1), lua_tointeger(L, 2), lua_tointeger(L, 3), lua_tointeger(L, 4));
	lua_pushinteger(L, Handle);
	M_RETURN(L, 1)
}

static int ChannelPlay(lua_State *L) {
	lua_pushboolean(L, BASS_ChannelPlay(lua_tointeger(L, 1), lua_toboolean(L, 2)));
	M_RETURN(L, 1)
}

static int ChannelStop(lua_State *L) {
	lua_pushboolean(L, BASS_ChannelStop(lua_tointeger(L, 1)));
	M_RETURN(L, 1)
}

static int ChannelPause(lua_State *L) {
	lua_pushboolean(L, BASS_ChannelPause(lua_tointeger(L, 1)));
	M_RETURN(L, 1)
}

static int ChannelSetSync(lua_State *L) {
	lua_pushinteger(L, BASS_ChannelSetSync(lua_tointeger(L, 1), lua_tointeger(L, 2), lua_tointeger(L, 3), &SyncDispatcher, NULL));
	M_RETURN(L, 1)
}

static int ChannelGetPosition(lua_State *L) {
	lua_pushinteger(L, BASS_ChannelGetPosition(lua_tointeger(L, 1), lua_tointeger(L, 2)));
	M_RETURN(L, 1)
}

static int ChannelGetLength(lua_State *L) {
	lua_pushinteger(L, BASS_ChannelGetLength(lua_tointeger(L, 1), lua_tointeger(L, 2)));
	M_RETURN(L, 1)
}

static int ChannelBytes2Seconds(lua_State *L) {
	lua_pushnumber(L, BASS_ChannelBytes2Seconds(lua_tointeger(L, 1), lua_tointeger(L, 2)));
	M_RETURN(L, 1)
}
*/
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
	Logging::Log(filename);
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

int luaF_Bass_ChannelPlay(lua_State *L)
{
	int handleIndex = (int) lua_tonumber(L, 1);
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
}

// lua thread
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
		HTTPManager::GetSingleton()->init_locks();
	}

	if (updates > 1){
		EventQueueM::GetSingleton()->ProcessEvents();
	}

	updates++;
	return do_game_update(thislol, a, b);
}

// Random dude who wrote what's his face?
// I 'unno, I stole this method from the guy who wrote the 'underground-light-lua-hook'
// Mine worked fine, but this seems more elegant.
int __fastcall luaL_newstate_new(void* thislol, int edx, char no, char freakin, int clue){
	int ret = luaL_newstate(thislol, no, freakin, clue);

	lua_State* L = (lua_State*)*((void**)thislol);
	if (!L) return ret;

	add_active_state(L);

	int stack_size = lua_gettop(L);

	CREATE_LUA_FUNCTION(luaF_pcall, "pcall")
	CREATE_LUA_FUNCTION(luaF_dofile, "dofile")
	CREATE_LUA_FUNCTION(luaF_dohttpreq, "dohttpreq")
	CREATE_LUA_FUNCTION(luaF_print, "log")
	CREATE_LUA_FUNCTION(luaF_unzipfile, "unzip")

	// Create console library
	luaL_Reg consoleLib[] = { { "CreateConsole", luaF_createconsole }, { "DestroyConsole", luaF_destroyconsole }, { NULL, NULL } };
	luaI_openlib(L, "console", consoleLib, 0);

	// Create file library
	luaL_Reg fileLib[] = { { "GetDirectories", luaF_getdir }, { "GetFiles", luaF_getfiles }, { "RemoveDirectory", luaF_removeDirectory }, { "DirectoryExists", luaF_directoryExists }, { NULL, NULL } };
	luaI_openlib(L, "file", fileLib, 0);

	// Create bass library
	luaL_Reg bassLib[] = {
		{ "Init", luaF_Bass_Init },
		{ "Free", luaF_Bass_Free },
		{ "ErrorGetCode", luaF_Bass_ErrorGetCode },
		{ "Flags", luaF_Bass_Flags },
		{ "GetVolume", luaF_Bass_GetVolume },
		{ "SetVolume", luaF_Bass_SetVolume },
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
		{ "ChannelGetData", luaF_Bass_ChannelGetData },
		{ NULL, NULL }
	};
	luaI_openlib(L, "Bass", bassLib, 0);

	// Add enumerators to bass library
	Bass_CreateBassEnums(L);

	// Load lua hook
	int result;
	Logging::Log("Initiating Hook");
	
	result = luaL_loadfile(L, "mods/base/base.lua");
	if (result == LUA_ERRSYNTAX){
		size_t len;
		Logging::Log(lua_tolstring(L, -1, &len), Logging::LOGGING_ERROR);
		return ret;
	}
	result = lua_pcall(L, 0, 1, 0);
	if (result == LUA_ERRRUN){
		size_t len;
		Logging::Log(lua_tolstring(L, -1, &len), Logging::LOGGING_ERROR);
		return ret;
	}

	lua_settop(L, stack_size);
	return ret;
}

void luaF_close(lua_State* L){
	remove_active_state(L);
	lua_close(L);
}


static HTTPManager mainManager;

void InitiateStates(){

	main_thread_id = std::this_thread::get_id();

	SignatureSearch::Search();


	FuncDetour* gameUpdateDetour = new FuncDetour((void**)&do_game_update, do_game_update_new);
	FuncDetour* newStateDetour = new FuncDetour((void**)&luaL_newstate, luaL_newstate_new);
	FuncDetour* luaCallDetour = new FuncDetour((void**)&lua_call, lua_newcall);
	FuncDetour* luaCloseDetour = new FuncDetour((void**)&lua_close, luaF_close);
	
	new EventQueueM();
}

void DestroyStates(){
	// Okay... let's not do that.
	// I don't want to keep this in memory, but it CRASHES THE SHIT OUT if you delete this after all is said and done.
	// if (gbl_mConsole) delete gbl_mConsole;
}
