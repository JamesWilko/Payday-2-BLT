#pragma once

#ifndef __cplusplus_cli
#define LUA_MULTRET (-1)
#define LUA_REGISTRYINDEX (-10000)
#define LUA_ENVIRONINDEX (-10001)
#define LUA_GLOBALSINDEX (-10002)
#define lua_upvalueindex(i)   (LUA_GLOBALSINDEX-(i))
#define lua_pushcfunction(L, f)	lua_pushcclosure(L, f, 0)
#define lua_tostring(L, i)		lua_tolstring(L, i, 0)
#define lua_getglobal(L, f)		lua_getfield(L, LUA_GLOBALSINDEX, f)
#define lua_setglobal(L, f)		lua_setfield(L, LUA_GLOBALSINDEX, f)
#define luaL_register(L, i, b)	luaL_openlib(L, i, b, 0)
#define lua_pop(L,n)			lua_settop(L, -(n)-1)
#define LUA_VERSION_NUM			503
#define LUA_COMPAT_MODULE		1
#define LUAL_BUFFERSIZE			BUFSIZ
#define LUA_TTRUE				1
#define LUA_TFALSE				0
#endif
#define LUA_IDSIZE				60

enum GC_OPS {
	LUA_GCSTOP = 0,
	LUA_GCRESTART,
	LUA_GCCOLLECT,
	LUA_GCCOUNT,
	LUA_GCCOUNTB,
	LUA_GCSTEP,
	LUA_GCSETPAUSE,
	LUA_GCSETSTEPMUL,
};

#define lua_isboolean(a,b)			(lua_type(a,b) == LUA_TBOOLEAN)
#define lua_isfunction(a,b)			(lua_type(a,b) == LUA_TFUNCTION)
#define lua_islightuserdata(a,b)	(lua_type(a,b) == LUA_TLIGHTUSERDATA)
#define lua_isnil(a,b)				(lua_type(a,b) == LUA_TNIL)
#define lua_isnone(a,b)				(lua_type(a,b) == LUA_TNONE)
#define lua_isnoneornil(a,b)		(lua_type(a,b) <= LUA_TNIL)
#define lua_isstring(a,b)			(lua_type(a,b) == LUA_TSTRING)
#define lua_istable(a,b)			(lua_type(a,b) == LUA_TTABLE)
#define lua_isthread(a,b)			(lua_type(a,b) == LUA_TTHREAD)
#define lua_isuserdata(a,b)			(lua_type(a,b) == LUA_TUSERDATA)

#define lua_pushfloat		lua_pushnumber
#define lua_newtable(L)		lua_createtable(L,0,0)

#define luaL_argcheck(L, cond,numarg,extramsg)	\
		((void)((cond) || luaL_argerror(L, (numarg), (extramsg))))
#define luaL_addchar(L, c)		luaL_addlstring(L, &(c), 1);
#define luaL_addstring(L,str)	luaL_addlstring(L, str, NULL)
#define luaL_checkint			luaL_checkinteger
#define luaL_checkstring(L,n)	luaL_checklstring(L, (n), NULL)
#define luaL_optstring(L,n,d)	luaL_optlstring(L, (n), (d), NULL)
#define luaL_getmetatable(L,n)  lua_getfield(L, LUA_REGISTRYINDEX, (n))
#define luaL_typename(L,i)		lua_typename(L, lua_type(L, (i)))
#define lua_pushliteral(L, s)	lua_pushlstring(L, "" s, (sizeof(s)/sizeof(char))-1)

enum LuaTypes {
	LUA_TNONE = -1,
	LUA_TNIL,
	LUA_TBOOLEAN,
	LUA_TLIGHTUSERDATA,
	LUA_TNUMBER,
	LUA_TSTRING,
	LUA_TTABLE,
	LUA_TFUNCTION,
	LUA_TUSERDATA,
	LUA_TTHREAD,
};

enum HookCallbackType
{
	NEWSTATE_CALLBACK,
	GAMETICK_CALLBACK,
	REQUIRE_CALLBACK,
	CLOSESTATE_CALLBACK,

	CALLBACK_ENUM_END,
};

#ifndef WINAPI
#define WINAPI __stdcall
#endif

typedef void lua_State;
typedef lua_State* pLua_State;
typedef int(*lua_CFunction) (lua_State *L);
typedef struct luaL_Buffer {
	lua_State *L;
	lua_State *tL;
	int ref;
} luaL_Buffer;
typedef struct lua_Debug {
	int event;
	const char *name;           /* (n) */
	const char *namewhat;       /* (n) */
	const char *what;           /* (S) */
	const char *source;         /* (S) */
	int currentline;            /* (l) */
	int nups;                   /* (u) number of upvalues */
	int linedefined;            /* (S) */
	int lastlinedefined;        /* (S) */
	char short_src[LUA_IDSIZE]; /* (S) */
	/* private part */
	LPVOID priv;
} lua_Debug;
typedef const char*(*lua_Reader) (lua_State *L, void *data, size_t *size);
typedef int(*lua_Writer) (lua_State *L, const void* p, size_t sz, void* ud);
typedef void(WINAPI* CallbackFunction)(lua_State*, LPCSTR, LPVOID);
typedef LPVOID(*lua_Alloc)(LPVOID, LPVOID, size_t, size_t);
typedef void(*lua_Hook)(lua_State *L, lua_Debug *ar);
typedef float lua_Number;

struct CallbackContainer {
	CallbackFunction func;
	LPVOID ptr;
	std::string name;
};
#ifndef luaL_reg
typedef struct luaL_Reg {
	const char *name;
	lua_CFunction func;
} luaL_Reg;
#define luaL_reg luaL_Reg
#endif

#ifndef PAYDAY2HOOK_EXPORTS
#ifdef __cplusplus
#define EXPORT	extern "C"
#else
#define EXPORT
#endif
#pragma comment(lib, "PD2HookAPI.lib")

#else

#define EXPORT		__declspec(dllexport) static
#define ELc(x)		((##x##_t) LuaHooking::Lua[PD2Functions::##x##].lpAddr)

class HookAPI
{
public:
#endif
	EXPORT void RegisterCallback(const HookCallbackType, CallbackFunction, LPVOID = NULL);
	EXPORT BOOL RegisterCallbackEx(const HookCallbackType, CallbackFunction, LPVOID = NULL, LPCSTR = NULL);
	EXPORT BOOL UnregisterCallback(const HookCallbackType, LPCSTR);
	EXPORT BOOL RegisterPrioPostReq(CallbackFunction, LPVOID = NULL, LPCSTR = NULL, int = 0);
	EXPORT BOOL UnregisterPrioPostReq(LPCSTR);
	EXPORT const char* lua_pushfstring(lua_State *L, const char* fmt, ...);
	EXPORT const char* lua_pushvfstring(lua_State *L, const char* fmt, va_list argp);
	EXPORT const char* lua_tolstring(lua_State *L, int idx, size_t *len);
	EXPORT const char* lua_typename(lua_State *L, int idx);
	EXPORT const char* luaL_checklstring(lua_State *L, int narg, size_t *l);
	EXPORT const char* luaL_optlstring(lua_State *L, int narg, const char *d, size_t *l);
	EXPORT const char* lua_getlocal(lua_State *L, lua_Debug *ar, int n);
	EXPORT const char* lua_setlocal(lua_State *L, lua_Debug *ar, int n);
	EXPORT const char* lua_getupvalue(lua_State *L, int funcindex, int n);
	EXPORT const char* lua_setupvalue(lua_State *L, int funcindex, int n);
	EXPORT int lua_checkstack(lua_State *L, int extra);
	EXPORT int lua_dump(lua_State *L, lua_Writer w, void* data);
	EXPORT int lua_equal(lua_State *L, int idx1, int idx2);
	EXPORT int lua_error(lua_State *L);
	EXPORT int lua_gc(lua_State *L, int what, int data);
	EXPORT int lua_gethookcount(lua_State *L);
	EXPORT int lua_gethookmask(lua_State *L);
	EXPORT int lua_getinfo(lua_State *L, const char *what, lua_Debug *ar);
	EXPORT int lua_getmetatable(lua_State *L, int idx);
	EXPORT int lua_getstack(lua_State *L, int level, lua_Debug *ar);
	EXPORT int lua_gettop(lua_State *L);
	EXPORT int lua_iscfunction(lua_State *L, int idx);
	EXPORT int lua_isnumber(lua_State *L, int idx);
	EXPORT int lua_lessthan(lua_State *L, int idx1, int idx2);
	EXPORT int lua_load(lua_State *L, lua_Reader reader, void* data, const char *chunkname);
	EXPORT int luaL_loadbuffer(lua_State *L, const char *chunk, size_t len, const char *chunkname);
	EXPORT int lua_next(lua_State *L, int idx);
	EXPORT int lua_cpcall(lua_State *L, lua_CFunction func, void* ud);
	EXPORT int lua_pcall(lua_State *L, int nargs, int nresults, int errfunc);
	EXPORT int lua_pushthread(lua_State *L);
	EXPORT int lua_rawequal(lua_State *L, int idx1, int idx2);
	EXPORT int lua_resume(lua_State *L, int narg);
	EXPORT int lua_sethook(lua_State *L, lua_Hook *f, int mask, int count);
	EXPORT int lua_setmetatable(lua_State *L, int idx);
	EXPORT int lua_toboolean(lua_State *L, int idx);
	EXPORT int lua_tointeger(lua_State *L, int idx);
	EXPORT int lua_type(lua_State *L, int idx);
	EXPORT int lua_yield(lua_State *L, int nresults);
	EXPORT int luaL_argerror(lua_State *L, int narg, const char *extramsg);
	EXPORT int luaL_checkinteger(lua_State *L, int narg);
	EXPORT int luaL_checkoption(lua_State *L, int narg, const char *def, const char *const lst[]);
	EXPORT int luaL_error(lua_State *L, const char *fmt, ...);
	EXPORT int luaL_loadfile(lua_State *L, const char *filename);
	EXPORT int luaL_loadstring(lua_State *L, const char *s);
	EXPORT int luaL_newmetatable(lua_State *L, const char *name);
	EXPORT lua_State* luaL_newstate();
	EXPORT int luaL_optinteger(lua_State *L, int narg, int d);
	EXPORT int luaL_ref(lua_State *L, int t);
	EXPORT int luaL_typerror(lua_State *L, int narg, const char *tname);
	EXPORT lua_Number lua_tonumber(lua_State *L, int idx);
	EXPORT lua_Number luaL_checknumber(lua_State *L, int narg);
	EXPORT lua_Number luaL_optnumber(lua_State *L, int narg, lua_Number d);
	EXPORT lua_State* lua_newthread(lua_State *L);
	EXPORT lua_State* lua_tothread(lua_State *L, int idx);
	EXPORT size_t lua_objlen(lua_State *L, int idx);
	EXPORT void lua_close(lua_State *L);
	EXPORT void lua_call(lua_State *L, int nargs, int nresults);
	EXPORT void lua_concat(lua_State *L, int n);
	EXPORT void lua_createtable(lua_State *L, int narr, int nrec);
	EXPORT void lua_getfield(lua_State *L, int idx, const char *k);
	EXPORT void lua_gettable(lua_State *L, int idx);
	EXPORT void lua_insert(lua_State *L, int idx);
	EXPORT lua_State* lua_newstate(lua_Alloc *f, LPVOID ud);
	EXPORT void lua_pushboolean(lua_State *L, int b);
	EXPORT void lua_pushcclosure(lua_State *L, lua_CFunction fn, int n);
	EXPORT void lua_pushnumber(lua_State *L, lua_Number n);
	EXPORT void lua_pushinteger(lua_State *L, int n);
	EXPORT void lua_pushlightuserdata(lua_State *L, void *ptr);
	EXPORT void lua_pushlstring(lua_State *L, const char *s, size_t len);
	EXPORT void lua_pushnil(lua_State *L);
	EXPORT void lua_pushstring(lua_State *L, const char* str);
	EXPORT void lua_pushvalue(lua_State *L, int idx);
	EXPORT void lua_rawget(lua_State *L, int idx);
	EXPORT void lua_rawgeti(lua_State *L, int idx, int n);
	EXPORT void lua_rawset(lua_State *L, int idx);
	EXPORT void lua_rawseti(lua_State *L, int idx, int n);
	EXPORT void lua_remove(lua_State *L, int idx);
	EXPORT void lua_replace(lua_State *L, int idx);
	EXPORT void lua_setfield(lua_State *L, int idx, const char *k);
	EXPORT void lua_settable(lua_State *L, int idx);
	EXPORT void lua_settop(lua_State *L, int idx);
	EXPORT void lua_xmove(lua_State *from, lua_State *to, int n);
	EXPORT void luaL_addlstring(luaL_Buffer *B, const char *s, size_t len);
	EXPORT void luaL_buffinit(lua_State *L, luaL_Buffer *B);
	EXPORT void luaL_checkstack(lua_State *L, int space, const char *mes);
	EXPORT void luaL_openlib(lua_State *L, const char *libname, const luaL_reg *l, int nup);
	EXPORT void luaL_pushresult(luaL_Buffer *B);
	EXPORT void luaL_unref(lua_State *L, int t, int ref);
	EXPORT void luaL_where(lua_State *L, int lvl);
	EXPORT void* lua_newuserdata(lua_State *L, size_t size);
	EXPORT void* lua_tocfunction(lua_State *L, int idx);
	EXPORT void* lua_topointer(lua_State *L, int idx);
	EXPORT void* lua_touserdata(lua_State *L, int idx);
	EXPORT void* luaL_checkudata(lua_State *L, int idx, const char *tname);
#ifdef PAYDAY2HOOK_EXPORTS
	typedef vector<pair<int, CallbackContainer>> PostReqVec;
	static void RunCallbacks(lua_State *L, LPCSTR op, HookCallbackType type);
	static void RunPrioFunc(function<void(PostReqVec&)> f);
private:
	static ThreadLock objLock;
	static array<vector<CallbackContainer>, CALLBACK_ENUM_END> Callbacks;
	static PostReqVec PrioPostReqs;
};
#endif
