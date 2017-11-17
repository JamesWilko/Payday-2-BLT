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
#include <fstream>

// Code taken from LuaJIT 2.1.0-beta2
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

	// From src/luaconf.h
#define LUA_NUMBER		double

	// From src/lua.h
	// type of numbers in Lua
	typedef LUA_NUMBER lua_Number;
	typedef struct lua_Debug lua_Debug;	// activation record
	// Functions to be called by the debuger in specific events
	typedef void(*lua_Hook) (lua_State* L, lua_Debug* ar);

	CREATE_NORMAL_CALLABLE_SIGNATURE(lua_call, void, "\x8B\x44\x24\x08\x8B\x54\x24\x04\xFF\x44\x24\x0C\x8D\x0C\xC5\x00", "xxxxxxxxxxxxxxxx", 0, lua_State*, int, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_pcall, int, "\x8B\x54\x24\x04\x8B\x4C\x24\x10\x53\x56\x8B\x72\x08\x8A", "xxxxxxxxxxxxxx", 0, lua_State*, int, int, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_gettop, int, "\x8B\x4C\x24\x04\x8B\x41\x14\x2B\x41\x10\xC1\xF8\x03\xC3", "xxxxxxxxxxxxxx", 0, lua_State*)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_settop, void, "\x8B\x44\x24\x08\x85\xC0\x78\x5B\x53\x56\x8B\x74\x24\x0C\x57\x8B", "xxxxxxxxxxxxxxxx", 0, lua_State*, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_toboolean, int, "\xFF\x74\x24\x08\xFF\x74\x24\x08\xE8\x00\x00\x00\x00\x83\xC4\x08\x83\x78\x04\xFE\x1B\xC0\xF7\xD8\xC3", "xxxxxxxxx????xxxxxxxxxxxx", 0, lua_State*, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_tointeger, ptrdiff_t, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x08\xFF\x75\x0C\xFF\x75\x08\xE8\x00\x00\x00\x00\x8B\x48\x04\x83\xC4\x08\x83\xF9\xF2\x73\x0C\xF2\x0F\x10\x00\xF2\x0F\x2C\xC0\x8B\xE5\x5D\xC3\x83\xF9\xFB\x75\x26", "xxxxxxxxxxxxxxxx????xxxxxxxxxxxxxxxxxxxxxxxxxxxx", 0, lua_State*, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_tonumber, lua_Number, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x08\xFF\x75\x0C\xFF\x75\x08\xE8\x00\x00\x00\x00\x8B\x48\x04\x83\xC4\x08\x83\xF9\xF2\x77\x06\xDD", "xxxxxxxxxxxxxxxx????xxxxxxxxxxxx", 0, lua_State*, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_tolstring, const char*, "\x83\xEC\x24\xA1\x00\x00\x00\x00\x33\xC4\x89\x44\x24\x20\x53\x8B\x5C\x24\x2C\x56\x8B\x74\x24\x34", "xxxx????xxxxxxxxxxxxxxxx", 0, lua_State*, int, size_t*)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_objlen, size_t, "\x83\xEC\x24\xA1\x00\x00\x00\x00\x33\xC4\x89\x44\x24\x20\x8B\x44", "xxxx????xxxxxxxx", 0, lua_State*, int)
		// This is actually luaL_loadfilex() (as per Lua 5.2) now. The new parameter corresponds to mode, and specifying NULL causes Lua
		// to default to "bt", i.e. 'binary and text'
		// https://www.lua.org/manual/5.2/manual.html#luaL_loadfilex
		// https://www.lua.org/manual/5.2/manual.html#pdf-load
		CREATE_NORMAL_CALLABLE_SIGNATURE(luaL_loadfilex, int, "\x81\xEC\x08\x02\x00\x00\xA1\x00\x00\x00\x00\x33\xC4\x89\x84\x24", "xxxxxxx????xxxxx", 0, lua_State*, const char*, const char*)
		CREATE_NORMAL_CALLABLE_SIGNATURE(luaL_loadstring, int, "\x8B\x54\x24\x08\x83\xEC\x08\x8B\xC2\x56\x8D\x70\x01\x8D\x49\x00", "xxxxxxxxxxxxxxxx", 0, lua_State*, const char*)
		//CREATE_CALLABLE_SIGNATURE(lua_load, int, "\x8B\x4C\x24\x10\x33\xD2\x83\xEC\x18\x3B\xCA", "xxxxxxxxxxx", 0, lua_State*, lua_Reader, void*, const char*)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_getfield, void, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x08\x56\x8B\x75\x08\x57\xFF\x75", "xxxxxxxxxxxxxxxx", 0, lua_State*, int, const char*)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_setfield, void, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x10\x56\x8B\x75\x08\x57\xFF\x75", "xxxxxxxxxxxxxxxx", 0, lua_State*, int, const char*)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_createtable, void, "\x56\x8B\x74\x24\x08\x8B\x4E\x08\x8B\x41\x14\x3B\x41\x18\x72\x07\x8B\xCE\xE8\x00\x00\x00\x00\x8B\x44\x24\x10\x85\xC0\x74\x12\x83", "xxxxxxxxxxxxxxxxxxx????xxxxxxxxx", 0, lua_State*, int, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_insert, void, "\x8B\x4C\x24\x08\x56\x57\x85\xC9\x7E\x21\x8B\x54\x24\x0C\x8D\x71", "xxxxxxxxxxxxxxxx", 0, lua_State*, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_replace, void, "\x56\x57\x8B\x7C\x24\x10\x81\xFF\xEE\xD8\xFF\xFF\x75\x16\x8B\x4C\x24\x0C\x5F\x8B\x41\x14\x8D\x71\x14\x8B\x40\xF8\x83\x06\xF8\x89", "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", 0, lua_State*, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_remove, void, "\x8B\x4C\x24\x08\x56\x57\x85\xC9\x7E\x21\x8B\x7C\x24\x0C\x8B\x47\x10\x8B\x57\x14\x8D\x77\x14\x8D\x04\xC8\x83\xC0\xF8\x3B\xC2\x72", "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", 0, lua_State*, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_newstate, lua_State*, "\x53\x8B\x5C\x24\x0C\x55\x8B\x6C\x24\x0C\x56\x57\x68\x40\x10\x00\x00\x6A\x00\x6A\x00\x53\xFF\xD5\x8B\xF0\x83\xC4\x10\x8D\x7E\x30", "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", 0, lua_Alloc, void*)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_close, void, "\x8B\x44\x24\x04\x53\x56\x57\x8B\x78\x08\x8B\x77\x74\x56\xE8", "xxxxxxxxxxxxxxx", 0, lua_State*)

		//CREATE_CALLABLE_SIGNATURE(lua_rawset, void, "\x51\x53\x55\x56\x57\x8B\xF1\xE8\x00\x00\x00\x00", "xxxxxxxx????", 0, lua_State*, int)
		// Reviving lua_settable() since the function exists again, and because the Crimefest 2015 alternative relied upon internal Lua
		// VM functions, which do not apply to LuaJIT
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_settable, void, "\x56\xFF\x74\x24\x0C\x8B\x74\x24\x0C\x56\xE8\x00\x00\x00\x00\x8B\x4E\x14\x83\xE9\x10\x51\x50\x56", "xxxxxxxxxxx????xxxxxxxxx", 0, lua_State*, int)

		//CREATE_CALLABLE_SIGNATURE(lua_pushnumber, void, "\x8B\x44\x24\x04\x8B\x48\x08\xF3\x0F\x10\x44\x24\x08", "xxxxxxxxxxxxx", 0, lua_State*, double)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_pushinteger, void, "\x66\x0F\x6E\x44\x24\x08\x8B\x4C\x24\x04\xF3\x0F\xE6\xC0\x8B\x41", "xxxxxxxxxxxxxxxx", 0, lua_State*, ptrdiff_t)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_pushboolean, void, "\x8B\x4C\x24\x04\x33\xC0\x39\x44\x24\x08\xBA\xFE\xFF\xFF\xFF\x0F", "xxxxxxxxxxxxxxxx", 0, lua_State*, bool)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_pushcclosure, void, "\x56\x8B\x74\x24\x08\x8B\x4E\x08\x8B\x41\x14\x3B\x41\x18\x72\x07\x8B\xCE\xE8\x00\x00\x00\x00\x8B\x46\x10\x8B\x40\xF8\x80\x78\x05", "xxxxxxxxxxxxxxxxxxx????xxxxxxxxx", 0, lua_State*, lua_CFunction, int);
	// lua_pushstring()'s signature was found before lua_pushlstring()'s, so I'm leaving it here now since it's valid anyway
	// It was used as a quick and dirty - and broken - workaround since most lua_pushlstring() calls are inlined, but it ended up
	// breaking HTTP downloads of zip archives due to its sensitivity to premature null characters. A non-inlined signature for
	// lua_pushlstring() was found by cross-referencing the string 'loaders' to lj_cf_package_require(), which is part of LuaJIT
	CREATE_NORMAL_CALLABLE_SIGNATURE(lua_pushlstring, void, "\x56\x8B\x74\x24\x08\x8B\x4E\x08\x8B\x41\x14\x3B\x41\x18\x72\x07\x8B\xCE\xE8\x00\x00\x00\x00\xFF", "xxxxxxxxxxxxxxxxxxx????x", 0, lua_State*, const char*, size_t)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_pushstring, void, "\x56\x8B\x74\x24\x08\x57\x8B\x7C\x24\x10\x85\xFF\x75\x0C\x8B\x46", "xxxxxxxxxxxxxxxx", 0, lua_State*, const char*)
		CREATE_NORMAL_CALLABLE_SIGNATURE(lua_checkstack, int, "\x8B\x54\x24\x08\x81\xFA\x40\x1F\x00\x00\x7F\x38\x8B\x4C\x24\x04", "xxxxxxxxxxxxxxxx", 0, lua_State*, int)

		// luaI_openlib() is really luaL_openlib(), see lauxlib.h in Lua 5.1's source code
		CREATE_NORMAL_CALLABLE_SIGNATURE(luaI_openlib, void, "\x55\x8B\xEC\x83\xE4\xF8\xF2\x0F\x10\x00\x00\x00\x00\x00\x83\xEC\x08\x56\x8B\x75\x08\x57\x8B\x46\x14\xF2\x0F", "xxxxxxxxx?????xxxxxxxxxxxxx", 0, lua_State*, const char*, const luaL_Reg*, int)
		CREATE_NORMAL_CALLABLE_SIGNATURE(luaL_ref, int, "\x55\x8B\xEC\x83\xE4\xF8\x8B\x55\x0C\x83\xEC\x08\x8D\x82\x0F\x27\x00\x00\x56\x8B\x75\x08\x57", "xxxxxxxxxxxxxxxx", 0, lua_State*, int);
	// Reviving lua_rawgeti() since the function exists again, and because the Crimefest 2015 alternative relied upon internal Lua VM
	// functions, which do not apply to LuaJIT
	CREATE_NORMAL_CALLABLE_SIGNATURE(lua_rawgeti, void, "\x55\x8B\xEC\x83\xE4\xF8\x83\xEC\x0C\x56\xFF\x75\x0C\x8B\x75\x08\x56\xE8", "xxxxxxxxxxxxxxxxxx", 0, lua_State*, int, int);
	CREATE_NORMAL_CALLABLE_SIGNATURE(lua_rawseti, void, "\x53\x56\x8B\x74\x24\x0C\x57\xFF\x74\x24\x14\x56\xE8\x00\x00\x00\x00\x8B\x38\x8B", "xxxxxxxxxxxxx????xxx", 0, lua_State*, int, int);
	CREATE_NORMAL_CALLABLE_SIGNATURE(lua_type, int, "\x56\xFF\x74\x24\x0C\x8B\x74\x24\x0C\x56\xE8\x00\x00\x00\x00\x8B\xD0\x83\xC4\x08\x8B\x4A\x04\x83\xF9\xF2\x77\x07\xB8\x03\x00\x00\x00\x5E\xC3\x8B\x46\x08\x05\x90", "xxxxxxxxxxx????xxxxxxxxxxxxxxxxxxxxxxxxx", 0, lua_State*, int);
	CREATE_NORMAL_CALLABLE_SIGNATURE(lua_typename, const char*, "\x8B\x44\x24\x08\x8B\x00\x00\x00\x00\x00\x00\xC3\xCC", "xxxxx??????xx", 0, lua_State*, int);
	CREATE_NORMAL_CALLABLE_SIGNATURE(luaL_unref, void, "\x53\x8B\x5C\x24\x10\x85\xDB\x78\x67\x56\x8B\x74\x24\x0C\x57\x8B", "xxxxxxxxxxxxxxxx", 0, lua_State*, int, int);
	CREATE_CALLABLE_CLASS_SIGNATURE(do_game_update, void*, "\x56\xFF\x74\x24\x0C\x8B\xF1\x68\x00\x00\x00\x00\xFF\x36\xE8", "xxxxxxxx????xxx", 0, int*, int*)
		CREATE_CALLABLE_CLASS_SIGNATURE(luaL_newstate, int, "\x53\x56\x33\xDB\x57\x8B\xF1\x39\x5C\x24\x18\x0F", "xxxxxxxxxxxx", 0, char, char, int)
		CREATE_CALLABLE_CLASS_SIGNATURE(luaL_newstate_vr, int, "\x8B\x44\x24\x0C\x56\x8B\xF1\x85\xC0\x75\x08\x50\x68\x70\x86\x7E", "xxxxxxxxxxxxxxxx", 0, char, char, int)

		// lua c-functions

		// From src/lua.h
		// Pseudo-indices
#define LUA_REGISTRYINDEX	(-10000)
#define LUA_ENVIRONINDEX	(-10001)
#define LUA_GLOBALSINDEX	(-10002)
#define lua_upvalueindex(i)	(LUA_GLOBALSINDEX-(i))

		// From src/lauxlib.h
#define LUA_NOREF       (-2)
#define LUA_REFNIL      (-1)

		// more bloody lua shit
		// Thread status; 0 is OK
#define LUA_YIELD	1
#define LUA_ERRRUN	2
#define LUA_ERRSYNTAX	3
#define LUA_ERRMEM	4
#define LUA_ERRERR	5
		// From src/lauxlib.h
		// Extra error code for 'luaL_load'
#define LUA_ERRFILE     (LUA_ERRERR+1)

		// From src/lua.h
		// Option for multiple returns in 'lua_pcall' and 'lua_call'
#define LUA_MULTRET	(-1)
#define LUA_TNONE		(-1)
#define LUA_TNIL		0
#define LUA_TBOOLEAN		1
#define LUA_TLIGHTUSERDATA	2
#define LUA_TNUMBER		3
#define LUA_TSTRING		4
#define LUA_TTABLE		5
#define LUA_TFUNCTION		6
#define LUA_TUSERDATA		7
#define LUA_TTHREAD		8

#define lua_pop(L,n)		lua_settop(L, -(n)-1)
#define lua_isfunction(L,n)	(lua_type(L, (n)) == LUA_TFUNCTION)
#define lua_istable(L,n)	(lua_type(L, (n)) == LUA_TTABLE)
#define lua_islightuserdata(L,n)	(lua_type(L, (n)) == LUA_TLIGHTUSERDATA)
#define lua_isnil(L,n)		(lua_type(L, (n)) == LUA_TNIL)
#define lua_isboolean(L,n)	(lua_type(L, (n)) == LUA_TBOOLEAN)
#define lua_isthread(L,n)	(lua_type(L, (n)) == LUA_TTHREAD)
#define lua_isnone(L,n)		(lua_type(L, (n)) == LUA_TNONE)
#define lua_isnoneornil(L, n)	(lua_type(L, (n)) <= 0)
#define lua_getglobal(L,s)	lua_getfield(L, LUA_GLOBALSINDEX, (s))
#define lua_setglobal(L,s)	lua_setfield(L, LUA_GLOBALSINDEX, (s))
#define lua_tostring(L,i)	lua_tolstring(L, (i), NULL)

	std::list<lua_State*> activeStates;
	void add_active_state(lua_State* L)
	{
		activeStates.push_back(L);
	}

	void remove_active_state(lua_State* L)
	{
		activeStates.remove(L);
	}

	bool check_active_state(lua_State* L)
	{
		std::list<lua_State*>::iterator it;
		for (it = activeStates.begin(); it != activeStates.end(); it++)
		{
			if (*it == L)
			{
				return true;
			}
		}
		return false;
	}

	FuncDetour* luaCallDetour = nullptr;

	// Not tracking the error count here so it can automatically be reset to 0 whenever the Lua state is deleted and re-created (e.g.
	// when transitioning to / from the menu to a level)
	void NotifyErrorOverlay(lua_State* L, const char* message)
	{
		lua_getglobal(L, "NotifyErrorOverlay");
		if (lua_isfunction(L, -1))
		{
			int args = 0;
			if (message)
			{
				lua_pushstring(L, message);
				args = 1;
			}
			int error = lua_pcall(L, args, 0, 0);
			if (error == LUA_ERRRUN)
			{
				// Don't bother logging the error since the error overlay is designed to be an optional component, just pop the error
				// message off the stack to keep it balanced
				lua_pop(L, 1);
				return;
			}
		}
		else
		{
			lua_pop(L, 1);
			static bool printed = false;
			if (!printed)
			{
				printf("Warning: Failed to find the NotifyErrorOverlay function in the Lua environment; no in-game notifications will be displayed for caught errors\n");
				printed = true;
			}
		}
	}

	void lua_newcall(lua_State* L, int args, int returns)
	{
		// https://stackoverflow.com/questions/30021904/lua-set-default-error-handler/30022216#30022216
		lua_getglobal(L, "debug");
		lua_getfield(L, -1, "traceback");
		lua_remove(L, -2);
		// Do not index from the top (i.e. use a negative index) as this has the potential to mess up if the callee function returns
		// values /and/ lua_pcall() is set up with a > 0 nresults argument
		int errorhandler = lua_gettop(L) - args - 1;
		lua_insert(L, errorhandler);

		int result = lua_pcall(L, args, returns, errorhandler);
		if (result != 0)
		{
			size_t len;
			const char* message = lua_tolstring(L, -1, &len);
			PD2HOOK_LOG_ERROR(message);
			NotifyErrorOverlay(L, message);
			// This call pops the error message off the stack
			lua_pop(L, 1);
			// No, don't touch this variable anymore
			message = nullptr;
		}
		// This call removes the error handler from the stack. Do not use lua_pop() as the callee function's return values may be
		// present, which would pop one of those instead and leave the error handler on the stack
		lua_remove(L, errorhandler);
	}

	int luaF_ispcallforced(lua_State* L)
	{
		lua_pushboolean(L, luaCallDetour ? true : false);
		return 1;
	}

	int luaF_forcepcalls(lua_State* L)
	{
		int args = lua_gettop(L);	// Number of arguments
		if (args < 1)
		{
			PD2HOOK_LOG_WARN("blt.forcepcalls(): Called with no arguments, ignoring request");
			return 0;
		}

		if (lua_toboolean(L, 1))
		{
			if (!luaCallDetour)
			{
				luaCallDetour = new FuncDetour((void**)&lua_call, lua_newcall);
				PD2HOOK_LOG_LOG("blt.forcepcalls(): Protected calls will now be forced");
			}
			//		else Logging::Log("blt.forcepcalls(): Protected calls are already being forced, ignoring request", Logging::LOGGING_WARN);
		}
		else
		{
			if (luaCallDetour)
			{
				delete luaCallDetour;
				luaCallDetour = nullptr;
				PD2HOOK_LOG_LOG("blt.forcepcalls(): Protected calls are no longer being forced");
			}
			//		else Logging::Log("blt.forcepcalls(): Protected calls are not currently being forced, ignoring request", Logging::LOGGING_WARN);
		}
		return 0;
	}

	bool vrMode = false;

	int luaF_getvrstate(lua_State* L) {
		lua_pushboolean(L, vrMode);
		return 1;
	}

	int luaH_getcontents(lua_State* L, bool files)
	{
		size_t len;
		const char* dirc = lua_tolstring(L, 1, &len);
		std::string dir(dirc, len);
		std::vector<std::string> directories;

		try
		{
			directories = Util::GetDirectoryContents(dir, files);
		}
		catch (int unused)
		{
			// Okay, okay - now shut up, compiler
			(void)unused;
			lua_pushboolean(L, false);
			return 1;
		}

		lua_createtable(L, 0, 0);

		std::vector<std::string>::iterator it;
		int index = 1;
		for (it = directories.begin(); it < directories.end(); it++)
		{
			if (*it == "." || *it == "..") continue;
			lua_pushinteger(L, index);
			lua_pushlstring(L, it->c_str(), it->length());
			lua_settable(L, -3);
			index++;
		}

		return 1;
	}

	int luaF_getdir(lua_State* L)
	{
		return luaH_getcontents(L, true);
	}

	int luaF_getfiles(lua_State* L)
	{
		return luaH_getcontents(L, false);
	}

	int luaF_directoryExists(lua_State* L)
	{
		size_t len;
		const char* dirc = lua_tolstring(L, 1, &len);
		bool doesExist = Util::DirectoryExists(dirc);
		lua_pushboolean(L, doesExist);
		return 1;
	}

	int luaF_unzipfile(lua_State* L)
	{
		size_t len;
		const char* archivePath = lua_tolstring(L, 1, &len);
		const char* extractPath = lua_tolstring(L, 2, &len);

		pd2hook::ExtractZIPArchive(archivePath, extractPath);
		return 0;
	}

	int luaF_removeDirectory(lua_State* L)
	{
		size_t len;
		const char* directory = lua_tolstring(L, 1, &len);
		bool success = Util::RemoveEmptyDirectory(directory);
		lua_pushboolean(L, success);
		return 1;
	}

	int luaF_pcall(lua_State* L)
	{
		int args = lua_gettop(L) - 1;

		lua_getglobal(L, "debug");
		lua_getfield(L, -1, "traceback");
		lua_remove(L, -2);
		// Do not index from the top (i.e. don't use a negative index)
		int errorhandler = lua_gettop(L) - args - 1;
		lua_insert(L, errorhandler);

		int result = lua_pcall(L, args, LUA_MULTRET, errorhandler);
		// lua_pcall() automatically pops the callee function and its arguments off the stack. Then, if no errors were encountered
		// during execution, it pushes the return values onto the stack, if any. Otherwise, if an error was encountered, it pushes
		// the error message onto the stack, which should manually be popped off when done using to keep the stack balanced
		if (result == LUA_ERRRUN)
		{
			size_t len;
			PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
			// This call pops the error message off the stack
			lua_pop(L, 1);
			return 0;
		}
		// Do not use lua_pop() as the callee function's return values may be present, which would pop one of those instead and leave
		// the error handler on the stack
		lua_remove(L, errorhandler);
		lua_pushboolean(L, result == 0);
		lua_insert(L, 1);

		//if (result != 0) return 1;

		return lua_gettop(L);
	}

	int luaF_dofile(lua_State* L)
	{

		int n = lua_gettop(L);

		size_t length = 0;
		const char* filename = lua_tolstring(L, 1, &length);
		int error = luaL_loadfilex(L, filename, nullptr);
		if (error != 0)
		{
			size_t len;
			//		Logging::Log(filename, Logging::LOGGING_ERROR);
			PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
		}
		else
		{
			lua_getglobal(L, "debug");
			lua_getfield(L, -1, "traceback");
			lua_remove(L, -2);
			// Example stack for visualization purposes:
			// 3 debug.traceback
			// 2 compiled code as a self-contained function
			// 1 filename
			// Do not index from the top (i.e. don't use a negative index)
			int errorhandler = 2;
			lua_insert(L, errorhandler);

			error = lua_pcall(L, 0, 0, errorhandler);
			if (error == LUA_ERRRUN)
			{
				size_t len;
				//			Logging::Log(filename, Logging::LOGGING_ERROR);
				PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
				// This call pops the error message off the stack
				lua_pop(L, 1);
			}
			// Do not use lua_pop() as the callee function's return values may be present, which would pop one of those instead and
			// leave the error handler on the stack
			lua_remove(L, errorhandler);
		}
		return 0;
	}

	struct lua_http_data
	{
		int funcRef;
		int progressRef;
		int requestIdentifier;
		lua_State* L;
	};

	void return_lua_http(void* data, std::string& urlcontents)
	{
		lua_http_data* ourData = (lua_http_data*)data;
		if (!check_active_state(ourData->L))
		{
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

	void progress_lua_http(void* data, long progress, long total)
	{
		lua_http_data* ourData = (lua_http_data*)data;

		if (!check_active_state(ourData->L))
		{
			return;
		}

		if (ourData->progressRef == 0) return;
		lua_rawgeti(ourData->L, LUA_REGISTRYINDEX, ourData->progressRef);
		lua_pushinteger(ourData->L, ourData->requestIdentifier);
		lua_pushinteger(ourData->L, progress);
		lua_pushinteger(ourData->L, total);
		lua_pcall(ourData->L, 3, 0, 0);
	}

	int luaF_directoryhash(lua_State* L)
	{ 
		PD2HOOK_TRACE_FUNC; 
		int n = lua_gettop(L); 

		size_t length = 0; 
		const char* filename = lua_tolstring(L, 1, &length); 
		std::string hash = Util::GetDirectoryHash(filename); 
		lua_pushlstring(L, hash.c_str(), hash.length()); 

		return 1; 
	} 

	int luaF_filehash(lua_State* L)
	{
		int n = lua_gettop(L);
		size_t l = 0;
		const char * fileName = lua_tolstring(L, 1, &l);
		std::string hash = Util::GetFileHash(fileName);
		lua_pushlstring(L, hash.c_str(), hash.length());
		return 1;
	}

	static int HTTPReqIdent = 0;

	int luaF_dohttpreq(lua_State* L)
	{
		PD2HOOK_LOG_LOG("Incoming HTTP Request/Request");

		int args = lua_gettop(L);
		int progressReference = 0;
		if (args >= 3)
		{
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

		if (progressReference != 0)
		{
			reqItem->progress = progress_lua_http;
		}

		HTTPManager::GetSingleton()->LaunchHTTPRequest(std::move(reqItem));
		lua_pushinteger(L, HTTPReqIdent);
		return 1;
	}

	CConsole* gbl_mConsole = NULL;

	int luaF_createconsole(lua_State* L)
	{
		if (gbl_mConsole) return 0;
		gbl_mConsole = new CConsole();
		return 0;
	}

	int luaF_destroyconsole(lua_State* L)
	{
		if (!gbl_mConsole) return 0;
		delete gbl_mConsole;
		gbl_mConsole = NULL;
		return 0;
	}

	int luaF_print(lua_State* L)
	{
		int top = lua_gettop(L);
		std::stringstream stream;
		for (int i = 0; i < top; ++i)
		{
			size_t len;
			const char* str = lua_tolstring(L, i + 1, &len);
			stream << (i > 0 ? "    " : "") << str;
		}
		PD2HOOK_LOG_LUA(stream.str());

		return 0;
	}

	int luaF_moveDirectory(lua_State * L)
	{
		int top = lua_gettop(L);
		size_t lf = 0;
		const char * fromStr = lua_tolstring(L, 1, &lf);
		size_t ld = 0;
		const char * destStr = lua_tolstring(L, 2, &ld);

		bool success = Util::MoveDirectory(fromStr, destStr);
		lua_pushboolean(L, success);
		return 1;
	}

	int updates = 0;
	std::thread::id main_thread_id;

	void* __fastcall do_game_update_new(void* thislol, int edx, int* a, int* b)
	{

		// If someone has a better way of doing this, I'd like to know about it.
		// I could save the this pointer?
		// I'll check if it's even different at all later.
		if (std::this_thread::get_id() != main_thread_id)
		{
			return do_game_update(thislol, a, b);
		}

		lua_State* L = (lua_State*)*((void**)thislol);
		if (updates == 0)
		{
			HTTPManager::GetSingleton()->init_locks();
		}

		if (updates > 1)
		{
			EventQueueMaster::GetSingleton().ProcessEvents();
		}

		updates++;
		return do_game_update(thislol, a, b);
	}

	// Random dude who wrote what's his face?
	// I 'unno, I stole this method from the guy who wrote the 'underground-light-lua-hook'
	// Mine worked fine, but this seems more elegant.
	int __fastcall luaL_newstate_new(void* thislol, int edx, char no, char freakin, int clue)
	{
		int ret = (vrMode ? luaL_newstate_vr : luaL_newstate)(thislol, no, freakin, clue);

		lua_State* L = (lua_State*)*((void**)thislol);
		printf("Lua State: %p\n", (void*)L);
		if (!L) return ret;

		add_active_state(L);

		lua_pushcclosure(L, luaF_print, 0);
		lua_setfield(L, LUA_GLOBALSINDEX, "log");

		lua_pushcclosure(L, luaF_pcall, 0);
		lua_setfield(L, LUA_GLOBALSINDEX, "pcall");

		lua_pushcclosure(L, luaF_dofile, 0);
		lua_setfield(L, LUA_GLOBALSINDEX, "dofile");

		lua_pushcclosure(L, luaF_unzipfile, 0);
		lua_setfield(L, LUA_GLOBALSINDEX, "unzip");

		lua_pushcclosure(L, luaF_dohttpreq, 0);
		lua_setfield(L, LUA_GLOBALSINDEX, "dohttpreq");

		luaL_Reg consoleLib[] = {
			{ "CreateConsole", luaF_createconsole },
			{ "DestroyConsole", luaF_destroyconsole },
			{ NULL, NULL }
		};
		luaI_openlib(L, "console", consoleLib, 0);

		luaL_Reg fileLib[] = {
			{ "GetDirectories", luaF_getdir },
			{ "GetFiles", luaF_getfiles },
			{ "RemoveDirectory", luaF_removeDirectory },
			{ "DirectoryExists", luaF_directoryExists },
			{ "DirectoryHash", luaF_directoryhash },
			{ "FileHash", luaF_filehash },
			{ "MoveDirectory", luaF_moveDirectory },
			{ NULL, NULL }
		};
		luaI_openlib(L, "file", fileLib, 0);

		// Keeping everything in lowercase since IspcallForced / IsPCallForced and Forcepcalls / ForcePCalls look rather weird anyway
		luaL_Reg bltLib[] = {
			{ "ispcallforced", luaF_ispcallforced },
			{ "forcepcalls", luaF_forcepcalls },
			{ "getvrstate", luaF_getvrstate },
			{ NULL, NULL }
		};
		luaI_openlib(L, "blt", bltLib, 0);

		int result;
		PD2HOOK_LOG_LOG("Initiating Hook");

		result = luaL_loadfilex(L, "mods/base/base.lua", nullptr);
		if (result == LUA_ERRSYNTAX)
		{
			size_t len;
			PD2HOOK_LOG_ERROR(lua_tolstring(L, -1, &len));
			return ret;
		}
		result = lua_pcall(L, 0, 1, 0);
		if (result == LUA_ERRRUN)
		{
			size_t len;
			PD2HOOK_LOG_LOG(lua_tolstring(L, -1, &len));
			return ret;
		}

		return ret;
	}

	int __fastcall luaL_newstate_new_vr(void* thislol, int edx, char no, char freakin, int clue) {
		vrMode = true;
		return luaL_newstate_new(thislol, edx, no, freakin, clue);
	}

	void luaF_close(lua_State* L)
	{
		remove_active_state(L);
		lua_close(L);
	}

	void InitiateStates()
	{
		main_thread_id = std::this_thread::get_id();

		SignatureSearch::Search();

		FuncDetour* gameUpdateDetour = new FuncDetour((void**)&do_game_update, do_game_update_new);
		FuncDetour* newStateDetour = new FuncDetour((void**)&luaL_newstate, luaL_newstate_new);
		FuncDetour* newStateDetourVr = new FuncDetour((void**)&luaL_newstate_vr, luaL_newstate_new_vr);
		FuncDetour* luaCloseDetour = new FuncDetour((void**)&lua_close, luaF_close);

		std::ifstream infile("mods/developer.txt");
		if(infile.good())
		{
			gbl_mConsole = new CConsole();
		}
	}

	void DestroyStates()
	{
		// Okay... let's not do that.
		// I don't want to keep this in memory, but it CRASHES THE SHIT OUT if you delete this after all is said and done.
		if (gbl_mConsole) delete gbl_mConsole;
	}

}