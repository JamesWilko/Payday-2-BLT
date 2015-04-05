#ifndef __SIGNATURE_HEADER__
#define __SIGNATURE_HEADER__

#include <string>
#include <vector>

#define LUAFUNC_PTR(name, retn, ...) \
	retn(*name)(__VA_ARGS__)

#define HOOK_PTR(name, calltype, retn, ...) \
	retn(calltype *name)(__VA_ARGS__)

#define RESOLVE(handle, name) \
	name = (decltype(name))GetProcAddress(handle, #name)

#define CREATE_CALLABLE_SIGNATURE(name, retn, signature, mask, offset, ...) \
	void** name##Ptr = (void**)&name; \
	SignatureSearch(name##Ptr, signature, mask, offset);

#define CREATE_CALLABLE_CLASS_SIGNATURE(name, retn, signature, mask, offset, ...) \
	void** name##Ptr = (void**)&name; \
	SignatureSearch(name##Ptr, signature, mask, offset);

#define CREATE_LUA_FUNCTION(lua_func, name) \
	lua_pushcclosure(L, lua_func, 0); \
	lua_setfield(L, LUA_GLOBALSINDEX, name);

struct SignatureF {
	const char* signature;
	const char* mask;
	int offset;
	void** address;
};

class SignatureSearch {
public:
	SignatureSearch(void** address, const char* signature, const char* mask, int offset);
	static void Search();
};

class FuncDetour {
public:
	FuncDetour(void** oldF, void* newF);
	~FuncDetour();
	void Attach();
	void Detach();
protected:
	void** oldFunction;
	void* newFunction;
};


#endif // __SIGNATURE_HEADER__