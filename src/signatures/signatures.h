#ifndef __SIGNATURE_HEADER__
#define __SIGNATURE_HEADER__

#define CREATE_CALLABLE_SIGNATURE(name, retn, signature, mask, offset, ...) \
	typedef retn(__fastcall *name ## ptr)(__VA_ARGS__); \
	name ## ptr name = NULL; \
	pd2hook::SignatureSearch name ## search(const_cast<const void **>(reinterpret_cast<void **>(&name)), signature, mask, offset);

#define CREATE_NORMAL_CALLABLE_SIGNATURE(name, retn, signature, mask, offset, ...) \
	typedef retn(*name ## ptr)(__VA_ARGS__); \
	name ## ptr name = NULL; \
	pd2hook::SignatureSearch name ## search(&name, signature, mask, offset);

#define CREATE_CALLABLE_CLASS_SIGNATURE(name, retn, signature, mask, offset, ...) \
	typedef retn(__thiscall *name ## ptr)(void*, __VA_ARGS__); \
	name ## ptr name = NULL; \
	pd2hook::SignatureSearch name ## search(const_cast<const void **>(reinterpret_cast<void **>(&name)), signature, mask, offset);

#define CREATE_LUA_FUNCTION(lua_func, name) \
	lua_pushcclosure(L, lua_func, 0); \
	_asm add esp, 4 \
	lua_setfield(L, LUA_GLOBALSINDEX, name); \
	_asm add esp, 4

namespace pd2hook
{
struct SignatureF {
	const char* signature;
	const char* mask;
	int offset;
	const void** address;
};

class SignatureSearch {
public:
	SignatureSearch(const void** address, const char* signature, const char* mask, int offset);
	static void Search();
};

class FuncDetour {
public:
	FuncDetour(void** oldF, void* newF);
	~FuncDetour();
protected:
	void** oldFunction;
	void* newFunction;
};
}

#endif // __SIGNATURE_HEADER__