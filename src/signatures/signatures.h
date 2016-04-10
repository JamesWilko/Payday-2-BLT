#ifndef __SIGNATURE_HEADER__
#define __SIGNATURE_HEADER__

#include <tuple>
#include <type_traits>

namespace pd2hook
{
// Credit to https://functionalcpp.wordpress.com/2013/08/05/function-traits/ for the function traits concept
template<typename F>
struct function_traits;
template<typename R, typename... Args>
struct function_traits<R(__fastcall *)(Args...)> : function_traits<R(Args...)> {};
template<typename R, typename... Args>
struct function_traits<R(*)(Args...)> : function_traits<R(Args...)> {};
template<typename R, typename... Args>
struct function_traits<R(Args...)>
{
	using return_type = R;

	static const size_t arity = sizeof...(Args);
	using args = std::tuple<Args...>;

	template<size_t At>
	struct argument : std::tuple_element<At, args>
	{};
};

namespace impl
{
template<typename... Args>
struct ArgumentsSize;
template<typename... Args>
struct ArgumentsSize<std::tuple<Args...>> : ArgumentsSize<Args...> {};
template<>
struct ArgumentsSize<> : std::integral_constant<size_t, 0> {};
template<typename Arg, typename... Args>
struct ArgumentsSize<Arg, Args...> : std::integral_constant<size_t, 
	sizeof(Arg) + ((sizeof(Arg) % sizeof(DWORD) == 0) ? 0 : (sizeof(DWORD) - (sizeof(Arg) % sizeof(DWORD)))) + ArgumentsSize<Args...>::value>
{};

template<bool FoundFirst, typename... Args>
struct RegisterUse;
template<typename... Args>
struct RegisterUse<false, std::tuple<Args...>> : RegisterUse<false, Args...> {};
template<bool FoundFirst>
struct RegisterUse<FoundFirst> : std::integral_constant<size_t, 0> {};
template<typename Arg, typename... Args>
struct RegisterUse<false, Arg, Args...> : std::integral_constant<size_t, (sizeof(Arg) <= sizeof(DWORD)) ? (sizeof(Arg) + RegisterUse<true, Args...>::value) : RegisterUse<false, Args...>::value> {};
template<typename Arg, typename... Args>
struct RegisterUse<true, Arg, Args...> : std::integral_constant<size_t, (sizeof(Arg) <= sizeof(DWORD)) ? sizeof(Arg) : RegisterUse<true, Args...>::value> {};
}

template<typename F>
struct StackCleanSize
{
	using args = typename function_traits<F>::args;

	using total_size = impl::ArgumentsSize<args>;
	using registeruse_size = impl::RegisterUse<false, args>;
	using stackcleansize = std::integral_constant<size_t, total_size::value - registeruse_size::value>;
};

template<typename Ret, size_t StackAdjust, typename... Args>
struct LuaCaller;

// The 0 StackAdjust specialisations aren't strictly necessary but they likely save at least 1 op.
template<typename Ret, typename... Args>
struct LuaCaller<Ret, 0, Args...>
{
	static Ret call(Ret(__fastcall *lua_fn)(Args...), Args... args)
	{
		return lua_fn(args...);
	}
};

template<typename... Args>
struct LuaCaller<void, 0, Args...>
{
	static void call(void(__fastcall *lua_fn)(Args...), Args... args)
	{
		lua_fn(args...);
	}
};

template<size_t StackAdjust, typename... Args>
struct LuaCaller<void, StackAdjust, Args...>
{
	static void call(void(__fastcall *lua_fn)(Args...), Args... args)
	{
		// Don't attempt to use StackAdjust directly in the asm. Trust me.
		const int adj = StackAdjust;
		lua_fn(args...);
		__asm { __asm add esp, adj }
	}
};

template<typename Ret, size_t StackAdjust, typename... Args>
struct LuaCaller<Ret, StackAdjust, Args...>
{
	static Ret call(Ret(__fastcall *lua_fn)(Args...), Args... args)
	{
		const int adj = StackAdjust;
		Ret val = lua_fn(args...);
		__asm { __asm add esp, adj }
		return val;
	}
};
}

/*
 This macro declares a function signature of the hooked lua function plus a variable to hold its address. It also generates a wrapper function that automatically
 adjusts the stack to clean up after the call. The stack clean up is calculated as:
 TOTAL ARGUMENT SIZE = SIZE(Arg0) + SIZE(Arg1) + SIZE(Arg2) + ... + SIZE(ArgN), where SIZE = sizeof(Arg) rounded up to the nearest DWORD boundary.
 REGISTER USE SIZE = The size of the arguments that are passed by the registers in the __fastcall. Basically how many arguments there are smaller than or equal to a DWORD up to a maximum of 2.
 STACK CLEAN UP SIZE = TOTAL ARGUMENT SIZE - REGISTER USE SIZE

 If the hooked functions are found differently in the future (perhaps not requiring stack adjustments?) then the LuaCaller can be changed accordingly.
*/
#define CREATE_CALLABLE_SIGNATURE(name, retn, signature, mask, offset, ...) \
	typedef retn(__fastcall *name ## ptr)(__VA_ARGS__); \
	name ## ptr name ## ptr_val = nullptr; \
	template<typename... Args> retn name ## impl(Args... args) { return pd2hook::LuaCaller<retn, pd2hook::StackCleanSize<name ## ptr>::stackcleansize::value, __VA_ARGS__>::call( name ## ptr_val, args... ); } \
	typedef retn(*name ## actual)(__VA_ARGS__); name ## actual name = &name ## impl; \
	pd2hook::SignatureSearch name ## search(const_cast<const void **>(reinterpret_cast<void **>(&name ## ptr_val)), signature, mask, offset);

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
	lua_setfield(L, LUA_GLOBALSINDEX, name)

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