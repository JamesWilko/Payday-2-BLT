#ifndef __ADDONS_HEADER__
#define __ADDONS_HEADER__

#include <string>
#include <list>

// They totally need to be aware of each other :>
class PaydayAddon;

class PaydayHook {
public:
	PaydayHook(PaydayAddon* addon, std::string script, std::string hook);
	void RunHook(void* luaState);
	std::string& GetHookID();
private:
	PaydayAddon* ownerAddon;
	std::string scriptPath;
	std::string hookID;
};

class PaydayAddon {
public:
	PaydayAddon(std::string ident, std::string name, std::string author);
	void AddHook(PaydayHook* hook);
	std::string& GetIdentifer();
private:
	std::list<PaydayHook*> hookList;
	std::string addonName;
	std::string authorName;
	std::string identifier;
};

void InitializeAllAddons();
void RunFunctionHook(std::string msgHook, void* lState);


#endif // __ADDONS_HEADER__