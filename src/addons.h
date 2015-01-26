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
	~PaydayAddon();
	void AddHook(PaydayHook* hook);
	std::string& GetIdentifer();
private:
	std::list<PaydayHook*> hookList;
	std::string addonName;
	std::string authorName;
	std::string identifier;
};

class AddonManager {
public:
	AddonManager();
	~AddonManager();
	void RunFunctionHook(std::string msgHook, void* lState);

	static AddonManager* GetSingleton();
private:
	static AddonManager* addonSingleton;
	std::list<PaydayAddon*> addonList;
	std::list<PaydayHook*> hookList;
};


namespace Configuration {
	void LoadConfiguration();
	bool IsDeveloperConsole();
}


#endif // __ADDONS_HEADER__