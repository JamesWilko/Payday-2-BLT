#ifndef __ADDONS_HEADER__
#define __ADDONS_HEADER__

#include <string>

class PaydayAddon {
public:

	void RunScript(void* luaState);

	std::string initScript;
	std::string postLoad;
private:

};

void InitializeAllAddons();
void RunFunctionHook(std::string msgHook, void* lState);


#endif // __ADDONS_HEADER__