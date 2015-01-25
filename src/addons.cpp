#include "addons.h"
#include "util/util.h"

#include "../../rapidjson/document.h"

std::list<PaydayAddon*> g_addonList;
std::list<PaydayHook*> g_hookList;

void InitializeAllAddons(){
	std::vector<std::string> files = Util::GetDirectoryContents("addons\\", true);
	std::vector<std::string>::iterator it;

	for (it = files.begin(); it < files.end(); it++){
		if (*it == "." || *it == "..") continue;
		std::string jsonFile = Util::GetFileContents("addons\\" + *it + "\\addon.txt");
		
		rapidjson::Document jsonDoc;
		jsonDoc.Parse(jsonFile.c_str());

		PaydayAddon* cAddon = new PaydayAddon(*it, jsonDoc["addon"].GetString(), jsonDoc["author"].GetString());

		rapidjson::Value& hookList = jsonDoc["hooks"];
		for (auto hookIt = hookList.Begin(); hookIt != hookList.End(); hookIt++){
			PaydayHook* newHook = new PaydayHook(cAddon, (*hookIt)["scriptPath"].GetString(), (*hookIt)["hookID"].GetString());
			cAddon->AddHook(newHook);
			g_hookList.push_back(newHook);
		}

		g_addonList.push_back(cAddon);
	}
}

void DestroyAddons(){
	std::list<PaydayAddon*>::iterator it;
	for (it = g_addonList.begin(); it != g_addonList.end(); it++){
		delete *it;
	}
}

void RunFunctionHook(std::string msgHook, void* lState){
	std::list<PaydayHook*>::iterator it;
	for (it = g_hookList.begin(); it != g_hookList.end(); it++){
		if (msgHook == (*it)->GetHookID()){
			(*it)->RunHook(lState);
		}
	}
}

PaydayAddon::PaydayAddon(std::string ident, std::string name, std::string author) : identifier(ident), addonName(name), authorName(author){

}

std::string& PaydayAddon::GetIdentifer(){
	return identifier;
}

PaydayAddon::~PaydayAddon(){
	std::list<PaydayHook*>::iterator it;
	for (it = hookList.begin(); it != hookList.end(); it++){
		delete *it;
	}
}

PaydayHook::PaydayHook(PaydayAddon* addon, std::string script, std::string hook) : ownerAddon(addon), scriptPath(script), hookID(hook){

}

void PaydayAddon::AddHook(PaydayHook* hook){
	hookList.push_back(hook);
}

std::string& PaydayHook::GetHookID(){
	return hookID;
}

// I am doing this hardstyle because I AM DA BOMB
// No, it's because we can have different return types
// They're largely going to be constant anyway, I see no reason for dynamic
// and there's no pointign having a GetString/Boolean/Integer function for all possible
// configurations.

namespace Configuration{
	bool isDevConsole = false;
}

void Configuration::LoadConfiguration(){
	std::string configContents = Util::GetFileContents("config.json");
	rapidjson::Document confJ;
	confJ.Parse(configContents.c_str());

	isDevConsole = confJ["isDeveloperConsole"].GetBool();
}

bool Configuration::IsDeveloperConsole(){
	return isDevConsole;
}