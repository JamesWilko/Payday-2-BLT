#include "addons.h"
#include "util/util.h"

#include "../../rapidjson/document.h"

std::vector<PaydayAddon*> addonList;

void InitializeAllAddons(){
	std::vector<std::string> files = Util::GetDirectoryContents("addons\\");
	std::vector<std::string>::iterator it;

	for (it = files.begin(); it < files.end(); it++){
		std::string jsonFile = Util::GetFileContents("addons\\" + *it);
		PaydayAddon* cAddon = new PaydayAddon();
		
		rapidjson::Document jsonDoc;
		jsonDoc.Parse(jsonFile.c_str());

		cAddon->initScript = jsonDoc["initScript"].GetString();
		cAddon->postLoad = jsonDoc["postLoad"].GetString();

		addonList.push_back(cAddon);
	}
}

void RunFunctionHook(std::string msgHook, void* lState){
	std::vector<PaydayAddon*>::iterator it;
	for (it = addonList.begin(); it < addonList.end(); it++){
		if (msgHook == (*it)->postLoad){
			(*it)->RunScript(lState);
		}
	}
}