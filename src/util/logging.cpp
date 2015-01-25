#include "util/util.h"

#include <fstream>
#include <iostream>

void Logging::Log(std::string msg){
	std::ofstream mFile;
	mFile.open("log.txt", std::ios::out | std::ios::app);
	mFile << "Log: ";
	mFile << msg.c_str();
	mFile << "\n";
	mFile.close();
}