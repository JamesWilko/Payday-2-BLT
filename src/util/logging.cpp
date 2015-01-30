#include "util/util.h"

#include <fstream>
#include <iostream>

#include <thread>

void Logging::Log(std::string msg){
	std::ofstream mFile;
	mFile.open("log.txt", std::ios::out | std::ios::app);
	std::thread::id this_id = std::this_thread::get_id();
	mFile << this_id;
	mFile << "Log: ";
	mFile << msg.c_str();
	mFile << "\n";
	mFile.close();
	printf(msg.c_str());
	printf("\n");
}