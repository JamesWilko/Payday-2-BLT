#include <curl/curl.h>
#include <openssl/crypto.h>
#include "http/http.h"
#include "threading/queue.h"
#include "util/util.h"

#include <thread>

HTTPManager* HTTPManager::httpSingleton = NULL;

void lock_callback(int mode, int type, const char* file, int line){
	if (mode & CRYPTO_LOCK){
		HTTPManager::GetSingleton()->SSL_Lock(type);
	} else {
		HTTPManager::GetSingleton()->SSL_Unlock(type);
	}
}

HTTPManager::HTTPManager(){
	// Curl Init
	curl_global_init(CURL_GLOBAL_ALL);
	Logging::Log("CURL INITD");

	// Singleton
	if (httpSingleton) delete httpSingleton;
	httpSingleton = this;
}

HTTPManager::~HTTPManager(){
	CRYPTO_set_locking_callback(NULL);
	Logging::Log("CURL CLOSED");
	curl_global_cleanup();

	delete[] openssl_locks;
}

void HTTPManager::init_locks(){
	numLocks = CRYPTO_num_locks();
	openssl_locks = new std::mutex[numLocks];
	CRYPTO_set_locking_callback(lock_callback);
}

HTTPManager* HTTPManager::GetSingleton(){
	return httpSingleton;
}

void HTTPManager::SSL_Lock(int lockno){
	openssl_locks[lockno].lock();
}

void HTTPManager::SSL_Unlock(int lockno){
	openssl_locks[lockno].unlock();
}

size_t write_http_data(char* ptr, size_t size, size_t nmemb, void* data){
	std::string newData = std::string(ptr, size*nmemb);
	HTTPItem* mainItem = (HTTPItem*)data;
	mainItem->httpContents += newData;
	return size*nmemb;
}

void run_http_event(void* data){
	HTTPItem* ourItem = (HTTPItem*)data;
	ourItem->call(ourItem->data, ourItem->httpContents);
	delete ourItem;
}

void launch_thread_http(HTTPItem* item){
	CURL *curl;
	curl = curl_easy_init();
	curl_easy_setopt(curl, CURLOPT_URL, item->url.c_str());
	curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
	//curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
	//curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0L);
	
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_http_data);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, item);
	curl_easy_perform(curl);
	curl_easy_cleanup(curl);


	EventQueueM::GetSingleton()->AddToQueue(run_http_event, item);
}

void HTTPManager::LaunchHTTPRequest(HTTPItem* callback){
	Logging::Log("Launching Async HTTP Thread");
	// This shit's gonna end eventually, how many threads are people going to launch?
	// Probably a lot.
	// I'll manage them I guess, but I've no idea when to tell them to join which I believe is part of the constructor.
	new std::thread(launch_thread_http, callback);
}