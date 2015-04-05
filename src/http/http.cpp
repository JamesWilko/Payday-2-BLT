#include <curl/curl.h>
#include <openssl/crypto.h>
#include "http/http.h"
#include "threading/queue.h"
#include "util/util.h"

#include <thread>

HTTPManager HTTPManager::httpSingleton;

void lock_callback(int mode, int type, const char* file, int line){
	if (mode & CRYPTO_LOCK){
		HTTPManager::GetSingleton().SSL_Lock(type);
	} else {
		HTTPManager::GetSingleton().SSL_Unlock(type);
	}
}

HTTPManager::HTTPManager(){
	// Curl Init
	curl_global_init(CURL_GLOBAL_ALL);
	Logging::Log("CURL INITD");
}

HTTPManager::~HTTPManager(){
	CRYPTO_set_locking_callback(NULL);
	Logging::Log("CURL CLOSED");
	curl_global_cleanup();

	for (auto&& it : threadList)
		it.join();
	threadList.clear();
}

void HTTPManager::init_locks(){
	numLocks = CRYPTO_num_locks();
	openssl_locks.reset(new std::mutex[numLocks]);
	CRYPTO_set_locking_callback(lock_callback);
}

HTTPManager& HTTPManager::GetSingleton(){
	return httpSingleton;
}

void HTTPManager::SSL_Lock(int lockno){
	openssl_locks[lockno].lock();
}

void HTTPManager::SSL_Unlock(int lockno){
	openssl_locks[lockno].unlock();
}

HTTPItem::HTTPItem(){
	progress = NULL;
	byteprogress = 0;
	bytetotal = 0;
}

size_t write_http_data(char* ptr, size_t size, size_t nmemb, void* data){
	std::string newData = std::string(ptr, size*nmemb);
	HTTPItem* mainItem = (HTTPItem*)data;
	mainItem->httpContents += newData;
	return size*nmemb;
}

struct HTTPProgressNotification{
	HTTPItem* ourItem;
	long byteProgress;
	long byteTotal;
};

void run_http_progress_event(void* data){
	HTTPProgressNotification* ourNotify = (HTTPProgressNotification*)data;
	HTTPItem* ourItem = ourNotify->ourItem;
	ourItem->progress(ourItem->data, ourNotify->byteProgress, ourNotify->byteTotal);
	delete ourNotify;
}

int http_progress_call(void* clientp, curl_off_t dltotal, curl_off_t dlnow, curl_off_t ultotal, curl_off_t ulnow){
	HTTPItem* ourItem = (HTTPItem*)clientp;
	if (!ourItem->progress) return 0;
	if (dltotal == 0 || dlnow == 0) return 0;
	if (dltotal == dlnow) return 0;
	if (ourItem->byteprogress >= dlnow) return 0;
	ourItem->byteprogress = dlnow;
	ourItem->bytetotal = dltotal;

	HTTPProgressNotification* notify = new HTTPProgressNotification();
	notify->ourItem = ourItem;
	notify->byteProgress = dlnow;
	notify->byteTotal = dltotal;

	EventQueueM::GetSingleton().AddToQueue(run_http_progress_event, notify);
	return 0;
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
	curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
	curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0L);

	curl_easy_setopt(curl, CURLOPT_TIMEOUT, 60);

	if (item->progress){
		curl_easy_setopt(curl, CURLOPT_XFERINFOFUNCTION, http_progress_call);
		curl_easy_setopt(curl, CURLOPT_XFERINFODATA, item);
		curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0);
	}
	
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_http_data);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, item);

	curl_easy_perform(curl);
	curl_easy_cleanup(curl);


	EventQueueM::GetSingleton().AddToQueue(run_http_event, item);
}

void HTTPManager::LaunchHTTPRequest(HTTPItem* callback){
	Logging::Log("Launching Async HTTP Thread");
	// This shit's gonna end eventually, how many threads are people going to launch?
	// Probably a lot.
	// I'll manage them I guess, but I've no idea when to tell them to join which I believe is part of the constructor.
	threadList.emplace_back(std::thread(launch_thread_http, callback));
}