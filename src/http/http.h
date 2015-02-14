#ifndef __HTTP_HEADER__
#define __HTTP_HEADER__

#include <string>
#include <mutex>
#include <thread>
#include <list>

typedef void(*HTTPCallback)(void* data, std::string& urlContents);
typedef void(*HTTPProgress)(void* data, long progress, long total);

struct HTTPItem {
	HTTPItem();
	HTTPCallback call;
	HTTPProgress progress;
	std::string url;
	std::string httpContents;
	void* data;

	long byteprogress;
	long bytetotal;
};

class HTTPManager {
public:
	HTTPManager();
	~HTTPManager();

	void init_locks();

	static HTTPManager* GetSingleton();

	void SSL_Lock(int lockno);
	void SSL_Unlock(int lockno);

	void LaunchHTTPRequest(HTTPItem* callback);
private:
	static HTTPManager* httpSingleton;
	std::mutex* openssl_locks;
	int numLocks;
	std::list<std::thread*> threadList;
};


#endif // __HTTP_HEADER__