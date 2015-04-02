#ifndef __QUEUE_HEADER__
#define __QUEUE_HEADER__

#include <deque>
#include <mutex>

// Thread-safe event manager

typedef void (*EventFunction)(void* data);

class EventItem {
public:
	EventItem(EventFunction runFunction, void* dataToRun);
	void RunFunction();
private:
	EventFunction mFunc;
	void* mData;
};

class EventQueueM {
public:
	EventQueueM();
	void ProcessEvents();
	void AddToQueue(EventItem&& newItem);
	void AddToQueue(EventFunction runFunction, void* dataToRun);

	static EventQueueM& GetSingleton();
private:
	static EventQueueM eventSingleton;
	std::mutex criticalLock;
	std::deque<EventItem> eventQueue;
};


#endif // __QUEUE_HEADER__