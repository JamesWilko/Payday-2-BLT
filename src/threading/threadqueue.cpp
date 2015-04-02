#include "threading/queue.h"
#include "util/util.h"

EventItem::EventItem(EventFunction toRun, void* data) : mFunc(toRun), mData(data){

}

void EventItem::RunFunction(){
	mFunc(mData);
}

EventQueueM EventQueueM::eventSingleton;

EventQueueM::EventQueueM(){

}

EventQueueM& EventQueueM::GetSingleton(){
	return eventSingleton;
}

void EventQueueM::ProcessEvents(){
	std::deque<EventItem> eventClone;
	criticalLock.lock();
	if (eventQueue.size() <= 0){
		criticalLock.unlock();
		return;
	}
	eventClone = eventQueue;
	eventQueue.clear();
	criticalLock.unlock();

	for (auto&& it : eventClone)
		it.RunFunction();
}

void EventQueueM::AddToQueue(EventItem&& newItem){
	criticalLock.lock();
	eventQueue.emplace_back(newItem);
	criticalLock.unlock();
}

void EventQueueM::AddToQueue(EventFunction func, void* data){
	AddToQueue(EventItem(func, data));
}

