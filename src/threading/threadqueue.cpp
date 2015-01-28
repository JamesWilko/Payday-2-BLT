#include "threading/queue.h"
#include "util/util.h"

EventItem::EventItem(EventFunction toRun, void* data) : mFunc(toRun), mData(data){

}

void EventItem::RunFunction(){
	mFunc(mData);
}

EventQueueM* EventQueueM::eventSingleton = NULL;

EventQueueM::EventQueueM(){
	if (eventSingleton) delete eventSingleton;
	eventSingleton = this;
}

EventQueueM* EventQueueM::GetSingleton(){
	return eventSingleton;
}

void EventQueueM::ProcessEvents(){
	std::deque<EventItem*> eventClone;
	Logging::Log("Processing Events");
	criticalLock.lock();
	if (eventQueue.size() <= 0){
		Logging::Log("No Events");
		criticalLock.unlock();
		return;
	}
	eventClone = eventQueue;
	eventQueue.clear();
	criticalLock.unlock();
	Logging::Log("Events Fetched");

	std::deque<EventItem*>::iterator it;
	for (it = eventClone.begin(); it != eventClone.end(); it++){
		Logging::Log("Event Ran");
		(*it)->RunFunction();
		delete (*it);
	}
	Logging::Log("Events Ran");
}

void EventQueueM::AddToQueue(EventItem* newItem){
	Logging::Log("Adding Event to Queue");
	criticalLock.lock();
	eventQueue.push_back(newItem);
	criticalLock.unlock();
	Logging::Log("Event Added to Queue");
}

void EventQueueM::AddToQueue(EventFunction func, void* data){
	EventItem* nItem = new EventItem(func, data);
	AddToQueue(nItem);
}

