#include "threading/queue.h"
#include "util/util.h"

namespace pd2hook
{
EventQueueMaster& EventQueueMaster::GetSingleton()
{
	static EventQueueMaster instance;
	return instance;
}

void EventQueueMaster::ProcessEvents()
{
	std::for_each(queues.begin(), queues.end(), [](IEventQueue *q) { q->ProcessEvents(); });
}

void EventQueueMaster::registerQueue(IEventQueue *q)
{
	queues.push_back(q);
}
}
