#include "util.h"

namespace pd2hook
{
namespace Util
{
Exception::Exception(const char *file, int line) :
	mFile(file), mLine(line)
{}

Exception::Exception(std::string msg, const char *file, int line) :
	mFile(file), mLine(line), mMsg(std::move(msg))
{}

const char *Exception::what() const
{
	if (!mMsg.empty())
	{
		return mMsg.c_str();
	}

	return std::exception::what();
}

const char *Exception::exceptionName() const
{
	return "An exception";
}

void Exception::writeToStream(std::ostream& os) const
{
	os << exceptionName() << " occurred @ (" << mFile << ':' << mLine << "). " << what();
}
}
}