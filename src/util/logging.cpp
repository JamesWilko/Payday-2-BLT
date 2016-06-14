#include "util/util.h"

#include <ctime>
#include <fstream>
#include <iostream>

#include <mutex>

namespace pd2hook
{
namespace Logging
{
namespace
{
std::string GetDateString()
{
	std::time_t currentTime = time(0);
	std::tm now;
	localtime_s(&now, &currentTime);

	char datestring[100];
	std::strftime(datestring, sizeof(datestring), "%Y_%m_%d", &now);
	return datestring;
}

std::ostream& LogTime(std::ostream& os)
{
	std::time_t currentTime = time(0);
	std::tm now;
	localtime_s(&now, &currentTime);

	char datestring[100];
	std::strftime(datestring, sizeof(datestring), "%I:%M:%S %p", &now);

	os << datestring << ' ';
	return os;
}

std::ostream& operator<<(std::ostream& os, LogType msgType)
{
	switch (msgType){
	case LogType::LOGGING_FUNC:
		break;
	case LogType::LOGGING_LOG:
		os << "Log: ";
		break;
	case LogType::LOGGING_LUA:
		os << "Lua: ";
		break;
	case LogType::LOGGING_WARN:
		os << "WARNING: ";
		break;
	case LogType::LOGGING_ERROR:
		os << "FATAL ERROR: ";
		break;
	default:
		os << "Message: ";
		break;
	}
	return os;
}

std::ostream& quick_endl(std::ostream& os)
{
	os << '\n';
	return os;
}

typedef decltype(&quick_endl) LineTerminator_t;

std::mutex& GetLoggerMutex()
{
	static std::mutex loggerMutex;
	return loggerMutex;
}

class LoggerImpl : public Logger
{
public:
	LoggerImpl(std::string&& file);

	void setForceFlush(bool forceFlush);

	bool isOpen() const { return mIsOpen; }

	void openFile(std::string&& file);
	void close();

	void log(const Message_t& msg);

private:
	bool mIsOpen = false;

	LineTerminator_t mEndl = quick_endl;
	std::string mFilename;
	std::ofstream mOut;
};

void LoggerImpl::setForceFlush(bool forceFlush)
{
	std::ostream& (*forceFlusher)(std::ostream&) = &std::endl;
	mEndl = forceFlush ? forceFlusher : quick_endl;
}

LoggerImpl::LoggerImpl(std::string&& file)
{
	openFile(std::forward<std::string>(file));
}

void LoggerImpl::openFile(std::string&& file)
{
	if (mFilename == file)
	{
		return;
	}

	std::lock_guard<std::mutex> lock(GetLoggerMutex());
	mOut.close();
	mOut = std::ofstream(file.c_str(), std::ios::app);
	mFilename = std::move(file);
	mIsOpen = !!mOut;
}

void LoggerImpl::close()
{
	std::lock_guard<std::mutex> lock(GetLoggerMutex());
	if (!mIsOpen)
	{
		return;
	}

	mOut.close();
	mFilename.clear();
	mIsOpen = false;
}

void LoggerImpl::log(const Message_t& msg)
{
	std::lock_guard<std::mutex> lock(GetLoggerMutex());
	if (!mIsOpen)
	{
		return;
	}

	mOut << msg << mEndl;
	std::cout << msg << mEndl;
}
}

Logger& Logger::Instance()
{
	static LoggerImpl logger("mods/logs/" + GetDateString() + "_log.txt");
	return logger;
}

void Logger::Close()
{
	static_cast<LoggerImpl&>(Instance()).close();
}

void Logger::setForceFlush(bool forceFlush)
{
	static_cast<LoggerImpl *>(this)->setForceFlush(forceFlush);
}

void Logger::log(const Message_t& msg)
{
	static_cast<LoggerImpl *>(this)->log(msg);
}

LogWriter::LogWriter(LogType msgType)
{
	*this << LogTime << msgType << ' ';
}

LogWriter::LogWriter(const char *file, int line, LogType msgType)
{
	*this << LogTime << msgType << " (" << file;
	if (line)
	{
		*this << ':' << line;
	}
	*this << ") ";
}
}
}