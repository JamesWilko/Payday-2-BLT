#ifndef __UTIL_HEADER__
#define __UTIL_HEADER__

#include <exception>
#include <vector>
#include <string>
#include <sstream>

namespace pd2hook
{

namespace Util {
	std::vector<std::string> GetDirectoryContents(const std::string& path, bool isDirs = false);
	std::string GetFileContents(const std::string& filename);
	void EnsurePathWritable(const std::string& path);
	bool RemoveEmptyDirectory(const std::string& dir);
	bool DirectoryExists(const std::string& dir);
	bool CreateDirectoryPath(const std::string& dir);
	// String split from http://stackoverflow.com/a/236803
	void SplitString(const std::string &s, char delim, std::vector<std::string> &elems);
	std::vector<std::string> SplitString(const std::string &s, char delim);

	class Exception : public std::exception
	{
	public:
		Exception(const char *file, int line);
		Exception(std::string msg, const char *file, int line);

		virtual const char *what() const override;

		virtual const char *exceptionName() const;
		virtual void writeToStream(std::ostream& os) const;
	
	private:
		const char * const mFile;
		const int mLine;
		const std::string mMsg;
	};

#define PD2HOOK_SIMPLE_THROW() throw pd2hook::Util::Exception(__FILE__, __LINE__)
#define PD2HOOK_SIMPLE_THROW_MSG(msg) throw pd2hook::Util::Exception(msg, __FILE__, __LINE__)

	inline std::ostream& operator<<(std::ostream& os, const Exception& e)
	{
		e.writeToStream(os);
		return os;
	}

	class IOException : public Exception
	{
	public:
		IOException(const char *file, int line);
		IOException(std::string msg, const char *file, int line);

		virtual const char *exceptionName() const;
	};

#define PD2HOOK_THROW_IO() throw pd2hook::Util::IOException(__FILE__, __LINE__)
#define PD2HOOK_THROW_IO_MSG(msg) throw pd2hook::Util::IOException(msg, __FILE__, __LINE__)
}


namespace Logging {

	enum class LogType{
		LOGGING_FUNC = 0,
		LOGGING_LOG,
		LOGGING_LUA,
		LOGGING_WARN,
		LOGGING_ERROR
	};

class Logger
{
public:
	using Message_t = std::string;
	using LogLevel = LogType;

	static Logger& Instance();
	static void Close();

protected:
	Logger() = default;

public:
	LogLevel getLoggingLevel() const { return mLevel; }
	void setLoggingLevel(LogLevel level) { mLevel = level; }

	void setForceFlush(bool forceFlush);

	void log(const Message_t& msg);
	void log(const Message_t& msg, LogType msgType) {
		if (msgType >= getLoggingLevel()) { log(msg); }
	}

private:
	LogLevel mLevel = LogLevel::LOGGING_LOG;
};

class LogWriter : public std::ostringstream
{
public:
	LogWriter(LogType msgType);
	LogWriter(const char *file, int line, LogType msgType = LogType::LOGGING_LOG);

	void write(Logger& logger) { logger.log(str()); }
};

class FunctionLogger
{
public:
	FunctionLogger(const char *funcName, const char *file);
	~FunctionLogger();

private:
	const char * const mFile;
	const char * const mFuncName;
};
}

bool ExtractZIPArchive(const std::string& path, const std::string& extractPath);
}

#ifdef PD2HOOK_ENABLE_FUNCTION_TRACE
#define PD2HOOK_TRACE_FUNC pd2hook::Logging::FunctionLogger funcLogger(__FUNCTION__, __FILE__)
#define PD2HOOK_TRACE_FUNC_MSG(msg) pd2hook::Logging::FunctionLogger funcLogger(msg)
#else
#define PD2HOOK_TRACE_FUNC
#define PD2HOOK_TRACE_FUNC_MSG(msg)
#endif

#define PD2HOOK_LOG_LEVEL(msg, level, file, line) do { \
	auto& logger = pd2hook::Logging::Logger::Instance(); \
	if(level >= logger.getLoggingLevel()) { \
		pd2hook::Logging::LogWriter writer(file, line, level); \
		writer << msg; \
		writer.write(logger); \
	}} while (false);

#define PD2HOOK_LOG_FUNC(msg) PD2HOOK_LOG_LEVEL(msg, pd2hook::Logging::LogType::LOGGING_FUNC, __FILE__, 0)
#define PD2HOOK_LOG_LOG(msg) PD2HOOK_LOG_LEVEL(msg, pd2hook::Logging::LogType::LOGGING_LOG, __FILE__, __LINE__)
#define PD2HOOK_LOG_LUA(msg) PD2HOOK_LOG_LEVEL(msg, pd2hook::Logging::LogType::LOGGING_LUA, __FILE__, __LINE__)
#define PD2HOOK_LOG_WARN(msg) PD2HOOK_LOG_LEVEL(msg, pd2hook::Logging::LogType::LOGGING_WARN, __FILE__, __LINE__)
#define PD2HOOK_LOG_ERROR(msg) PD2HOOK_LOG_LEVEL(msg, pd2hook::Logging::LogType::LOGGING_ERROR, __FILE__, __LINE__)
#define PD2HOOK_LOG_EXCEPTION(e) PD2HOOK_LOG_WARN(e)

#define PD2HOOK_DEBUG_CHECKPOINT PD2HOOK_LOG_LOG("Checkpoint")

namespace pd2hook
{
namespace Logging
{
inline FunctionLogger::FunctionLogger(const char *funcName, const char *file) :
	mFile(file), mFuncName(funcName)
{
	PD2HOOK_LOG_LEVEL(">>> " << mFuncName, LogType::LOGGING_FUNC, mFile, 0);
}

inline FunctionLogger::~FunctionLogger()
{
	PD2HOOK_LOG_LEVEL("<<< " << mFuncName, LogType::LOGGING_FUNC, mFile, 0);
}
}
}

#endif // __UTIL_HEADER__
