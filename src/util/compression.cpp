#include "util.h"

#include <zlib.h>

#include <algorithm>
#include <cstdint>
#include <fstream>
#include <list>
#include <memory>

namespace pd2hook
{
namespace
{
const int32_t MagicFileHeader = 0x04034b50;

typedef std::pair<int32_t, std::string> DataPair_t;

class ByteStream {
public:
	ByteStream(const std::string& path);

	template<typename T>
	T readType();
	std::string readString(int length);

private:
	std::ifstream mainStream;
};

struct ZIPFileData {
	std::string filepath;
	std::string decompressedData;
	int compressedSize;
	int uncompressedSize;
};

ByteStream::ByteStream(const std::string& path) : mainStream(path.c_str(), std::ifstream::binary) {}

template<typename T>
T ByteStream::readType() {
	T read;

	mainStream.read(reinterpret_cast<char *>(&read), sizeof(T));
	return read;
}

std::string ByteStream::readString(int length) {
	std::unique_ptr<char[]> readData(new char[length + 1]);
	mainStream.read(readData.get(), length);
	return std::string(readData.get(), length);
}

std::string DecompressData(const DataPair_t& compressedData)
{
	z_stream stream;
	stream.zalloc = Z_NULL;
	stream.zfree = Z_NULL;
	stream.opaque = Z_NULL;

	stream.avail_in = 0;
	stream.next_in = Z_NULL;

	auto ret = inflateInit2(&stream, -MAX_WBITS);

	stream.avail_in = compressedData.second.size();
	stream.next_in = reinterpret_cast<unsigned char*>(const_cast<char *>(compressedData.second.data()));

	std::unique_ptr<unsigned char[]> out(new unsigned char[compressedData.first + 1]);

	stream.avail_out = compressedData.first;
	stream.next_out = out.get();

	ret = inflate(&stream, Z_NO_FLUSH);
	inflateEnd(&stream);

	return std::string(reinterpret_cast<const char *>(out.get()), compressedData.first);
}

std::unique_ptr<ZIPFileData> ReadFile(ByteStream& mainStream)
{
	auto fileHeader = mainStream.readType<int32_t>();
	if (fileHeader != MagicFileHeader) return nullptr;

	auto versionNeeded = mainStream.readType<int16_t>();

	// Ignore 'general purpose bit flag'
	mainStream.readType<int16_t>();

	auto compressionMethod = mainStream.readType<int16_t>();

	// Ignore modified file times.
	mainStream.readType<int32_t>();

	auto crc32 = mainStream.readType<int32_t>();

	std::unique_ptr<ZIPFileData> newFile(new ZIPFileData());
	newFile->compressedSize = mainStream.readType<int32_t>();
	newFile->uncompressedSize = mainStream.readType<int32_t>();

	auto fileNameLength = mainStream.readType<int16_t>();
	auto extraFieldLength = mainStream.readType<int16_t>();

	newFile->filepath = mainStream.readString(fileNameLength);
	std::string extraField = mainStream.readString(extraFieldLength);
	(void)extraField;

	std::string compressedData = mainStream.readString(newFile->compressedSize);

	switch (compressionMethod) {
	case 0:
		newFile->decompressedData = std::move(compressedData);
		break;
	case 8: // Deflate
		newFile->decompressedData = DecompressData(std::make_pair(newFile->uncompressedSize, std::move(compressedData)));
		break;
	}

	return newFile;
}

bool WriteFile(const std::string& extractPath, const ZIPFileData& data)
{
	const std::string finalWritePath = extractPath + "/" + data.filepath;
	PD2HOOK_LOG_LOG("Extracting to " << finalWritePath);
	Util::EnsurePathWritable(finalWritePath);

	std::ofstream outFile(finalWritePath.c_str(), std::ios::binary);
	if (!outFile)
	{
		return false;
	}

	outFile.write(data.decompressedData.data(), data.uncompressedSize);
	return true;
}
}

bool ExtractZIPArchive(const std::string& path, const std::string& extractPath)
{
	ByteStream mainStream(path);

	std::list<std::unique_ptr<ZIPFileData>> files;
	{
		std::unique_ptr<ZIPFileData> file;
		while (file = ReadFile(mainStream))
		{
			files.push_back(std::move(file));
		}
	}

	bool result = true;
	std::for_each(files.cbegin(), files.cend(), [extractPath, &result](const std::unique_ptr<ZIPFileData>& data) { result &= WriteFile(extractPath, *data); });
	return result;
}
}
