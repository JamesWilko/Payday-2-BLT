#include <zlib.h>
#include "util.h"


ByteStream::ByteStream(std::string path){
	mainStream = std::ifstream(path.c_str(), std::ifstream::binary);
}

ByteStream::~ByteStream(){
	mainStream.close();
}

template<typename T>
T ByteStream::readType(){
	T read;
	mainStream.read((char*)&read, sizeof(T));
	return read;
}

std::string ByteStream::readString(int length){
	char* readData = new char[length + 1];
	mainStream.read(readData, length);
	std::string strData(readData, length);
	delete[] readData;
	return strData;
}

ZIPArchive::ZIPArchive(std::string path, std::string extractPath) : mainStream(path){
	extractTo = extractPath;
}

ZIPArchive::~ZIPArchive(){
	std::list<ZIPFileData*>::iterator it;
	for (it = readFiles.begin(); it != readFiles.end(); it++){
		delete (*it);
	}
}

void ZIPArchive::ReadArchive(){
	while (ReadFile()) {

	}

	std::list<ZIPFileData*>::iterator it;
	for (it = readFiles.begin(); it != readFiles.end(); it++){
		WriteFile(*it);
	}
}

bool ZIPArchive::ReadFile(){
	int fileHeader = mainStream.readType<int>();
	if (fileHeader != 0x04034b50) return false;

	printf("Version Needed: %d.%d\n", mainStream.readType<char>(), mainStream.readType<char>());

	// Ignore 'general purpose bit flag'
	mainStream.readType<short>();

	int compressionMethod = mainStream.readType<short>();

	// Ignore modified file times.
	mainStream.readType<int>();

	//return true;
	int crc32 = mainStream.readType<int>();

	ZIPFileData* newFile = new ZIPFileData();
	newFile->compressedSize = mainStream.readType<int>();
	newFile->uncompressedSize = mainStream.readType<int>();

	int fileNameLength = mainStream.readType<short>();
	int extraFieldLength = mainStream.readType<short>();

	newFile->filepath = mainStream.readString(fileNameLength);
	std::string extraField = mainStream.readString(extraFieldLength);

	newFile->compressedData = mainStream.readString(newFile->compressedSize);

	switch (compressionMethod){
	case 0:
		newFile->decompressedData = newFile->compressedData;
		break;
	case 8: // Deflate
		DecompressFile(newFile);
		break;
	}

	readFiles.push_back(newFile);
	return true;
}

bool ZIPArchive::WriteFile(ZIPFileData* data){
	std::string finalWritePath = extractTo + "/" + data->filepath;
	Logging::Log("Extracting to " + finalWritePath);
	Util::EnsurePathWritable(finalWritePath);
	std::ofstream outFile;
	outFile.open(finalWritePath.c_str(), std::ios::out | std::ios::binary);
	if (!outFile.good()){
		outFile.close();
		return;
	}
	outFile.write(data->decompressedData.c_str(), data->uncompressedSize);
	outFile.close();
	return true;
}

void ZIPArchive::DecompressFile(ZIPFileData* data){

	z_stream stream;

	stream.zalloc = Z_NULL;
	stream.zfree = Z_NULL;
	stream.opaque = Z_NULL;

	stream.avail_in = 0;
	stream.next_in = Z_NULL;

	int ret = inflateInit2(&stream, -MAX_WBITS);

	stream.avail_in = data->compressedSize;
	stream.next_in = (unsigned char*)data->compressedData.c_str();

	unsigned char* out = new unsigned char[data->uncompressedSize + 1];

	stream.avail_out = data->uncompressedSize;
	stream.next_out = out;

	ret = inflate(&stream, Z_NO_FLUSH);
	inflateEnd(&stream);

	data->decompressedData = std::string((char*)out, data->uncompressedSize);

	delete[] out;
}