#ifndef __SIGNATURE_HEADER__
#define __SIGNATURE_HEADER__

#include <string>
#include <vector>

#define CREATE_CALLABLE_SIGNATURE(name, retn, signature, mask, offset, ...) \
	typedef retn(*name ## ptr)(__VA_ARGS__); \
	name ## ptr name = NULL; \
	SignatureSearch name ## search(&name, signature, mask, offset);


struct SignatureF {
	const char* signature;
	const char* mask;
	int offset;
	void* address;
};

class SignatureSearch {
public:
	SignatureSearch(void* address, const char* signature, const char* mask, int offset);
	static void Search();
};




#endif // __SIGNATURE_HEADER__