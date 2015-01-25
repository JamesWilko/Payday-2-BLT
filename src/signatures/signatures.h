#ifndef __SIGNATURE_HEADER__
#define __SIGNATURE_HEADER__

#include <string>
#include <vector>

#define CREATE_CALLABLE_SIGNATURE(name, retn, signature, mask, offset, ...) \
	typedef retn(*name ## ptr)(__VA_ARGS__); \
	name ## ptr name = NULL; \
	static SignatureSearch name ## search(&name, signature, mask, offset);


struct SignatureF {
	std::string signature;
	std::string mask;
	int offset;
	void* address;
	void* target;
};

class SignatureSearch {
public:
	SignatureSearch(void* target, std::string signature, std::string mask, int offset);
	static std::vector<SignatureF> signatures;
	static void Search();
};




#endif // __SIGNATURE_HEADER__