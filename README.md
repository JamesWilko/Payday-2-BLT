# Raid BLT
An open source Lua hook for Raid, designed and created for ease of use for both players and modders.  
This is the developer repository.
The Lua component of the BLT which controls mod loading can be found in it's own repository, [Payday-2-BLT-Lua](https://github.com/JamesWilko/Payday-2-BLT-Lua).

## Documentation
Documentation for the BLT can be found on the [GitHub Wiki](https://github.com/JamesWilko/Payday-2-BLT/wiki) for the project.

## Dependencies
Raid BLT requires the following dependencies, which are all statically linked.
* OpenSSL
* cURL
* zlib
* Detours

### OpenSSL
OpenSSL should be compiled as static libraries and the libraries placed in the lib directory, and the headers in the incl directory

### cURL
cURL should be compiled as static, with the WITH_SSL parameter set to 'static' and the previously compiled OpenSSL libraries in the dependencies directory.

### zLib
zLib should be compiled as static.
I had to add SAFESEH handling to the MASM objects in order for this to be compatible with Payday2-BLT

### Detours
A compiled version of detours is included, and all terms of the included Microsoft Research Shared Source License Agreement (detours_license.rtf) are applicable.

## Contributors
- Payday 2 BLT Team
	* [James Wilkinson](http://jameswilko.com/) ([Twitter](http://twitter.com/_JamesWilko))
	* [SirWaddlesworth](http://genj.io/)
	* [Will Donohoe](https://will.io/)

- Contributors, Translators, Testers and more
	* saltisgood
	* Kail
	* Dougley
	* awcjack
	* BangL
	* chromKa
	* xDarkWolf
	* Luffyyy
	* NHellFire
	* TdlQ
	* Mrucux7
	* Simon
	* goontest
	* aayanl
	* cjur3
	* Kilandor
	* Joel Juvél
	* PlayYou
	* and others who haven't been added yet
