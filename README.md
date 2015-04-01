# Payday 2 BLT
An open source Lua hook for Payday 2, designed and created for ease of use for both players and modders.  
This is the developer repository, and should only be used if you know what you're doing. If you don't, visit the website at [PaydayMods.com](http://paydaymods.com/) to get an up-to-date drag-drop install.  

## Download
Visit [PaydayMods.com](http://paydaymods.com/) to get the latest stable download.  

## Dependencies
Payday2 BLT requires the following dependencies, which are all statically linked.
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

## Documentation
All documentation can be found via the navigation bar the Payday Mods site, or just [go to it directly](http://payday-2-blt-docs.readthedocs.org/en/latest/). It's also available on [GitHub](https://github.com/JamesWilko/Payday-2-BLT-Docs) (or click the 'Edit on GitHub' button) so that you can contribute stuff if you need to, or need us to fix something.  
The documentation is written in [markdown](http://daringfireball.net/projects/markdown/) with the help of [MkDocs](http://www.mkdocs.org/) and [ReadTheDocs](https://readthedocs.org/).  

## Developers
The Payday 2 BLT was made by [James Wilkinson](http://jameswilko.com/) and [SirWaddlesworth](http://sirwaddlesworth.com/). We're friendly guys sometimes, so if you need help or want to discuss something you can reach us at any of the contact details we've got listed [over here](http://paydaymods.com/contact/).  
We've also had help from a bunch of other people who've tested stuff, reported bugs, suggested changes, and everthing else. So thanks to GREAT BIG BUSHY BEARD, Kail, Dougley, and everybody else!

## Client Compilation
BLT can be compiled with the preprocessor flag BLT_AS_CLIENT set which will produce a library that can be dropped into the lib/Native folder used by the existing hook. This results in providing all the required functionality for mods based around BLT without sacrificing compatibility with the existing hook's features. A binary version exists at the root of the project called PD2BLT.dll
