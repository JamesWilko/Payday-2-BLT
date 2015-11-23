# Payday 2 BLT with Moonscript Support
An open source Lua hook for Payday 2, designed and created for ease of use for both players and modders.  
This is the developer repository, and should only be used if you know what you're doing. If you don't, visit the website at [PaydayMods.com](http://paydaymods.com/) to get an up-to-date drag-drop install.  

Now BLT comes with [Moonscript](http://moonscript.org) support, which I find
a little more pleasant to read than Lua. The moonscript code base is almost a 
clone of [GMod Moonscript](https://github.com/wyozi/gmod-moonscript). Many thanks 
to its author wyozi and its original contributor Mijyuoon and corresponding 
project [starfall](https://github.com/Mijyuoon/starfall).

## Loading Moonscript
Moonscript are treated almost identical to lua -- while previously you specify
in the script field with "script.lua", you can put "script.moon". The mod
manager with load the script based on its extension, for example:

~~~~~~~~~~~~~~~~~~~~~
	"hooks" : [
		{ 	"hook_id" : "lib/managers/menumanager",
			"script_path" : "json_example.moon"
		}
	]
~~~~~~~~~~~~~~~~~~~~~

Since moonscript is compatible with lua, lua functions like "CloneClass" etc can
be used inside moonscript.

## Todo
The error handling part of current moonscript loader is horrible. The loader
seems to be quite permissive for loading ill-behaved moonscript files, which
should be enhanced in the future.

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
