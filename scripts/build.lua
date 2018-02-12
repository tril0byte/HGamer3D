local glue = require("glue")
local ffi = require("ffi")
glue.luapath(glue.bin .. "/lualib")

require("lfs")

local function getOS()
	local os = string.lower(ffi.os)
	local arch
	if ffi.arch == "x64" then
		arch = "amd64"
	else
		arch = ffi.arch
	end
	return os, arch 
end

function pathSep()
	o, a = getOS()
	if o == "windows" then
		return '\\'
	else 
		return '/'
	end
end

function getPlatString(name, version)
	o, a = getOS()
	return (name .. '-' .. a .. '-' .. o .. '-' .. version)
end

-- local utility functions

local function aioString()
	o, a = getOS()
	if o == "windows" then
		return "..\\" .. glue.bin .. "\\win\\aio.exe"	
	elseif o == "osx" then
		return "../" .. glue.bin .. "/darwin/aio"	
	elseif o == "linux" then
		return "../" .. glue.bin .. "/linux/aio"	
	end
end

-- get version of gamegio from CMakeLists file

local function versionGameGio()
	cmakeFile = glue.bin .. "/../gamegio-library/src/CMakeLists.txt"
	io.input(cmakeFile)

	local major = ""
	local minor = ""
	local patch = ""

	while true do
		local line = io.read()
		if line == nil then break end
		local r = string.match(line, "VERSION_MAJOR (.*)%)")
		if r then
			major = r
		end
		local r = string.match(line, "VERSION_MINOR (.*)%)")
		if r then
			minor = r
		end
		local r = string.match(line, "VERSION_PATCH (.*)%)")
		if r then
			patch = r
		end
	end

	return major .. "." .. minor .. "." .. patch
end


local function runSample()
	o, a = getOS()
	if o == "windows" then
		os.execute(aioString() .. " start http://www.hgamer3d.org/tools/Run.0517 Samples.exe")
		os.exit(0)
	else
		os.execute(aioString() .. " start http://www.hgamer3d.org/tools/Run.0517 ./Samples")
		os.exit(0)
	end
end



local function initBuildSystem()
	lfs.chdir(glue.bin)	
	os.execute(aioString() .. " http://www.hgamer3d.org/tools/Stack.0617 setup --resolver lts-8.20")
end


local function buildGameGio()
	-- change into build dir
	local currDir = lfs.currentdir()
	local version = versionGameGio()
	o, a = getOS()
	lfs.chdir(glue.bin .. "/..")
	lfs.mkdir("gamegio-build") 
	lfs.chdir("gamegio-build")

	-- build dll
	if o == "windows" then
		os.execute(aioString() .. " http://www.hgamer3d.org/tools/Urho3D-1.6 cmd /C cmake ../gamegio-library/src -G \"Visual Studio 15 2017 Win64\"")
		os.execute("cmake --build . --config Release")
	else
		os.execute(aioString() .. " http://www.hgamer3d.org/tools/Urho3D-1.6 bash -c \"cmake ../gamegio-library/src\"")
		os.execute("cmake --build . --config Release")
	end

	-- package component
	lfs.mkdir("package")
	local plat = getPlatString("gamegio", version)
	lfs.mkdir("package/" .. plat)
	if o == "windows" then
		local platdir = "package\\" .. plat 
		os.execute("copy ..\\gamegio-library\\arriccio.toml package\\arriccio.toml")
		os.execute("copy Release\\game_gio_lib.dll " .. platdir .. "\\game_engine.gio")
	elseif o == "osx" then
		local platdir = "package/" .. plat
		os.execute("cp ../gamegio-library/arriccio.toml package/arriccio.toml")
		os.execute("cp libgame_gio_lib.dylib " .. platdir .. "/game_engine.gio")
	else
		local platdir = "package/" .. plat
		os.execute("cp ../gamegio-library/arriccio.toml package/arriccio.toml")
		os.execute("cp libgame_gio_lib.so " .. platdir .. "/game_engine.gio")
	end
	-- change dir back
	lfs.chdir(currDir)
end

local function buildHGamer3D()
	lfs.chdir("src")
	os.execute(aioString() .. " http://www.hgamer3d.org/tools/Stack.0617 build")
	--os.execute("aio http://www.hgamer3d.org/tools/Stack.0617 sdist")
	os.exit(0)
end

local function buildSamples()
	lfs.chdir("samples")
	os.execute(aioString() .. " http://www.hgamer3d.org/tools/Stack.0617 install --local-bin-path .")
	--os.execute("aio http://www.hgamer3d.org/tools/Stack.0617 sdist")
	os.exit(0)
end

local function helpText()
	print([[

HGamer3D build script, usage:

build <command>

command might be:
  init-build-system
  samples
  run-samples
  HGamer3D
  gamegio
  version-gamegio
  register-gamegio
  unregister-gamegio
	]])
end

-- main script

if #arg > 0 then

	-- prepare host for building
	if arg[1] == "gamegio" then
		buildGameGio()
		os.exit(0)

	-- version of gamegio component
	elseif arg[1] == "init-build-system" then
	 	initBuildSystem()	
		os.exit(0)

	-- version of gamegio component
	elseif arg[1] == "version-gamegio" then
		print(versionGameGio())
		os.exit(0)

	-- register component for local use
	elseif arg[1] == "register-gamegio" then
		lfs.chdir("gamegio-build")
		os.execute(aioString() .. " local http://www.hgamer3d.org/component/HG3DEngineGio.0517 package")
		os.exit(0)

	-- unregister component for local use
	elseif arg[1] == "unregister-gamegio" then
		lfs.chdir("gamegio-build")
		os.execute(aioString() .. " remove-local http://www.hgamer3d.org/component/HG3DEngineGio.0517")
		os.exit(0)

	-- build HGamer3D
	elseif arg[1] == "HGamer3D" then
		buildHGamer3D()
		os.exit(0)

	-- build HGamer3D
	elseif arg[1] == "samples" then
		buildSamples()
		os.exit(0)

	elseif arg[1] == "run-samples" then
		lfs.chdir("samples")
		runSample()
	end


	print("wrong argument to build script:", arg[1])

-- give hints about commands
end

-- in case no command exits, still give help and exit then
helpText()
os.exit(-1)




