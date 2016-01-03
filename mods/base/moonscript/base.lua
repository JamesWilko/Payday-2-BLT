local compile = loadmodule("moonscript.compile")
local parse = loadmodule("moonscript.parse")
local concat, insert, remove
do
  local _obj_0 = table
  concat, insert, remove = _obj_0.concat, _obj_0.insert, _obj_0.remove
end
local split, dump, get_options, unpack
do
  local _obj_0 = loadmodule("moonscript.util")
  split, dump, get_options, unpack = _obj_0.split, _obj_0.dump, _obj_0.get_options, _obj_0.unpack
end
local CompileString = loadstring
local dirsep, line_tables, to_lua, loadstring, loadfile, dofile
dirsep = "/"
line_tables = loadmodule("moonscript.line_tables")
to_lua = function(text, options)
  if options == nil then
    options = { }
  end
  if "string" ~= type(text) then
    local t = type(text)
    return nil, "expecting string (got " .. t .. ")"
  end
  local tree, err = parse.string(text)
  if not tree then
    return nil, err
  end
  local code, ltable, pos = compile.tree(tree, options)
  if not code then
    return nil, compile.format_error(ltable, pos, text)
  end
  return code, ltable
end

loadstring = function(...)
  local options, str, chunk_name = get_options(...)
  chunk_name = chunk_name or "LoadStringMS"
  local code, ltable_or_err = to_lua(str, options)
  if not (code) then
    return nil, ltable_or_err
  end
  if chunk_name then
    line_tables[chunk_name] = ltable_or_err
  end
  return CompileString(code, chunk_name, false)
end
loadfile = function(fname, loc, ...)
  local fdata = file.Read(fname, loc)
  if not (fdata) then
    return nil
  end
  return loadstring(fdata, "@" .. tostring(fname), ...)
end
dofile = function(fname, loc, ...)
  local fn = loadfile(fname, loc, ...)
  assert(type(fn) == "function", fn)
  return fn()
end
return {
  _NAME = "moonscript",
  to_lua = to_lua,
  dirsep = dirsep,
  dofile = dofile,
  loadfile = loadfile,
  loadstring = loadstring
}
