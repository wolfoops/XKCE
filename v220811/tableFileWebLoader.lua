----
local function getWebFile(url)
  local inet, r = getInternet()
  if inet and url then
    r = inet.getURL(url), inet.Destroy()
    if type(r)~='string' or r:find'^%d+'then
      return nil, r and tostring(r)or 'unknown error'
    else
      return r
    end
  end
end

local mod_cache = {}

local function modLoader(mod, url)return mod_cache[mod] end

local function tableFileWebSearcher(...)
  local pt = type(...)
  local urlRoots = pt=='string'and {...} or pt=='table'and ...
  if not urlRoots then return nil,'invalid input : url '..tostring(... or nil)end
  return function(mod)
    local tf, url, ok, r, root = findTableFile(mod)
    if not tf then
      for _, urlRoot in ipairs(urlRoots)do
        root = urlRoot..mod:gsub('%.','/')
        for _,p in ipairs{'.lua','/init.lua'} do
          url = root ..p
          ok, r = pcall(getWebFile, url)
          if ok and type(r)=='string' then
            tf = createTableFile(mod)
            if tf then tf.Stream.writeAnsiString(r); break end
          end
        end
        if tf then break end
      end
    else
      r = tf.Stream.readAnsiString()
    end
    if tf and r then
      mod_cache[mod], ok, r  = nil, pcall(load,r,'-',nil,_G)
      if ok then ok, r = pcall(r, mod, url)end
      if ok then
        mod_cache[mod] = r or true
        return modLoader, url
      else
        error('compile error : '..mod..' ; '..tostring(r),3)
      end
    end
    return '** tablefile loader failed: '
           ..tostring(mod)..' @ '
           ..tostring(url)
  end
end
---
local pkg_myloader_position = 1+#package.searchers
return function(...)package.searchers[pkg_myloader_position] = tableFileWebSearcher(...)end
