function callnext(delay, ...)if type(delay)=='number'then return synchronize(createTimer, delay,...)else return synchronize(createTimer, 1, delay, ...)end end
function xcLuaLoad(urlBase, env)
  local pkg, inet, busy = package, getInternet()
  local prefixXKCE, prefixUTIL = 'xkce.','xkce.util.'
  local function doload(Name, ...)
    local cnt, timeOut, n, tn = 0, os.clock()+60.0, select('#',...), type(Name)
    if tn=='table'then
      while timeOut>os.clock() do
        for i=1,#Name do if doload(Name[i]) then cnt = cnt + 1 end end
        if cnt==#Name then break else cnt=0 end
        sleep(250)
      end--while timeOut
      if cnt~=#Name then return nil,'Not all module loaded on timeout' end
    elseif tn~='string' then return nil,'module name not a string'else
      local name = Name:gsub('^%.',prefixXKCE):gsub('^%#',prefixUTIL)
      local mod, path, loader, src, ok, savetf, url, ret = pkg.loaded[name]
      if not mod then-- from table file
        src = findTableFile(name) -- from embedded table file
        src = src and src.Stream.readAnsiString()
        if not src then--from disk
          savetf, ok, loader, path = true, pcall(pkg.searchers[2], name)
          if ok and type(loader)=='function'and type(path)=='string'then
            src = io.open(path,'rb')
            src = src and src:read'*a', src and src:close()
          end
        end-- from disk
        if not src then--from inet
          if busy then callnext(math.random(250,500), doload, Name, ...); return end
          url = (urlBase or 'https://raw.githubusercontent.com/wolfoops/XKCE/master/')..name:gsub('%.','/')..'.lua'
          busy = true
          src = inet.getURL(url)
          busy = false
        end-- from inet
        if src then-- load src as module
          ok, ret = pcall(load, src, string.format('=(module:%s)',name),nil,env or _G)
          if ok then ok, mod = pcall(ret, name)end
          mod = ok and (mod or true)
        end
        if savetf and src and mod then-- save src as table file so no inet need
          local tf = createTableFile(name)
          tf.Stream.writeAnsiString(src)
        end
        if not mod then return elseif not pkg.loaded[name] then pkg.loaded[name] = mod end
      end
    end -- type(name)~='table'
    if n>0 then callnext(...)end
    return true
  end -- doload

  function _import(Name)
    if type(Name)~='string' then return nil,'module name not a string'else
      local name = Name:gsub('^%.',prefixXKCE):gsub('^%#',prefixUTIL)
      local mod = pkg[name]
      if mod then return mod else
        local timeOut = os.clock()+30.0
        while timeOut>os.clock() do if doload(Name) then break else sleep(250)end end
        mod = pkg[name]
        if mod then return mod else return nil,'module '..Name..' not loaded' end
      end
    end
  end--_import

  return doload
end -- LuaLoad
if not LuaLoad then LuaLoad = xcLuaLoad end
