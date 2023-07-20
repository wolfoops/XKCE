-- xk.memoize.simple.recurrent
--:  memoize function for function of 1 input 1 output, but with 
--:  1st argument in function definition the memoized object
--:  ie. memoed = require'xk.memoize.simple'(function(mm, arg1)...end)
--:    where mm is memoed, this allow memoize recursive function
--:    the only difference from *.simple is the pcall has mm as 1st parameter.
--:  Similary, memoed (memoed) to clear cache
local D = require'debugging'
local match, type, require, print = string.match, type, require, print

if type(... or nil)=='string' and match(...,'[_%a]')then
  ---------------------- module start
  local pairs, pcall, error, tostring, rawset, setmetatable = 
        pairs, pcall, error, tostring, rawset, setmetatable
  
  local fns_store = require'xk.memoize'
  local memo = setmetatable({},{
    __call = function(me, fn, pre_seeded)
      local mm = fns_store[fn]
      if not mm then
        local cache = {}
        if type(pre_seeded)=='table' then 
          for k,v in pairs(pre_seeded)do
            cache[k] = v
          end          
        end
        mm = setmetatable(cache, {
          __call = function(me, arg1)
            if arg1 == me then 
              -- if arg1 is the memoed function self, clear the cache
              D:p('clearing cache, me:', me, 'arg1:', arg1)
              for k in pairs(me)do me[k]=nil end
              return me
            end            
            return me[arg1]
          end,
          __index = function(me, arg1)
            local ok, ret = pcall(fn, me, arg1)
            if not ok then
              error(ret and tostring(ret) or 'unknown error',4)
            elseif ret==ret and ret~=nil then -- store only if not NaN or nil
              D:p('new:',arg1,ret)
              rawset(me, arg1, ret)
            end
            return ret
          end          
        })
        D:p('new memo:',fn)
        rawset(fns_store, fn, mm)
      end
      return mm
    end,
  })  
  return memo
  ---------------------- Module end
end
  ---------------------- test 
print('arg[0]:',arg and arg[0])  

require'debugging':set(true)

local mm = require'xk.memoize.simple.recurrent'
local function fac(me, n)
  if n < 2 then
    return 1
  else
    return n * me[n-1]
  end
-- not this, which is not tail-call  
-- return n < 2 and 1 or n * me[n-1]
end
local msqx = mm(fac)
for i=-3,3 do
  local j = i*i
  print(i, msqx(i), '--',j, msqx(j))
end
print'---- memo same fn'
local msqy = mm(fac)
for i=-3,3 do
  local j = i*i
  print(i, msqy(i), '--',j, msqy(j))
end

print'test no debug print'
do
  require'debugging':set(false)

  local mm = require'xk.memoize.simple.recurrent'
  
  local msqx = mm(fac)
  for i=-3,3 do
    local j = i*i
    print(i, msqx(i), '--',j, msqx(j))
  end
  print'---- memo same fn'
  local msqy = mm(fac)
  for i=-3,3 do
    local j = i*i
    print(i, msqy(i), '--',j, msqy(j))
  end
end




