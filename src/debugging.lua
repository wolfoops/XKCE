-- debugging : global debuging state
local in_debug 
local match, type, require, print = string.match, type, require, print

if type(... or nil)=='string' and match(...,'[_%a]')then
  ---------------------- module start
  return setmetatable({},{
    __call = function(me)-- toggle, return previus state
      in_debug = not in_debug
      return not in_debug 
    end,
    __index = {
      get = function(me)return in_debug end,
      set = function(me,v) 
        in_debug = v and true or false
        return me
      end,
      p = function(me, ...)if in_debug then print(...)end return me end,
      assert = function(me, ...)if in_debug then assert(...)end return me end
    },
    __newindex = function()error('no assignment',2)end
  })
  ---------------------- Module end
end
  ---------------------- test 
print('arg[0]:',arg and arg[0])  

local mm = require'debugging'

local pp = mm()
local qq = mm()
print(pp, qq)
print'-- should hide'
mm:p('test hide')
mm()
print'-- should show'
mm:p('test show')
