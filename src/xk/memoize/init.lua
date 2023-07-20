-- xk.memoize
local match, type, require, print = string.match, type, require, print
if type(... or nil)=='string' and match(...,'[_%a]')then
  ---------------------- module start
  return setmetatable({},{__mode='k'})
  
  ---------------------- Module end
end
  ---------------------- test 
print('arg[0]:',arg and arg[0])  