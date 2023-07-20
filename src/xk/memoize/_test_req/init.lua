-- xk.memoize._test_req
local match, type, require, print = string.match, type, require, print
if type(... or nil)=='string' and match(...,'[_%a]')then
  ---------------------- module start
  
  local child, sibbling, parent = require'req'(...)
  print(child, sibbling, parent)
  print(parent'vargs')
  print(sibbling'simple.simple1')
  print(child'req_child')
  return '@xk.memoize'
  
  ---------------------- Module end
end
  ---------------------- test   
print('arg[0]:',arg and arg[0])
local test = require'xk.memoize._test_req'