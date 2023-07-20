-- req : return child/sibbling/parent relative module requiring function
local match, type, require, print = string.match, type, require, print
if type(... or nil)=='string' and match(...,'[_%a]')then
  ---------------------- module start
  local gsub = string.gsub
  return function(base) 
    if type(base)~='string'then base=''end
    
    local _, dotcnt = gsub(base,'%.','%1')
    local sibbling = dotcnt==0 and '' or match(base, '^(.+%.)[^%.]-$')
    local child = base~='' and base ..'.'
    local parent = dotcnt >1 and match(sibbling, '^(.+%.)[^%.]-%.$')
    --print(base, dotcnt, child, sibbling, parent)
    return 
      -- requiring from child module
      child and function(modname)return require(child .. modname)end,
      -- requiring from sibbling module     
      function(modname)return require(sibbling .. modname)end,
      -- requiring from parent module     
      parent and function(modname)return require(parent .. modname)end     
  end
  
  ---------------------- Module end
end
  ---------------------- test 
print('arg[0]:',arg and arg[0])  