if ... then
  local require, type, match = require, type, string.match
  return function(base)
    if type(base)~='string'then base=''end
    local loc = match(base, '^(.+)%..-$')
    if not loc then loc = '' else loc = loc .. '.'end
    local rel = base ..'.'

    return 
      -- local, requiring sibbling module
      function(modname)return require(loc .. modname)end,
      -- relative, requiring child module
      function(modname)return require(rel .. modname)end
  end
end  