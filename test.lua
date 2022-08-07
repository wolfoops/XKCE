-- 123
local S = string
local sub, rep, fms = S.sub, S.rep, S.format
local select = select
local function prt(...)return print(sub(fms(rep('\t%s',select('#',...)),...),2))end
prt('in test:',select('#',...),...)
prt'print test'
return 'a test'
