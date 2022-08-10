
if getCEVersion()<7.1 then MessageDialog('require CE 7.1+',1,4) error'oops'end

inDebug = true

if not _AA_Custom_Inited then--- init aa custom

local backtick = string.char(96)
local backslash = string.char(47)

---------------------------------------------------------------------------
--- mono_findClass/Method EX
---------------------------------------------------------------------------

function mono_findClassEX(cn, asm)
  if type(cn)=='number' then return cn
  elseif not monopipe then
    pcall(LaunchMonoDataCollector)
    if not monopipe then return end
  end

  monopipe.lock()

  local as = mono_enumAssemblies()
  if type(cn)~='string'or type(as)~='table'then return end
  for j=1,#as do
    local im = mono_getImageFromAssembly(as[j])
    local nm = mono_image_get_name(im)
    im = nm==asm and im or asm==nil and im
    local c= im and mono_image_enumClasses(im)

    if c then
      for i=1, #c do
        --check that classname is in c[i].classname
        local ccn, cqn = c[i].classname, mono_class_getFullName(c[i].class)
        if ccn==cn or cqn==cn then
          monopipe.unlock()
          return c[i].class
        end
      end
      for i=1, #c do
        --check that classname is in c[i].classname
        local ccn, cqn = c[i].classname, mono_class_getFullName(c[i].class)
        if type(ccn)=='string' and ccn:find(cn,1,true) or type(cqn)=='string' and cqn:find(cn,1,true) then
          monopipe.unlock()
          return c[i].class
        end
      end
    end

  end
  monopipe.unlock()

end




function monoFindClassEX(assm)
  return function(class)return mono_findClassEX(class,assm)end
end


function mono_findMethodEX(...)
  local n, mth, ks, asm = select('#',...),...
  if type(mth)=='string' and mth:find':'then
    if n>1 then asm = ks end
    ks, mth = mth:match'^([^:]+):(.-)$'
  end
  if type(asm)~='string' then asm = nil end
  if type(ks)=='string' then ks = mono_findClassEX(ks, asm)end
  if type(ks)=='number' and type(mth)=='string'then
    mth = mono_class_findMethod(ks,mth)
  end
  if type(mth)=='number' and math.tointeger(mth) and mth>0 then
    return mono_compile_method(mth)
  end
end
function monoFindMethEX (class)
  return function(meth) return mono_findMethodEX(meth, class) end
end
function monoFindMethAX (assm)
  return function(class)
    return function(meth) return mono_findMethodEX(meth, class, assm) end
  end
end


function mono_findOffsetEX(...)
  local n, fld, ks, asm = select('#',...),...
  fld = fld or '<nullfield>'
--  if type(fld)~='string' then return nil,'field name not string' end
  if type(asm)~='string' then asm = nil end
  if type(ks)=='string' then ks = mono_findClassEX(ks, asm)end
  if type(ks)=='number' and ks>0 then
    local fs = mono_class_enumFields(ks, true)
    local pat = fld:gsub('^$()%.[]*+-?','%%%1')
    if pat:sub(1,2)=='%^'then pat = '^'..par:sub(3)end
    if pat:sub(-2,-1)=='%$'then pat = par:sub(1,-3)..'$'end
    pat = pat:match'^%s*(.-)%s*$'
--    print('pat:','['..pat..']')
    local static, enums, inst, result = {},{},{}
    if fs then
      for i=1,#fs do
        local e = fs[i]
        local n,o = e.name, e.offset
        if type(n)=='string' and type(o)=='number' then
          fs[n]=i
          if not result and n:find(pat)then result = o end
          if e.isStatic then
            local fx = e.flags

            if e.isConst and (fx & 0x40)>0 -- attr_FAMILY or unknown flag?
              and (fx & 0x1)==0            -- not private
              and (fx & 0x8000)>0          -- attr_HAS_Default
--            and isStatic  -- attr_Static or attr_RVA
            then
              enums[1+#enums]=n
            else
              static[n]=o
            end
          else
            inst[n]=o
          end
        end
      end
      for i=1,#enums do enums[enums[i]]=i-1 end

      return result,inst, enums, static, fs
    end
  end
end

function monoFindOffsetEX(class)
  return function(fld)return mono_findOffsetEX(fld, class)end
end
function monoFindOffsetAX(assm)
  return function(class)
    return function(fld)return mono_findOffsetEX(fld, class, assm)end
  end
end

---------------------------------------------------------------------------
--- cutsomJumpCall
---------------------------------------------------------------------------
if not _cutsomJumpCall then
_pidContext = _pidContext or {now=os.clock()+3; reset=os.clock()+60}
function getPidContext()
  local p, pid, now = _pidContext, getOpenedProcessID(), os.clock()
  if now > p.reset then
    p.reset = now + 60
    local ps = getProcessList()
    for k,v in pairs(p)do
      if type(k)=='number' and (not ps[k] or ps[k]~=v[1]) then p[k] = nil end
    end
  end
  if now > p.now and not readInteger(process)then
    return {}
  elseif p[pid] and now<p.now and p.pid == pid and p.process==process then
    return p[pid][2]
  elseif not readInteger(process) then
    return {}
  end
  p.now, p.pid, p.process = now+3, pid, process
  if not p[pid] or p[pid][1]~=process then
    local px = enumModules()[1]
    p[pid] = {process,{PID=pid,PROCESS=process,ExeBase=px.Address, ExeFile=px.PathToFile,ExeSize=getModuleSize(process)}}
  end
  return p[pid][2]
end
getPidContext()
local EMPTY = {}
local function toInt(n)return type(n)=='number'and math.tointeger(n)end
function GetTrampoline(target, hint, noNewAlloc)
  if not toInt(target)or not toInt(hint) then return nil end
  local p = getPidContext()
  p.Trampoline = p.Trampoline or {}
  local t,tcnt, diff = p.Trampoline,0
  for from, to in pairs(t) do
    diff = from - hint
    if diff>-0x7ffffffb and diff<0x80000005 then
      if to==target then return from end
      if to==EMPTY then
        local bs, r = string.pack('I8I8',0xb8480000000225ff ,target),{}
        for c in bs:gmatch'.'do r[1+#r]=c:byte()end
        if writeBytes(from, r)==16 then
          t[from] = target
          return from
        end
      end
    end
  end
  -- no previous allocation, make new one
  if not noNewAlloc then
    local addr = allocateMemory(0x1000,hint)
    diff = addr and addr - hint
    if not diff or diff<=-0x7ffffffb or diff>=0x7ffff005 then
      if addr then deAlloc(addr)end
      return nil,'fail allocate trampoline'
    end
    p.TrmpAllocCnt = not p.TrmpAllocCnt and 1 or p.TrmpAllocCnt + 1
    for i=0,255 do t[addr+i*16]=EMPTY end
    return GetTrampoline(target, hint, true)
  end
end

if _cutsomJumpCall then
  _cutsomJumpCall = nil,unregisterAssembler(_cutsomJumpCall)
end

_cutsomJumpCall = registerAssembler(function (addr, inst)

  local force, target, isJmp, forceShort, forceNear, forceLong =
    inst:match'^%s*[jJ][mM][pP]%s*!%s*(%a*)%s+(.-)%s*$'
  if target then isJmp = true else
    force, target = inst:match'^%s*[cC][aA][lL][lL]%s*!%s*(%a*)%s+(.-)%s*$'
  end
  if not target then return end

  target = target and target:len()>1 and GetAddressSafe(target)

  if target and force:len()>1 then
    force=force:lower()
    if force=='short' then forceShort = true-- should be redundancy?
    elseif force=='near' then forceNear = true
    elseif force=='long' then forceLong = true
    else target = nil end -- unknown jump distance modifier, error
  end

  if not target then
    return --nil,'invalid :'..inst
  else
    local cmd = isJmp and {0xeb,0xe9,0x25ff} or {0xe8,0xe8,0x08eb0000000215ff}
    local diff, r, bs = target - addr, {}
    if isJmp and (forceShort or not forceNear and not forceLong) and diff>-0x7e and diff <0x82 then
      bs = string.pack('Bb',cmd[1], diff-2)
    elseif not targetIs64Bit() or not forceLong and diff>-0x7ffffffb and diff<0x80000005 then
      bs = string.pack('Bi4',cmd[2],diff-5)
    elseif not forceNear or addr<0x1000 then
      if isJmp then
        bs = string.pack('I6I8',cmd[3],target)
      else
        bs = string.pack('I8I8',cmd[3],target)
      end
    elseif diff<=-0x7ffffffb or diff>=0x80000005 then
      local trmp, errmsg = GetTrampoline(target, addr)
      if not trmp then return nil,(errmsg or '!')..', no trampoline:'..inst end
      bs = string.pack('Bi4',cmd[2], trmp - addr -5)
    end
    if bs then
      for c in bs:gmatch'.' do r[1+#r]=c:byte()end
      return r
    end
  end
end)
end --- cutsomJumpCall
---------------------------------------------------------------------------
--- function : compileByDesc/monoFnDesc
---------------------------------------------------------------------------
function monoOffset(klass)
  local st = monoAA_GETMONOSTRUCT(klass)
  return function(Target)
    local ofs, target, found = 0,Target
    st:gsub('%f[%S](%S+)%s*:%s*resb%s+(%d+)',function(n,d)
      if not found then
        if n==target then found=true else ofs = ofs + tonumber(d) end
      end
    end)
    if found then return ofs,target else
      ofs = 0
      st:gsub('%f[%S](%S+)%s*:%s*resb%s+(%d+)',function(n,d)
        if not found then
          n = n:match'<(%S+)>'
          if n and target:match'^<.+>$' then n = '<'..n ..'>' end
          if n==target then found=true else ofs = ofs + tonumber(d) end
        end
      end)
      if found then return ofs,target else return nil,'not found '..Target end
    end
  end
end

function fmap(fn,...)
  if select('#',...)>0 then return fn((...)),fmap(fn,select(2,...))end
end
function nonNull(e)return type(e)=='number' and e~=0 and e end
function monoMethByParam(fn, pn)
  if type(fn)~='number'then return nil,'monoMethByParam: not number'end
  local cs = mono_method_getClass(fn)
  local mn = mono_method_getName(fn)
--  print(string.format('jit:  %X',cs or 0))
  local ms = cs and mono_class_enumMethods(cs)
--  print('pn:',tostring(pn))
--  print('nc:',tostring(mn),tostring(cs))
  pn = type(pn)=='string' and pn:len()>0 and pn:lower():gsub('%s+','')
--  print(0,tostring(pn))
  if type(pn)=='string'then
    for i=1,#ms do
      local m = ms[i].method
      local n = mono_method_getName(m)
      local _,p = mono_method_getSignature(m)
--      print(1,tostring(p),n)
      p = type(p)=='table' and n==mn and table.concat(p,','):lower():gsub('%s','')
--      print(2,tostring(p))
      if p and n==mn and p:find(pn,1,true)then
        return m
  --      print(string.format('%3d %X %s %s ',i,m,n,tostring(p)))
      end
    end
  end
end

function compileByDesc(...)
  local meth,params,assembly,fn = ...
--  print(1,tostring(meth),2,tostring(params),3,tostring(assembly))
  if params and params:find'%b()'then params=params:sub(2,-2) else
    meth,params,assembly = meth,nil,params
  end
--  print(1,tostring(meth),2,tostring(params),3,tostring(assembly))
  if type(meth)=='string' then
    assembly = assembly or DefaultAssembly or 'Assembly-CSharp'
    fn = mono_findMethodByDesc(assembly,meth)
  end
  fn = nonNull(fn) and params and monoMethByParam(fn, params) or fn
  fn = nonNull(fn) and mono_compile_method(fn)
  fn = nonNull(fn)
  return fn ,nonNull(fn)and string.format('%X',fn)or'oops: '..tostring(meth)
end
monoFnDesc = compileByDesc
---------------------------------------------------------------------------
--- command : eval
---------------------------------------------------------------------------

local function escRE(s)return (s:gsub('(%W)','%%%1'))end
local function escGsub(s,From,To)
  local from,rid = escRE(From),'_'..math.random(0x100000,0xffffff)..'_'
  return (s:gsub(from..from,rid):gsub(from,To):gsub(rid,From))
end
local function RID()return '_'..math.random(0x100000,0xffffff)..'_' end
local function evalLua(xp,ctx)
  local rid = RID()
  xp, ctx = 'return '..escGsub(xp,backtick,"'"),ctx or _G
  local ok,ret = pcall(load,xp,'_',nil,ctx)
  if ok then ok,ret = pcall(ret)end
  if ok then return ret else return nil,ret end
end
function ctxEval(ctx)return function(xp)return evalLua(xp,ctx)end end
local function ALL(t,pred)
  for i=1,#t do if not pred(t[i])then return false end end
  return true
end
local function MAP(t,xfrm)
  local r = {}
  for i=1,#t do r[i]= xfrm(t[i])end
  return r
end
local function hxfmt(n)return string.format('%02X',n)end
local function isByte(n)
  return type(n)=='number' and math.tointeger(n)and n<256 and n>=0
end
function aaDisp(v)
  local tv,sv = type(v),tostring(v)
  local nv,iv,ivn = tv=='number'and v, tv=='number' and math.tointeger(v),
    tv=='number' and v<0 and math.tointeger(-v) and -v
  if tv=='table' and ALL(v,isByte)then return table.concat(MAP(v,hxfmt),' ')
  elseif not nv then return sv
  elseif ivn then return string.format('-0%X',ivn)
  elseif iv then
    if iv<256 then return string.format('%02X',iv)
    else return string.format('0%X',iv)end
  else return sv end
end
function render(tmpl, disp)
  if tmpl:find'<_(%d+)_<%x+>_%1_>' then tmpl = aa_Restore(tmpl) end
  disp = disp or aaDisp
  local txt,rid,n = {},RID(),1
  tmpl = tmpl:gsub('%$%$',rid)
  tmpl:gsub('()$(%b())()',function(pre,s,pos)
    txt[1+#txt] = string.format('%q',tmpl:sub(n,pre-1):gsub(rid,'$'))
    txt[1+#txt] = 'DISP'..s:gsub(rid,'$')
    n = pos
  end)
  txt[1+#txt] = string.format('%q',tmpl:sub(n):gsub(rid,'$'))
  txt = 'function(DISP,CTX)local _ENV=CTX or _G return table.concat({ '..table.concat(txt,',\r\n')..'}) end '
  DISP = evalLua(txt)
  if not DISP then return nil,'not a func:'..txt end
  return function(ctx)
    return DISP(disp,ctx)
  end
end

function aa_eval(s,sc)
  local sym, xpr = s:match('^%s*([.#@_%a][.#@_%w]*)%s*,%s*(.-)%s*$')
  if not sym then return nil,'no symbol:'..s end
--  print(0,type(xpr),xpr)
  xpr = render(xpr)
  xpr = xpr and xpr()
--  print(1,type(xpr),xpr)
  xpr = xpr and aaDisp(type(xpr)=='string' and GetAddressSafe(xpr) or xpr)
--  print(2,type(xpr),xpr)
  return xpr and 'define('..sym..','..xpr..')'
end
function readRIP(a)
  a = type(a)=='number' and a or GetAddressSafe(a)
  return a and readInteger(a,true) and a+readInteger(a,true)+4
end
local cmd = 'Eval'
unregisterAutoAssemblerCommand(cmd)
registerAutoAssemblerCommand(cmd,aa_eval)
---[[
cmd = 'multiDef'

function aa_multiDef(s,sc)
  local prefix, comma, xp = s:match'^%s*([^;%s]*)%s*(;?)(.-)$'
  if comma:len()==0 then -- no prefix
    prefix, xp = nil, prefix..xp
  end
  local ret = evalLua(xp)
  if not ret then return nil,"can't eval : "..xp
  elseif type(ret)~='table'then return nil,"should be table of values, "..xp end
  local r = {}
  for k,v in pairs(ret)do
    if type(k)=='string' and k:match'^[_%a][_%w]*$'then
      local sym = prefix and prefix..'.'..k or k
      r[1+#r] = string.format('define(%s,%s)',sym,aaDisp(v))
    end
  end
  return table.concat(r,'\r\n')
end
unregisterAutoAssemblerCommand(cmd)
registerAutoAssemblerCommand(cmd,aa_multiDef)
--]]

function aa_printcheck(S,sc)
  local s = render(S)or s
  s = s and s() or tostring(s)
  if sc then print((s:gsub(';;;*','\r\n')))
  elseif inDebug then print((s:gsub(';;;*','\r\n')))end
end

local cmd = 'printcheck'
unregisterAutoAssemblerCommand(cmd)
registerAutoAssemblerCommand(cmd,aa_printcheck)

---------------------------------------------------------------------------
--- command : exScan
---------------------------------------------------------------------------

local function extractSymbol(s)
  local symx = s:match'!([.#_%a][.#_%w]*)'
  local sym = symx or s:match'([.#_%a][.#_%w]*)'
  local esym = sym and sym:gsub('%W','%%%1')
  local pat = symx and s:gsub('!'..esym,'%%X'):gsub(esym,'%%X')or
              sym and s:gsub(esym,'%%X')
--  print(sym,pat)
  return sym,pat
end
local scanMethods = {'aobscan','aobscanModule','aobscanRegion'}
local pk,upk,fmt,cat = table.pack,table.unpack,string.format,table.concat
function aa_exscan(S,sc)
  local s = render(S)
  s = s and s() or S
  local ps = {}
  for a in s:gmatch'[^,]+'do ps[1+#ps]=a:match'^%s*(.-)%s*$' end
  local meth, sym, pat, addr, addy = scanMethods[#ps-1], extractSymbol(ps[1])
  if not sym or not meth then
    return nil,'Invalid: '..s
  elseif sc then
    addr, addy = 0x666, 0x666 -- dummy for syntaxcheck
  else
    local uid = sym..'_'..math.random(0x100000,0x999999)..'_'
    if #ps==4 and ps[3]~='-1' and ps[3]:find'^[-+]'then
      ps[3]=ps[2]..ps[3]
    end
    local rhs = #ps>2 and cat(ps,',',2,#ps) or ps[2]
    local exec = fmt([[
  %s(%s)
  registersymbol(%s)
  ]],meth,cat({uid,rhs},','),uid)
    if autoAssemble(exec)and getAddressSafe(uid)then
      addr = getAddressSafe(uid)
      addy = getAddressSafe(fmt(pat,addr))
      unregisterSymbol(uid)
    end
  end
  if not addr or not addy then return nil,'scan fail: '..s end
  return fmt([[
define(%s,%X)
define(%s,%X)
]],sym..'.1',addr, sym,addy)
end
local cmd = 'ExScan'
unregisterAutoAssemblerCommand(cmd)
registerAutoAssemblerCommand(cmd,aa_exscan)
---------------------------------------------------------------------------
Arity = setmetatable({},{__index=function(me,n)
  n = type(n)=='number' and n >=0 and n or 0
  local r = {} for i=1,n do r[i]='_'..i end r = table.concat(r,',')
  return load(string.format('local %s=... return %s',r,r))
end,__call=function(me,n,...)return me[n](...)end})
function toAddr(v)
  if type(v)=='string' then return GetAddressSafe(v) else return v end
end
aa_aob=setmetatable({},{__index = function(me,k,edn)
    if k=='s'then
      return function(s)return s:gsub('.',function(c)return string.format(' %02X',c:byte())end):sub(2)end
    else
      local pat = k:gsub('_',function()edn = edn=='>'and '<' or '>'return edn end)
      return function(...)return me.s(string.pack(pat,fmap(toAddr,...) ))end
    end
  end})

---------------------------------------------------------------------------

math.randomseed(os.time())

function aa_Restore(s)
--  print('restoring:',s)
  s = s:gsub('<_(%d%d%d%d%d)_<(%x+)>_%1_>',function(_,hx)
    return hx:gsub('%x%x',function(h)return string.char(tonumber(h,16))end)
  end)
--  print('result:',s)
  return s
end

function aa_prologue(postAOB)
  local fmt = string.format
  local function escRE(s)return (s:gsub('(%W)','%%%1'))end
  local function hxs(s)
    return s:gsub('.',function(c)return fmt('%02X',c:byte())end):sub(1)
  end
  local function removeComment(s)
    local ss,n = s,1
    s = s:gsub(backslash:rep(2)..'[^\n\r]*',''):sub(1)
    while n and n<=#s do
      local a,b,c,d = s:find(backslash..'%*',n)
      if a then
        c,d = s:find('%*'..backslash,a+1)
      end
      if not a or not d then n=nil else
        s,n = s:sub(n,a-1)..s:sub(d+1),d+1
      end
    end
    if s~=ss then
--      print('comment:\n',ss)
--      print('removed:\n',s)
    end
    return s
  end
  local scanData = postAOB and {
    backtick.."'='","'="..backtick.."","\n","\r"
  } or {
    '{','}'
  }
--  print(tostring(postAOB))
  local function change(withBraket)
--    print(tostring(postAOB))
    local obra = withBraket and {'(',')'} or {'<','>'}
    local rid = '_'..math.random(10000,99999)..'_'
    local fid = fmt('<%s<%%s>%s>',rid,rid)
    local function conv(s)return fmt(fid,hxs(s))end
    return function (inStr)
      inStr = removeComment(inStr)
--      print('cntScanData:',#scanData)
--      print((table.concat(obra,' ') )..(postAOB and 'POST/' or 'PRE/' )..' from:',inStr)
      for i,pat in ipairs(scanData)do
--        print(i,'pat:',pat)
        local from,to = pat:match'^(.-)=(.-)$'
        if not from then from,to = pat,conv end
        from = escRE(from)
--        print('>>',inStr)
        inStr = inStr:gsub(from,to)
--        print(i,'==',inStr)
      end
--      print('to:',inStr)
      if not postAOB then return obra[1]..backtick..inStr..backtick..obra[2] end
      return withBraket and '('..inStr..')' or inStr
    end
  end
  return function(Code, sc)
  if Code.Count==0 or not (Code[0]:match'^////+' or Code[0]:match'^define%(MRID,%d+%)$') then return end
    local code = Code.Text
      :gsub("%("..backtick.."(.-)"..backtick.."%)",change(true))
      :gsub("<"..backtick.."(.-)"..backtick..">",change(false))
    Code.Text = code
  end
end

if AA_Prologue_Post then
  AA_Prologue_Post = nil,unregisterAutoAssemblerPrologue(AA_Prologue_Post)
end
if AA_Prologue_Pre then
  AA_Prologue_Pre = nil,unregisterAutoAssemblerPrologue(AA_Prologue_Pre)
end

AA_Prologue_Pre  = registerAutoAssemblerPrologue(aa_prologue(false))
AA_Prologue_Post = registerAutoAssemblerPrologue(aa_prologue(true))


--------------------------------------------------------------------------


function mr_execute(...)
  local fmt,np,ps,mr,newstate,succeeded =
    string.format,select('#',...),{...},...
  if np==2 then -- preExecute
--    print('preExecute '..tostring(mr.varType)..' '..type(mr.varType))
--    print(tostring(mr.ID)..' '..type(mr.ID))
    if mr.varType == 'vtAutoAssembler' then
      local s = mr.Script
--      print('<<===\n',s,'\n===>>')
      local currentID = s:match'^define%(MRID,(%d+)%)'
      if s:match'^////+' or currentID and mr.ID~=tonumber(currentID) then
--        print(1,tostring(s:match'^////+'))
--        print(2,tostring(s:match'^define%(MRID,%d+%)'))
        local lhs,rhs = s:match'^([^\r\n]+)(.-)$'
        s = fmt('define(MRID,%d) ',mr.ID)
          ..(lhs:match'^.-[/ ]*(//[^/].-)$' or'')..rhs
--        print('<<---\n',s,'\n--->>')
--        synchronize(function()
        mr.Script = s ; mr.Active = newstate
--        end)
      end
    end
  elseif np==3 then -- postExecute
--    print'postExecute'
  else
--  else -- something wrong
--    print'Something wrong...'
    if not mr then
      print('oops no mr?'..tostring(mr))
    else
      print()
      print(fmt('mr id: %s, desc: %s',mr.ID,mr.Description))
      for i=1,np do ps[i]='['..i..'] '..tostring(ps[i])end
      print(fmt('input cnt: %d , inputs: %s',np,table.concat(ps,', ')))
      print()
    end
  end
  if _PrevMRPreExe then _PrevMRPreExe(...)end
  if _PrevMRPostExe then _PrevMRPostExe(...)end
end

_PrevMRPreExe = _PrevMRPreExe or onMemRecPreExecute
_PrevMRPostExe = _PrevMRPostExe or onMemRecPostExecute
onMemRecPreExecute = mr_execute
onMemRecPostExecute = mr_execute
---------------------------------------------------------------------------

function cycleFullCompact(sender,force) local state = not(compactmenuitem.Caption == 'Compact View Mode'); if force~=nil then state = not force end; compactmenuitem.Caption = state and 'Compact View Mode' or 'Full View Mode'; getMainForm().Splitter1.Visible = state; getMainForm().Panel4.Visible    = state; getMainForm().Panel5.Visible    = state; end; function addCompactMenu() if compactmenualreadyexists then return end; local parent = getMainForm().Menu.Items; compactmenuitem = createMenuItem(parent); parent.add(compactmenuitem); compactmenuitem.Caption = 'Compact View Mode'; compactmenuitem.OnClick = cycleFullCompact; compactmenualreadyexists = 'yes'; end; addCompactMenu(); --cycleFullCompact(nil,true)
---------------------------------------------------------------------------

function isMonoCheck()return (readInteger'mono_domain_get' or readInteger'GameAssembly.dll') end

_AA_Custom_Inited = true
end --- done aa init


if not syntaxcheck and isMonoCheck() then
--  local p = getPidContext()
--  synchronize(print,tostring(p.monoSymbolLoaded),p.PID,p.PROCESS)
--  if p.monoSymbolLoaded then return end

  local mr = memrec
  if mr and not mr.Async then
    local id = mr.ID
    synchronize(createTimer,500,function()
      mr = GetAddressList().getMemoryRecordByID(id)
      if mr then mr.Async = true end
      createTimer(500,mr.SetActive,true)
    end)
    error'Retry'
  end

  if type(monopipe)=='userdata' and monopipe.Destroy then
--      monopipe=nil,monopipe.Destroy()
      ---[[
    if monopipe.OnTimeOut then
      synchronize(monopipe.OnTimeOut,monopipe)
    else
      monopipe=nil,monopipe.Destroy()
    end
    sleep(1000)
    --]]
  end
  local ok = pcall(LaunchMonoDataCollector)
    sleep(1000)
    --  synchronize(print,'ok:',tostring(ok),tostring(monopipe),tostring(monoSymbolList),tostring(monoSymbolList and monoSymbolList.FullyLoaded))

    --synchronize(print,'isIl2cpp? ',tostring(monopipe and monopipe.IL2CPP))

  --if ok and monopipe and not monopipe.IL2CPP then
--    p.monoSymbolLoaded = true
--    synchronize(print,'ok:',tostring(monopipe and monopipe.IL2CPP))
--[[  else--]]if ok and type(monopipe)=='userdata' and monopipe.IL2CPP and monoSymbolList and not monoSymbolList.FullyLoaded then
    local timeout,cnt = os.clock()+60.0 , 0
    while timeout > os.clock()and monoSymbolList and not monoSymbolList.FullyLoaded do
--      synchronize(print,'cnt:',cnt,tostring(monoSymbolList and monoSymbolList.FullyLoaded)) cnt=cnt+1
--      synchronize(processMessages)
      sleep(250)
    end
    if monopipe.IL2CPP and (not monoSymbolList or not monoSymbolList.FullyLoaded) then
      synchronize(createTimer,1000,print,'loading il2cpp symbol TimeOut or error, try again pls.')
      error'Retry'
    end
  end
end
