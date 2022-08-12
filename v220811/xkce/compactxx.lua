--
do
  local mf,pvh = MainForm
  local now, stats = 2,{' Mini  ','Restore','Compact',}
  local function compactxxMode()
     local p5, so = mf.panel5, mf.gbScanOptions
     local pc = p5.Constraints
     if now == 1 then
       so.Visible, p5.Height, pc.MinHeight, pvh = true, pvh,1
     elseif now == 2 then
       so.Visible, pvh, p5.Height, pc.MinHeight = false,
         p5.Height, so.Top, 1
     else
       local fc = mf.foundCountLabel
       p5.Height = fc.Top + fc.Height+mf.btnMemoryView.Height
     end
  end

  if not miCompactXX then
    local root = getMainForm().Menu.Items
    local mi = createMenuItem(root)
    root.add(mi)
    mi.Caption = 'Compact'
    mi.OnClick = function(me)
      pcall(compactxxMode)
      now = (now + 1)% #stats
      me.Caption = stats[now+1]
    end
    miCompactXX = mi
  end
  return function()miCompactXX.OnClick(miCompactXX)end
end
