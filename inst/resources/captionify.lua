function Para(el)
  local txt = pandoc.utils.stringify(el)

  if txt:match("^Figure:%s*(.+)") then
    local cap = txt:match("^Figure:%s*(.+)")
    return pandoc.Figure({}, pandoc.Caption(cap))
  end

  if txt:match("^Table:%s*(.+)") then
    local cap = txt:match("^Table:%s*(.+)")
    return pandoc.Table({}, pandoc.Caption(cap))
  end
end