-- NOTE run in neovim with `:source`

--- @param input string
--- @return integer
local mul_res = function(input)
  return vim
    .iter(input:gmatch [[mul%((%d+),(%d+)%)]])
    :map(function(x, y) return tonumber(x) * tonumber(y) end)
    :fold(0, function(x, acc) return x + acc end)
end

-- Taking `do` and `don't` into account
--- @param input string
--- @return integer
local full_res = function(input)
  local funccalls = vim.iter(input:gmatch [[[%a']+%(.-%)]])
  local res = funccalls:fold({ sum = 0, enabled = true }, function(acc, v)
    if v:match 'do%(%)$' then
      return { sum = acc.sum, enabled = true }
    elseif v:match [[don't%(%)$]] then
      return { sum = acc.sum, enabled = false }
    elseif acc.enabled then
      local x1, x2 = v:match [[mul%((%d+),(%d+)%)]]
      if x1 == nil then
        return acc
      else
        return { sum = acc.sum + tonumber(x1) * tonumber(x2), enabled = acc.enabled }
      end
    else
      return acc
    end
  end)
  return res.sum
end

local main = function()
  local file = assert(io.open('resources/input.txt', 'r'))
  local input = file:read '*a'
  file:close()
  print(mul_res(input))
  print(full_res(input))
end

main()
