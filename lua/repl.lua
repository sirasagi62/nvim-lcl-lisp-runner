-- Copyright (c) 2024 OKABE Gota
if type(jit) ~= 'table' then
   print("Sorry! This program must be run on LuaJIT!")
   os.exit(1)
end
print("Welcome to CommonLisp REPL!")
print("To Exit REPL, input Ctlr-C.")
if (input_file == nil) then
  function run_with_arg()
    dofile("lcl.lua")
    eval('(load "repl-none-load.lisp")')
  end
  while true do
    local e,msg = pcall(run_with_arg)
    print(msg)
    if(e) then
      break
    end
  end
else
  function run_with_arg()
    dofile("lcl.lua")
    eval('(load "repl.lisp")')
    eval('(start-repl "' .. input_file .. '")')
  end
  while true do
    local e,msg = pcall(run_with_arg)
    print(msg)
    if(e) then
      break
    end
  end
end
