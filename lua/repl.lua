-- Copyright (c) 2024 OKABE Gota
print("Welcome to Common Lisp REPL!")
if (arg[1] == nil) then
  function run_without_arg()
    dofile("lcl.lua")
    eval('(load "repl.lisp")')
    eval('(start-repl)')
  end
  while true do
    local e,msg = pcall(run_without_arg)
    print(msg)
    if(e) then
      break
    end
  end
else
  function run_without_arg()
    dofile("lcl.lua")
    eval('(load "repl.lisp")')
    eval('(start-repl "' + arg[1] + '")')
  end
  while true do
    local _,msg = pcall(run_without_arg)
    print(msg)
    if(e) then
      break
    end
  end
end
print("Bye.")
