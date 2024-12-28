-- Copyright (c) 2024 OKABE Gota
if type(jit) ~= 'table' then
   print("Sorry! This program must be run on LuaJIT!")
   os.exit(1)
end
-- Load LCL produced LCL
dofile("lcl.lua")
-- Compile all stdlib files
eval('(load "eliza.lisp")')
