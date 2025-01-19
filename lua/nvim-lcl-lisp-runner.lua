-- Copyright (c) 2024 OKABE Gota
-- This program is provided by under MIT License.
-- See more detail LICENSE.txt and README.md
local M = {}
local vim = vim
-- Configuration
local config = {
  use_clisp_env = false,
  clisp_cmd = {},
  clisp_with_file_cmd = {}
}

-- Helper function to get the directory of the current script
local function get_script_dir()
  local info = debug.getinfo(1, "S")
  local script_path = info.source:sub(2)         -- Remove the '@'
  return vim.fn.fnamemodify(script_path, ":p:h") -- Get absolute path and directory
end

-- Helper function to run a command in a floating terminal
local function run_in_floating_terminal(cmd)
  local buf = vim.api.nvim_create_buf(false, true)
  local width = math.floor(vim.o.columns * 0.8)
  local height = math.floor(vim.o.lines * 0.8)
  local row = math.floor((vim.o.lines - height) / 2)
  local col = math.floor((vim.o.columns - width) / 2)

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    row = row,
    col = col,
    style = "minimal",
    border = "rounded",
  })

  --vim.api.nvim_buf_set_option(buf, "buftype", "terminal")
  vim.fn.termopen(cmd, {
    on_exit = function()
      vim.api.nvim_win_close(win, true)
    end,
  })
  vim.cmd("startinsert")
end

-- Helper function to run a command in a horizontal split terminal
--[[local function run_in_horizontal_terminal(cmd)
  vim.cmd("split")

  --vim.api.nvim_buf_set_option(buf, "buftype", "terminal")
  vim.fn.termopen(cmd, {
    on_exit = function()
      vim.cmd("bdelete!")
    end,
  })
  vim.cmd("resize 10") -- Adjust height if needed
  vim.cmd("startinsert")
end
]]

-- Helper function to choose terminal type based on configuration
local function run_command(cmd)
  run_in_floating_terminal(cmd)
  --[[
  if config.use_floating_window then
    run_in_floating_terminal(cmd)
  else
    run_in_horizontal_terminal(cmd)
  end
  ]]
end

-- Run CommonLisp file
M.run_clisp = function(file_path)
  local script_dir = get_script_dir()
  local repl_path = script_dir .. "/repl.lua"
  local cmd = { "nvim", "--headless", "-c", "cd" .. script_dir .. "|" .. "luafile" .. repl_path, "-c", "qa", "-u", "NONE" }
  if (file_path) then
    local nvim_cmd = "cd" .. script_dir .. "|" .. "lua input_file='" .. file_path .. "' dofile('" .. repl_path .. "')"
    cmd = { "nvim", "--headless", "-c", nvim_cmd, "-c", "qa", "-u", "NONE" }
  end
  -- Use the appropriate terminal type for execution
  run_command(cmd)
end

-- Run Prolog
M.run_prolog = function()
  local script_dir = get_script_dir()
  local prolog_path = script_dir .. "/prolog.lua"
  if config.clisp_with_file_cmd then
    M.run_clisp_env(script_dir.."/prolog-repl.lisp")
    return
  end
  local cmd = { "nvim", "--headless", "-c", "cd" .. script_dir .. "|" .. "luafile" .. prolog_path, "-c", "qa", "-u",
    "NONE" }

  -- Use the appropriate terminal type for execution
  run_command(cmd)
end

-- Talk with Eliza
M.eliza = function()
  local script_dir = get_script_dir()
  local eliza_path = script_dir .. "/eliza.lua"
  if config.clisp_with_file_cmd then
    M.run_clisp_env(script_dir.."/eliza.lisp")
    return
  end
  local cmd = { "nvim", "--headless", "-c", "cd" .. script_dir .. "|" .. "luafile" .. eliza_path, "-c", "qa", "-u",
    "NONE" }

  -- Use the appropriate terminal type for execution
  run_command(cmd)
end

M.run_clisp_env = function(file_path)
  local cmd = vim.list_extend(config.clisp_cmd,{})
  if (file_path) then
    cmd = vim.list_extend(vim.deepcopy(config.clisp_with_file_cmd), { file_path })
  end
  -- Use the appropriate terminal type for execution
  run_command(cmd)
end

M.setup = function(tbl)
  config = tbl
  -- define clisp cmd
  if (config.clisp_cmd and config.clisp_with_file_cmd) then
    vim.api.nvim_create_user_command("RunClisp", function(opts)
      local file_path = opts.fargs[1] and vim.fn.expand(opts.fargs[1] .. ":p") or nil
      M.run_clisp_env(file_path)
    end, { nargs = "?" })
  end
end

-- Command definitions
vim.api.nvim_create_user_command("RunLCL", function(opts)
  local file_path = opts.fargs[1] and vim.fn.expand(opts.fargs[1] .. ":p") or nil
  M.run_clisp(file_path)
end, { nargs = "?" })



vim.api.nvim_create_user_command("RunProlog", function()
  M.run_prolog()
end, {})

vim.api.nvim_create_user_command("RunEliza", function()
  M.eliza()
end, {})

return M
