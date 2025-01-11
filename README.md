# nvim-lcl-lisp-runner

This Neovim plugin allows you to execute CommonLisp code directly from your editor using a floating window. The plugin also provides Prolog and Eliza implemented in CommonLisp for demo.

## Features

- Run CommonLisp files using the `:RunLCL` command.
- Run Prolog programs written in CommonLisp using the `:RunProlog` command.
- Run Eliza programs written in CommonLisp using the `:RunEliza` command.
- Automatically closes the terminal window after execution completes.

## Requirements

- Neovim 0.5 or later built with LuaJIT (not Lua).

## Installation

Using [packer.nvim](https://github.com/wbthomason/packer.nvim):

```lua
use {
  'siraragi62/nvim-lcl-lisp-runner',
  config = function()
    require('nvim-lcl-lisp-runner'))
  end
}
```

## Setup

All you need is calling `require('nvim-lcl-lisp-runner')`.

```lua
require('nvim-lcl-lisp-runner')
```

## Commands

### `:RunLCL [file]`

Loads and runs the specified CommonLisp file, with the REPL taking over after execution. If `%` is given, the current file is used. If no file is given, the REPL starts without loading any file. 

Example:

```vim
:RunLCL
:RunLCL path/to/file.lisp
:RunLCL %
```

### `:RunProlog`

Executes the PAIP Prolog interpreter in CommonLisp.

Example:

```vim
:RunProlog
```

### `:RunEliza`

Executes the PAIP Eliza in CommonLisp.

Example:

```vim
:RunEliza
```

## How it works

CommonLisp runs on [LCL](https://codeberg.org/gsou/LCL), a CommonLisp implementation in Lua. Eliza and Prolog are both implemented by running the CommonLisp implementation included in PAIP on LCL.

## Limitations

Since LCL is not a complete CommonLisp implementation, there are some constraints on execution.
- Some standard library functions are not referenced in this version, even though they are implemented by LCL. Programs that depend on them must reimplement the standard functions themselves. [Standard library implementations](https://codeberg.org/gsou/LCL/src/branch/main/stdlib) are available in the LCL repository.
- CommonLisp cannot be run on the original Lua interpreter in this version. Run it on Neovim with LuaJIT (official binaries, default build settings use LuaJIT)


## Tips

LCL can call Lua functions through the LCL Lua API, so you can extend as needed. See the [README](https://codeberg.org/gsou/LCL#headline-9) in the LCL repository for instructions on how to use the LCL Lua API.

## License

This plugin is licensed under the MIT License.

## Acknowledgement

LCL, a Lua implementation of CommonLisp, is provided under the MIT License by gsou. Eliza and Prolog implementations in CommonLisp are provided under the MIT License by Peter Norvig at PAIP. All of these components are copyrighted by the original copyright holder under the MIT License.
