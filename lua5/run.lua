local zmq = require "lzmq"
local json = require "dkjson"


function execute_code(code_id, code)
    --[[ Replace global `print` (and `io.write`) functions to save contents to a
         variable (tmp_stdout).

    Capturing std* (stdin, stdout, stderr) in Lua seems not to be trival for me.
    So, I just redefined global `print` and `io.write` to append values to a
    variable (tmp_stdout). This variable keeps all the printed contents, so it can
    be returned as stdout.

    However, if user code writes something to stdout with functions other than
    `print` and `io.write` this strategy surely will fail. It is not very wise to
    find and redefine every functions that writes to stdout/stderr. So, this is
    just a temporary implementation.

    One possible alternative approach would be redefine `lua_writestring`,
    `lua_writeline`, and `lua_writestringerror` in `lauxlib.h`. These are
    "Abstraction Layers" for basic report of messages and errors for Lua.
    For example, `lua_writestring` is defined as:

        #if !defined(lua_writestring)
        #define lua_writestring(s,l)   fwrite((s), sizeof(char), (l), stdout)
        #endif

    So, replacing `stdout` to some other controllable stream may allow us to capture
    Lua's stdout/stderr (not tested, and actually I don't know how to yet;;).
    However, this would make us unable to use `normal stdout` in entire lua container
    since we should re-compile Lua with modified `lauxlib.h`. Thus, I'm not very
    confident that this is better approach.
    ]]
    local original_print, original_io_write = print, io.write
    local tmp_stdout = ""
    print = function (...) tmp_stdout = tmp_stdout .. ... .. '\n' end  -- `print` appends new line
    io.write = function (...) tmp_stdout = tmp_stdout .. ... end  -- `io.write` do not append new line

    local ok, result = pcall(assert(loadstring(code)))
    local exceptions = {}

    -- Restore original `print` function
    print, io.write = original_print, original_io_write

    if not ok then
        local exception_info = {result, {}, false, nil}
        table.insert(exceptions, exception_info)
    end

    -- I'm not sure there's a clear difference between stderr and exception in Lua.
    local stdout, stderr = tmp_stdout, ""
    return stdout, stderr, exceptions
end

-- main routine
function main(socket)
    while true do
        -- Receive code id and code
        local code_id = socket:recv()
        local code = socket:recv()

        -- Evaluate code
        local out, err, exceptions = execute_code(code_id, code)

        -- Return results
        local result = {
            stdout = out,
            stderr = err,
            exceptions = exceptions
        }

        local json_result = json.encode(result)
        socket:send(json_result)
    end
end

-- Create ZMQ context and socket
local ctx = zmq.context()
local socket = ctx:socket(zmq.REP)
local port = "tcp://*:2001"
socket:bind(port)
print("serving at port 2001...")

-- Run main routine in a protected mode to destroy ZMQ socket and context in any case
pcall(main, socket)

-- Terminate ZMQ socket and context
socket:close()
ctx:destroy()
print("exit.")
