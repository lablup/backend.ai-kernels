local zmq = require "lzmq"


function execute_code(in_socket, out_socket, code_id, code_txt)
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
    print = function(...)
        out_socket:sendx("stdout", "" .. ... .. "\n")
    end
    io.write = function(...)
        out_socket:sendx("stdout", "" .. ...)
    end
    io.read = function()
        out_socket:sendx("stdout", "STDIN> ")
        out_socket:sendx("waiting-input", "{\"is_password\": false}")
        local code_id = in_socket:recv()
        local user_input = in_socket:recv()
        return user_input
    end

    local chunk, err = load(code_txt, "<user-input>", "t")

    if chunk == nil then
        out_socket:sendx("stderr", err)
    else
        -- Run!
        local ok, result = pcall(chunk)

        -- Restore original `print` function
        print, io.write = original_print, original_io_write

        if not ok then
            out_socket:sendx("stderr", result)
        end
    end
    out_socket:sendx("finished", "")
end

function main(in_socket, out_socket)
    while true do
        local code_id = in_socket:recv()
        local code_txt = in_socket:recv()
        execute_code(in_socket, out_socket, code_id, code_txt)
    end
end

local ctx = zmq.context()
local in_socket = ctx:socket(zmq.PULL)
local out_socket = ctx:socket(zmq.PUSH)
in_socket:bind("tcp://*:2000")
out_socket:bind("tcp://*:2001")
print("start serving...")

-- Run in a protected mode to prevent destruction of ZMQ context/sockets
main(in_socket, out_socket)

in_socket:close()
out_socket:close()
ctx:destroy()
print("exit.")

-- vim: sts=4 sw=4 et
