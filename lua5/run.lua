local zmq = require "lzmq"
local json = require "dkjson"


function execute_code(code_id, code)
    -- TODO: implement execute code feature
    status, err = pcall(EVAL_LIKE_FUNCTION(code))
    if status then  -- success
        -- TODO: store stdout of the execution
    else  -- error
        -- TODO: store stderr or exceptions
    end

    return stdout, stderr, exceptions
end

-- main routine
function main()
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
socket:connect(port)
print("serving at port 2001...")

-- Run main routine in a protected mode to destroy ZMQ socket and context in any case
pcall(main)

-- Terminate ZMQ socket and context
socket:close()
ctx:destroy()
print("exit.")
