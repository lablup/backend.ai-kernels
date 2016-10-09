using ZMQ
import JSON


function parseall(code)
    pos = start(code)
    exs = []

    while !done(code, pos)
       ex, pos = parse(code, pos)
       push!(exs, ex)
    end

    if length(exs) == 0
       throw(ParseError("end of input"))
    elseif length(exs) == 1
       return exs[1]
    else
       return Expr(:block, exs...)
    end
end


function execute_code(code_id, code)
    # Intercept STDOUT and STDERR
    const (OLDOUT, OLDERR) = STDOUT, STDERR
    (outRead, outWrite) = redirect_stdout()
    (errorRead, errorWrite) = redirect_stderr()

    # Julia's Base.parse() does not parse multi-line string by design
    exprs = parseall(code)
    eval(exprs)

    # Read stdout and stderr during code execution
    redirect_stdout(OLDOUT)
    close(outWrite); out = readstring(outRead); close(outRead)
    redirect_stderr(OLDERR)
    close(errorWrite); err = readstring(errorRead); close(errorRead)

    return out, err
end


# Set home directory
cd("/home/work")

# ZMQ setup
ctx = Context()
socket = Socket(ctx, REP)
port = "tcp://*:2001"
ZMQ.bind(socket, port)
println("serving at port 2001...")

try
    while true
        # Receive code_id
        msg = ZMQ.recv(socket)
        io = seek(convert(IOStream, msg), 0)
        code_id = takebuf_string(io)

        # Receive code
        msg = ZMQ.recv(socket)
        io = seek(convert(IOStream, msg), 0)
        code = takebuf_string(io)

        try
            out, err = execute_code(code_id, code)

            result = Dict(
                "stdout" => out,
                "stderr" => err,
                "exceptions" => []
            )

            ZMQ.send(socket, Message(JSON.json(result)))
        catch e
            # Todo: Handle exceptions
        end
    end
finally
    ZMQ.close(socket)
    ZMQ.close(ctx)
    println("exit.")
end
