using ZMQ
import JSON


function parseall(code)
    pos = start(code)
    exprs = []

    while !done(code, pos)
       expr, pos = parse(code, pos)
       push!(exprs, expr)
    end

    if length(exprs) == 0
       throw(ParseError("end of input"))
    elseif length(exprs) == 1
       return exprs[1]
    else
       return Expr(:block, exprs...)
    end
end


function execute_code(code_id, code)
    # Intercept STDOUT and STDERR
    const (OLDOUT, OLDERR) = STDOUT, STDERR
    (outRead, outWrite) = redirect_stdout()
    (errorRead, errorWrite) = redirect_stderr()

    # Julia's Base.parse() does not parse multi-line string by design
    exprs = parseall(code)

    str = err = ""
    exceptions = []
    try
        eval(exprs)
    catch e
        exception_info = [split(string(e), "\n")[1], [], false, nothing]
        push!(exceptions, exception_info)
    finally
        # Store stdout and stderr during evaluation, and restore them with original ones
        redirect_stdout(OLDOUT)
        close(outWrite); out = readstring(outRead); close(outRead)
        redirect_stderr(OLDERR)
        close(errorWrite); err = readstring(errorRead); close(errorRead)

        return out, err, exceptions
    end
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

        # Evaluate code
        out, err, exceptions = execute_code(code_id, code)

        # Return results
        result = Dict(
            "stdout" => out,
            "stderr" => err,
            "exceptions" => exceptions
        )
        ZMQ.send(socket, Message(JSON.json(result)))
    end
finally
    ZMQ.close(socket)
    ZMQ.close(ctx)
    println("exit.")
end
