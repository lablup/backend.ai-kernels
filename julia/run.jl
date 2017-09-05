include("IJuliaEmul.jl")


module SornaExecutor

"""
This submodule is exposed to the user codes as their "Main" module.
"""
module SornaExecutionEnv # submodule

import ZMQ
import JSON
import Base: Display, display, readline, flush, PipeEndpoint, info, warn
import IJulia

# overloading hack from IJulia
const StdioPipe = Base.PipeEndpoint

mutable struct SornaDisplay <: Display
    isock::ZMQ.Socket
    osock::ZMQ.Socket
    olock::Base.ReentrantLock
end

function display(d::SornaDisplay, M::MIME"image/png", x)
    local data = reprmime(M, x)
    local o = Dict(
        "type" => string(M),
        "data" => base64encode(data),
    )
    lock(d.olock)
    ZMQ.send(d.osock, "media", true)
    ZMQ.send(d.osock, JSON.json(o))
    unlock(d.olock)
end

function display(d::SornaDisplay, M::MIME"image/svg+xml", x)
    local data = reprmime(M, x)
    local o = Dict(
        "type" => string(M),
        "data" => data,
    )
    lock(d.olock)
    ZMQ.send(d.osock, "media", true)
    ZMQ.send(d.osock, JSON.json(o))
    unlock(d.olock)
end

function display(d::SornaDisplay, ::MIME"text/html", x)
    data = reprmime(M, x)
    o = Dict(
        "type" => "text/html",
        "data" => data,
    )
    lock(d.olock)
    ZMQ.send(d.osock, "media", true)
    ZMQ.send(d.osock, JSON.json(o))
    unlock(d.olock)
end

function display(d::SornaDisplay, M::MIME"text/plain", x)
    data = reprmime(M, x)
    lock(d.olock)
    ZMQ.send(d.osock, "stdout", true)
    ZMQ.send(d.osock, data)
    unlock(d.olock)
end

display(d::SornaDisplay, x) = display(d, MIME"text/plain"(), x)

function info(io::StdioPipe, msg...; kw...)
    if io == STDERR
        o = chomp(string(msg...))
        lock(d.olock)
        ZMQ.send(sorna.osock, "stderr", true)
        ZMQ.send(sorna.osock, "INFO: $o\n")
        unlock(d.olock)
        #o = Dict(
        #    "level" => "info",
        #    "msg"   => chomp(string(msg...)),
        #)
        #ZMQ.send(sorna.osock, "log", true)
        #ZMQ.send(sorna.osock, JSON.json(o))
    else
        invoke(info, Tuple{supertype(StdioPipe)}, io, msg..., kw...)
    end
end

function warn(io::StdioPipe, msg...; kw...)
    if io == STDERR
        o = chomp(string(msg...))
        lock(d.olock)
        ZMQ.send(sorna.osock, "stderr", true)
        ZMQ.send(sorna.osock, "WARN: $o\n")
        unlock(d.olock)
        #o = Dict(
        #    "level" => "warn",
        #    "msg"   => chomp(string(msg...)),
        #)
        #ZMQ.send(sorna.osock, "log", true)
        #ZMQ.send(sorna.osock, "warn", true)
        #ZMQ.send(sorna.osock, JSON.json(o))
    else
        invoke(warn, Tuple{supertype(StdioPipe)}, io, msg..., kw...)
    end
end

"""
Read a String from the front-end web browser.
This function is a Sorna version of IJulia's readprompt.
"""
function readprompt(prompt::AbstractString; password::Bool=false)
    global sorna
    local opts = Dict("is_password" => password)
    lock(sorna.olock)
    ZMQ.send(sorna.osock, "stdout", true)
    ZMQ.send(sorna.osock, prompt)
    ZMQ.send(sorna.osock, "waiting-input", true)
    ZMQ.send(sorna.osock, JSON.json(opts))
    unlock(sorna.olock)
    code_id = unsafe_string(ZMQ.recv(sorna.isock))
    code_txt = unsafe_string(ZMQ.recv(sorna.isock))
    return code_txt
end

"Native-like readline function that emulates stdin using readprompt."
function readline(io::StdioPipe)
    if io == STDIN
        return readprompt("STDIN> ")
    else
        invoke(readline, Tuple{supertype(StdioPipe)}, io)
    end
end

function flush(io::StdioPipe)
    invoke(flush, Tuple{supertype(StdioPipe)}, io)
    oslibuv_flush()
    if io == STDIN
        # TODO: implement
    elseif io == STDERR
        # TODO: implement
    end
end

function init_sorna(isock::ZMQ.Socket, osock::ZMQ.Socket,
                    output_lock::Base.ReentrantLock)
    global sorna
    sorna = SornaDisplay(isock, osock, output_lock)
    pushdisplay(sorna)
end

function eval_sorna(exprs)
    for hook in IJulia.preexecute_hooks
        @eval $hook()
    end
    try
        Core.eval(SornaExecutionEnv, exprs)
        for hook in IJulia.postexecute_hooks
            @eval $hook()
        end
    catch ex
        st = catch_stacktrace()
        println(STDERR, format_stacktrace(st, ex))
        for hook in IJulia.posterror_hooks
            @eval $hook()
        end
    end
end

end # submodule SornaExecutionEnv


import ZMQ
import .SornaExecutionEnv

"""
Makes a block expression from the given (multi-line) string.
We need this special function since Julia's vanilla parse()
function only takes a single line of string.
"""
function parseall(code::AbstractString)
    exprs = []
    pos = start(code)
    lineno = 1
    try
        while !done(code, pos)
            expr, pos = parse(code, pos)
            push!(exprs, expr)
            lineno += 1
        end
    catch ex
        if isa(ex, ParseError)
            # Add the line number to parsing errors
            throw(ParseError("$(ex.msg) (Line $lineno)"))
        else
            rethrow()
        end
    end

    if length(exprs) == 0
        throw(ParseError("Premature end of input"))
    elseif length(exprs) == 1
        return exprs[1]
    else
        return Expr(:block, exprs...)
    end
end

function redirect_stream(rd::IO, target::AbstractString,
                         osock::ZMQ.Socket,
                         output_lock::Base.ReentrantLock)
    try
        while !eof(rd)
            nb = nb_available(rd)
            if nb > 0
                lock(output_lock)
                ZMQ.send(osock, target, true)
                ZMQ.send(osock, read(rd, nb))
                unlock(output_lock)
            end
        end
    catch e
        if islocked(output_lock)
            unlock(output_lock)
        end
        if isa(e, InterruptException)
            redirect_stream(rd, target, output_socket)
        else
            rethrow()
        end
    end
end

# format_stackframe is taken from https://github.com/invenia/StackTraces.jl
# which is now part of Julia's standard library since v0.5.
# Unfortunately the standard library does not expose formatting functions... :(
function format_stackframe(frame::StackFrame; full_path::Bool=false)
    file_info = string(
        full_path ? frame.file : basename(string(frame.file)),
        ":", frame.line
    )
    if :inlined_file in fieldnames(frame) && frame.inlined_file != Symbol("")
        inline_info = "[inlined code from $file_info] "
        file_info = string(
            full_path ? frame.inlined_file : basename(string(frame.inlined_file)),
            ":", frame.inlined_line
        )
    else
        inline_info = ""
    end
    func_name = string(frame.func)
    if (func_name == "execute_code" || func_name == "parseall") && basename(string(frame.file)) == "run.jl"
        nothing
    else
        "in $inline_info$(frame.func != "" ? frame.func : "?") at $file_info"
    end
end

function format_stacktrace(stack::StackTrace, ex::Exception = nothing; full_path::Bool=false)
    if ex == nothing
        prefix = ""
    else
        if isa(ex, ErrorException)
            prefix = "ERROR: $(ex.msg)"
        elseif isa(ex, ArgumentError) ||
               isa(ex, AssertionError) ||
               isa(ex, ParseError)
            prefix = "ERROR: $(typeof(ex)): $(ex.msg)"
        else
            prefix = "ERROR: $ex"
        end
    end
    if isempty(stack)
        return prefix
    end
    pieces = []
    for f in stack
        piece = format_stackframe(f, full_path=full_path)
        if piece == nothing
            break
        end
        push!(pieces, piece)
    end
    string(
        prefix, "\n",
        join(map(piece -> "  $piece", pieces), "\n"),
    )
end

function execute_code(input_socket, output_socket, output_lock, code_id, code_txt)
    try
        exprs = parseall(code_txt)
        SornaExecutionEnv.eval_sorna(exprs)
    finally
        # ensure reader tasks to run once more.
        sleep(0.001)
        lock(output_lock)
        ZMQ.send(output_socket, "finished", true)
        ZMQ.send(output_socket, "")
        unlock(output_lock)
    end
end

function run_query_mode()

    # ZMQ setup
    ctx = ZMQ.Context()
    input_socket = ZMQ.Socket(ctx, ZMQ.PULL)
    output_socket = ZMQ.Socket(ctx, ZMQ.PUSH)
    ZMQ.bind(input_socket, "tcp://*:2000")
    ZMQ.bind(output_socket, "tcp://*:2001")

    # Create a lock for coroutine task synchronization.
    # This is to prevent interleaving during sending ZMQ multipart messages.
    output_lock = Base.ReentrantLock()

    SornaExecutionEnv.init_sorna(input_socket, output_socket, output_lock)

    println("start serving...")

    const read_stdout = Ref{Base.PipeEndpoint}()
    const read_stderr = Ref{Base.PipeEndpoint}()

    read_stdout[], = redirect_stdout()
    read_stderr[], = redirect_stderr()

    # Intercept STDOUT and STDERR
    readout_task = @async redirect_stream(read_stdout[], "stdout", output_socket, output_lock)
    readerr_task = @async redirect_stream(read_stderr[], "stderr", output_socket, output_lock)

    try
        while true
            # Receive code_id and code_txt
            code_id = unsafe_string(ZMQ.recv(input_socket))
            code_txt = unsafe_string(ZMQ.recv(input_socket))

            # Evaluate code
            execute_code(input_socket, output_socket, output_lock, code_id, code_txt)
        end
    finally
        ZMQ.close(input_socket)
        ZMQ.close(output_socket)
        ZMQ.close(ctx)
    end
end

end # module SornaExecutor


import SornaExecutor

SornaExecutor.run_query_mode()

# vim: ts=8 sts=4 sw=4 et
