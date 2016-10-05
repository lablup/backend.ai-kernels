using ZMQ

function main()
    # Set home directory
    # cd("/home/work")

    # Intercept STDOUT and STDERR
    const (OLDOUT, OLDERR) = STDOUT, STDERR
    (outRead, outWrite) = redirect_stdout()
    (errorRead, errorWrite) = redirect_stderr()

    # ZMQ setup
    ctx = Context()
    socket = Socket(ctx, REP)
    port = "tcp://*:2001"
    ZMQ.bind(socket, port)

    # Todo: keep reading request from ZMQ

    # Todo: execute code and retrieve result / handle exceptions

    # Read stdout and stderr during code execution
    redirect_stdout(OLDOUT)
    close(outWrite); out = readall(outRead); close(outRead)
    redirect_stderr(OLDERR)
    close(errorWrite); err = readall(errorRead); close(errorRead)

    # Todo: reply with ZMQ

    # Todo: handle termination signals
end

main()
