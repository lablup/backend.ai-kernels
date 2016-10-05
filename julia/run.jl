using ZMQ

function main()
    # Intercept STDOUT and STDERR
    const (OLDOUT, OLDERR) = STDOUT, STDERR
    (outRead, outWrite) = redirect_stdout()
    (errorRead, errorWrite) = redirect_stderr()

    # ZMQ setup
    ctx = Context()
    socket = Socket(ctx, REP)
    port = "tcp://*:2001"
    ZMQ.bind(socket, port)

    # cd("/home/work")

    # Read stdout and stderr and revert to normal STDOUT and STDERR
    # Todo: this script hangs on read*() function if outRead or errorRead is empty.
    write(STDOUT, 0x61)
    write(STDERR, 0x61)
    close(outWrite)
    output = readavailable(outRead)
    close(outRead)
    close(errorWrite)
    errors = readavailable(errorRead)
    close(errorRead)
    redirect_stdout(OLDOUT)
    redirect_stderr(OLDERR)
end

main()
