var main = function() {
    var process = require('process');
    var zmq = require('zmq');
    var socket = zmq.socket('rep');
    var port = 'tcp://*:2001';

    // refs: https://gist.github.com/pguillory/729616
    function hook_stdout(callback) {
        var old_stdout_write = process.stdout.write;
        process.stdout.write = (function(write) {
            return function(string, encoding, fd) {
                write.apply(process.stdout, arguments);
                callback(string, encoding, fd);
            };
        })(process.stdout.write);
        return function() {
            process.stdout.write = old_stdout_write;
        }
    }

    function hook_stderr(callback) {
        var old_stderr_write = process.stderr.write;
        process.stderr.write = (function(write) {
            return function(string, encoding, fd) {
                write.apply(process.stderr, arguments);
                callback(string, encoding, fd);
            };
        })(process.stderr.write);
        return function() {
            process.stderr.write = old_stderr_write;
        }
    }

    function execute(code) {
        eval(code);
    }

    process.chdir('/home/work');

    socket.on('message', function(code_id, code) {
        var stdout = '';
        var stderr = '';
        var exceptions = [];  // exceptions not supported yet
        var unhook_stdout = hook_stdout(function(string, encoding, fd) {
           stdout += string;
        });
        var unhook_stderr = hook_stderr(function(string, encoding, fd) {
           stderr += string;
        });
        execute(code.toString());
        var result = {
            'stdout': stdout,
            'stderr': stderr,
            'exceptions': exceptions
        };
        socket.send(JSON.stringify(result));
        unhook_stdout();
        unhook_stderr();
    });

    socket.on('SIGINT', function() {
        socket.close();
    });

    socket.bind(port, function(err) {
        if (err) {
            throw err;
        } else {
            console.log('Serving at ' + port + '...');
        }
    });
}

if (require.main === module) {
    main();
}