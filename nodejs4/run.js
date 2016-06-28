function main() {
    const deasync = require('deasync');
    const domain = require('domain');
    const streams = require('memory-streams');
    const vm = require('vm');
    const zmq = require('zmq');
    const Console = require('console').Console;
    const Module = require('module');

    var socket = zmq.socket('rep');
    var port = 'tcp://*:2001';

    var requireDepth = 0;

    function makeRequireFunction() {
        const Module = this.constructor;
        const self = this;

        function require(path) {
            try {
                requireDepth += 1;
                return self.require(path);
            } finally {
                requireDepth -= 1;
            }
        }
        require.resolve = function(request) {
            return Module._resolveFilename(request, self);
        };
        require.main = process.mainModule;

        // Enable support to add extra extension types.
        require.extensions = Module._extensions;
        require.cache = Module._cache;
        return require;
    }

    function exception_handler(e, exceptions) {
        var exception_info = [e.stack.split('\n')[0], [], false, null];
        exceptions.push(exception_info);
    }

    process.chdir('/home/work');

    var mod = new Module('<repl>');
    var ctx = vm.createContext();
    ctx.module = module;
    ctx.require = makeRequireFunction.call(mod);
    for (var i in global) ctx[i] = global[i];
    ctx.global = ctx;
    ctx.global.global = ctx;
    var userDomain = domain.create();

    socket.on('message', function(code_id, code) {

        var stdout = new streams.WritableStream();
        var stderr = new streams.WritableStream();
        var exceptions = [];
        ctx.console = new Console(stdout, stderr);
        var result = {
            'stdout': '',
            'stderr': '',
            'exceptions': exceptions
        };

        userDomain.on('error', function(err) {
            exception_handler(err, exceptions);
        });
        try {
            var script = new vm.Script(`'use strict'; void 0; ${code}`);
            socket.unref();
            userDomain.run(function() {
                script.runInContext(ctx);
                deasync.loopUntilNoMoreEvents();
            });
        } catch (err) {
            exception_handler(err, exceptions);
        } finally {
            socket.ref();
            result['stdout'] = stdout.toString();
            result['stderr'] = stderr.toString();
            socket.send(JSON.stringify(result));
        }

    });

    function shutdown() {
        socket.close();
        console.log('exit.');
    }
    process.on('SIGTERM', shutdown);
    process.on('SIGINT', shutdown);

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

// vim: ts=8 sts=4 sw=4 et
