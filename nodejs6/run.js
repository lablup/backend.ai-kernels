function main() {
  const assert = require('assert');
  const deasync = require('deasync');
  const domain = require('domain');
  const vm = require('vm');
  const zmq = require('zmq');
  const Console = require('console').Console;
  const Module = require('module');
  const Writable = require('stream').Writable;

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
    require.resolve = (request) => {
      return Module._resolveFilename(request, self);
    };
    require.main = process.mainModule;

    // Enable support to add extra extension types.
    require.extensions = Module._extensions;
    require.cache = Module._cache;
    return require;
  }

  let mod = new Module('<repl>');
  let ctx = vm.createContext();
  ctx.module = module;
  ctx.require = makeRequireFunction.call(mod);
  for (var i in global) ctx[i] = global[i];
  ctx.global = ctx;
  ctx.global.global = ctx;
  let userDomain = domain.create();

  let inSocket = zmq.socket('pull');
  let outSocket = zmq.socket('push');

  class OutputWritable extends Writable {
    constructor(opts) {
      super(opts);
      this.target = opts.target;
      assert(this.target == 'stdout' || this.target == 'stderr');
    }

    write(chunk, encoding, cb) {
      outSocket.send([this.target, chunk]);
      if (cb) cb();
    }
  }

  let stdout = new OutputWritable({ target: 'stdout' });
  let stderr = new OutputWritable({ target: 'stderr' });
  ctx.console = new Console(stdout, stderr);

  /* TODO: implement this someday...
  ctx.input = (prompt, opts, callback) => {
    var opts = opts || {};
    opts.is_password = opts.is_password || false;
    inSocket.once('message', (code_id, code_txt) => {
      console.log('recv input')
      inSocket.unref();
      callback(code_txt);
    });
    inSocket.ref();
    outSocket.send(['stdout', prompt]);
    outSocket.send(['waiting-input', JSON.stringify(opts)]);
  };
  */

  function execute_handler(code_id, code_txt) {
    userDomain.on('error', (err) => {
      ctx.console.error(err);
    });
    try {
      let script = new vm.Script(`'use strict'; void 0; ${code_txt}`);
      inSocket.unref();
      outSocket.unref();
      userDomain.run(() => {
        script.runInContext(ctx);
        deasync.loopUntilNoMoreEvents();
      });
    } catch (err) {
      ctx.console.error(err);
    } finally {
      inSocket.ref();
      outSocket.ref();
      process.nextTick(() => {
        inSocket.once('message', execute_handler);
      });
      outSocket.send(['finished', '']);
    }
  }

  function shutdown() {
    inSocket.close();
    outSocket.close();
    console.log('exit.');
  }
  process.on('SIGTERM', shutdown);
  process.on('SIGINT', shutdown);

  inSocket.bind('tcp://*:2000', (err) => { });
  outSocket.bind('tcp://*:2001', (err) => { });

  inSocket.once('message', execute_handler);
}

if (require.main === module) {
  main();
}

// vim: ts=8 sts=2 sw=2 et
