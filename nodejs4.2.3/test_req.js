var process = require('process');
var zmq = require('zmq');
var socket = zmq.socket('req');
var port = 'tcp://0.0.0.0:2001';

socket.on('message', function(result) {
    console.log('Returned result: ' + result);
});

socket.connect(port);

setInterval(function() {
    var code = 'var a = 12; console.log(a + 5);';
    var req_data = ['1', code];
    socket.send(req_data);
    //console.log('Asking: ' + req_data);
}, 1000);