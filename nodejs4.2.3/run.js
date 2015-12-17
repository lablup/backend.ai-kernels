var express = require('express');
var PORT = 2001;

var app = express();
app.get('/', function(req, resp) {
  resp.send('Hello world\n');
});

app.listen(PORT);
console.log('success!! port: ', PORT);