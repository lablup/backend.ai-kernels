pkg load zeromq

function [stdout, stderr, media, exceptions] = execute_code(code)
  stdout = [];
  stderr = [];
  media = [];
  exceptions = [];
  try
    evalc(code);
    if strfind(code, "plot")
	  print("test.svg")
    elseif strfind(code, "figure")
	  print("test.svg")
    endif
  catch
    exceptions = lasterror.message;
  end_try_catch
endfunction

sock = zmq_socket(ZMQ_PULL);
zmq_bind(sock, "tcp://*:2001");
printf ("serving at port 2001...")

%cd /home/work;
while(true)
  codeid = zmq_recv(sock, 100, 0);
  code = zmq_recv(sock, 100000, 0);
  [stdout, stderr, media, exceptions] = execute_code(char(code));
endwhile
