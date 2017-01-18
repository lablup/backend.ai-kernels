pkg load zeromq

addpath('./jsonlab-master');
% Override memory specific function
function clear (varargin)
  %for i = 1:length(varargin)
	%  printf (varargin{i})
  %endfor
endfunction

function result = execute_code(code)
  stdout = [];
  stderr = [];
  media = [];
  exceptions = [];
  try
    evalc(code);
    if strfind(code, "plot") || strfind(code, "figure")
      print("octave_figure_internal.svg");
      fstr = fileread("octave_figure_internal.svg");
      media = fstr;
    endif
  catch
    exceptions = lasterror.message;
  end_try_catch
  result.stdout = stdout;
  result.stderr = stderr;
  result.media = media;
  result.exceptions = exceptions;
endfunction

sock = zmq_socket(ZMQ_PULL);
zmq_bind(sock, "tcp://*:2001");
printf ("serving at port 2001...")

%cd /home/work;
while(true)
  codeid = zmq_recv(sock, 100, 0);
  code = zmq_recv(sock, 100000, 0);
  result = execute_code(char(code));
endwhile
