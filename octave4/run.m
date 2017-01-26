pkg load zeromq
addpath('/home/sorna/jsonlab-master');
%addpath('./jsonlab-master');

% Override memory specific function
function clear (varargin)
  %for i = 1:length(varargin)
	%  printf (varargin{i})
  %endfor
endfunction

function result = execute_code(code)
  stdout = [];
  stderr = [];
  media = cell();
  exceptions = [];
  try
    stdout = evalc(code, 'stderr = lasterror.message;');
    if strfind(code, "plot") || strfind(code, "figure")
      allFigInW = findall(0, 'type', 'figure');
      _mediaCount = 1;
      for i = 1:length(allFigInW)
        print(allFigInW(i), "octave_figure_internal.svg");
        fstr = fileread("octave_figure_internal.svg");
        fstr = strrep(fstr, '  ', ' ')
        fstr = strrep(fstr, '	', ' ')
        media{_mediaCount} = {"image/svg+xml", fstr};
        _mediaCount = _mediaCount + 1;
        unlink("octave_figure_internal.svg");
      endfor
    endif
  catch
    exceptions = lasterror.message;
  end_try_catch
  result.stdout = stdout;
  result.stderr = stderr;
  result.media = media;
  result.exceptions = exceptions;
endfunction

sock = zmq_socket(ZMQ_REP);
zmq_bind(sock, "tcp://*:2001");
printf (["Octave version : ", version, "\n"])
printf ("serving at port 2001...")

while(true)
  codeid = zmq_recv(sock, 100, 0);
  code = zmq_recv(sock, 100000, 0);
  result = execute_code(char(code));
  a = savejson('', result);
  zmq_send(sock, a);
endwhile
