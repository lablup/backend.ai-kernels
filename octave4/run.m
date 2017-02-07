pkg load zeromq
addpath('/home/sorna/jsonlab-master');

% Override memory specific function
function clear (varargin)
  if length(varargin) == 0
    args = '-x _sorna_sock';
  else
    for i = 1:length(varargin)
      if varargin{i} == "all"
        varargin{i} = '-x _sorna_sock';
      endif
    endfor
    args = sprintf (', "%s"', varargin{:});
  endif
  evalin ("caller", ['builtin ("clear"' args ')']);
endfunction

function result = execute_code(code)
  try
    stdout = evalc(code, 'stderr = lasterror.message;');
    stdout = [];
    media = cell();
    exceptions = [];
    if strfind(code, "plot") || strfind(code, "figure")
      allFigInW = findall(0, 'type', 'figure');
      _mediaCount = 1;
      for i = 1:length(allFigInW)
        print(allFigInW(i), "octave_figure_internal.svg");
        fstr = fileread("octave_figure_internal.svg");
        fstr = strrep(fstr, '  ', ' ');
        fstr = strrep(fstr, '	', ' ');
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

_sorna_sock = zmq_socket(ZMQ_REP);
zmq_bind(_sorna_sock, "tcp://*:2001");
printf (["Octave version : ", version, "\n"])
printf ("serving at port 2001...")

while(true)
  codeid = zmq_recv(_sorna_sock, 100, 0);
  code = zmq_recv(_sorna_sock, 100000, 0);
  result = execute_code(char(code));
  a = savejson('', result);
  zmq_send(_sorna_sock, a);
endwhile
