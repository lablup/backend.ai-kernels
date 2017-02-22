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
    if strfind(code, 'plot') || strfind(code, 'figure')
      allFigInW = findall(0, 'type', 'figure');
      _mediaCount = 1;
      for i = 1:length(allFigInW)
        print(allFigInW(i), 'octave_figure_internal.svg');
        fstr = fileread('octave_figure_internal.svg');
        fstr = strrep(fstr, '  ', ' ')
        fstr = strrep(fstr, '   ', ' ')
        media{_mediaCount} = {'image/svg+xml', fstr};
        _mediaCount = _mediaCount + 1;
        unlink('octave_figure_internal.svg');
      endfor
    endif
  catch
    exceptions = lasterror.message;
  end_try_catch

  if stdout  % TODO: check not empty
    zmq_send(outsock, 'stdout', ZMQ_SNDMORE);
    zmq_send(outsock, stdout);
  endif
  if stderr  % TODO: check not empty
    zmq_send(outsock, 'stderr', ZMQ_SNDMORE);
    zmq_send(outsock, stderr);
  endif
  for m in media % TODO: iterate
    item = cell();
    item.type = '...' % TODO
    item.data = '...' % TODO
    zmq_send(outsock, 'media', ZMQ_SNDMORE);
    zmq_send(outsock, savejson(item));
  endfor
endfunction

insock = zmq_socket(ZMQ_PULL);
outsock = zmq_socket(ZMQ_PUSH);
zmq_bind(insock, 'tcp://*:2000');
zmq_bind(outsock, 'tcp://*:2001');
printf(['Octave version : ', version, '\n'])
printf('start serving...')

while(true)
  codeid = zmq_recv(insock, 100, 0);
  codetxt = zmq_recv(insock, 100000, 0);
  execute_code(char(code));
  zmq_send(outsock, 'finished', ZMQ_SNDMORE);
  zmq_send(outsock, '');
endwhile

% vim: sts=2 sw=2 et
