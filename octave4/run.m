pkg load zeromq
addpath('/home/sorna/jsonlab-master');

% Override memory-clear function to preserve our precious sockets
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

function result = execute_code(insock, outsock, code)
  stdout = [];
  stderr = [];
  media = cell();
  exceptions = [];
  try
    stdout = evalc (code, 'stderr = lasterror.message;');
    if strfind (code, 'plot') || strfind (code, 'figure')
      allFigInW = findall (0, 'type', 'figure');
      _mediaCount = 1;
      for i = 1:length(allFigInW)
        tmpf = tmpname ();
        print (allFigInW (i), tmpf);
        fstr = fileread(tmpf);
        fstr = strrep(fstr, '  ', ' ')
        fstr = strrep(fstr, '   ', ' ')
        media{_mediaCount} = {'image/svg+xml', fstr};
        _mediaCount = _mediaCount + 1;
        unlink (tmpf);
      endfor
    endif
  catch
    exceptions = lasterror.message;
  end_try_catch

  if (!isempty (stdout))
    zmq_send (outsock, 'stdout', ZMQ_SNDMORE);
    zmq_send (outsock, char(stdout));
  endif
  if (!isempty (stderr))
    zmq_send (outsock, 'stderr', ZMQ_SNDMORE);
    zmq_send (outsock, char(stderr));
  endif
  for m = media
    item.type = m{1}
    item.data = m{2}
    zmq_send (outsock, 'media', ZMQ_SNDMORE);
    zmq_send (outsock, savejson(item));
  endfor
endfunction

_sorna_insock  = zmq_socket (ZMQ_PULL);
_sorna_outsock = zmq_socket (ZMQ_PUSH);
zmq_bind (_sorna_insock, 'tcp://*:2000');
zmq_bind (_sorna_outsock, 'tcp://*:2001');
printf (['Octave version : ', version, '\n'])
printf ('start serving...')

while(true)
  codeid  = zmq_recv (_sorna_insock, 100, 0);
  codetxt = zmq_recv (_sorna_insock, 100000, 0);
  execute_code (_sorna_insock, _sorna_outsock, char(codetxt));
  zmq_send (_sorna_outsock, 'finished', ZMQ_SNDMORE);
  zmq_send (_sorna_outsock, '');
endwhile

% vim: sts=2 sw=2 et
