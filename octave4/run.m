pkg load zeromq
addpath('/home/sorna/jsonlab-master');

% Override memory-clear function to preserve our precious sockets
function clear (varargin)
  exclude_arg = '"-x", "_sorna_*"';
  if length(varargin) == 0
    args = [', ' exclude_arg];
  else
    for i = 1:length(varargin)
      if varargin{i} == "all"
        varargin{i} = exclude_arg;
      endif
    endfor
    args = [', ' varargin{:}];
  endif
  evalin ('caller', ['builtin ("clear"' args ')']);
endfunction

function result = execute_code(_sorna_insock, _sorna_outsock, _sorna_code)
  _sorna_stdout = [];
  _sorna_stderr = [];
  _sorna_media = cell();
  try
    _sorna_stdout = evalc (_sorna_code, '_sorna_stderr = lasterror.message;');
    if strfind (_sorna_code, 'plot') || strfind (_sorna_code, 'figure')
      gobjs = findall (0, 'type', 'figure');
      disp(sprintf('has %d plots!', length(gobjs)))
      for i = 1:length(gobjs)
        tmpf = tmpname ();
        print (gobjs(i), tmpf);
        fstr = fileread (tmpf);
        fstr = strrep (fstr, '  ', ' ');
        fstr = strrep (fstr, '   ', ' ');
        _sorna_media{i} = {'image/svg+xml', fstr};
        unlink (tmpf);
      endfor
    endif
  catch
    % do nothing
  end_try_catch

  if (!isempty (stdout))
    zmq_send (_sorna_outsock, 'stdout', ZMQ_SNDMORE);
    zmq_send (_sorna_outsock, char(_sorna_stdout));
  endif
  if (!isempty (stderr))
    zmq_send (_sorna_outsock, 'stderr', ZMQ_SNDMORE);
    zmq_send (_sorna_outsock, char(_sorna_stderr));
  endif
  for m = _sorna_media
    item.type = m{1}
    item.data = m{2}
    zmq_send (_sorna_outsock, 'media', ZMQ_SNDMORE);
    zmq_send (_sorna_outsock, savejson(item));
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
