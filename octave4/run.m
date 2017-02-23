pkg load zeromq
addpath('/home/sorna/jsonlab-master');

% Override memory specific function to do nothing.
function clear (varargin)
%  for i = 1:length(varargin)
%    printf (varargin{i})
%  endfor
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

insock  = zmq_socket (ZMQ_PULL);
outsock = zmq_socket (ZMQ_PUSH);
zmq_bind (insock, 'tcp://*:2000');
zmq_bind (outsock, 'tcp://*:2001');
printf (['Octave version : ', version, '\n'])
printf ('start serving...')

while(true)
  codeid  = zmq_recv (insock, 100, 0);
  codetxt = zmq_recv (insock, 100000, 0);
  execute_code (insock, outsock, char(codetxt));
  zmq_send (outsock, 'finished', ZMQ_SNDMORE);
  zmq_send (outsock, '');
endwhile

% vim: sts=2 sw=2 et
