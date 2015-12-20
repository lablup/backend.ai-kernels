<?php

$exceptions = array();

$errhandler = function ($errlvl, $errstr, $errfile, $errline, $errcontext)
              use (&$exceptions)
{
    array_push($exceptions, array(
        'exc' => "$errstr (Line: $errline)",
        'args' => array(),
        'raised_before_exec' => false,
        'traceback' => NULL
    ));
    return true;  # do NOT continue PHP's own error processing
};

$exchandler = function ($ex) use (&$exceptions)
{
    $exc_name = get_class($ex);
    array_push($exceptions, array(
        'exc' => "$exc_name",
        'args' => array($ex->getMessage()),
        'raised_before_exec' => false,
        'traceback' => $ex->getTraceAsString()
    ));
};

chdir('/home/work');

$context = new ZMQContext();
$server = new ZMQSocket($context, ZMQ::SOCKET_REP);
$server->bind('tcp://*:2001');
echo 'serving at port 2001...';

while (true) {
    $data = $server->recvMulti();

    $exceptions = array();  # reinit the array
    set_error_handler($errhandler);
    ob_start();

    try {
	# $data[0] (cell_id) is unused currently.
	eval($data[1]);
    } catch (Exception $ex) {
	$exchandler($ex);
    } finally {
	$output = ob_get_flush();
	restore_error_handler();
	$stderr = NULL;
	$reply = array(
	    'stdout' => $output,
	    'stderr' => $stderr,
	    'exceptions' => $exceptions
	);
	$server->send(json_encode($reply));
    }
}

# vim: ts=8 sts=4 sw=4 et
