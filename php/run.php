<?php

# This class hides REPL's variable scope from user codes.
class CodeExecutor {
    public function execute($code, &$input_handler) {
        $scope = new StdClass;
        $scope->input = $input_handler;
        $f = function () use ($code) {
            @eval($code);
        };
        $f = $f->bindTo($scope);
        $f();
    }
}

function _main() {

    fclose(STDIN);

    # enable signal handlers
    declare(ticks=1);
    pcntl_signal(SIGINT, function() {
        echo "SIGINT\n";
    });
    pcntl_signal(SIGTERM, function() {
        echo "SIGTERM\n";
    });

    $context = new ZMQContext();
    $input_sock = new ZMQSocket($context, ZMQ::SOCKET_PULL);
    $input_sock->bind('tcp://*:2000');
    $output_sock = new ZMQSocket($context, ZMQ::SOCKET_PUSH);
    $output_sock->bind('tcp://*:2001');

    $ob_callback = function ($buffer) use (&$output_sock)
    {
        $output_sock->sendMulti(['stdout', $buffer]);
    };

    $exchandler = function ($ex) use (&$output_sock)
    {
        $output_sock->sendMulti(['stderr', '' . $ex]);
    };

    $errhandler = function ($severity, $message, $file, $line)
    {
        // Convert legacy errors into exceptions.
        if (!(error_reporting() & $severity)) {
            // This error code is not included in error_reporting
            return;
        }
        throw new ErrorException($message, 0, $severity, $file, $line);
    };

    $input_handler = function ($prompt) use (&$output_sock, &$input_sock)
    {
        $output_sock->sendMulti(['stdout', $prompt]);
        $output_sock->sendMulti(['waiting-input', json_encode(
            array('is_password' => False)
        )]);
        $data = $input_sock->recvMulti();
        return $data[1];
    };

    while (true) {
        $executed = false;
        try {
            $data = $input_sock->recvMulti();
            # $data[0] (cell_id) is unused currently.
            $c = new CodeExecutor();
            set_error_handler($errhandler);
            ob_start($ob_callback);
            $executed = true;
            $c->execute($data[1], $input_handler);
        } catch (ParseError $ex) {
            $output_sock->sendMulti(['stderr', '' . $ex]);
        } catch (ZMQSocketException $ex) {
            usleep(1);
            break;
        } catch (Error $ex) {
            $exchandler($ex);
        } catch (Exception $ex) {
            $exchandler($ex);
        } finally {
            if ($executed) {
                restore_error_handler();
                ob_end_flush();
                $output_sock->sendMulti(['finished', '']);
            }
        }
    }
    echo "terminated\n";
}

_main();

# vim: ts=8 sts=4 sw=4 et
