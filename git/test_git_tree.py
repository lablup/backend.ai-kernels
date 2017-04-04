import os
import subprocess

json_opts = {}
try:
    import simplejson as json
    json_opts['namedtuple_as_object'] = False
except ImportError:
    import json
import pytest

from run import TerminalRunner


def run_cmd(cmd):
    res = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE)
    stdout = res.stdout.decode('utf-8')
    stderr = res.stderr.decode('utf-8')
    return stdout, stderr


class TestDoShow:
    @pytest.fixture
    def mock_runner(self, event_loop, mocker):
        mock_ev_term = mocker.MagicMock()
        mock_sock_in = mocker.MagicMock()
        mock_sock_out = mocker.MagicMock()
        runner = TerminalRunner(mock_ev_term, mock_sock_in, mock_sock_out,
                                event_loop)
        return runner

    @pytest.fixture
    def mock_args(self, mocker, tmpdir):
        mock_args = mocker.MagicMock()
        mock_args.target = 'graph'
        mock_args.path = tmpdir
        return mock_args

    def test_do_show_return_correct_commit_structure(self, mock_runner,
                                                     mock_args, tmpdir):
        # Build commit tree
        os.chdir(tmpdir)
        run_cmd('git init')
        run_cmd('git config --global user.email "tester@lablup.com"')
        run_cmd('git config --global user.name "Tester Park"')
        run_cmd('touch test.txt')
        run_cmd('git add test.txt')
        run_cmd('git commit -m "first commit"')
        run_cmd('touch test2.txt')
        run_cmd('git add test2.txt')
        run_cmd('git commit -m "second commit"')
        run_cmd('touch test3.txt')
        run_cmd('git add test3.txt')
        run_cmd('git commit -m "third commit"')
        run_cmd('git branch new-branch')
        run_cmd('git checkout new-branch')
        run_cmd('touch test4.txt')
        run_cmd('git add test4.txt')
        run_cmd('git commit -m "new branch commit"')
        run_cmd('touch test5.txt')
        run_cmd('git add test5.txt')
        run_cmd('git commit -m "second branch commit"')
        run_cmd('git checkout master')
        run_cmd('git branch another-branch')
        run_cmd('git checkout another-branch')
        run_cmd('touch test6.txt')
        run_cmd('git add test6.txt')
        run_cmd('git commit -m "another branch commit"')
        run_cmd('touch test7.txt')
        run_cmd('git add test7.txt')
        run_cmd('git commit -m "another second branch commit"')
        run_cmd('git checkout master')
        run_cmd('touch test8.txt')
        run_cmd('git add test8.txt')
        run_cmd('git commit -m "fourth commit"')
        run_cmd('git merge new-branch --no-edit')
        run_cmd('touch test9.txt')
        run_cmd('git add test9.txt')
        run_cmd('git commit -m "fifth commit"')

        mock_runner.do_show(mock_args)

        call_args = mock_runner.sock_out.write.call_args_list[0][0][0]
        assert b'media' == call_args[0]
        content = json.loads(call_args[1].decode('utf-8'))
        type = content['type']
        assert 'application/vnd.sorna.gitgraph' == type
        data = content['data']
        stdout, _ = run_cmd('git log --pretty=format:%s --all --topo-order --reverse')
        messages = stdout.split('\n')
        for idx, commit in enumerate(data):
            assert commit['message'] == messages[idx]

    def test_raise_error_if_no_target(self, mock_runner, mock_args):
        mock_args.target = 'nograph'
        with pytest.raises(ValueError):
            mock_runner.do_show(mock_args)

    def test_send_stderr_if_dir_is_not_git_repo(self, mock_runner, mock_args):
        mock_runner.do_show(mock_args)
        mock_runner.sock_out.write.assert_called_once_with([
            b'stderr', b'Not a git repository'
        ])

    def test_send_stderr_if_no_commit(self, mock_runner, mock_args, tmpdir):
        # Directory is git initialized, but no commit.
        os.chdir(tmpdir)
        subprocess.run('git init', shell=True)

        mock_runner.do_show(mock_args)
        mock_runner.sock_out.write.assert_called_once_with([
            b'stderr', b'Does not have any commits'
        ])
