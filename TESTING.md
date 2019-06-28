# User Acceptance Testing

## Framework

Purebred comes with an extensive suite of user acceptance tests. It
expects that the `purebred` binary can be found in the `$PATH`.

## Writing new user acceptance tests

A user acceptance test performs a set of actions each using scree dumps heavily.
Writing a new test I think should be mostly straight forward, but be aware of:

* Keep in mind that these tests are timing sensitive, depending on hardware: If
  you get randomly failing tests, make sure that you steps wait for brick/tmux
  to repaint the UI. Don't just **capture** and expect the UI to be repainted.
  We've added code for `sendKeys` to "wait" for the UI to be re-painted.
* even though `send-keys` accepts multiple keys, we sometimes forget that we
  perform unconscious steps. For example most input is applied with pressing
  ENTER, which you have to send via tmux as well.
* Sometimes, widgets swallow all your keys you intended to send as control
  characters if they have the focus.
  
  
## Running individual tests

You can limit which tests to run. Tasty provides extensive documentation
[on the topic](https://github.com/feuerbach/tasty#patterns), but here is an
example:

    # would only run a user acceptance test with an "error" substring
    cabal test --show-details=streaming --test-option='-p "tests/user*/*error*"'
    
    # On NixOS, keep in mind that you will need to build the binary
    # first and then run the acceptance tests in a NIX shell
    nix-build
    PATH=$PATH:result/bin cabal test --show-details=streaming --test-option='-p "tests/user*/*error*"'

## Debugging test failures

### Failure to start purebred

When `purebred` can not be started, it typically is a hint that the
environment is not correctly setup or interferes with the
compilation. We've put in great effort to make the acceptance tests as
environment independent as possible, but we can not rule out factors
we haven't accounted for. Here are a few hints you can use to debug.

* First isolate the test which fails (if all, pick one). See the
  additional test arguments you can pass to run one single test above.

* Usually temporary directories and tmux sessions are cleaned up. The
  `withTmuxSession` function has a `setUp` and a `tearDown`. Replace
  the `tearDown` with a `\_ -> pure ()` to avoid the cleanup being
  run. Then use tmux to attach to the left over session for further
  investigation:

       $ tmux list-sessions
       0: 5 windows (created Mon May 27 21:32:58 2019) [135x31] (attached)
       keepalive: 1 windows (created Fri Jun 28 15:14:32 2019) [80x24]
       purebredtest-7-error-handling: 1 windows (created Fri Jun 28 15:14:32 2019) [80x24]

       $ tmux attach-session -t purebredtest-7-error-handling

### Failures matching sub-strings

When tests fail to match a screen shot check if the sub-string is not
split up between new lines or colour codes. We match against the raw
output (including colour codes).

If you get flakyness, check if your sub-string is not too generic and
appears unchanged between repaints of the screen.

## Running purebred with a custom config

Purebred accepts environment variables which are also used in the acceptance
tests. To point purebred to a custom config:

    PUREBRED_CONFIG_DIR=configs cabal new-run purebred
