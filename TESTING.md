# User Acceptance Testing

## Framework

The current test framework uses tmux and executes certain key strokes against
the started application.

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
    stack test --test-arguments='-p "tests/user*/*error*"'

## Running purebred with a custom config and stack

Purebred accepts environment variables which are also used in the acceptance
tests. To run purebred with stack and a custom config, here is an example:

    # Purebred invokes: "stack ghc --" + additional compile arguments
    GHC="stack" GHC_ARGS="ghc --" PUREBRED_CONFIG_DIR=configs stack exec purebred
