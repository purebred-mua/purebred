# User Acceptance Testing

## Framework

The current test framework uses tmux and executes certain key strokes against
the started application.

## Writing new user acceptance tests

A user acceptance test performs a set of actions each using scree dumps heavily.
Writing a new test I think should be mostly straight forward, but be aware of:

* even though `send-keys` accepts multiple keys, we sometimes forget that we
  perform unconscious steps. For example most input is applied with pressing
  ENTER, which you have to send via tmux as well.
* Sometimes, widgets swallow all your keys you intended to send as control
  characters if they have the focus.
* If your test takes a while, you can observe what is happening by attaching to
  the tmux session.
