# User Acceptance Testing

## Framework

I spent the week on getting a little proof-of-concept "framework" running and I
think it's ok enough to build upon. Running most commands through tmux comes
with it's own set of problems and is fiddly, but so far it seems to yield the
best possibilities to test the application.

My current state is in 21d494dbd61c654b1176a488e6ecbfb7abe57967 and it runs a
simple test which starts the application and takes a screen dump. The dump is
written to a file which is compared to a golden file using
[tasty-golden](https://hackage.haskell.org/package/tasty-golden).

## Writing new user acceptance tests

A user acceptance test performs a set of actions resulting in a screen dump and
asserts it against a previously recorded screen dump, the golden file.

* create a new test case in `test/TestUserAcceptance.hs`
* define the steps using the `ApplicationStep` data type. Be mindful when you
  send literal input and what "special" key you need to press to apply it in the
  application (e.g. user input needs an additional `Enter`)
* run the tests and
  [tasty-golden](https://hackage.haskell.org/package/tasty-golden) will
  automatically create the golden file with the last snapshot of the application
  state in tmux. However also be mindful, that this might not be the application
  state you want to assert against. Which brings us to...
* test your steps before by creating a tmux test session and execute the
  `send-keys` steps as to confirm the desired end state of the application you
  are asserting your screen shot against

### The way it works as a TL;DR:
I'm invoking tmux commands as separate processes on the test suite side for
setup, communication and teardown.

On the application side, I'm using a socket to communicate with the application
to signal purebred has started up. In the tests I'm forking a new thread which
waits for a ready signal, while I start the application. I then use a special
key binding I'm sending through tmux to signal that the application has started
successfully and is ready for use. That all is wrapped in a timeout to avoid
blocking infidelity. Not race free, but narrows down possible failures. I
thought we could conditionally compile this test code.

Once started, I can then perform all common user actions by sending key input to
the application. This has potential to also register a mail catcher and test the
mail sending as well. At the moment I brought back a mandatory `--database`
option which purebred has to be invoked with, but this should obviously be
either conditionally compiled or simply optional.

### Problems so far:
* Although not specific to this branch, I ran into problems linking hs-notmuch
  under Ubuntu. Travis uses Ubuntu and I was trying to find out why my first
  version failed. HEAD (05272915f19320362d3a2eb2ffc80eacd7162b7a) seems to be
  compiling/linking fine, but I ran into segfaults later on. I have not
  investigated further. Trusty seems to be "pretty old", perhaps it has
  something to do with old versions of dependencies. Not sure. The linker error
  is:
```--  While building package notmuch-0.1.0.0 using:
      /root/.stack/setup-exe-cache/x86_64-linux/Cabal-simple_mPHDZzAJ_1.24.2.0_ghc-8.0.2 --buil
ddir=.stack-work/dist/x86_64-linux/Cabal-1.24.2.0 build lib:notmuch --ghc-options " -ddump-hi -
ddump-to-file"
    Process exited with code: ExitFailure 1
    Logs have been written to: /home/dev/purebred/.stack-work/logs/notmuch-0.1.0.0.log

    Configuring notmuch-0.1.0.0...
    Preprocessing library notmuch-0.1.0.0...
    /usr/bin/ld: .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/Notmuch/Talloc.dyn_o: unrec
ognized relocation (0x29) in section `.text'
    /usr/bin/ld: final link failed: Bad value
    collect2: error: ld returned 1 exit status
    `gcc' failed in phase `Linker'. (Exit code: 1)
```
* No logs are currently written during a test session, which makes debugging a
  PITA. That is something which needs to be added too, otherwise figuring out
  what went wrong will remain painful. There is potential to use tmux
  `pipe-pane` though, but I haven't investigated that further.
* To further narrow down startup problems, I thought about extending the socket
  communication to indicate maybe a difference between 'starting up' and 'being
  ready'.
