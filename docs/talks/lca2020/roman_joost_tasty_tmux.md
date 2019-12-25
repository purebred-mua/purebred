% Automated acceptance tests for terminal applications
% Róman Joost
% 15th January 2020

# TOC

## Presentation Outline

* Terminology
* Motivation
* What is the Test Framework
* Pitfalls
* Gains
* Future Ideas

## About Me

* live in Brisbane
* contributed to The GNU Image Manipulation program for years
* worked at Red Hat on various internal systems
* working for Superloop, an ISP based in Brisbane on a firewalling appliance

# Terminology

## Terminal Applications

* full text user interfaces not command line interfaces (CLI)
* [Purebred](https://github.com/purebred-mua) - A terminal based mail user agent based on notmuch
* no sophisticated test frameworks for terminal applications

## System-/Black Box Testing

* Selenium/Puppeteer are frameworks for web applications
* initial costs to setup are higher
* depends on input methods in order to interact with the system under test
  * prone to random failures

## Functional-/Integration Testing

* less API dependendent as it tests bigger parts of the application

## Unit Testing

* very quick to write
* fast to execute
* helps finding and debugging problems very quickly

## Acceptance Testing

* Test whether the system/feature fullfills it's acceptance criteria
* testing from the users point of view (e.g. user stories)
* can be any type (unit-, integration-, system tests)

## How is the Testing Carried Out?

Manual vs Automated

* Humans are very good at finding outliers or differences
* automated is "infinitely" repeatable and fast

##
![](automatedVsManualTesting.gif){ width=600 }

# Motivation

## Acceptance Testing Terminal Applications

* Selenium doesn't exist for terminal applications
* support for command line interface apps, but less for text UIs

## Unit Testing the Terminal Application UI

* usually straight forward for parts
* ... until

## Side-effects
![](unnecessary_automation.webp)


::: notes
  * repaints the terminal
  * does networking
  * etc
  * overall: has side-effects
:::

## Mocking to the Rescue!

* your application internals change, but your mocks/tests stay the same without noticing
* however can become a burden with a maturing code base
  * "Is this test still valid?"
  * test code changes to make it "pass", but encodes wrong program behaviour

## What is tmux?

* terminal multiplexer allowing to run multiple terminal programs in a single terminal
* You may know of GNU/screen which is similar

## Why tmux?

* allows to "remote control" the terminal with keyboard/mouse events
* allows to "screen shot" the terminal

# How do we use tmux for testing

## Test Framework: tasty-tmux

* moved into separate library from our main repository: purebred

## Demo

![](demo.gif)

## Setup

* starts a tmux session (which starts the server)
* use a keep alive session (see pitfalls later)
* prepare test environment (application specific)
* register clean up

## Example Code

* test steps provide input and expectations
* tasty-tmux "detects" a screen redraw by checking for strings or
  regular expressions in the terminal output (polling)
* failure shows the last screen capture and the raw output

## Test

```
test1 :: TestCase SharedEnv
test1 = withTmuxSession setup teardown "putFile" $ \step -> do
  -- test environment is availabe via ask
  TestEnv _ sharedDir testDir <- ask

  -- send a command to the tmux session and wait for "Done"
  sendLine ("myProg putFile " <> sharedDir) (Substring "Done.")

  -- save a snapshot of the terminal state and make some assertions
  snapshot
  assertSubstringS "The output should contain this substring"
  assertRegexS "The output should match this [Rr]eg[Ee]x"
```

## Teardown

* kills all tmux sessions
* removes maildirs and runs registered clean up code

## CI

* **All** tests are run before code is merged

# The Pitfalls

## Timing Problems

* most common
* happens when state changes of the system are not recognised by the test framework
* best to tackle with deep knowledge of the system and it's (state-) behaviour
* use a churn CI setup running tests randomly/different speed

## Backwards Compatibility

* Different behaviour between older tmux releases in Travis

```
old: \ESC\[30m;43mPurebred
new: \ESC[30m\ESC[43mPurebred
```

<img src="./tmux_version_diff_demo.svg" />

## Races

* between new-session and tmux server

## Environmental Differences

* Assertion against terminal colours
* Environment variables set to different values (e.g. `$TEMP`, `$PATH`, `$TERM`)
* Be careful which type of terminal you emulate
* Line wrapping in the terminal (your shell prompt already takes up a fair amount of characters)

## Underlying Hardware or it's Emulation

* e.g. MTU is slightly off between network devices
* Firmware is software too

# Preventative Measures

## Protocol Occurrence of Random Failures

## Have CI Run all Tests Before Merge

## Have CI Run all Tests Regularly (e.g. CRON)

## Run Tests in Random Order/Speed

## Time to Debug Failures

* improves debugging skills of your engineers
* better ability to reason out a complex system

# The Gains

## Ship when CI is green

![](throw_stuff_on_christmas_tree.gif)

## Confidence that the pull request does not regress the application behaviour

## Smaller likelihood that internal changes regress the application smaller

## Fast!*

* ... compared to manual testing which can take days

## Benefits of automation

* repetition: humans are getting tired much quicker than the hardware does
* can be optimised
* infinitely repeatable

# Competitors

* plenty of solutions for CLI (not terminal UI)
* from 2011: [IBM "Rational Functional Tester"](https://www.ibm.com/developerworks/rational/library/test-terminal-based-applications-automatically/index.html)
* Python: [hecate](https://github.com/DRMacIver/hecate)
* Python: [libtmux](https://github.com/tmux-python/libtmux)
* code using tmux, but internal to project, e.g.o [https://github.com/devfort/conch/blob/master/conch_check.py](https://github.com/devfort/conch/blob/master/conch_check.py)


# Future Ideas

* More reliable way to determine UI has been repainted (see https://github.com/purebred-mua/tasty-tmux/issues/7)
* Regexes behave differently on different OSes (https://github.com/purebred-mua/tasty-tmux/issues/9)
* Run all tests in parallel (https://github.com/purebred-mua/tasty-tmux/issues/18)

## References

* Purebred project on github [https://github.com/purebred-mua](https://github.com/purebred-mua)
* tasty-tmux [https://github.com/purebred-mua/tasty-tmux](https://github.com/purebred-mua/tasty-tmux)
* Behaviour driven development (e.g. Cucumber)
* E-mail: roman@bromeco.de
* Twitter: Fraser Tweedale - @hackuador | Róman Joost - @romanofski

# Thank you

Join Purebred to learn some real world Haskell!

https://github.com/purebred-mua