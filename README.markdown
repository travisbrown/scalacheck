# ScalaCheck 2.0.0

This is the `scalacheck2` branch of ScalaCheck. It is under active development,
barely usable, lacks documentation, fails testing, and will frequently be
rebased.

If you're looking for something useful, you should head over to the `master`
branch!

[![Build Status](https://secure.travis-ci.org/rickynils/scalacheck.png?branch=scalacheck2)](http://travis-ci.org/rickynils/scalacheck)

## Implementation Goals

* Keep end-user API as intact as possible. No fundamental conceptual changes,
  still just generators and properties. This will not be ScalaCheck2, just
  ScalaCheck 2.0.0.

* Support exhaustible generators. This should make it possible for ScalaCheck
  to prove properties true, if the generator domain is small enough.

* Better support for reproducible test runs. Failed tests should report a seed,
  and that seed can be used to re-run the exact same property test. Possibly,
  `sbt` integration could be implemented to make it even more convenient.

* Generators will become more central, properties will just be generators with
  a specific return type.

* Shrinking will be closely integrated with generators, which should hopefully
  solve the longstanding problem that ScalaCheck comes up with confusing values
  when shrinking failed test cases. Should also allow the shrinkers to be
  smarter.

## Feedback

At some point, a Release Candidate build will be released in order to collect
feedback from the community. Of course, feedback is welcome at any point in
time, in the form of GitHub issues, PRs, code comments, mails or tweets.
