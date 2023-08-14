9m – Unicode-aware URL Shortener
================================

[![builds.sr.ht status](https://builds.sr.ht/~ehamberg/9m.svg)](https://builds.sr.ht/~ehamberg/9m?)

9m Unicode URL Shortener. Generates a shortcut from <http://9m.no> using two
unicode characters, e.g. <http://9m.no/പ湛>.

The server will choose two characters at random from the all the printable
characters and then cross its fingers and hope you use a great font.

(This is a horrible idea in many ways and was a quick hack for fun.)

To install/run:

0. [Install *stack*](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
1. `stack build`
2. `stack exec 9m`
3. `open 'http://localhost:7000'`
