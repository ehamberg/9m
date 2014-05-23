9m – Unicode-aware URL Shortener
================================

9m Unicode URL Shortener. Generates a shortcut from <http://9m.no> using two
unicode characters, e.g. <http://9m.no/പ湛>.

The server will choose two characters at random from the all the printable
characters and then cross its fingers and hope you use a great font.

(This is a horrible idea in many ways and was a quick hack for fun.)

To install:

1. `cabal sandbox init`
2. `cabal install --only-dependencies`
3. `cabal build`
4. `dist/build/9m/9m`
5. `open 'http://localhost:7000'`
