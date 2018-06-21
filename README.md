# parsix

Adventures in parser combinators.

This is basically a [Trifecta](https://github.com/ekmett/trifecta) clone, i.e. an implementation of the [parsers](https://github.com/ekmett/parsers/) interface, with the following differences:

* Add error recovery (see `withRecovery`) based on [Megaparsec](https://github.com/mrkkrp/megaparsec)'s.
* Use the `text` library instead of `bytestring` for input strings. This means that the library interfaces better with the rest of the Haskell library ecosystem and that slicing (see `sliced`) returns `Text`.
* Use the [prettyprinter](https://github.com/quchen/prettyprinter) library for pretty-printing.
* Actually implement the highlighting interface from `parsers`. This means that error messages that show input code are syntax highlighted.
