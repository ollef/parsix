# parsix

Adventures in parser combinators.

This is basically a [Trifecta](https://github.com/ekmett/trifecta) clone, with the following differences:

* Add error recovery (see `withRecovery`).
* Remove support for incremental parsing. This simplifies the library internals. (I can probably be convinced to add it back if someone needs it though.)
* Use the `text` library instead of `bytestring` for input strings. This means that the library interfaces better with the rest of the Haskell library ecosystem and that slicing (see `sliced`) returns `Text`.
