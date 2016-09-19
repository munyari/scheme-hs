<a href='https://www.recurse.com' title='Made with love at the Recurse Center'>
<img src='https://cloud.githubusercontent.com/assets/2883345/11325206/336ea5f4-9150-11e5-9e90-d86ad31993d8.png' height='20px'/></a>
[![GPL Licence](https://badges.frapsoft.com/os/gpl/gpl.svg?v=103)][license]

A Scheme interpreter in Haskell, from working through [**Write Yourself a Scheme in 48 Hours**][wyas]

[wyas]: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

In the future, I'd like to remove all deprecated code (`Control.Monad.Error`)
and implement a larger subset of R7RS scheme.

# Requirements
This was written with GHC version 8 but does not use any particularly modern
language features and should be compatible with any GHC released in the past
decade. Should work on any platform that supports GHC, but has only been tested
on GNU/Linux.

# Building and Running
You can run the following command to build and run the REPL.

```bash
ghc -o parser --make parser.hs && ./parser
```

# Contributing
Contributions are welcome under the terms of the GNU Public License, version 3.
Please see [LICENSE][license] for more details. Please discuss any major
changes in an issue before opening a large pull request, and keep pull requests
small.

[license]: https://github.com/munyari/scheme-hs/blob/master/LICENSE
