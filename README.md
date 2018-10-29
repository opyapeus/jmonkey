# jmonkey

[![Build status](https://travis-ci.org/opyapeus/jmonkey.svg?branch=master)](https://travis-ci.org/opyapeus/jmonkey)

Jmonkey is very restricted but handy EDSL for javascript.

The DOM effect that jmonkey can do is just to change classes and ids of HTML elements.

So it only supports some on-actions and condition checks.

Instead of limited functions, it can be called internally unlike other rich javascript EDSLs that require external calls.

If you manage some states for complex frontend actions, jmonkey won't be usable.

Jmonkey may be useful when you implement some actions that css can not handle.

## Example

A practical implementation is shown in [example](example).

Clone this repository first, and execute following.

```sh
stack run
```

Then access to localhost:3000.

## Documentation

- [API documentation on Hackage](http://hackage.haskell.org/package/jmonkey)

## Contribution

If you find a bug or want new features or else, making issues and PRs are very welcome.