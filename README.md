# [Type-driven development with Idris](https://www.manning.com/books/type-driven-development-with-idris)

As started at <https://www.meetup.com/London-Haskell/events/237369958/>

## macOS

On macOS with `libffi` installed from Homebrew, put the following in
your `~/.stack/global-projects/stack.yaml`:

```
packages: []
resolver: lts-12.14
flags:
  idris:
    ffi: true
```

Now install Idris like this:

```bash
PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig/ stack install idris
```
