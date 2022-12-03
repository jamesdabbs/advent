# Setup

Note that auto-discovery of imports in `src/Problems` currently depends on a
light `ruby` script, so a functioning `ruby` install is required.

To auth input and prompt fetches, log in in the web and copy the `session`
cookie to `./session`.

# Usage

```bash
# Print full help message
$ stack run -- advent help

# Scaffold prompt and solution for a day
$ stack run -- advent [-y <year>] [-d <day>] init

# Run a solution
$ stack run -- advent [-y <year>] [-d <day>] run

# Run a solution and submit the answer
$ stack run -- advent [-y <year>] [-d <day>] run --submit <part>

# Run with profiling
$ stack run --profile -- advent run +RTS -p
```

