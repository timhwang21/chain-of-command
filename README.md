# Chain of Command

[![asciicast](https://asciinema.org/a/eatpLhfcSLxYx2bMJRATnjhGF.svg)](https://asciinema.org/a/eatpLhfcSLxYx2bMJRATnjhGF)

Analyzes your `.bash_history` or similar file to identify commonly reoccurring sequences and workflows that may benefit from extraction into functions or aliases.

## Usage

```shell
runhaskell Main.hs [path] [runLength]
```

## Sample Output

```shell
runhaskell Main.hs ~/.bash_history 1 | head -5
# tig: 92
# vim: 31
# ls: 25
# git checkout: 22
# clear: 15

runhaskell Main.hs ~/.bash_history 2 | head -5
# git checkout > tig: 10
# tig > git checkout: 9
# tig > git push: 9
# git pull > tig: 8
# gs > tig: 7

runhaskell Main.hs ~/.bash_history 3 | head -5
# tig > git checkout > tig: 7
# git checkout > tig > tig: 4
# git pull > git pull > tig: 3
# git pull > tig > git push: 3
# gs > tig > git reset: 3
```

## Options

#### `path`

Path to a plaintext history file.

#### `runLength`

Number of commands that make up a sequence. Must be greater than one, and should generally be less than 3 for realistic data.

## TODO

* [ ] Change CLI arguments into flags
