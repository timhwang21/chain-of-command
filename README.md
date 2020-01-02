# Chain of Command

[![asciicast](https://asciinema.org/a/eatpLhfcSLxYx2bMJRATnjhGF.svg)](https://asciinema.org/a/eatpLhfcSLxYx2bMJRATnjhGF)

Analyzes your `.bash_history` or similar file to identify commonly reoccurring sequences and workflows that may benefit from extraction into functions or aliases.

## Usage

```shell
./bin/chain-of-command ~/.bash_history
```

## Sample Output

```shell
./bin/chain-of-command ~/.bash_history | head -5
# v → tig: 214
# git fetch → grom: 171
# tig → v: 96
# tig → amend: 79
# tig → git fetch: 44

./bin/chain-of-command ~/.bash_history --runLength=5 | head -5
# tig → amend → tig → amend → tig: 7
# git fetch → grom → gp -f → v → tig: 6
# tig → git stash → gch master → git fetch → grom: 6
# tig → v → tig → v → tig: 6
# tig → amend → git fetch → grom → gp -f: 5

./bin/chain-of-command ~/.bash_history --minCount=15 | tail -5
# tig → ga: 16
# tig → gb: 16
# tig → gch master: 16
# tig → yarn tsc: 16
# tig → clean: 15
```

## Options

#### `--minCount` (`-m`)

Minimum number of matches needed to be displayed, to reduce noise.

Defaults to `5`.

#### `--runLength` (`-r`)

Number of commands that make up a sequence. Must be greater than one, and should generally be less than 3 for realistic data. For example, a run length of 3 will print repeated sequences of three commands.

Defaults to `2`.

#### `--wordCount` (`-w`)

Number of words to pull from commands for analysis.

Defaults to `2`.

## Development

### Build

```shell
ghc -o bin/chain-of-command Main.hs
```

### TODO

* [x] Change CLI arguments into flags
