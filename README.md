# Chain of Command

[![asciicast](https://asciinema.org/a/JyiFGL7fx9I0e0KswRolLJycm.svg)](https://asciinema.org/a/JyiFGL7fx9I0e0KswRolLJycm)

Analyzes your `.bash_history` or similar file to identify commonly reoccurring sequences and workflows that may benefit from extraction into functions or aliases.

## Installation

```shell
git clone https://github.com/timhwang21/chain-of-command/
cd chain-of-command

# You can call the executable directly
./bin/coc [options]

# You can run Main.hs as a script if you have Haskell installed
runhaskell Main.hs

# Optionally, you can make this accessible in $PATH
ln -s $( pwd )/bin/coc /usr/local/bin/coc
coc [options]
```

## Usage

```shell
# Passing file path
coc -f ~/.bash_history

# Reading from stdin -- useful for filtering common commands
cat ~/.bash_history | grep -v "git" | coc
```

## Options

#### `--file` (`-f`)

Path to history file. Overrides `STDIN`. If not supplied, a history file must be piped to `coc`.

#### `--minCount` (`-m`)

Minimum number of matches needed to be displayed, to reduce noise.

Defaults to `5`.

#### `--runLength` (`-r`)

Number of commands that make up a sequence. Must be greater than one, and should generally be less than 3 for realistic data. For example, a run length of 3 will print repeated sequences of three commands.

Defaults to `2`.

#### `--wordCount` (`-w`)

Number of words to pull from commands for analysis. Flags are also treated as words. For example, if `-w 1` is passed, `git push`, `git pull`, `git fetch` would all be counted as `git`. With `-w 2`, `git push` and `git push -f` would both be treated as the same command.

Defaults to `2`.

## Sample Output

```shell
coc -f ~/.bash_history | head -5
# v                → tig             : 215
# git fetch        → grom            : 172
# tig              → v               : 98
# tig              → amend           : 81
# tig              → git fetch       : 44

# Show 4-command sequences
coc -f ~/.bash_history --runLength=4 | head -5
# v                → tig              → v                → tig             : 24
# tig              → v                → tig              → amend           : 13
# v                → tig              → amend            → tig             : 12
# tig              → git fetch        → grom             → gp              : 10
# tig              → gp               → v                → tig             : 10

# Show least used sequences that occur over 15 times
coc -f ~/.bash_history --minCount=15 | tail -5
# tig              → ga              : 16
# tig              → gb              : 16
# tig              → gch master      : 16
# tig              → yarn tsc        : 16
# tig              → clean           : 15

# Ignore sequences containing certain commands
cat ~/.bash_history | grep -v "git\|vim\|tmux\|tig" | coc | head -5
# gch master       → grom            : 47
# v                → amend           : 46
# gp -f            → v               : 39
# grom             → gp -f           : 31
# grom             → v               : 29
```

## Development

### Build

```shell
ghc -o bin/coc Main.hs
```
