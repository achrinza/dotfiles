# Rifa's Config Files

## Prerequisites

- GNU Stow

## How to use

Run these commands inside this root directory of this repository.

To apply config (will fail if any file conflict is found):

```sh
$ stow foss
```

To remove config:

```sh
$ stow -D foss
```

If conflicting regular (aka not symlinked) file is found in the target directory, either:

1. delete that regular file and re-run GNU Stow, or

2. use `stow --adopt` to replace this repo's file with the conflicting file

