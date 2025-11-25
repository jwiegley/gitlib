# git-monitor

Passively snapshots working tree changes efficiently.

## Building

This package is part of the gitlib multi-package repository. Build from the repository root:

```bash
# From the gitlib repository root
nix build    # Builds git-monitor (default package)
```

Or from this directory:

```bash
# From git-monitor directory
nix build ../#default
```

## Development

For development with direnv:

```bash
# From git-monitor directory
direnv allow  # The .envrc file loads the parent flake
```

Or manually enter the development shell:

```bash
nix develop ../#default
```

## Dependencies

- gitlib >= 3.3.0
- gitlib-libgit2 >= 3.3.0
- Other dependencies listed in git-monitor.cabal

All dependencies are managed at the repository root level via the main flake.nix and cabal.project files.
