# Installation Guide

This guide covers installing Louter on various platforms and build systems.

## Prerequisites

- **GHC 9.6+** (Glasgow Haskell Compiler)
- **Stack** or **Cabal** (Haskell build tools)
- **Git** (for cloning the repository)

## Platform-Specific Setup

### macOS

#### Using Homebrew

```bash
# Install GHC and Stack
brew install ghc stack

# Or install GHC and Cabal
brew install ghc cabal-install
```

#### Using GHCup (Recommended)

```bash
# Install GHCup (Haskell toolchain installer)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install recommended toolchain
ghcup install ghc 9.6.7
ghcup install stack latest
ghcup install cabal latest

# Set as default
ghcup set ghc 9.6.7
```

### Linux

#### Ubuntu/Debian

```bash
# Using GHCup (recommended)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Or using APT (older versions)
sudo apt-get update
sudo apt-get install ghc cabal-install
```

#### Fedora/RHEL

```bash
# Using GHCup (recommended)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Or using DNF
sudo dnf install ghc cabal-install
```

#### Arch Linux

```bash
sudo pacman -S ghc stack cabal-install
```

### Windows

#### Using Chocolatey

```powershell
# Install Chocolatey first (if not installed)
# Then install GHC and Stack
choco install ghc stack
```

#### Using GHCup

Download and run the installer from: https://www.haskell.org/ghcup/

### NixOS / Nix

```bash
# Using Nix flakes
nix develop

# Or using nix-shell
nix-shell
```

## Installation Methods

### Method 1: Using Stack (Recommended for Beginners)

```bash
# Clone the repository
git clone https://github.com/yourusername/louter.git
cd louter/haskell/louter

# Build the project
stack build

# Install executables to ~/.local/bin
stack install

# Verify installation
louter-server --help
```

**Advantages:**
- Isolated dependencies
- Reproducible builds
- Easy to get started

### Method 2: Using Cabal

```bash
# Clone the repository
git clone https://github.com/yourusername/louter.git
cd louter/haskell/louter

# Update package index
cabal update

# Build the project
cabal build

# Install executables
cabal install

# Verify installation
louter-server --help
```

**Advantages:**
- More control over dependencies
- Better integration with system GHC

### Method 3: Using Nix

```bash
# Clone the repository
git clone https://github.com/yourusername/louter.git
cd louter

# Enter development shell
nix develop

# Build with cabal inside Nix shell
cd haskell/louter
cabal build
```

**Advantages:**
- Fully reproducible builds
- Isolated environment
- Declarative dependencies

## Building from Source

### Debug Build (Fast compilation)

```bash
cd haskell/louter

# Stack
stack build --fast

# Cabal
cabal build -O0
```

### Release Build (Optimized)

```bash
cd haskell/louter

# Stack
stack build --ghc-options="-O2"

# Cabal
cabal build -O2
```

## Installing as a Library

### Adding to Your Project

**stack.yaml:**
```yaml
extra-deps:
  - git: https://github.com/yourusername/louter.git
    commit: <commit-hash>
    subdirs:
      - haskell/louter
```

**package.yaml:**
```yaml
dependencies:
  - louter
```

**Or in .cabal file:**
```cabal
build-depends:
  base >= 4.7 && < 5,
  louter
```

### Local Development

```bash
# Clone the repository
git clone https://github.com/yourusername/louter.git

# In your project's stack.yaml:
packages:
  - .
  - path/to/louter/haskell/louter

# Or in cabal.project:
packages:
  .
  path/to/louter/haskell/louter
```

## Verifying Installation

### Check Version

```bash
louter-server --help
```

### Run Health Check

```bash
# Start the server
louter-server --config config.yaml --port 9000

# In another terminal, check health
curl http://localhost:9000/health
```

Expected output:
```json
{
  "service": "louter",
  "status": "ok"
}
```

### Run Tests

```bash
cd haskell/louter

# Stack
stack test

# Cabal
cabal test
```

## Troubleshooting

### Common Issues

#### "Could not find module..."

```bash
# Update package database
cabal update

# Clean and rebuild
cabal clean
cabal build
```

#### Stack resolver errors

```bash
# Try updating Stack
stack upgrade

# Or use a specific resolver
stack build --resolver lts-22.0
```

#### GHC version mismatch

```bash
# Check GHC version
ghc --version

# Install correct version with GHCup
ghcup install ghc 9.6.7
ghcup set ghc 9.6.7
```

#### Out of memory during compilation

```bash
# Limit parallel jobs
stack build --jobs 1

# Or for Cabal
cabal build --jobs=1
```

#### Linker errors on macOS

```bash
# Install Xcode Command Line Tools
xcode-select --install

# Or update existing tools
softwareupdate --install -a
```

### Platform-Specific Issues

#### Linux: Missing C libraries

```bash
# Ubuntu/Debian
sudo apt-get install build-essential libgmp-dev zlib1g-dev

# Fedora/RHEL
sudo dnf install gmp-devel zlib-devel
```

#### macOS: dylib warnings

These are typically harmless warnings about library versions. If they cause issues:

```bash
# Rebuild with updated libraries
stack clean
stack build
```

## Development Setup

### Editor Integration

#### VS Code

Install extensions:
- Haskell Language Server
- Haskell Syntax Highlighting

#### Emacs

```elisp
;; In your init.el
(use-package haskell-mode)
(use-package lsp-haskell
  :config
  (setq lsp-haskell-server-path "haskell-language-server"))
```

#### Vim/Neovim

```vim
" Install coc-haskell or use built-in LSP
Plug 'neovimhaskell/haskell-vim'
```

### Setting up HLS (Haskell Language Server)

```bash
# Install via GHCup
ghcup install hls

# Or via Stack
stack install haskell-language-server
```

## Next Steps

- Read the [Getting Started Guide](docs/haskell/GETTING_STARTED.md)
- Configure your first proxy: [Proxy Setup](docs/haskell/PROXY_SETUP.md)
- Learn the library API: [Library API Guide](docs/haskell/LIBRARY_API.md)
- Start contributing: [Contributing Guide](CONTRIBUTING.md)

## Support

- 📖 [Documentation](docs/)
- 🐛 [Report Issues](https://github.com/yourusername/louter/issues)
- 💬 [Discussions](https://github.com/yourusername/louter/discussions)
