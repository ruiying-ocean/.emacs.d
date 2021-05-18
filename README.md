# About
 This is my Emacs init file(s) to implement a *self-use* cross-platform editor/IDE. It has been tested for Emacs >27 version on OSX (Big Sur), Linux(Ubuntu). Windows_NT may not be well supported because I use WSL2.

# How to install

Install Emacs and download the init files in your `$HOME` directory:
```bash
cd ~
git clone https://github.com/Leslieying/.emacs.d.git
```
Then have some beverage and wait for downloading :smile:

# Structure
I divide my init files into several different elisp files and use init.el to call them.
The structure is like:
```text
  +-- ~/.emacs.d
     +-- init.el
     +-- lisp folder
         +-- init-*.el
```

# Features
I should say *all the features are from community's efforts*, not mine.

## Modern UI

## Programming languages support

- Python
- Fortran
- R
- C
- TeX

## IDE features: auto-completion/debug/go-to-definition/document

## Spellcheck (both text and programming)

## Project manage

## Git

## Remote environment