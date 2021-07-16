# .dotfiles

## kick-it-start

- clone into *~/.dotfiles* and cd into it
- `stow -nv -R .` to see what's gonna happen
- remove `-n` to let it happen if it satisfies you

## how to stow

use either one of these:

- https://github.com/kols/pkgsrc/tree/develop/sysutils/stow
- https://github.com/Homebrew/homebrew/blob/master/Library/Formula/stow.rb

## emacs

### clone melpa repo

```sh
mkdir -p ~/.ghq/github.com/melpa
git clone git@github.com:melpa/melpa.git ~/.ghq/github.com/melpa
```

### install input mono fonts

[Download](https://input.fontbureau.com/)
