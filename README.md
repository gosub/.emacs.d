# Emacs Configuration

Personal Emacs configuration managed with GNU Stow.

## Structure

### init.el

The configuration is organized in the following sections:

- **PACKAGE CEREMONY** - Package archives setup (MELPA)
- **PACKAGES OF MINE** - Configuration for the personal packages in `user-lisp/`
- **BUILT-IN PACKAGES CONFIG** - Configuration for Emacs built-in packages
- **EXTERNAL PACKAGES** - Third-party packages from MELPA
- **AUTOSAVE AND BACKUP** - File backup and auto-save settings
- **UX AND GFX** - Visual settings and user experience

### user-lisp/

Directory containing personal elisp packages, each file a self-contained
package. Emacs 31 picks this directory up on its own: before loading
`init.el` it byte-compiles (and native-compiles) the files, scrapes their
`;;;###autoload` cookies into `.user-lisp-autoloads.el` and adds the
directory to `load-path`. See the Info node `(emacs) User Lisp Directory`.

A new command therefore only needs a `;;;###autoload` cookie to be reachable
with `M-x`; `init.el` mentions a package only when it needs configuration,
such as options, keybindings or load order. `M-x prepare-user-lisp` picks up
changes without restarting Emacs.

### early-init.el

Pre-initialization settings that run before the package system and GUI are
loaded.

## License

All code in this repository is licensed under the GNU General Public License
version 3 (GPL-3.0) unless otherwise specified in individual files.
