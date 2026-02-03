# Emacs Configuration

Personal Emacs configuration managed with GNU Stow.

## Structure

### init.el

The configuration is organized in the following sections:

- **PACKAGE CEREMONY** - Package archives setup (MELPA) and load paths
- **PACKAGES OF MINE** - Personal packages from `gg-lisp/`
- **BUILT-IN PACKAGES CONFIG** - Configuration for Emacs built-in packages
- **EXTERNAL PACKAGES** - Third-party packages from MELPA
- **AUTOSAVE AND BACKUP** - File backup and auto-save settings
- **UX AND GFX** - Visual settings and user experience

### gg-lisp/

Directory containing personal elisp packages. Each file is a self-contained
package loaded via `use-package`. The directory is added to `load-path` at
startup.

### early-init.el

Pre-initialization settings that run before the package system and GUI are
loaded.

## License

All code in this repository is licensed under the GNU General Public License
version 3 (GPL-3.0) unless otherwise specified in individual files.
