# renerd-icons-dired

Fast Nerd Icons integration for Emacs Dired.

`renerd-icons-dired` is designed around large directories.  It does **not** do a
whole-buffer overlay refresh when the mode is enabled or when the window scrolls.
It annotates visible lines immediately, queues a small look-ahead region, reuses
existing overlays, and caches icon strings.

This is an independent implementation.  It uses the public `nerd-icons` API, but
is not derived from `nerd-icons-dired`.

## Install

Put this directory on `load-path` and make sure `nerd-icons` is available:

```elisp
(add-to-list 'load-path "/path/to/renerd-icons-dired")
(require 'renerd-icons-dired)
(add-hook 'dired-mode-hook #'renerd-icons-dired-mode)
```

If you are replacing `nerd-icons-dired`, remove its hook first:

```elisp
(remove-hook 'dired-mode-hook #'nerd-icons-dired-mode)
(add-hook 'dired-mode-hook #'renerd-icons-dired-mode)
```

## Behavior

- Icons are displayed before Dired file names.
- `.` and `..` use `renerd-icons-dired-special-prefix-string`, matching the blank
  prefix behavior of `nerd-icons-dired`.
- File and directory icon functions are customizable:
  - `renerd-icons-dired-file-icon-function`
  - `renerd-icons-dired-dir-icon-function`
- `renerd-icons-dired-refresh` refreshes displayed windows and queued look-ahead
  entries.  With a prefix argument it clears all overlays in the buffer first.
- `renerd-icons-dired-clear-cache` clears icon caches and refreshes active Dired
  buffers.

## Performance design

The hot path is bounded by displayed lines, not directory size:

1. Visible window lines are annotated synchronously so the user sees icons right
   away.
2. A configurable look-ahead area is queued with `renerd-icons-dired-prefetch-lines`.
3. Queued work runs in idle chunks of at most `renerd-icons-dired-chunk-size`.
4. Filename positions come from the `dired-filename` text property instead of
   `dired-get-filename`, and directory detection reads the ls type column
   instead of calling `file-directory-p` (symlinks still fall back to it).
5. A buffer-wide coverage record `(generation tick start end)` makes repeated
   window updates and small scrolls free; only the uncovered fringe of a range
   is ever scanned.
6. Buffer edits drop affected overlays through `after-change-functions`, so
   renames do not leave stale icons.
7. Icon strings are cached.  Directory icons are cached by absolute path because
   Nerd Icons may check `.git`, symlink and remote status.  File icons are
   cached by the provided Dired file name and icon function.
8. With the default `nerd-icons-icon-for-file`, names that cannot match
   `nerd-icons-regexp-icon-alist` (checked via two combined regexps) resolve
   through a per-extension cache instead of calling the icon function for
   every name.  Set `renerd-icons-dired-fast-file-icons` to nil to always call
   the configured function; results are identical either way.

Tradeoffs:

- The package does not eagerly annotate every off-screen entry.  Icons appear for
  newly visible lines as you scroll.
- Directory icon cache entries can become stale after file-system state changes
  such as creating `.git`; call `renerd-icons-dired-clear-cache` or revert the
  Dired buffer when needed.
- TRAMP directories should benefit from visible-range-first updates, but TRAMP
  was not validated in the initial tests.

## Tests and benchmark

If `nerd-icons` is not already in your Emacs load path, pass its path via
`LOAD_PATH_FLAGS`:

```sh
make check LOAD_PATH_FLAGS='-L /path/to/nerd-icons'
```

To compare against `nerd-icons-dired`, also add its load path:

```sh
make benchmark LOAD_PATH_FLAGS='-L /path/to/nerd-icons -L /path/to/nerd-icons-dired'
```

The benchmark creates temporary directories with 1,000 and 10,000 files.  It
measures mode enable and ten refreshes.  For `renerd-icons-dired`, the benchmark
simulates a 40-line visible window and measures visible + look-ahead work; for
`nerd-icons-dired`, enable/refresh annotate the whole buffer.  This compares the
intended update strategies, not identical eager-all-entries behavior.

Local run on macOS / Emacs 31.1 (unique file names):

| Files | Backend | Enable | 10 refreshes | Sweep (all lines, warm icon cache) | Idle annotate all (cold icon cache) |
| ---: | --- | ---: | ---: | ---: | ---: |
| 1,000 | renerd-icons-dired | 92 ms | 0.12 ms | 6 ms | 6 ms |
| 1,000 | nerd-icons-dired | 43 ms | 116 ms | — | — |
| 10,000 | renerd-icons-dired | 0.13 ms | 0.14 ms | 30 ms | 162 ms |
| 10,000 | nerd-icons-dired | 440 ms | 4,974 ms | — | — |

"Sweep" applies one window update per disjoint 40-line window across the whole
buffer after the icon cache is warm (~0.12 ms per window); "idle annotate all"
covers the first full-buffer annotation including per-name icon resolution.

The first renerd run still includes one-time Dired/Nerd Icons/cache setup not
fully isolated by this simple benchmark.  Use repeated, interleaved
fresh-process runs before claiming a stable speedup factor.

## License

GPL-3.0-or-later.  See [LICENSE](LICENSE).
