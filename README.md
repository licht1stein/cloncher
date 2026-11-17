# Cloncher

Cloncher ports the desktop launcher from `~/dotfiles` to Jolt and CUI. It reads XDG application entries, Niri windows, the Emacs project list, and local files. The launcher menu, ranking, session state, calculator integration, and restricted desktop Lisp evaluator run in Jolt. Desktop actions still call the programs and helper scripts they automate.

## Develop in a window

```sh
jolt dev
jolt dev off
```

The dev window stays open when the embedded launcher is dismissed or a result is accepted. **Open launcher** starts a fresh preview session. The preview accepts input and navigates the same views as production. Its completed actions return it to Off without executing desktop actions.

From another terminal in the project:

```sh
jolt eval desktop --ns user -e '(cloncher.dev/state)'
jolt eval desktop --ns user -e '(cloncher.dev/off!)'
jolt eval desktop --ns user -e '(cloncher.dev/show! :projects)'
jolt eval desktop --ns user -e '(cloncher.dev/show! {:kind :apps :initial-query "editor" :fixture {:rows [{:label "Editor" :subtitle "A fixture" :action :fixture}]}})'
jolt eval desktop --ns user -e '(cloncher.dev/reload!)'
jolt eval desktop --ns user -e '(cloncher.dev/screenshot!)'
```

`show!` accepts a view keyword or a view map. A `:fixture` can supply `:rows` or an `:error` for repeatable previews. `screenshot!` returns the written PNG path, defaulting to `.dev/screenshots/`. The session reducer lives in [machine.clj](src/cloncher/machine.clj) and the shared panel presentation in [view.clj](src/cloncher/view.clj).

## Run the launcher

```sh
jolt prod
jolt prod menu connect
jolt prod projects
jolt prod files
printf '%s\n' Alpha Beta | jolt prod dmenu -p "Choose a line"
jolt prod secret -p "Password"
```

For a keybinding or a script, invoke `bin/cloncher` by absolute path. Production opens one launcher window and exits on dismissal or completion. Use arrows or Page Up and Page Down to navigate, Enter to select, and Escape to go back or dismiss. At the root, use `/` for files, `=` for calculator history or expressions, `>` for shell commands, and Ctrl+R for shell history.

Cloncher uses the CUI host library from the Jolt/Polylith template. Set `CLONCHER_CUI_LIBRARY` to an existing `libcui_host.so` or run `bin/build-cui /path/to/jolt-poly-template` to build it with Nix. The launcher also discovers the library in the usual sibling template checkout. `jolt release` builds a native launcher and places the CUI library beside it in `target/`.

The CUI host uses a regular native window. The original launcher’s resident GTK layer-shell service and its live keybindings remain separate. Some existing dotfiles helpers still call the original launcher path for their own pickers.
