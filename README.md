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
jolt prod projects add ~/Code/my-project
jolt prod files
printf '%s\n' Alpha Beta | bin/cloncher dmenu -p "Choose a line"
bin/cloncher secret -p "Password"
```

Invoke `bin/cloncher` by absolute path from desktop keybindings and picker scripts. It uses the release binary when present. Set `CLONCHER_SOURCE=1` to run the checkout's source. Arrow keys, Tab, Page Up and Page Down, and the result scrollbar navigate the list. Use Enter or click a row to select it. Type `;` followed by a displayed label, or hold Alt while typing it. Backspace in an empty query returns to the previous view. Escape dismisses or backs out of a nested view.

At the root, `/` enters file search, `@` opens projects, `=` opens calculator history, and `(` starts a restricted desktop Lisp expression. `>` starts a shell command. `>>` browses history in Kitty and `>>>` browses history for background execution. Ctrl+R toggles command history. Ctrl+C copies the selected command or file path. Tab completes commands and project directories.

The daemon accepts one active request and uses a private Unix socket. It propagates the client's environment to menu actions. The control commands work from another terminal.

```sh
bin/cloncher daemon
bin/cloncher ping
bin/cloncher screenshot /tmp/cloncher.png
bin/cloncher close
```

`LAUNCHER_STANDALONE=1` forces a new standalone window. `bin/cloncher --screenshot PATH` opens, captures, and closes a root window. The daemon's `screenshot` command captures an already open window and refuses secret sessions.

The panel reads Noctalia's `~/.config/launcher/colors.css` and resolves app icons through the desktop entry and XDG icon directories. File and document previews use the site's local tools for raster, video, and PDF decoding. CUI draws the preview and the rest of the panel.

## Build and verify

Set `CLONCHER_CUI_LIBRARY` to an existing `libcui_host.so`, or run `bin/build-cui /path/to/jolt-poly-template` to build it with Nix. Cloncher also discovers the library in the usual sibling template checkout.

```sh
jolt -M:test
jolt release
./target/cloncher
```

On compositors that support Wayland layer-shell, production opens as an overlay centered 140 points from the top of the active output. Other desktops use an ordinary window. The development host opens as a tiled window. Set `CLONCHER_TILING=1` to use an ordinary production window as well.
