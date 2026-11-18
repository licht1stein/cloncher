# Cloncher

**A keyboard-first Linux launcher for apps, files, and desktop actions.**

I open Cloncher with Mod+Space. From that prompt I can search for an app or a menu action, type `/` to find a file, or type `=` to open the calculator. The same prompt can show nearby Wi-Fi networks, switch Niri windows, browse projects, and run custom commands.

[Watch the demos](#demos) · [Performance](#performance) · [Build it](#build-and-run) · [Make it yours](#configuration)

![Cloncher's main menu with keyboard labels for Apps, Windows, Projects, and desktop actions](docs/media/main-menu.png)

## Where it came from

Cloncher is a personal project extracted from my Linux dotfiles. I used the original launcher every day. Its window was written in Rust, with a Babashka backend that supplied the menus and desktop actions. I rebuilt it in Jolt and CUI, moved the menu tree into EDN, and gave it its own repository so it can grow in public. [Jolt](https://github.com/jolt-lang/jolt) · [CUI](https://github.com/yappologistic/CUI)

The bundled menus reflect the Niri, Noctalia, Kitty, and Emacs setup I use. You can keep account-specific actions and paths in your local EDN file, or replace the menu with a smaller setup of your own.

## Performance

I use Cloncher throughout the day. I want to hit Mod+Space and type in the same motion, so **16 ms is my budget for immediate feedback** when opening the menu, moving between views, or typing a query.

On my Niri desktop, 20 warm opens of the compiled launcher took **14.6 ms median, 16.8 ms at the 95th percentile, and 18.6 ms at worst**. I timed from starting the hotkey client to its first launcher buffer submission to Wayland. This measures an already-running daemon through frame submission. Display scanout and daemon startup are outside that measurement.

The daemon warms the window and fonts and caches commonly used rows. File search starts with cached recent files while the full index loads. Longer operations give immediate feedback where they start. A Wi-Fi scan, for example, shows a spinner in its own row while NetworkManager works.

## Demos

These are recordings of the compiled launcher. The Wi-Fi inventory and files are sample data. Click any loop for its MP4.

### Wi-Fi stays in the menu

See signal strength, security, and the connected network at a glance. Start a scan and the spinner appears on the scan row itself. The result list refreshes in place when the scan finishes.

[![Wi-Fi demo showing an inline scan spinner and a new network appearing](docs/media/wifi.gif)](docs/media/wifi.mp4)

### Calculate and reuse an answer

Type `=` for the tape, or enter a calculation like `=256 MiB to MB`. Enter copies the current result. Pick a tape entry to bring its expression back into the prompt and edit it.

[![Calculator demo converting MiB to MB and recalling an expression from history](docs/media/calculator.gif)](docs/media/calculator.mp4)

### Find a file, preview it, then choose an action

Type `/` to start with recent files. Narrow the list by typing, preview the selected file, and press Enter for actions such as Open, Reveal in Dired, or Copy path. Backspace from an empty search returns to the previous menu.

[![File search demo finding a cover image, previewing it, and opening the file action menu](docs/media/files.gif)](docs/media/files.mp4)

## A few good keys to know

| Key | What it does |
| --- | --- |
| Type at the root | Search apps and actions across searchable submenus. [Watch the menu demo](docs/media/navigation.mp4). |
| `;` then a row label | Jump to the labeled result. You can also hold Alt and type the label. |
| `/` · `@` · `=` | Open Files, Projects, or calculator history from the root. |
| `>` · `>>` · `>>>` | Enter a command or browse history for terminal or background execution. |
| `(` | Start a restricted desktop Lisp expression for actions such as finding and focusing a window. |
| Ctrl+Enter | Run the selected row's alternate action, where one is defined. |
| Backspace on an empty query · Esc | Go back from a nested view. Esc closes the root. |

Apps come from XDG desktop entries. Projects use Emacs's project list and can expose Babashka tasks, Just recipes, and Make targets. File search starts from a small recent-files cache while the daemon prepares the full index in the background.

## Build and run

The primary desktop is Linux with Niri. Cloncher opens as a layer-shell overlay where the Wayland compositor supports it, and uses an ordinary window on X11 or when layer-shell is unavailable.

On x86-64 Linux, `nix-shell` brings in Jolt, a C compiler, and common integration tools. With direnv hooked into your shell, `direnv allow` loads the same environment on entry. Run `jolt -Srepro -M:test` to verify the setup.

Building the CUI host currently requires access to my private CUI template checkout. Cloncher finds it in a sibling `jolt-poly-template` directory or via `CLONCHER_CUI_TEMPLATE`. If you already have a compatible `libcui_host.so`, point `CLONCHER_CUI_LIBRARY` at it for `jolt dev` or `jolt prod`. [Install Jolt without Nix](https://github.com/jolt-lang/jolt#install)

```sh
CLONCHER_CUI_TEMPLATE=/path/to/jolt-poly-template jolt release
./target/cloncher-client
```

The release puts the client, application, and CUI host library together in `target/`. Keep all three together when installing them. For a source run, set `CLONCHER_CUI_LIBRARY` to the host library if Cloncher cannot find it.

Start the daemon from your graphical session and bind Mod+Space to the absolute path of `target/cloncher-client`.

```sh
./target/cloncher daemon
```

The hotkey client talks to the daemon through a private Unix socket. The daemon prepares the native window and fonts while idle, then attaches the overlay when you open it. Use `bin/cloncher ping`, `bin/cloncher close`, or `bin/cloncher screenshot /tmp/cloncher.png` from another terminal.

The integrations use the tools you already have. Install `qalc` for the calculator, `fd` for file indexing, and NetworkManager's `nmcli` for Wi-Fi. Wayland clipboard actions use `wl-copy`. PDF and video thumbnails use `pdftoppm` and `ffmpeg`. A Noctalia palette at `~/.config/launcher/colors.css` can theme the panel.

### Use it from a script

```sh
printf '%s\n' Alpha Beta | bin/cloncher dmenu -p 'Choose a line'
bin/cloncher menu wifi
bin/cloncher files
```

The picker returns the selected line on stdout and exits with status 1 on cancellation. `bin/cloncher secret -p 'Password'` opens a masked prompt. It keeps the response out of ordinary search history, and the daemon refuses screenshots of secret prompts.

## Configuration

First launch copies the [bundled menu](resources/cloncher/default.edn) to `$XDG_CONFIG_HOME/cloncher/config.edn`, usually `~/.config/cloncher/config.edn`. Later launches preserve your edits. Production reads valid changes on the next open and keeps the last valid config in the running process if an edit fails.

A menu is an ordered list of static rows and live providers. Here is a complete two-menu config.

```clojure
{:menus
 {:main
  {:prompt "Menu"
   :items [{:id :wifi
            :label "Wi-Fi"
            :action {:menu :wifi}}]}

  :wifi
  {:prompt "Wi-Fi networks"
   :items [{:provider :wifi/networks}
           {:id :wifi/scan
            :label "Scan Wi-Fi networks"
            :action {:call :wifi/scan}
            :refresh :current
            :progress "Scanning Wi-Fi networks…"}]}}}
```

Use `:exec` with an argument vector for commands, `:shell` for shell syntax, or `:terminal` for a command that needs a terminal. Set `:search` to add aliases, `:shortcut` to pin a row label, or `:searchable? false` on a submenu to keep its contents out of parent searches.

To configure Tailscale switching, add a top-level `:tailnets` vector to your local config. Each entry has string fields `:label` (menu name), `:tailnet` (name reported by `tailscale status --json`), and `:account` (argument accepted by `tailscale switch`).

## Develop in the open window

Build the host once with `bin/build-cui` or export `CLONCHER_CUI_LIBRARY` before launching.

```sh
jolt dev
jolt eval desktop --ns user -e '(cloncher.dev/show! :files)'
jolt eval desktop --ns user -e '(cloncher.dev/reload-config!)'
jolt eval desktop --ns user -e '(cloncher.dev/screenshot!)'
```

The tiled dev host stays open while you turn its embedded launcher on and off. You can inspect its state, open a view with fixture rows, reload code with `cloncher.dev/reload!`, and capture the result without closing the window. Dev previews stop at the point where a production action would run. The [session state machine](src/cloncher/machine.clj) and [panel](src/cloncher/view.clj) are shared with the production overlay.

```sh
jolt -Srepro -M:test
```

If you try Cloncher on another Linux setup, I'd especially like to hear which parts worked out of the box and which still assume my desktop.
