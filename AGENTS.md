# Agent notes

## Interactive development

The README's [Develop in the open window](README.md#develop-in-the-open-window) section is the reference for `jolt dev`, `jolt eval`, and the `cloncher.dev` helpers. These notes cover what an agent shell needs on top of it.

- Run every `jolt` command through the Nix shell: `direnv exec . jolt …`. Agent shells don't load `.envrc`, and without the pinned Mesa the window never draws.
- Check for a running window before starting one: `pgrep -af 'jolt.*-M:dev'` or `niri msg windows | grep -A5 'Cloncher Dev'`. Reuse it with `direnv exec . jolt eval desktop --ns user -e '(cloncher.dev/reload!)'`. A second instance overwrites `.nrepl-port`, so `jolt eval` would reach only the newest window.
- `jolt dev` blocks until the window closes. Start it as a background service, ready on the log line `Cloncher Dev nREPL on`.
- Bring the window to the user with `niri msg action focus-window --id <id>`, using the ID from `niri msg windows`.
- `(cloncher.dev/screenshot!)` returns the absolute path of a PNG under `.dev/screenshots/`. Read that file to see the window.
- To stop, kill both the `jolt dev` parent and its `jolt -Srepro -M:dev` child, then confirm `Cloncher Dev` is gone from `niri msg windows`.
