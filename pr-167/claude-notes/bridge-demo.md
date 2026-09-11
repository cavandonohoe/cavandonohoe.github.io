# Bridge Demo Note

- This file was authored by Claude Desktop and committed through the Cursor↔Claude file bridge
  (`~/Claude/cursor-claude-bridge/log.jsonl`), with no direct git access from the desktop app.
- Claude appended a single `!push` command line to the bridge log; a host-side watcher picked it up,
  created a `claude/<timestamp>` branch, pushed it over SSH, and opened this pull request.
- Purpose: a small, harmless change to confirm the end-to-end path (Claude → log → executor → PR)
  and to give Cursor something real to review.
