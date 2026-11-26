---
description: "Use when writing, modifying, or reviewing the GitHub Pages documentation in docs/ of the bittorrent-tracker-editor project. Covers front matter, links, mermaid diagrams, and which files must be kept in sync."
applyTo: "docs/**/*.md"
---
# Documentation (GitHub Pages) Guidelines

The site is Jekyll with the `just-the-docs` remote theme, published from branch `main`, folder `/docs`. Settings are in `docs/_config.yml`.

## Pages

- Every page starts with front matter: `title`, `nav_order`, and for a child page `parent:` with the **exact** title of the parent (`User guide` or `Programmer guide`). The parents are `docs/user/index.md` and `docs/developer/index.md` (`has_children: true`).
- Links between pages are relative and end with `.html` (`[GUI guide](gui.html)`, `[Architecture](../developer/architecture.html)`). Section links use the heading id (`console-mode.html#console_logtxt`).
- Pictures are linked relative to the page (`../assets/x.svg`).
- Liquid reads `{{ ... }}` and `{% ... %}` also inside code blocks. Wrap Pascal code that contains such text (for example the hint `{%H-}`) in `{% raw %}...{% endraw %}`.
- Use callouts with `{: .note }`, `{: .tip }` or `{: .warning }` on the line before a paragraph.
- Use the exact menu captions of `main.lfm` and the exact parameters of the program. Short sentences: many readers do not speak English as a first language.

## Mermaid diagrams

- Fenced code block with the language `mermaid`.
- Put a label in double quotes when it has `( ) [ ] { } < > ; :`. Use `<br/>` for a line break.
- A diagram must match the code. After a code change that touches it, update the diagram in the same commit.
- Keep diagrams small. Two small diagrams are better than one big one.

## Keep in sync (same commit as the code change)

| Change | Update |
|--------|--------|
| `enduser/readme.txt` | `docs/user/console-mode.md` and `docs/user/tracker-order-modes.md` (the same text) |
| `docs/assets/tracker-order-modes.svg` | Nothing: it is the only copy. The Windows and Linux CI copy it to `enduser/` for the release zips. |
| Menu item, tab or caption in `main.lfm`, menu handler in `main.pas` | `docs/user/gui.md` (and `docs/developer/gui.md` for a structural change) |
| Console parameter, exit code, `console_log.txt`, file location rule | `docs/user/console-mode.md`, `docs/user/files-and-folders.md`, `docs/user/troubleshooting.md`, `docs/developer/console-mode.md`, `enduser/readme.txt` |
| Unit added/removed/renamed, `uses` dependency changed | `docs/developer/architecture.md` (unit table and the mermaid dependency graph) |
| Merge algorithm, `TTrackerListOrder` | `docs/developer/tracker-pipeline.md`, `docs/user/tracker-order-modes.md`, the svg, `enduser/readme.txt` |
| `bencode.pas`, `decodetorrent.pas` | `docs/developer/torrent-format.md` |
| `newtrackon.pas`, `ngosang_trackerslist.pas`, `trackerlist_online.pas`, `fix_openssl.pas` | `docs/developer/online-services.md` |
| Makefile, workflows, test suites | `docs/developer/build-and-test.md` |
| New release | `docs/history.md`: new section at the top, with `Compiler Lazarus: vX.Y`; bump `PROGRAM_VERSION` in `main.pas` |

`README.md` has no software history: it only links to the Pages site. There is no `enduser/version.txt`.

Commit messages for documentation only: `docs(pages): ...`.
