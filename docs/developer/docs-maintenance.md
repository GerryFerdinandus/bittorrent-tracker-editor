---
title: Maintaining this documentation
parent: Programmer guide
nav_order: 9
---

# Maintaining this documentation
{: .no_toc }

The site you are reading is plain Markdown in the [`docs/`](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/tree/main/docs) folder, published by GitHub Pages with the *just-the-docs* theme.

1. TOC
{:toc}

## Publishing

One time setting in the repository: **Settings > Pages > Build and deployment > Source: Deploy from a branch > Branch `main`, folder `/docs`**.
After that every push to `main` that changes `docs/` rebuilds the site: <https://gerryferdinandus.github.io/bittorrent-tracker-editor/>.

## Layout of `docs/`

| Path | Content |
|:-----|:--------|
| `_config.yml` | Site settings: theme (`just-the-docs@v0.10.0` as `remote_theme`), search, callouts, mermaid version. |
| `index.md` | Home page. |
| `history.md` | Software history (changelog). |
| `user/` | The user guide (GUI, console mode, tracker order modes, files, troubleshooting). |
| `developer/` | The programmer guide (this section). |
| `assets/` | Pictures. `tracker-order-modes.svg` is a copy of `enduser/tracker-order-modes.svg`. |

## Rules for writing pages

- Every page starts with front matter: `title`, `nav_order`, and for a child page `parent: User guide` or `parent: Programmer guide` (the parent title must match **exactly**). The two parents are `user/index.md` and `developer/index.md` (`has_children: true`).
- Links between pages are relative and end with **`.html`** (`[GUI guide](gui.html)`, `[Architecture](../developer/architecture.html)`). They work on the site wherever it is hosted. Links to a section use the heading id, for example `console-mode.html#console_logtxt`.
- Pictures are linked relative to the page (`../assets/x.svg`).
- Diagrams are fenced code blocks with the language `mermaid`. They are drawn in the browser.
  - Put a label in double quotes when it contains `( ) [ ] { } < > ; :`, and use `<br/>` for a line break.
  - Keep a diagram small enough to read. Prefer two diagrams over one huge one.
- Liquid (the template language of Jekyll) reads `&#123;&#123; ... &#125;&#125;` and `&#123;% ... %&#125;` **everywhere**, also in code blocks. Pascal code with a Lazarus hint such as `&#123;%H-&#125;` must be wrapped in `&#123;% raw %}...&#123;% endraw %&#125;`.
- Callouts: put `{: .note }`, `{: .tip }` or `{: .warning }` on the line before a paragraph.
- Use the exact names of the program: menu captions as they are in `main.lfm`, parameters as in `torrent_miscellaneous.pas`.
- Write short sentences. The readers are end users and programmers who do not speak English as a first language.

## What must be updated together

| If you change ... | ... then also update |
|:------------------|:---------------------|
| `enduser/readme.txt` | `docs/user/console-mode.md` (and `tracker-order-modes.md` for the `-Ux` text). **Keep both the same.** |
| `enduser/tracker-order-modes.svg` | The copy `docs/assets/tracker-order-modes.svg` (identical file). |
| A menu item, a tab or a caption in `main.lfm`, or the behaviour of a menu handler in `main.pas` | `docs/user/gui.md` (and `docs/developer/gui.md` for a structural change). |
| A console parameter, the exit code or the content of `console_log.txt` | `docs/user/console-mode.md`, `docs/user/troubleshooting.md`, `docs/developer/console-mode.md`, `enduser/readme.txt`. |
| Where the text files are loaded and saved (`DetermineTrackerListFolder`, packaging) | `docs/user/files-and-folders.md`, `docs/developer/console-mode.md`. |
| The tracker merge algorithm or `TTrackerListOrder` | `docs/user/tracker-order-modes.md`, `docs/developer/tracker-pipeline.md`, the svg. |
| A unit is added, removed or renamed, or a `uses` clause changes the dependencies | `docs/developer/architecture.md`: the unit table **and the mermaid dependency graph**. |
| `bencode.pas` or `decodetorrent.pas` | `docs/developer/torrent-format.md`. |
| `newtrackon.pas`, `ngosang_trackerslist.pas`, `trackerlist_online.pas`, `fix_openssl.pas` | `docs/developer/online-services.md`. |
| The Makefile, a workflow, the test projects or test suites | `docs/developer/build-and-test.md`. |
| A new release | `docs/history.md`: add a section at the top, with the line `Compiler Lazarus: vX.Y` (the Lazarus version used for the release). Also change `PROGRAM_VERSION` in `main.pas`. |
| A convention or an accepted design decision | `docs/developer/conventions.md` (and the instruction files in `.github/`). |

The same rules are in `.github/instructions/docs.instructions.md`, so that the AI assistants follow them too.

## Check the dependency graph

The graph in [Architecture](architecture.html) must match the real `uses` clauses. Open each changed unit and read the `uses` of its `interface` and `implementation` sections. Only our own units (and the few external ones named in the graph) are drawn.

## Preview on your computer

You need Ruby with Bundler. GitHub builds the site without a `Gemfile`, so the repository does not have one. For a preview, create a `Gemfile` in `docs/` and do **not** commit it:

```ruby
source "https://rubygems.org"
gem "github-pages", group: :jekyll_plugins
gem "webrick"
```

```sh
cd docs
bundle install
bundle exec jekyll serve
```

Then open <http://127.0.0.1:4000/bittorrent-tracker-editor/>. The theme and the mermaid library are downloaded from the internet, so you need a connection. Delete `Gemfile`, `Gemfile.lock` and `_site/` when you are done.

To check a diagram alone, paste it in <https://mermaid.live/> or use the mermaid command line tool: `npx -p @mermaid-js/mermaid-cli mmdc -i diagram.mmd -o diagram.svg`.
