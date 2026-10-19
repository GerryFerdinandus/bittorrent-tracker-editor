---
name: build-release-only
description: "Build the project in Release mode only, without running tests. Quick compilation check."
disable-model-invocation: true
---
Build the bittorrent-tracker-editor project in Release mode:

```
lazbuild --build-all --build-mode=Release source/project/tracker_editor/trackereditor.lpi
```

Report whether the build succeeded or failed, including any compiler errors.
