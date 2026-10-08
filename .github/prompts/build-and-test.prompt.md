---
description: "Build the project and run unit tests. Compiles Release build then Debug tests, runs FPCUnit suite, and reports results."
agent: "agent"
tools: ["terminal"]
---
Build and test the bittorrent-tracker-editor project. Run these steps in sequence, stopping on failure:

1. **Build Release**
   ```
   lazbuild --build-all --build-mode=Release source/project/tracker_editor/trackereditor.lpi
   ```

2. **Build CLI (Release)**
   ```
   lazbuild --build-all --build-mode=Release source/project/tracker_editor/trackereditor_cli.lpi
   ```

3. **Build Unit Tests (Debug)**
   ```
   lazbuild --build-all --build-mode=Debug source/project/unit_test/tracker_editor_test.lpi
   ```

4. **Run Tests**
   ```
   enduser/test_trackereditor -a --format=plain
   ```

Report a summary: build success/failure, number of tests passed/failed, and any error details.
