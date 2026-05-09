# Tests

Tests are organized by version. Every version directory is an independent CMake subdirectory added in `tests/CMakeLists.txt`. All tests from all versions run together under a single CTest invocation — a new version must not break older tests.

## Build and run

```bash
cmake -S . -B build
cmake --build build
ctest --test-dir build --output-on-failure
```

To run only one version's tests:

```bash
ctest --test-dir build -R "Testv010" --output-on-failure
```

## CMake framework

Tests are registered with the `add_kdtest` macro defined in `cmake/AddKdTest.cmake`:

```cmake
add_kdtest(TestName)              # normal test: passes on exit 0
add_kdtest(TestName WILL_FAIL)    # inverted test: passes on non-zero exit
```

Each call creates a standalone executable from `TestName.f90`, links it against `kdtree`, and registers it with CTest. The executable name and the CTest test name are both `TestName`.

`WILL_FAIL` is used for tests that verify error guards — the program is expected to call `error stop`. CTest inverts the pass/fail logic: the test passes only when the program exits non-zero.

## Conventions

**One assertion per file.** Each test file has exactly one `stop 1` path (or one `error stop` for WILL_FAIL tests). This makes failures unambiguous — a failing test name tells you exactly which assertion broke.

**Naming.** `Testv{version}_{WHAT_IS_TESTED}`. For rNN tests with a metric suffix: `_RNN_{EUCLIDEAN|MANHATTAN|CHEBYSHEV}`. For expected-fail tests: append `_WILL_FAIL` (registered with `WILL_FAIL` in CMake).

**Error output.** On failure, print `'--- subroutineName ---'` then the expected vs. actual values before `stop 1`.

## Version layout

| Directory | Tests | What is covered |
|-----------|-------|-----------------|
| `v0.1.0`  | 17    | Tree build: empty, single, two-point, axis arrays, collinear, duplicate geometries |
| `v0.2.0`  | 80+   | Node/point distance functions (euclidean, manhattan, chebyshev); rNN_Centroid and rNN_Node with all metrics and all error guards; same build geometry suite as v0.1.0 extended with per-metric rNN count checks |
| `v0.2.1`  | TBD   | Planned (see `v0.2.1/TestRecs.md`) |
