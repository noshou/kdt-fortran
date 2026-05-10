# Tests

Tests are organized by version. Every version directory is an independent CMake subdirectory added in `tests/CMakeLists.txt`. All tests from all versions run together under a single CTest invocation — a new version must not break older tests.

## Build and run

Tests are not built by default. Pass `-DBUILD_TESTS=ON` to enable them:

```bash
cmake -B build -DBUILD_TESTS=ON
cmake --build build
ctest --test-dir build --output-on-failure
```

Note: toggling `BUILD_TESTS` on an existing build directory leaves stale `CTestTestfile.cmake` files — do a clean configure (`rm -rf build`) when switching from ON to OFF.

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

### Registering tests in a subdirectory

 List each test explicitly in the subdirectory's `CMakeLists.txt`:

```cmake
add_kdtest(Testv010_EMPTY)
add_kdtest(Testv010_ONE_POINT)
add_kdtest(Testv020_RNN_NODE_NEGATIVE_RADIUS WILL_FAIL)
```

## Directory structure

Each version directory contains subdirectories, one per unit under test. Each subdirectory has its own `CMakeLists.txt` and holds all the `.f90` files for that unit:

```
tests/
├── v0.1.0/
│   ├── Testv010_SIMPLE/          # empty, one-point, two-point trees
│   ├── Testv010_MULTI_AXIS/      # trees built from multi-axis arrays
│   ├── Testv010_COLLINEAR/       # collinear point geometries
│   └── Testv010_DUPLICATES/      # duplicate point geometries
├── v0.2.0/
│   ├── Testv020_NODE_QUERY/      # distance functions queried by node
│   ├── Testv020_POINT_QUERY/     # distance functions queried by point
│   ├── Testv020_EMPTY_TREE/      # rNN on an empty tree
│   ├── Testv020_NON_MEMBER/      # rNN with a non-member query point
│   ├── Testv020_SINGLE_NODE_QUERY/ # rNN on a single-node tree
│   ├── Testv020_ASSERTIONS/      # error guards (negative radius, null target)
│   ├── Testv020_DUPLICATES/      # rNN on duplicate-point geometries
│   ├── Testv020_COLLINEAR_ONE/   # rNN on one-axis collinear geometries
│   └── Testv020_COLLINEAR_TWO/   # rNN on two-axis collinear geometries
└── v0.2.1/
    ├── Testv021_EMPTY_RESULT_POPULATED_TREE/  # rNN returns empty on populated tree
    ├── Testv021_ZERO_RADIUS_POPULATED_TREE/   # rNN with zero radius
    └── Testv021_BUFFER_SIZE/                  # bufferSize parameter: default, small (1), exact, large (1 M), invalid (0 / negative)
```

The subdirectory name matches the common prefix of all test files within it.

## Conventions

**One assertion per file.** Each test file has exactly one `stop 1` path (or one `error stop` for WILL_FAIL tests). This makes failures unambiguous — a failing test name tells you exactly which assertion broke.

**Naming.** `Testv{version}_{WHAT_IS_TESTED}`. For rNN tests with a metric suffix: `_RNN_{EUCLIDEAN|MANHATTAN|CHEBYSHEV}`. For expected-fail tests: append `_WILL_FAIL` (registered with `WILL_FAIL` in CMake).

**Error output.** On failure, print `'--- subroutineName ---'` then the expected vs. actual values before `stop 1`.

## Version layout

| Directory | Tests | What is covered |
|-----------|-------|-----------------|
| `v0.1.0`  | 17    | Tree build: empty, single, two-point, axis arrays, collinear, duplicate geometries |
| `v0.2.0`  | 69    | Node/point distance functions (euclidean, manhattan, chebyshev); rNN_Centroid and rNN_Node with all metrics and all error guards; same build geometry suite as v0.1.0 extended with per-metric rNN count checks |
| `v0.2.1`  | 28    | rNN returning empty results on a populated tree; rNN with zero radius; bufferSize parameter (default, small=1, exact, large=1M, invalid 0/negative — 4 WILL_FAIL) |
