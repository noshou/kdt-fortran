# Tests

Tests are organized by version. Every version directory is an independent CMake subdirectory added in `tests/CMakeLists.txt`. All tests from all versions run together under a single CTest call.

## Build and run

Tests are not built by default. Pass `-DBUILD_TESTS=ON` to enable them:

```bash
cmake -B build -DBUILD_TESTS=ON
cmake --build build
ctest --test-dir build --output-on-failure
```

Note: toggling `BUILD_TESTS` on an existing build directory leaves stale `CTestTestfile.cmake` files; do a clean configure (`rm -rf build`) when switching from ON to OFF.

To run only one version's tests:

```bash
ctest --test-dir build -R "Testv010" --output-on-failure
```

To run only the multithreaded tests:

```bash
ctest --test-dir build -R "MULTITHREAD" --output-on-failure
```

To run only the single-threaded `addNodes` tests (excludes multithread suite):

```bash
ctest --test-dir build -R "Testv030_ADD_NODES" --output-on-failure
```

### Thread count

Multithreaded tests hardcode `NUM_THREADS(4)` in their `!$OMP PARALLEL DO` directives. The `OMP_NUM_THREADS` environment variable is ignored for those loops; however, OpenMP's thread pool is still initialised from the environment before the directives run, so setting `OMP_NUM_THREADS=4` (or higher) in advance avoids pool-resize overhead:

```bash
OMP_NUM_THREADS=4 ctest --test-dir build -R "MULTITHREAD" --output-on-failure
```

On machines with fewer than 4 physical threads the tests still pass -> OpenMP over-subscribes transparently -> but runtime will be higher.

## CMake framework

Tests are registered with macros defined in `cmake/AddKdTest.cmake` and `cmake/AddKdTestOmp.cmake` :

```cmake
add_kdtest(TestName)              # normal test: passes on exit 0
add_kdtest(TestName WILL_FAIL)    # inverted test: passes on non-zero exit
```

Each call creates a standalone executable from `TestName.f90`, links it against `kdtree`, and registers it with CTest. The executable name and the CTest test name are both `TestName`.

Use `add_kdtest_omp` when the test program itself contains `!$OMP` directives. The `kdtree` library links OpenMP privately (it is not propagated to dependents), so test executables that use OpenMP must link it explicitly via this macro. Tests that only call the library -> even if the library uses OpenMP internally -> do not need `add_kdtest_omp` and should use `add_kdtest` instead.

`WILL_FAIL` is used for tests that verify error guards, where the program is expected to call `error stop`. CTest inverts the pass/fail logic: the test passes only when the program exits non-zero.

### Registering tests in a subdirectory

 List each test explicitly in the subdirectory's `CMakeLists.txt`:

```cmake
add_kdtest(Testv010_EMPTY)
add_kdtest(Testv010_ONE_POINT)
add_kdtest(Testv020_RNN_NODE_NEGATIVE_RADIUS WILL_FAIL)
```

### Conventions

##### **One assertion per file**

Each test file has exactly one `stop 1` path (or one `error stop` for WILL_FAIL tests).

##### **Naming**

`Testv{version}_{WHAT_IS_TESTED}`.

##### **Error output**

On failure, print `'--- Testv{version}_{WHAT_IS_TESTED} ---'` then the expected vs. actual values before `stop 1`.

For error guards, programs are expected to fail at some point, so the program should print stdout at the end of successful execution.

##### **Order Matters**

add_kdtest and add_subdirectory execute/open the respective tests/directories sequentially.

## Versions

Each release has a directory, and each version has its own set subdirectory. Each version contains subdirectories, one per unit under test.

Each subdirectory has its own `CMakeLists.txt` and holds all the `.f90` files for that unit. The subdirectory name matches the common prefix of all test files within it. New versions must pass all regresison tests.