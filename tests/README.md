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

## CMake framework

Tests are registered with macros defined in `cmake/AddKdTest.cmake`:

```cmake
add_kdtest(TestName)              # normal test: passes on exit 0
add_kdtest(TestName WILL_FAIL)    # inverted test: passes on non-zero exit
```

Each call creates a standalone executable from `TestName.f90`, links it against `kdtree`, and registers it with CTest. The executable name and the CTest test name are both `TestName`.

Use `add_kdtest_omp` when the test program itself contains `!$OMP` directives (e.g. threading tests). The `kdtree` library links OpenMP privately, so it is not propagated to test executables; they must link it explicitly.

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

Each subdirectory has its own `CMakeLists.txt` and holds all the `.f90` files for that unit. Note that below is ordered on **execution order *not* alphabetic order**tests/

```
 ├── v0/
 │  ├── v0.1.0/
 │  │   ├── Testv010_SIMPLE/          			# empty, one-point, two-point trees
 │  │   ├── Testv010_MULTI_AXIS/      			# trees built from multi-axis arrays
 │  │   ├── Testv010_COLLINEAR/       			# collinear point geometries
 │  │   └── Testv010_DUPLICATES/      			# duplicate point geometries
 │  ├── v0.2.0/
 │  │   ├── Testv020_ASSERTIONS/      			# error guards (negative radius, null target)
 │  │   ├── Testv020_NODE_QUERY/      			# distance functions queried by node
 │  │   ├── Testv020_POINT_QUERY/     			# distance functions queried by point
 │  │   ├── Testv020_EMPTY_TREE/      			# rNN on an empty tree
 │  │   ├── Testv020_NON_MEMBER/      			# rNN with a non-member query point
 │  │   ├── Testv020_SINGLE_NODE_QUERY/ 		# rNN on a single-node tree
 │  │   ├── Testv020_DUPLICATES/      			# rNN on duplicate-point geometries
 │  │   ├── Testv020_COLLINEAR_ONE/   			# rNN on one-axis collinear geometries
 │  │   └── Testv020_COLLINEAR_TWO/   			# rNN on two-axis collinear geometries
 │  ├── v0.2.1/
 │  │   ├── Testv021_TREE_GETTERS/ 			# tree%getDim(), tree%getPop(), tree%getInitState; tested after freeing, populated, and empty
 │  │   ├── Testv021_LIFECYCLE/ 			# tests lifecylce (destroy, double free, build -> free -> destroy -> build, multiple builds on tre)
 │  │   ├── Testv021_BUFFER_SIZE/                  	# bufferSize parameter: default, small (1), exact, large (1 M), invalid (0 / negative)
 │  │   ├── Testv021_EMPTY_RESULT_POPULATED_TREE/  	# rNN returns empty on populated tree
 │  │   ├── Testv021_ZERO_RADIUS_POPULATED_TREE/   	# rNN with zero radius
 │  │   ├── Testv021_IS_MEMBER/				# tests checks against searching on non-member nodes
 │  │   └── Testv021_DATA_INPUT/   			# data inputs (valid and invalid) on tree
```

The subdirectory name matches the common prefix of all test files within it. New versions must pass all regresison tests.
