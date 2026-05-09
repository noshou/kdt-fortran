# v0.2.0 Test Coverage

Gap analysis against the public API after v0.2.0 shipped.

## Already covered (v0.1.0 + v0.2.0)

- **v0.1.0** — tree construction: empty, single point, multi-point, 1D–4D
  axis-aligned arrays, collinear points, duplicates.
- **v0.2.0** — all distance functions (node-to-node, node-to-point) for
  Euclidean, Manhattan, and Chebyshev, including error stops (unallocated
  coords, axis mismatch); `rNN_Centroid` and `rNN_Node` error stops (empty
  tree, non-member, null target, negative radius, invalid metric string);
  correctness on single-node geometry with all three metrics and both query
  forms; non-member rejection; duplicate points (1–4 axes, all metrics);
  collinear points (1-axis and 2-axis varieties, all metrics).

---

## Coverage gaps for v0.2.0

### Radius edge cases

- **Empty result in a populated tree** — radius small enough that nothing is
  captured; confirms early termination returns an empty array, not a crash or
  stale results.
- **Radius = 0** — degenerate sphere; `rNN_Node` should return only the target
  itself; `rNN_Centroid` with a centroid not in the tree should return empty.
  Pin the contract.
- **Boundary inclusion** — a point sitting at distance exactly `r` from the
  query: is it included (`<=`) or excluded (`<`)? Pin the semantics in a test
  so a refactor cannot silently change them.
- **Self-inclusion** — when the target node is in the tree, is it included in
  the `rNN_Node` result? Document the contract and test it explicitly.

Suggested names:

- `Testv021_RNN_EMPTY_RESULT`
- `Testv021_RNN_RADIUS_ZERO_NODE`
- `Testv021_RNN_RADIUS_ZERO_CENTROID`
- `Testv021_RNN_BOUNDARY_INCLUSION`
- `Testv021_RNN_SELF_INCLUSION`

### `bufferSize` overflow

`rNN_Node` and `rNN_Centroid` pre-allocate a result buffer of `bufferSize`
(default 1000) and resize on return. If the result count exceeds the initial
allocation the buffer must grow correctly. This is the most likely place for a
silent regression.

- Build a tree with > 1000 points and query with a radius that captures all of
  them. Assert `size(res).eq.n`.

Suggested name: `Testv021_RNN_INITIAL_SIZE_OVERFLOW`

### Accessors: `getDim` / `getPop`

Currently unexercised by any test.

- Pre-`build`: both must return 0.
- Post-`build`: `getDim` must match `size(coords, 1)`, `getPop` must match
  `size(coords, 2)`.
- After `destroy`: both must return 0 again.

Suggested names:

- `Testv021_ACCESSORS_PRE_BUILD`
- `Testv021_ACCESSORS_POST_BUILD`
- `Testv021_ACCESSORS_AFTER_DESTROY`

### `isMember` direct tests

`isMember` is currently only exercised indirectly through the non-member error
stops in `rNN_Node`. It needs direct coverage as a logical function.

- A node retrieved from tree `t` must return `.true.` for `t%isMember(node)`.
- A node retrieved from tree `t2` must return `.false.` for `t1%isMember(node)`.
- A null/disassociated pointer — document and pin the contract.

Suggested names:

- `Testv021_ISMEMBER_TRUE`
- `Testv021_ISMEMBER_FALSE`
- `Testv021_ISMEMBER_NULL` (likely `WILL_FAIL` if contract is error stop)

### Lifecycle: `destroy` and the finalizer

- **`build → destroy → build`** on the same `Tree` variable: the second build
  must produce the same result as if the variable were fresh. Catches pool
  leaks and stale state.
- **Double-`destroy`** must be a no-op (no crash, no double-free).
- **Finalizer path** — a `Tree` going out of scope without an explicit
  `destroy`. Best verified under valgrind; at minimum assert no crash.

Suggested names:

- `Testv021_REBUILD_SAME_TREE`
- `Testv021_DOUBLE_DESTROY`

### Data payload (`build` optional `data` argument)

`Node%data` is `class(*), allocatable`. The optional `data` argument to `build`
has never been exercised by any test. Minimum required coverage:

- **Integer payload** — build with `data = [1, 2, ..., n]`, retrieve via
  `getData`, `select type` to `integer`, assert value matches.
- **Real payload** — same with `real(real64)`.
- **Data survives reordering** — `build` reorders points; store the original
  column index as the payload and assert each node's `coords` matches
  `coords(:, node%getData())` from the original input array. This is the most
  important data test.
- **`rNN` returns nodes with intact data** — query a populated tree and walk
  the returned `NodePtr` array; assert each result's `getData()` is allocated
  and correct. Catches bugs where pruning or pool reuse silently clears the
  payload.
- **Data length mismatch** — `size(data) .ne. size(coords, 2)` must error stop.
- **Rebuild replaces data** — `build` with payload A, then `build` again with
  payload B on the same `Tree`; old payload must not leak, new payload must be
  retrievable.

Suggested names:

- `Testv021_BUILD_WITH_INT_DATA`
- `Testv021_BUILD_WITH_REAL_DATA`
- `Testv021_DATA_SURVIVES_REORDERING`
- `Testv021_RNN_RETURNS_DATA`
- `Testv021_DATA_LENGTH_MISMATCH` (`WILL_FAIL`)
- `Testv021_REBUILD_REPLACES_DATA`

### Brute-force cross-check and fuzz

Hand-written geometry tests catch specific bugs but miss the "subtle off-by-one
in bounds-pruning" class. A brute-force oracle costs ~5 lines and covers that
entire class.

**Pattern:**

```fortran
! reference: collect every point within radius the dumb way
do i = 1, n
    if (norm2(coords(:,i) - centroid) <= radius) refCount = refCount + 1
end do
if (size(res) .ne. refCount) stop 1
```

Replace `norm2` with `sum(abs(...))` for Manhattan and `maxval(abs(...))` for
Chebyshev.

- **Fixed-geometry cross-check** — a handful of hand-picked inputs (e.g. 20
  points in a unit cube, several radii), all three metrics, both query forms.
  Assert result size and that every returned node actually lies within radius.
- **Randomised fuzz with fixed seed** — loop over many random point sets and
  query parameters; cross-check each against brute force. Fixed seed =
  reproducible failures.

```fortran
call random_seed(put=[42, 42, ...])  ! deterministic
```

Suggested names:

- `Testv021_BRUTE_FORCE_EUCLIDEAN`
- `Testv021_BRUTE_FORCE_MANHATTAN`
- `Testv021_BRUTE_FORCE_CHEBYSHEV`
- `Testv021_FUZZ_ALL_METRICS`

---

## Priority order

1. **Data-payload tests** — `class(*)` path is entirely untested; `DATA_SURVIVES_REORDERING` is the highest-value single test.
2. **`bufferSize` overflow** — single most likely regression site.
3. **Radius edge cases** — boundary inclusion and self-inclusion pin implicit contracts before anyone changes them.
4. **`isMember` direct tests** — currently only covered by side-effect.
5. **Lifecycle** — `build → destroy → build` and double-destroy lock down memory management.
6. **Accessors** — `getDim`/`getPop` are trivial to test and currently completely dark.
7. **Brute-force cross-check** — highest payoff for search-correctness confidence; do the fixed-geometry version first, then the fuzz loop.
