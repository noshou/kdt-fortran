# kdt-fortran

A thread safe balanced kd-tree in modern Fortran, with radius nearest-neighbour search.

---

## v0.4.0

### New API

#### `KdNodeBucket` type

A new public type returned by the `findNodes_*` family. Each bucket holds a parallel array of `KdNodePtr` for the nodes matched by one query point.

```fortran
type :: KdNodeBucket
    type(KdNodePtr), allocatable :: nodes(:)
    contains
        procedure :: destroy
end type KdNodeBucket
```

`nodes` is empty (`size 0`) when a query has no match. Call `%destroy()` to free the owned node copies, or let the bucket go out of scope — a `final` finalizer runs automatically.

#### `rNN_Coords(coords, metric, epsilon, bufferSize)`

Searches the tree for nodes matching a set of query coordinates. `coords` is a `real(real64)` array of shape `[dim, nQuery]` — one column per query point. Returns a parallel array `res(nQuery)` of `KdNodeBucket`; `res(i)%nodes` holds all nodes within `epsilon` of `coords(:,i)`.

```fortran
real(real64) :: query(2, 3) = reshape([...], [2, 3])
type(KdNodeBucket), allocatable :: res(:)

res = t%rNN_Coords(query)                            ! defaults: euclidean, epsilon=1e-15
res = t%rNN_Coords(query, epsilon=0.5_real64)
res = t%rNN_Coords(query, metric='manhattan', epsilon=1.0_real64)
```


| Parameter    | Type                | Default       | Notes                                       |
| ------------ | ------------------- | ------------- | ------------------------------------------- |
| `coords`     | `real(real64)(:,:)` | —             | shape `[dim, nQuery]`                       |
| `metric`     | `character(*)`      | `'euclidean'` | `'euclidean'`, `'manhattan'`, `'chebyshev'` |
| `epsilon`    | `real(real64)`      | `1e-15`       | search radius; must be `>= 0`               |
| `bufferSize` | `integer`           | `1000`        | initial rNN buffer; must be `> 0`           |


**Error guards** (`stop 1`): uninitialized tree; `dim` mismatch; `epsilon < 0`; unrecognised metric; `bufferSize <= 0`.

Returns empty buckets (not an error) when the tree has zero nodes.

#### `rNN_Ids(coords, ids, metric, epsilon, bufferSize)`

Like `rNN_Coords`, but applies a second filter: for each query `i`, only nodes whose internal `nodeId` equals `ids(i)` are kept. `ids` must have the same length as the number of query columns.

```fortran
integer(int64) :: ids(3) = [1_int64, 4_int64, 7_int64]
res = t%rNN_Ids(query, ids, epsilon=1e-10_real64)
```

The two-phase search (radius scan → id filter) means `res(i)%nodes` is empty when either no node falls within `epsilon` or no node in that radius has the matching id.

**Error guards** (`stop 1`): same as `rNN_Coords`, plus `size(ids) .ne. size(coords, 2)`.

---

### Source restructuring

The source tree was flattened, `.inc` files were merged, and directories were prefixed with `kd` to match the type rename convention, since `fortls` struggled to resolve symbols across the nested layout. Associated submodule names were changed as well.


| v0.3.0 name                                   | v0.4.0 name                                               |
| --------------------------------------------- | --------------------------------------------------------- |
| `KdTree`                                      | `KdTreeFortran`                                           |
| `Tree`                                        | `KdTree`                                                  |
| `Node`                                        | `KdNode`                                                  |
| `NodePtr`                                     | `KdNodePtr`                                               |
| `~/kdt-fortran/`                              | `~/KdTreeFortran/`                                        |
| `~/kdt-fortran/src/node/`                     | `~/KdTreeFortran/src/kdnode/`                             |
| `~/kdt-fortran/src/node/NodeDistance.f90`     | `~/KdTreeFortran/src/kdnode/KdNodeDistance.f90`           |
| `~/kdt-fortran/src/node/NodeGetters.f90`      | `~/KdTreeFortran/src/kdnode/KdNodeGetters.f90`            |
| `~/kdt-fortran/src/node/NodeUtils.f90`        | `~/KdTreeFortran/src/kdnode/KdNodeUtils.f90`              |
| `~/kdt-fortran/src/tree/`                     | `~/KdTreeFortran/src/kdtree/`                             |
| `~/kdt-fortran/src/tree/BuildSubmod.f90`      | `~/KdTreeFortran/src/kdtree/KdTreeBuild.f90`              |
| `~/kdt-fortran/src/tree/TreeGetters.f90`      | `~/KdTreeFortran/src/kdtree/KdTreeGetters.f90`            |
| `~/kdt-fortran/src/tree/TreeUtils.f90`        | `~/KdTreeFortran/src/kdtree/KdTreeUtils.f90`              |
| `~/kdt-fortran/src/tree/TreeModder.f90`       | `~/KdTreeFortran/src/kdtree/KdTreeModders.f90`            |
| `~/kdt-fortran/src/tree/search/`              | `~/KdTreeFortran/src/kdtree/search_modules`               |
| `~/kdt-fortran/src/tree/search/RnnModule.f90` | `~/KdTreeFortran/src/kdtree/search_modules/KdTreeRnn.f90` |
| `~/kdt-fortran/src/KdTree.f90`                | `~/KdTreeFortran/KdTreeFortran.f90`                       |


---

#### Guard-chain fix

`if/else if` chains in all assertion subroutines replaced with independent `if` blocks, so every condition is checked rather than only the first branch taken.

---

### Test coverage added in v0.4.0

#### `Testv040_FIND_NODES/`


| Suite        | What is tested                                                                                                                                                                                                                                                                                                                                                                            |
| ------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ASSERTIONS` | Error guards: uninitialized tree (both variants), dim mismatch, negative epsilon, bad metric, bad buffer size, ids/coords size mismatch                                                                                                                                                                                                                                                   |
| `COORDS`     | Single-query hit with coord verification; multi-query parallel results; no hit (empty bucket); zero epsilon (exact match only); empty tree returns empty buckets without error; manhattan and chebyshev metric correctness; node found after `addNodes`; tree state (pop/dim/mods/initState) unchanged after search; found node fields (`getCoords`, `getSplitAxis`) accessible and valid |
| `IDS`        | Single hit on 1-node tree (nodeId=1); no hit with sentinel id=0; zero epsilon exact match; empty tree returns empty buckets; node found after `addNodes` with correct id; wrong id at same coords returns empty                                                                                                                                                                           |


---

## v0.3.0

### New API

#### `addNodes(coordsList, dataList)`

Appends one or more nodes to an existing, initialized tree. `coordsList` is a `real(real64)` array of shape `[dim, N]`; the optional `dataList` is a polymorphic array of length N carrying one payload per node.

```fortran
real(real64) :: new_pts(2, 3) = reshape([1.0, 0.0,  2.0, 0.0,  3.0, 0.0], [2, 3])
call t%addNodes(new_pts)

! with polymorphic payload
character(len=1) :: labels(3) = ['a', 'b', 'c']
call t%addNodes(new_pts, labels)
```

After adding N nodes to a tree whose population was P, the rebuild decision is:

```
old_modifications + N  >  rebuildRatio * P
```

When the threshold is exceeded the tree is rebuilt from scratch (full quickselect rebalance, `modifications` reset to 0); otherwise the N new nodes are inserted at leaves of the existing structure and `modifications` increments by N. The split axis for each leaf node is assigned by cycling one step past its parent's split axis (`saxs(parentAxis, dim)`).

Adding to an empty tree (built from a zero-column array) always triggers a rebuild since `N > 0 = 0.0` is unconditionally true.

**Error guards** (`error stop`): uninitialized tree; `dim` mismatch; `dataList` present but wrong length or wrong polymorphic type vs existing nodes; `dataList` absent when the tree carries data, or vice versa.

#### `setRebuildRatio(ratio)` / `getRebuildRatio()`

Controls the rebuild threshold. Default is `0.25`. Must satisfy `0 < ratio < 1`; anything outside that range triggers `error stop`. The ratio can also be set at `build` time via the optional `rebuildRatio` argument.

```fortran
call t%setRebuildRatio(0.5_real64)
print *, t%getRebuildRatio()   ! 0.5000...
```

#### `getNumMods()`

Returns the number of insertions since the last rebuild (or since `build`). Reset to 0 by any rebuild and by `destroy`.

```fortran
integer(int64) :: n
n = t%getNumMods()
```

### Internal implementation

#### Node fields added in v0.3.0


| Field                | Type             | Purpose                                                                                                                                                           |
| -------------------- | ---------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `nodeId`             | `integer(int64)` | Unique monotone ID assigned at allocation; stamped from `KdTree%currNodeId`                                                                                       |
| `numRemovesSnapshot` | `integer(int64)` | Value of `KdTree%numRemoves` at the moment this node was allocated; used by `isMember` fast path (no pool scan needed if no removes have occurred since dispatch) |


#### Tree fields added in v0.3.0


| Field           | Type             | Purpose                                                                                           |
| --------------- | ---------------- | ------------------------------------------------------------------------------------------------- |
| `modifications` | `integer(int64)` | Pending insertions since last rebuild; reset to 0 by rebuild and by `destroy`                     |
| `rebuildRatio`  | `real(real64)`   | Threshold controlling rebuild vs leaf-insert (default 0.25)                                       |
| `currNodeId`    | `integer(int64)` | Monotone counter; incremented once per node allocated in both `build` and `addNodes`; never reset |


#### Node pool reallocation

`addNodes` allocates a new pool of size `old_pop + N`, copies the existing entries with a whole-array assignment (`nodePoolTmp(1:old_pop) = this%nodePool(1:old_pop)`), fills in new node slots (coords, `nodeId`, `treeId`, `numRemovesSnapshot`, child indices zeroed), then swaps the pointer and deallocates the old pool. The entire realloc block is serialized under `!$OMP CRITICAL (tree_mutate)`.

#### Rebuild procedure

The internal `rebuild` subroutine allocates a fresh index array `[1..pop]` and calls `buildSubtree` to reorder nodes in-place using the pool-index representation (nodes stay at their current pool positions; only `lch`, `rch`, `splitAxis`, and `rootIdx` are updated via quickselect). After rebuild, `modifications` is set to 0. Rebuild runs inside the same `!$OMP CRITICAL (tree_mutate)` region as the pool reallocation that precedes it.

#### Two-phase critical-section design

`addNodes` uses a single named critical region `!$OMP CRITICAL (tree_mutate)` that covers both the pool realloc and the rebuild/leaf-insert decision. This serializes all mutation while leaving concurrent read-only `rNN` calls completely unblocked. `build` uses only `!$OMP ATOMIC CAPTURE` for the `nextTreeId` increment, so independent trees can be built on separate threads without contention.

#### `isMember` fast path

When no removals have occurred since a node was dispatched (`target%numRemovesSnapshot == this%numRemoves`), membership is confirmed immediately without scanning the pool. A full scan is only needed after removals. The `numRemoves` / `numRemovesSnapshot` infrastructure is present and maintained but `removeNode` is not yet implemented; the fields are scaffolded for a future release.

### Thread safety model


| Operation                                            | Concurrent-safe?                                    |
| ---------------------------------------------------- | --------------------------------------------------- |
| `rNN_Centroid` / `rNN_Node` on a shared tree         | Yes — read-only, no locking                         |
| `addNodes` on the same tree from multiple threads    | Yes — serialized via `!$OMP CRITICAL (tree_mutate)` |
| `build` on independent trees from multiple threads   | Yes — only `nextTreeId` increment is atomic         |
| `build` and `addNodes` on the same tree concurrently | No — `build` is not guarded                         |
| `destroy` concurrently with anything                 | No                                                  |


### Source module additions / changes


| File (v0.3.0 path, renamed in v0.3.1)  | Change                                                                                                                                    |
| -------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| `src/tree/TreeModder/TreeModder.f90`   | New submodule: `addNodes`, `setRebuildRatio`, internal `rebuild` subroutine                                                               |
| `src/tree/TreeGetters/TreeGetters.f90` | Added `getRebuildRatio`, `getNumMods` procedures                                                                                          |
| `src/tree/TreeType.inc`                | Added `modifications`, `rebuildRatio`, `currNodeId` fields; added `setRebuildRatio`, `getRebuildRatio`, `addNodes`, `getNumMods` bindings |
| `src/node/NodeType.inc`                | Added `nodeId`, `numRemovesSnapshot` fields                                                                                               |


### Test coverage added in v0.3.0

#### `Testv030_ADD_NODES/`


| Suite                       | What is tested                                                                                                                                                      |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ASSERTIONS`                | Error guards: uninitialized tree, dim mismatch, data count/type mismatch, missing/extra data, empty data list                                                       |
| `GETTERS`                   | `getPop`, `getDim`, `getInitState` after `addNodes`                                                                                                                 |
| `LIFECYCLE`                 | `nodePool`/`root` associated after add; `destroy` clears state; `destroy -> rebuild -> addNodes` works; zero-pop build then add                                     |
| `IS_MEMBER`                 | Original node still member after add; added node is member; not member after `destroy`; not member after `rebuild`                                                  |
| `RNN_CENTROID` / `RNN_NODE` | rNN finds added nodes (3 metrics); radius excluding added nodes returns only originals                                                                              |
| `DATA`                      | Polymorphic data on added nodes; correct data returned by rNN (3 metrics)                                                                                           |
| `REBUILD`                   | Force rebuild (`getNumMods=0` after); leaf insert (`getNumMods=1` after); rNN correct in both cases (3 metrics); leaf insert preserves all original nodes in search |
| `DUPLICATES`                | Add duplicate-coordinate nodes (2D-4D, 3 metrics)                                                                                                                   |
| `COLLINEAR`                 | Add collinear nodes along one axis and two axes (3 metrics)                                                                                                         |
| `MULTI_ADD`                 | Three consecutive `addNodes` calls; cumulative pop correct; all nodes searchable (3 metrics)                                                                        |
| `SET_REBUILD_RATIO`         | Default/set/via-build ratio; ratio affects rebuild vs leaf decision; error guards (0, negative, >=1)                                                                |
| `NUM_MODS`                  | `getNumMods=0` after build; accumulates across leaf inserts; resets after rebuild; resets after `destroy -> build`                                                  |
| `INCREMENTAL`               | Single-node `addNodes` repeated 5x; pop/numMods/state after each step; rNN finds each new node (3 metrics)                                                          |
| `REBUILD_BOUNDARY`          | Exact boundary (stays leaf); one-over (triggers rebuild); accumulated mods crossing threshold over multiple calls                                                   |
| `ZERO_ADD`                  | `addNodes` with zero-column array: pop/numMods/state unchanged; rNN still correct                                                                                   |


#### `Testv030_MULTITHREAD/`


| Suite               | What is tested                                                                                                                                                              |
| ------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `SIMPLE`            | Basic parallel build and search sanity                                                                                                                                      |
| `INTERNAL_STATE`    | `pop`, `numMods`, `rebuildRatio`, `getInitState` consistency after parallel `addNodes`; rebuild-boundary and leaf-boundary cases; independent-tree and shared-tree variants |
| `INDEPENDENT_TREES` | 4 threads each build/search/`addNodes` their own tree; all 4 `treeId`s are distinct                                                                                         |
| `ADD_NODES`         | Sequential `addNodes` then 4 concurrent `rNN` searches on the same tree                                                                                                     |
| `CONCURRENT_ADD`    | 4 threads call `addNodes` concurrently on the same tree; final `pop` is exact; all nodes are searchable afterwards                                                          |
| `CONCURRENT_RNN`    | 4 threads call `rNN_Centroid` concurrently on a shared built tree (all 3 metrics)                                                                                           |
| `V010`              | v0.1.0 build, collinear, and duplicate cases run concurrently across 4 threads                                                                                              |
| `V020`              | v0.2.0 search scenarios (single-node, empty result, duplicates, collinear, all 3 metrics) run concurrently across 4 threads                                                 |
| `V021`              | v0.2.1 lifecycle, `isMember`, and data-payload scenarios run concurrently across 4 threads                                                                                  |


## v0.2.1

> **Note:** `Node` → `KdNode` and `NodePtr` → `KdNodePtr` in v0.3.1. Code examples below use the v0.2.1 names.

### API changes

`**rNN_Node` target parameter type changed.**

The `target` argument was `type(node), pointer`; it is now `type(NodePtr)`.  
Callers pass the `NodePtr` returned directly from a prior search instead of  
dereferencing its `%p` field. This lets the implementation use the private  
`%src_` pointer (which points into the tree's node pool) for exact physical  
identity checks .

```fortran
! v0.2.0
type(Node), pointer        :: target
res    = t%rNN_Centroid(centroid, r)
target => res(1)%p
res    = t%rNN_Node(target, r2)

! v0.2.1
type(NodePtr), allocatable :: centroid_res(:)
centroid_res = t%rNN_Centroid(centroid, r)
res          = t%rNN_Node(centroid_res(1), r2)
```

### New features

#### `excludeTarget` optional parameter on `rNN_Node`

When `.true.`, the target node is removed from the result set.  
Removal uses physical pointer identity (`src_` against the result's `src_`),  
so it is correct even when the tree contains duplicate coordinates.

```fortran
res = t%rNN_Node(centroid_res(1), radius, excludeTarget=.true.)
```

#### `bufferSize` validation on search functions

Passing `bufferSize <= 0` now triggers `error stop` immediately instead of

allocating a zero- or negative-length buffer.

#### `build` data-length precondition

The optional `data` argument to `build` must satisfy  
`size(data) == size(coords, 2)` or `size(data) == 0` (empty array is allowed  
and leaves nodes without a payload). Any other length triggers `error stop`.

#### `NodePtr` type extended

In v0.2.0 `NodePtr` was a bare wrapper around `%p`. In v0.2.1 it gains:

- `%src_`  private back-pointer to the original tree node (used internallyfor identity; not accessible to callers).
- `%destroy()` explicit method to free the owned copy and null both pointers.
- `final` finalizer copy freed automatically when the `NodePtr` goes out of  
scope. Double-destroy (explicit then implicit, or two explicit calls) is a  
no-op.

#### Tree Internal State

##### `Tree%associatedNodePool(this, assoc)` / `Tree%associatedRoot(this, assoc)`

Diagnostic predicates that set `assoc` to `.true.` iff the internal node pool  
array (respectively the root pointer) is currently associated. Both return  
`.false.` on an uninitialized or destroyed tree. Primarily used in lifecycle  
tests to verify that `destroy` fully resets internal state.

##### `Tree%getTreeId()`

Returns the unique `integer(int64)` ID assigned to the tree at `build` time.  
Each call to `build` increments a module-level counter and stamps every node  
in the pool with the new ID, which is what `isMember` compares against.

##### `Tree%getInitState(this, isInit)`

Sets `isInit` to `.true.` if the tree has been built and not yet destroyed,  
`.false.` otherwise. Useful for guarding re-builds and testing lifecycle state.

#### **Integer width promotion**

`treeId`, `splitAxis`, and the module-level `nextTreeId` counter were widened

from default `integer` to `integer(int64)`.

### Source module additions


| File                               | Role                                                                                 |
| ---------------------------------- | ------------------------------------------------------------------------------------ |
| `src/node/NodeUtils/NodeUtils.f90` | `destroyNodePtr` and `finalizerNodePtr` submodule procedures (merged into NodeUtils) |


### Test coverage added in v0.2.1

#### **Buffer size**

`Testv021_BUFFER_SIZE/`

Both `rNN_Node` and `rNN_Centroid` are exercised with the default buffer (1000),  
an exact-fit buffer, a buffer smaller than the result set (exercises the  
doubling-resize path), a buffer larger than the result set, and invalid sizes  
0 and −1 (both must `error stop`).

#### **Radius edge cases**

`Testv021_EMPTY_RESULT_POPULATED_TREE/`

`Testv021_ZERO_RADIUS_POPULATED_TREE/`

Both query forms (`rNN_Node`, `rNN_Centroid`) are tested with a radius small  
enough that no node is captured in a populated tree, and with radius = 0.  
All three metrics (Euclidean, Manhattan, Chebyshev) are covered in each case.  
`rNN_Node` variants include `excludeTarget=.true.` to exercise the removal path  
and confirm a zero-element result is returned correctly.

#### **Data payload**

`Testv021_DATA_INPUT/`

`build` with integer and real `data` arrays; data survives tree reordering  
(original column index stored as payload, verified after build); `rNN` returns  
nodes with intact payloads; size-mismatch error stops for both populated and  
unpopulated data arrays; empty-tree build with empty and non-empty data.

#### **Tree lifecycle**

`Testv021_LIFECYCLE/`  
Uninitialized tree (no `build`), `destroy` on a built tree, double-`destroy`,  
`build → destroy → build` with identical coordinates, and  
`build → destroy → build` with different coordinates. Double-`build` without  
an intervening `destroy` must `error stop`.

#### **NodePtr lifecycle**

`Testv021_NODEPTR_LIFECYCLE/`

Explicit `%destroy()` frees the copy; double-`destroy` is a no-op (no crash,  
no double-free).

#### **Tree getters**

`Testv021_TREE_GETTERS/`

`getDim`, `getPop`, and `getInitState` are verified across all four tree  
states: uninitialized, empty (built from a zero-column array), populated, and  
freed.

#### Node Membership

`Testv021_IS_MEMBER_*/`

Direct coverage of the membership predicate: a node from the same tree returns  
`.true.`; a node from a different tree returns `.false.`; after `destroy` any  
node must fail membership; after `build → destroy → build` the new nodes must  
pass and old ones must fail.

---

## v0.2.0

Tree construction, all distance functions (node-to-node and node-to-point) for  
Euclidean, Manhattan, and Chebyshev metrics with error stops (unallocated  
coords, axis mismatch); `rNN_Centroid` and `rNN_Node` with error stops (empty  
tree, non-member, null target, negative radius, invalid metric string);  
single-node geometry correctness for all three metrics and both query forms;  
non-member rejection; duplicate points (1–4 axes, all metrics); collinear  
points (1-axis and 2-axis varieties, all metrics).

---

## v0.1.0

Tree construction: empty tree, single point, multi-point, 1D–4D axis-aligned  
arrays, collinear points, duplicates.

---