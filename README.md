# kdt-fortran

A balanced kd-tree in modern Fortran with radius nearest-neighbour search.

---

## v0.2.1

### API changes

**`rNN_Node` target parameter type changed.**

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

| File                          | Role                                                             |
| ----------------------------- | ---------------------------------------------------------------- |
| `src/node/NodePtrUtils.f90` | `destroyNodePtr` and `finalizerNodePtr` submodule procedures |

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

## v0.2.2 (planned)

Thread-safety testing. Concurrent read-only searches (`rNN_Node`,
`rNN_Centroid`) on a shared built tree are expected to be safe since they do
not mutate the tree; this will be pinned with OpenMP parallel-do tests.
Concurrent `build` or `destroy` is explicitly out of scope.
