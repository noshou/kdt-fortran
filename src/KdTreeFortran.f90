module KdTreeFortran
    
    use iso_fortran_env, only: int64, real64, output_unit
    implicit none
    private
    public                  :: KdTree, KdNode, KdNodePtr
    integer(int64), save    :: nextTreeId = 0_int64

    !> Pointer to an owned copy of a Node returned by a search.
    !! p is always a pointer to a heap-allocated copy; call destroy() or let it
    !! go out of scope to free the copy.
    type :: KdNodePtr
        type(KdNode), pointer :: p    => null()
        contains
            procedure       :: destroy => destroyNodePtr
            final           :: finalizerNodePtr
    end type KdNodePtr

    type :: KdNode
        private
        logical                         :: hasData = .false. !> flagged true if node contains data
        class(*), allocatable           :: data
        real(kind=real64), allocatable  :: coords(:)
        integer(int64)                  :: splitAxis        !> tracks which index in coords is the splitting plane
        integer(int64)                  :: numRemovesSnapshot = 0_int64, nodeId = 0_int64
        integer(int64)                  :: lch = 0_int64, rch = 0_int64  !> indices into nodePool; 0 = no child
        integer(int64)                  :: treeId
        contains    
            procedure                   :: euclideanDist
            procedure                   :: euclideanDistPoint
            procedure                   :: manhattanDist
            procedure                   :: manhattanDistPoint
            procedure                   :: chebyshevDist 
            procedure                   :: chebyshevDistPoint
            procedure                   :: printNode
            procedure                   :: printNodeSingle
            procedure                   :: getData
            procedure                   :: getCoords
            procedure                   :: getSplitAxis
    end type KdNode
    
    !> A kd-tree
    type :: KdTree
    private
    integer(int64)       :: dim = 0_int64, pop = 0_int64, TreeId = 0_int64
    integer(int64)       :: currNodeId = 0_int64, numRemoves = 0_int64
    logical              :: initialized = .false.       !> true iff tree%build() is called successfully
    type(KdNode), pointer  :: nodePool(:) => null()     !> pool of allocated nodes
    integer(int64)       :: rootIdx = 0_int64           !> index into nodePool for the root; 0 = empty
    integer(int64)       :: modifications = 0_int64     !> total number of insertions/deletions on tree
    real(real64)         :: rebuildRatio = 0.25_real64  !> if modifications > rebuildRatio * pop, trigger rebuild 

        contains
            procedure    :: getDim
            procedure    :: getPop
            procedure    :: build
            procedure    :: printTree
            procedure    :: rNN_Node
            procedure    :: rNN_Centroid
            procedure    :: isMember
            procedure    :: getInitState
            procedure    :: getTreeId
            procedure    :: setRebuildRatio
            procedure    :: getRebuildRatio
            procedure    :: associatedNodePool
            procedure    :: associatedRoot
            procedure    :: addNodes
            procedure    :: getNumMods
            procedure    :: assert
            procedure    :: destroy
            final        :: finalizer
    end type KdTree

    interface
        
        !===================================!
        !========= NodeGetters.f90 =========!
        !===================================!

        !> Returns the data stored in this node.
        !! The dynamic type of the result matches 
        !! the type passed as data to build().  
        !! Use a select type construct 
        !! to compare or assign the value.
        module function getData(this) result(data)
            class(KdNode), intent(in) :: this
            class(*), allocatable     :: data
        end function getData

        !> Returns the coordinates of this node
        module function getCoords(this) result(coords)
            class(KdNode), intent(in) :: this
            real(real64), allocatable :: coords(:)
        end function getCoords

        !> Returns the splitting axis of this node
        module function getSplitAxis(this) result(splitAxs)
            class(KdNode), intent(in) :: this
            integer(int64)            :: splitAxs
        end function getSplitAxis

        !=================================================!

        !====================================!
        !========= NodeDistance.f90 =========!
        !====================================!

        !> Calculates the euclidean distance between two Nodes
        !! @param[in] that The Node to calculate distance from          
        !!
        !! @return The euclidean distance between two Nodes
        module function euclideanDist(this, that) result(dist)
            class(KdNode), intent(in) :: this
            type(KdNode), intent(in)  :: that
            real(kind=real64)       :: dist
        end function euclideanDist

        !> Calculates the euclidean distance between this node 
        !! and a point.
        !! @param[in] point the point 
        !! 
        !! @return the euclidean distance from the node to the point
        module function euclideanDistPoint(this, point) result(dist)
            class(KdNode), intent(in)             :: this
            real(real64), allocatable, intent(in) :: point(:)
            real(kind=real64)                     :: dist
        end function euclideanDistPoint

        !> Calculates the Manhattan (L1) distance between two Nodes
        !! @param[in] that The Node to calculate distance from
        !!
        !! @return The Manhattan distance between two Nodes
        module function manhattanDist(this, that) result(dist)
            class(KdNode), intent(in) :: this
            type(KdNode), intent(in)  :: that
            real(kind=real64)       :: dist
        end function manhattanDist

        !> Calculates the Manhattan (L1) distance between this node
        !! and a point.
        !! @param[in] point the point
        !!
        !! @return the Manhattan distance from the node to the point
        module function manhattanDistPoint(this, point) result(dist)
            class(KdNode), intent(in)             :: this
            real(real64), allocatable, intent(in) :: point(:)
            real(kind=real64)                     :: dist
        end function manhattanDistPoint

        !> Calculates the Chebyshev (L∞) distance between two Nodes
        !! @param[in] that The Node to calculate distance from
        !!
        !! @return The Chebyshev distance between two Nodes
        module function chebyshevDist(this, that) result(dist)
            class(KdNode), intent(in) :: this
            type(KdNode), intent(in)  :: that
            real(kind=real64)         :: dist
        end function chebyshevDist

        !> Calculates the Chebyshev (L∞) distance between this node
        !! and a point.
        !! @param[in] point the point
        !!
        !! @return the Chebyshev distance from the node to the point
        module function chebyshevDistPoint(this, point) result(dist)
            class(KdNode), intent(in)             :: this
            real(real64), allocatable, intent(in) :: point(:)
            real(kind=real64)                     :: dist
        end function chebyshevDistPoint

        !==========================================================!
        !=================================!
        !========= NodeUtils.f90 =========!
        !=================================!

        !> Recursively prints this Node and its subtree in pre-order.
        !! @param[in] depth the depth of this Node
        !! @param[in] unit  optional output unit (defaults to stdout)
        module recursive subroutine printNode(this, depth, nodePool, unit)
            class(KdNode), intent(in)              :: this
            integer(int64), intent(in)             :: depth
            type(KdNode),     intent(in)           :: nodePool(:)
            integer,        intent(in), optional   :: unit
        end subroutine printNode

        !> Prints the current node, but not its subtree
        !!
        !! Use: printNode(depth) for the full subtree
        !! @param[in] unit  optional output unit (defaults to stdout)
        module subroutine printNodeSingle(this, unit)
            class(KdNode), intent(in)     :: this
            integer, intent(in), optional :: unit
        end subroutine printNodeSingle

        !> Frees the owned node copy and nulls p.
        module subroutine destroyNodePtr(this)
            class(KdNodePtr), intent(inout) :: this
        end subroutine destroyNodePtr

        !> Frees the owned node copy when NodePtr goes out of scope.
        module subroutine finalizerNodePtr(this)
            type(KdNodePtr), intent(inout)  :: this
        end subroutine finalizerNodePtr

        !================================================================!

        !===================================!
        !========= TreeGetters.f90 =========!
        !===================================!

        !> Returns the dimension (number of splitting axis) of the tree
        module function getDim(this) result(k)
            class(KdTree), intent(in) :: this
            integer(int64)            :: k
        end function getDim

        !> Returns the number of nodes in the tree
        module function getPop(this) result(n)
            class(KdTree), intent(in) :: this
            integer(int64)            :: n
        end function getPop

        !> Sets isInit to true iff the tree is initialized
        !! param[inout] isInit the state of the tree
        module subroutine getInitState(this, isInit)
            class(KdTree), intent(in)    :: this
            logical,     intent(inout)   :: isInit
        end subroutine getInitState

        !> Returns the unique integer ID assigned to this tree at build time
        module function getTreeId(this) result(id)
            class(KdTree), intent(in) :: this
            integer(int64)            :: id
        end function getTreeId


        !> Returns the rebuildRatio.
        !! If this%modifications > this%rebuildRatio * this%pop,
        !! trigger a rebuild of the tree.
        module function getRebuildRatio(this) result(rebuildRatio)
            class(KdTree), intent(in) :: this
            real(real64)              :: rebuildRatio
        end function getRebuildRatio

        !> Returns the number of modifications done on tree.
        !! If this%modifications > this%rebuildRatio * this%pop,
        !! trigger a rebuild of the tree.
        module function getNumMods(this) result(numMods)
            class(KdTree), intent(in) :: this
            integer(int64)            :: numMods
        end function getNumMods

        !==================================================================!

        !=================================!
        !========= TreeUtils.f90 =========!
        !=================================!

        !> Prints the entire tree in post-order.
        !! @param[in] unit  optional output unit (defaults to stdout)
        module subroutine printTree(this, unit)
            class(KdTree), intent(in)       :: this
            integer, intent(in), optional   :: unit
        end subroutine printTree

        !> Checks if a target node is a member of this tree instance
        !! @param[in] target the target node to check
        !!
        !! @return true if a member, false if not
        module function isMember(this, target) result(res)
            class(KdTree), intent(in)         :: this
            type(KdNode), pointer, intent(in) :: target
            logical                           :: res
        end function isMember

        !> Compares t%printTree output against expected as a sorted
        !! multiset of coord-tuples (indent and `[axis=N]` prefix stripped).
        !! Looseness is required: with ties on a split axis, points can
        !! swap siblings or change depth across runs.
        !! @param[in] testName diagnostic label
        !! @param[in] expected per-node lines in printNode format
        module subroutine assert(this, testName, expected)
            class(KdTree),    intent(in) :: this 
            character(len=*), intent(in) :: testName
            character(len=*), intent(in) :: expected(:)
        end subroutine assert

        !> Sets assoc to true iff a nodePool is allocated.
        !! Should agree with associatedSubtree and !(this%initialized)
        !! @param[inout] assoc true if this%nodePool is allocated, 
        !!                     false otherwise
        module subroutine associatedNodePool(this, assoc)
            class(KdTree),    intent(in)    :: this 
            logical,          intent(inout) :: assoc
        end subroutine associatedNodePool

        !> Sets assoc to true iff a root node is allocated.
        !! Should agree with associatedNodePool and !(this%initialized)
        !! @param[inout] assoc true if this%root is allocated, false otherwise
        module subroutine associatedRoot(this, assoc)
            class(KdTree),    intent(in)    :: this 
            logical,          intent(inout) :: assoc
        end subroutine associatedRoot

        !> Calculates the split axis of a node.
        !! Param "a" can either be depth of node or 
        !! parent's splitting axis. 
        !!
        !! @param[in] a the split axis of the parent or the depth of the node
        !! @param[in] k the dimension of the tree
        !!
        !! @return    the new split axis
        !!
        !! Examples:
        !!  - Depth Counter (0-based)
        !!
        !!  mod(0, k)+1 = 1, mod(1, k)+1 = 2, ... cycles through axes 1→2→...→dim→1
        !!
        !!   - Parent's split axis (1-based)
        !!
        !!  mod(parent_axis, dim)+1 steps to the next axis 
        !! e.g: mod(2, 3)+1 = 3, mod(3, 3)+1 = 1
        module function saxs(a, k) result(splitAxis)
            integer(int64), intent(in) :: a, k
            integer(int64)             :: splitAxis
        end function saxs

        !> Frees the node pool and resets tree state.
        module subroutine destroy(this)
            class(KdTree), intent(inout) :: this
        end subroutine destroy

        !> Frees tree automatically   when it goes out of scope
        module subroutine finalizer(this)
            type(KdTree), intent(inout) :: this
        end subroutine finalizer

        !=========================================================================!

        !===================================! 
        !========= BuildSubmod.f90 =========! 
        !===================================!

        !> Builds a balanced Kd-Tree from a set of points.
        !! @param[in] coords       A (k, n) array where n is the number of points
        !!                         and k is the dimensionality of the splitting axes.
        !! @param[in] data         (Optional) A rank-1 array of size n, where each
        !!                         element is the data associated with a point in coords.
        !!                         The element type determines what select type cases the
        !!                         caller must handle when calling getData() on a result node.
        !! @param[in] rebuildRatio If this%modifications > this%rebuildRatio * this%pop,
        !!                         then a tree rebuild is triggered. Defaulted to 0.25.
        module subroutine build(this, coords, data, rebuildRatio)
            class(KdTree), intent(inout)            :: this
            real(kind=real64), intent(in)           :: coords(:,:)
            class(*), intent(in), optional          :: data(:)
            real(kind=real64), intent(in), optional :: rebuildRatio
        end subroutine build

        !> Recursively builds a balanced subtree from the node pool.
        !! @param[inout] this      the tree being built
        !! @param[out]   root      index of the root of the tree
        !! @param[in]    depth     current depth, used to cycle the split axis
        !! @param[inout] indices   index permutation array, rearranged in-place by quickSelect
        !! @param[in]    lowerIdx  lower bound of the index range for this subtree
        !! @param[in]    upperIdx  upper bound of the index range for this subtree
        recursive module subroutine buildSubtree(   &
            this,                                   &
            rootIdx,                                &
            depth,                                  &
            indices,                                &
            lowerIdx,                               &
            upperIdx                                &
        )
            type(KdTree),   intent(inout) :: this
            integer(int64), intent(out)   :: rootIdx
            integer(int64), intent(inout) :: indices(:)
            integer(int64), intent(in)    :: lowerIdx, upperIdx, depth
        end subroutine buildSubtree

        !====================================================================================!

        !==================================!
        !========= TreeModder.f90 =========!
        !==================================!

        !> Overwrites rebuildRatio. Must be in (0, 1).
        !! Not thread-safe; do not call concurrently with addNodes().
        !! @param[in] ratio the new rebuildRatio
        module subroutine setRebuildRatio(this, ratio)
            class(KdTree),  intent(inout) :: this
            real(real64), intent(in)    :: ratio
        end subroutine setRebuildRatio

        !> Inserts new nodes into the tree.
        !!
        !! coordsList is a (k, n) array of n points in k dimensions;
        !! k must match the dimension of the tree.
        !! If the tree holds data, dataList must be provided and have
        !! exactly n elements of the same type as the existing data.
        !! New nodes are appended to the pool then either inserted at leaves
        !! or trigger a full rebuild if modifications exceed rebuildRatio * pop.
        !!
        !! Thread safety: concurrent calls to addNodes() are serialized internally
        !! via !$OMP CRITICAL. The precondition checks (before the critical section)
        !! are NOT thread-safe; callers must ensure the tree is initialized and
        !! stable before calling.
        !!
        !! @param[in] coordsList  (k, n) array of coordinates to add
        !! @param[in] dataList    optional rank-1 array of n data values
        module subroutine addNodes(this, coordsList, dataList)
            class(KdTree),  intent(inout)   :: this
            real(real64), intent(in)        :: coordsList(:,:)
            class(*), intent(in), optional  :: dataList(:)
        end subroutine addNodes

        !==========================================================================!

        !====================================! 
        !========= SearchSubmod.f90 =========! 
        !====================================! 

        !> Performs radius nearest neighbour search on a target node
        !!
        !! Searches for nodes within a given radius of target node;
        !! defaults to euclidean metric, and including target node in results.
        !! @param[in] target        the target node
        !! @param[in] radius        the search radius; error stop if negative
        !! @param[in] bufferSize    optional initial result buffer size; defaults to 1000;
        !!                          doubles when full; error stop if <= 0
        !! @param[in] metric        optional metric ('euclidean', 'manhattan', 'chebyshev');
        !!                          error stop if unrecognised
        !! @param[in] excludeTarget if .true., removes the target node from the returned list
        !!
        !! @return list of nodes within the search radius
        module function rNN_Node(   &
            this,                   & 
            target,                 &
            radius,                 &
            bufferSize,             &
            metric,                 &
            excludeTarget           &
        ) result(res)
            class(KdTree), intent(in)              :: this
            type(KdNodePtr), intent(in)            :: target
            real(kind=real64), intent(in)          :: radius
            integer, intent(in), optional          :: bufferSize
            character(len=*), intent(in), optional :: metric
            logical, intent(in), optional          :: excludeTarget
            type(KdNodePtr), allocatable           :: res(:)
        end function rNN_Node

        !> Performs radius nearest neighbour search on a centroid
        !!
        !! Searches for nodes within a given radius of an arbitrary point;
        !! defaults to euclidean metric.
        !! @param[in] centroid    the centre of the search sphere; must match the tree dimension
        !! @param[in] radius      the search radius; error stop if negative
        !! @param[in] bufferSize  optional initial result buffer size; defaults to 1000;
        !!                        doubles when full; error stop if <= 0
        !! @param[in] metric      optional metric ('euclidean', 'manhattan', 'chebyshev');
        !!                        error stop if unrecognised
        !!
        !! @return list of nodes within the search radius
        module function rNN_Centroid(   &
            this,                       &
            centroid,                   &
            radius,                     &
            bufferSize,                 &
            metric                      &
        ) result(res)
            class(KdTree), intent(in)              :: this
            real(kind=real64), intent(in)          :: radius, centroid(:)
            integer, intent(in), optional          :: bufferSize
            character(len=*), intent(in), optional :: metric 
            type(KdNodePtr), allocatable           :: res(:)

        end function rNN_Centroid

        !>
        module function searchByCoords(this, id, coords) result(res)
            class(KdTree),  intent(in)  :: this
            integer(int64), intent(in)  :: id
            real(real64),   intent(in)  :: coords(:)
            type(KdNode)                :: res
        end function searchByCoords

        !======================================================================================!    
    end interface

end module KdTreeFortran