module KdTree 
    
    use iso_fortran_env, only: real64, output_unit
    implicit none
    private
    public              :: Tree, Node, NodePtr
    integer, save       :: nextTreeId = 0

    type :: Node
        private
        class(*), allocatable           :: data
        real(kind=real64), allocatable  :: coords(:)
        integer                         :: splitAxis ! tracks which index in coords is the splitting plane
        type(Node), pointer             :: leftChild => null(), rightChild => null()
        integer                         :: treeId
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
    end type Node

    !> pointer to a Node
    type :: NodePtr
        type(Node), pointer :: p => null()
    end type NodePtr
    
    type :: Tree
    private
    integer              :: dim = 0, pop = 0, TreeId = 0
    type(node), pointer  :: nodePool(:) => null()
    type(node), pointer  :: root => null()
        contains
            procedure    :: getDim
            procedure    :: getPop
            procedure    :: build
            procedure    :: printTree
            procedure    :: rNN_Node
            procedure    :: rNN_Centroid
            procedure    :: isMember
            procedure    :: assert
            procedure    :: destroy
            final        :: finalizer
    end type Tree


    interface
        !=======================================================!
        !=================== NodeGetters.f90 ===================!
        !=======================================================!
        
        !> Returns the data stored in this node 
        module function getData(this) result(data)
            class(Node), intent(in) :: this
            class(*), allocatable   :: data
        end function getData

        !> Returns the coordinates of this node
        module function getCoords(this) result(coords)
            class(Node), intent(in)   :: this 
            real(real64), allocatable :: coords(:)
        end function getCoords

        !> Returns the splitting axis of this node
        module function getSplitAxis(this) result(splitAxs)
            class(Node), intent(in) :: this 
            integer                 :: splitAxs
        end function getSplitAxis

        !=======================================================!
        
        !=======================================================!
        !=================== NodeDistance.f90 ==================!
        !=======================================================!

        !> Calculates the euclidean distance between two Nodes
        !! @param[in] that The Node to calculate distance from          
        !!
        !! @return The euclidean distance between two Nodes
        module function euclideanDist(this, that) result(dist)
            class(Node), intent(in) :: this
            type(Node), intent(in)  :: that
            real(kind=real64)       :: dist
        end function euclideanDist

        !> Calculates the euclidean distance between this node 
        !! and a point.
        !! @param[in] point the point 
        !! 
        !! @return the euclidean distance from the node to the point
        module function euclideanDistPoint(this, point) result(dist)
            class(Node),  intent(in)              :: this
            real(real64), allocatable, intent(in) :: point(:)
            real(kind=real64)                     :: dist
        end function euclideanDistPoint

        !> Calculates the Manhattan (L1) distance between two Nodes
        !! @param[in] that The Node to calculate distance from
        !!
        !! @return The Manhattan distance between two Nodes
        module function manhattanDist(this, that) result(dist)
            class(Node), intent(in) :: this
            type(Node), intent(in)  :: that
            real(kind=real64)       :: dist
        end function manhattanDist

        !> Calculates the Manhattan (L1) distance between this node
        !! and a point.
        !! @param[in] point the point
        !!
        !! @return the Manhattan distance from the node to the point
        module function manhattanDistPoint(this, point) result(dist)
            class(Node),  intent(in)              :: this
            real(real64), allocatable, intent(in) :: point(:)
            real(kind=real64)                     :: dist
        end function manhattanDistPoint

        !> Calculates the Chebyshev (L∞) distance between two Nodes
        !! @param[in] that The Node to calculate distance from
        !!
        !! @return The Chebyshev distance between two Nodes
        module function chebyshevDist(this, that) result(dist)
            class(Node), intent(in) :: this
            type(Node), intent(in)  :: that
            real(kind=real64)       :: dist
        end function chebyshevDist

        !> Calculates the Chebyshev (L∞) distance between this node
        !! and a point.
        !! @param[in] point the point
        !!
        !! @return the Chebyshev distance from the node to the point
        module function chebyshevDistPoint(this, point) result(dist)
            class(Node),  intent(in)              :: this
            real(real64), allocatable, intent(in) :: point(:)
            real(kind=real64)                     :: dist
        end function chebyshevDistPoint

        !=======================================================!

        !=======================================================!
        !==================== NodeUtils.f90 ====================!
        !=======================================================!
        
        !> Recursively prints this Node and its subtree in pre-order.
        !! @param[in] depth the depth of this Node
        !! @param[in] unit  optional output unit (defaults to stdout)
        module recursive subroutine printNode(this, depth, unit)
            class(Node), intent(in)       :: this
            integer,     intent(in)       :: depth
            integer, intent(in), optional :: unit
        end subroutine printNode

        !> Prints the current node, but not its subtree
        !!
        !! Use: printNode(depth) for the full subtree
        !! @param[in] unit  optional output unit (defaults to stdout)
        module subroutine printNodeSingle(this, unit)
            class(Node), intent(in)       :: this
            integer, intent(in), optional :: unit
        end subroutine printNodeSingle

        !=======================================================!

        !=======================================================!
        !=================== TreeGetters.f90 ===================!
        !=======================================================!

        !> Returns the dimension (number of splitting axis) of the tree
        module function getDim(this) result(k)
            class(Tree), intent(in) :: this
            integer                 :: k
        end function getDim

        !> Returns the number of nodes in the tree
        module function getPop(this) result(n)
            class(Tree), intent(in) :: this
            integer                 :: n
        end function getPop

        !=======================================================!

        !=======================================================!
        !==================== TreeUtils.f90 ====================!
        !=======================================================!
        
        !> Prints the entire tree in post-order.
        !! @param[in] unit  optional output unit (defaults to stdout)
        module subroutine printTree(this, unit)
            class(Tree), intent(in)       :: this
            integer, intent(in), optional :: unit
        end subroutine printTree
        
        !> Checks if a target node is a member of this tree instance
        !! @param[in] target the target node to check
        !!
        !! @return true if a member, false if not
        module function isMember(this, target) result(res)
            class(Tree), intent(in)         :: this
            type(node), pointer, intent(in) :: target
            logical                         :: res
        end function isMember
        
        !> Compares t%printTree output against expected as a sorted
        !! multiset of coord-tuples (indent and `[axis=N]` prefix stripped).
        !! Looseness is required: with ties on a split axis, points can
        !! swap siblings or change depth across runs.
        !! @param[in] testName diagnostic label
        !! @param[in] expected per-node lines in printNode format
        module subroutine assert(this, testName, expected)
            class(Tree),      intent(in) :: this 
            character(len=*), intent(in) :: testName
            character(len=*), intent(in) :: expected(:)
        end subroutine assert

        !> Frees the node pool and resets tree state.
        module subroutine destroy(this)
            class(Tree), intent(inout) :: this
        end subroutine destroy
        
        !> Frees tree automatically   when it goes out of scope
        module subroutine finalizer(this)
            type(Tree), intent(inout) :: this
        end subroutine finalizer

        !=======================================================!

        !=======================================================!
        !=================== BuildSubmod.f90 ===================!
        !=======================================================!

        !> Builds a balanced Kd-Tree from a set of points.
        !! @param[in] coords A (k, n) array where n is the number of points 
        !!                   and k is the dimensionality of the splitting axes.
        !! @param[in] data   (Optional) A rank-1 array of size n, where each 
        !!                   element is the data associated with a point in coords.
        module subroutine build(this, coords, data)
            class(tree), intent(inout)      :: this
            real(kind=real64), intent(in)   :: coords(:,:)
            class(*), intent(in), optional  :: data(:)
        end subroutine build
        
        !=======================================================!

        !=======================================================!
        !================== SearchSubmod.f90  ==================!
        !=======================================================!
        
        !> Performs radius nearest neighbour search on a target node
        !!
        !! Searches for nodes within a given radius of target node;
        !! defaults to euclidean metric. 
        !! @param[in] target      the target node 
        !! @param[in] radius      the search radius 
        !! @param[in] initialSize optional initial array size; defaults to 1000
        !! @param[in] metric      optional metric (euclidean, chebyshev, manhattan)
        !!
        !! @return list of nodes within the search radius
        module function rNN_Node(this, target, radius, initialSize, metric) result(res)
            class(tree), intent(in)                :: this
            type(node), pointer, intent(in)        :: target
            real(kind=real64), intent(in)          :: radius
            integer, intent(in), optional          :: initialSize
            character(len=*), intent(in), optional :: metric 
            type(nodePtr), allocatable             :: res(:)
        end function rNN_Node

        !> Performs radius nearest neighbour search on a centroid
        !! 
        !! Searches for nodes within a given radius of the centroid;
        !! defaults to euclidean metric.
        !! @param[in] centroid    the center of the search sphere  
        !! @param[in] radius      the search radius 
        !! @param[in] initialSize optional initial array size; defaults to 1000
        !! @param[in] metric      optional metric (euclidean, chebyshev, manhattan)
        !!
        !! @return list of nodes within the search radius
        module function rNN_Centroid(this, centroid, radius, initialSize, metric) result(res)
            class(tree), intent(in)                :: this
            real(kind=real64), intent(in)          :: radius, centroid(:)
            integer, intent(in), optional          :: initialSize
            character(len=*), intent(in), optional :: metric 
            type(nodePtr), allocatable             :: res(:)

        end function rNN_Centroid

        !=======================================================!

    end interface
    
end module KdTree