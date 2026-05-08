module KdTreeModule 
    
    use iso_fortran_env, only: real64
    use KdNodeModule,    only: Node, NodePtr
    implicit none
    private
    public              :: Tree, Node, NodePtr
    integer, save       :: nextTreeId = 0

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
        !================== HelpersSubmod.f90 ==================!
        !=======================================================!

        !> Prints the entire tree in post-order.
        !! @param[in] unit  optional output unit (defaults to stdout)
        module subroutine printTree(this, unit)
            class(Tree), intent(in)       :: this
            integer, intent(in), optional :: unit
        end subroutine printTree
        
        !> Returns the dimension (number of splitting axis) of the tree
        module function getDim(this) result(k)
            class(Tree), intent(in) :: this
            integer                 :: k
        end function getDim
        
        !> Returns the number of nodes in the tree
        module function getPop(this) result(n)
            class(Tree), intent(in) :: this
        end function getPop

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
        !! Searches for nodes within a given radius of target node
        !! @param[in] target      the target node 
        !! @param[in] radius      the search radius 
        !! @param[in] initialSize optional initial array size; defaults to 1000
        !!
        !! @return list of nodes within the search radius
        module function rNN_Node(this, target, radius, initialSize) result(res)
            class(tree), intent(in)          :: this
            type(node), pointer, intent(in)  :: target
            real(kind=real64)                :: radius
            integer, intent(in), optional    :: initialSize
            type(nodePtr), allocatable       :: res(:)
        end function rNN_Node

        !> Performs radius nearest neighbour search on a centroid
        !! 
        !! Searches for nodes within a given radius of the centroid
        !! @param[in] centroid    the center of the search sphere  
        !! @param[in] radius      the search radius 
        !! @param[in] initialSize optional initial array size; defaults to 1000
        !!
        !! @return list of nodes within the search radius
        module function rNN_Centroid(this, centroid, radius, initialSize) result(res)
            class(tree), intent(in)          :: this
            real(kind=real64)                :: centroid(:)
            real(kind=real64)                :: radius
            integer, intent(in), optional    :: initialSize
            type(nodePtr), allocatable       :: res(:)
        end function rNN_Centroid

        !=======================================================!

    end interface
    
end module KdTreeModule