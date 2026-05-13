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

!> Sets assoc to true iff a nodePool is allocated.
!! Should agree with associatedSubtree and !(this%initialized)
!! @param[inout] assoc true if this%nodePool is allocated, 
!!                     false otherwise
module subroutine associatedNodePool(this, assoc)
    class(Tree),      intent(in)    :: this 
    logical,          intent(inout) :: assoc
end subroutine associatedNodePool

!> Sets assoc to true iff a root node is allocated.
!! Should agree with associatedNodePool and !(this%initialized)
!! @param[inout] assoc true if this%root is allocated, false otherwise
module subroutine associatedRoot(this, assoc)
    class(Tree),      intent(in)    :: this 
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
    class(Tree), intent(inout) :: this
end subroutine destroy

!> Frees tree automatically   when it goes out of scope
module subroutine finalizer(this)
    type(Tree), intent(inout) :: this
end subroutine finalizer
