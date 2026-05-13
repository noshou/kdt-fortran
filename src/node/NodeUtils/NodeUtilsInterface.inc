!> Recursively prints this Node and its subtree in pre-order.
!! @param[in] depth the depth of this Node
!! @param[in] unit  optional output unit (defaults to stdout)
module recursive subroutine printNode(this, depth, nodePool, unit)
    class(Node),    intent(in)           :: this
    integer(int64), intent(in)           :: depth
    type(Node),     intent(in)           :: nodePool(:)
    integer,        intent(in), optional :: unit
end subroutine printNode

!> Prints the current node, but not its subtree
!!
!! Use: printNode(depth) for the full subtree
!! @param[in] unit  optional output unit (defaults to stdout)
module subroutine printNodeSingle(this, unit)
    class(Node), intent(in)       :: this
    integer, intent(in), optional :: unit
end subroutine printNodeSingle

!> Frees the owned node copy and nulls p.
module subroutine destroyNodePtr(this)
    class(NodePtr), intent(inout) :: this
end subroutine destroyNodePtr

!> Frees the owned node copy when NodePtr goes out of scope.
module subroutine finalizerNodePtr(this)
    type(NodePtr), intent(inout) :: this
end subroutine finalizerNodePtr
