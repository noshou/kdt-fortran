!> Returns the dimension (number of splitting axis) of the tree
module function getDim(this) result(k)
    class(Tree), intent(in) :: this
    integer(int64)          :: k
end function getDim

!> Returns the number of nodes in the tree
module function getPop(this) result(n)
    class(Tree), intent(in) :: this
    integer(int64)          :: n
end function getPop

!> Sets isInit to true iff the tree is initialized
!! param[inout] isInit the state of the tree
module subroutine getInitState(this, isInit)
    class(Tree), intent(in)    :: this
    logical,     intent(inout) :: isInit
end subroutine getInitState

!> Returns the unique integer ID assigned to this tree at build time
module function getTreeId(this) result(id)
    class(Tree), intent(in) :: this
    integer(int64)          :: id
end function getTreeId


!> Returns the rebuildRatio.
!! If this%modifications > this%rebuildRatio * this%pop,
!! trigger a rebuild of the tree.
module function getRebuildRatio(this) result(rebuildRatio)
    class(Tree), intent(in) :: this
    real(real64)            :: rebuildRatio
end function getRebuildRatio

!> Returns the number of modifications done on tree.
!! If this%modifications > this%rebuildRatio * this%pop,
!! trigger a rebuild of the tree.
module function getNumMods(this) result(numMods)
    class(Tree), intent(in) :: this
    integer(int64)          :: numMods
end function getNumMods
