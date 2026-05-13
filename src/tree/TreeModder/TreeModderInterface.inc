!> Overwrites rebuildRatio. Must be in (0, 1).
!! Not thread-safe; do not call concurrently with addNodes().
!! @param[in] ratio the new rebuildRatio
module subroutine setRebuildRatio(this, ratio)
    class(Tree),  intent(inout) :: this
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
    class(Tree),  intent(inout)     :: this
    real(real64), intent(in)        :: coordsList(:,:)
    class(*), intent(in), optional  :: dataList(:)
end subroutine addNodes
