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
    class(Tree), intent(inout)              :: this
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
    type(Tree), intent(inout)     :: this
    integer(int64), intent(out)   :: rootIdx
    integer(int64), intent(inout) :: indices(:)
    integer(int64), intent(in)    :: lowerIdx, upperIdx, depth
end subroutine buildSubtree
