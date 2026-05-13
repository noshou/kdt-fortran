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
    class(tree), intent(in)                :: this
    type(NodePtr), intent(in)              :: target
    real(kind=real64), intent(in)          :: radius
    integer, intent(in), optional          :: bufferSize
    character(len=*), intent(in), optional :: metric
    logical, intent(in), optional          :: excludeTarget
    type(nodePtr), allocatable             :: res(:)
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
    class(tree), intent(in)                :: this
    real(kind=real64), intent(in)          :: radius, centroid(:)
    integer, intent(in), optional          :: bufferSize
    character(len=*), intent(in), optional :: metric 
    type(nodePtr), allocatable             :: res(:)

end function rNN_Centroid