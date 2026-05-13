!> Returns the data stored in this node.
!! The dynamic type of the result matches the type passed as data to build().
!! Use a select type construct to compare or assign the value.
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
module function getSplitAxis(this) result(splitsaxs)
    class(Node), intent(in) :: this 
    integer(int64)          :: splitsaxs
end function getSplitAxis
