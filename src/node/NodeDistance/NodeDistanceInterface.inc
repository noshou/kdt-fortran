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
