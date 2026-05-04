!> Calculates the euclidean distance between two nodes
!! @param[in] this The reference node
!! @param[in] that The node to calculate distance from
!!
!! @return The euclidean distance between two nodes
function eucDist(this, that) result(dist)
    class(node), intent(in) :: this
    class(node), intent(in) :: that
    real(kind=real64) :: dist
    if (.not. allocated(this%coords) .or. .not. allocated(that%coords)) then
        error stop "coords not allocated"
    else if ((size(this%coords).eq. 0) .or. (size(that%coords) .eq. 0)) then 
        error stop "axis size must be g.t zero"
    else if (size(this%coords) .ne. size(that%coords)) then  
        error stop "axis size mismatch"
    else 
        dist = sqrt(sum((that%coords - this%coords)**2))
    end if
end function eucDist

!> Recursively prints this node and its subtree in pre-order.
!! @param[in] this the node and its subtree to print
!! @param[in] depth the depth of this node
recursive subroutine printNode(this, depth)
    class(node), intent(in) :: this
    integer,     intent(in) :: depth
    integer :: i

    ! indentation: 2 spaces per depth level
    do i = 1, depth
        write(*, '(A)', advance='no') '  '
    end do

    ! print this node's coords and split axis
    write(*, '(A,I0,A)', advance='no') '[axis=', this%splitAxis, '] ('
    do i = 1, size(this%coords)
        if (i > 1) write(*, '(A)', advance='no') ', '
        write(*, '(G0.4)', advance='no') this%coords(i)
    end do
    write(*, '(A)') ')'

    if (associated(this%leftChild))  call this%leftChild%printNode(depth + 1)
    if (associated(this%rightChild)) call this%rightChild%printNode(depth + 1)
end subroutine printNode