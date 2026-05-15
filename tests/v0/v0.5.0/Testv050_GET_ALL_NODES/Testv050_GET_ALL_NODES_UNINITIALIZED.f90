program Testv050_GET_ALL_NODES_UNINITIALIZED
    use KdTreeFortran
    implicit none
    call getAllNodesUninitialized()
    contains
        !> getAllNodes on an uninitialized tree must error stop.
        subroutine getAllNodesUninitialized()
            type(KdTree)                 :: t
            type(KdNodePtr), allocatable :: nodes(:)
            nodes = t%getAllNodes()
            write(*, '(A)') '--- Testv050_GET_ALL_NODES_UNINITIALIZED ---'
            write(*, '(A)') 'expected error stop before build, but returned normally'
        end subroutine getAllNodesUninitialized
end program Testv050_GET_ALL_NODES_UNINITIALIZED
