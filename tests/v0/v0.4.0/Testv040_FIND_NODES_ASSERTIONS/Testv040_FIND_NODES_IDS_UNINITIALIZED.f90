program Testv040_FIND_NODES_IDS_UNINITIALIZED
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call findNodesIdsUninitialized()
    contains
        !> rNN_Ids on a tree that has never been built must stop 1.
        subroutine findNodesIdsUninitialized()
            type(KdTree) :: t
            type(KdNodeBucket), allocatable :: res(:)
            real(real64)   :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            integer(int64) :: ids(1) = [1_int64]

            res = t%rNN_Ids(query, ids)
            write(*, '(A)') '--- Testv040_FIND_NODES_IDS_UNINITIALIZED ---'
            write(*, '(A)') 'expected stop 1, but rNN_Ids returned normally'
        end subroutine findNodesIdsUninitialized
end program Testv040_FIND_NODES_IDS_UNINITIALIZED
