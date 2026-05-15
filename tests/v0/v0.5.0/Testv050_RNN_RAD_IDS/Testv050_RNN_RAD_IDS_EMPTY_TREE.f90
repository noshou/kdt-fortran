program Testv050_RNN_RAD_IDS_EMPTY_TREE
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rnnRadIdsEmptyTree()
    contains
        !> rNN_RadIds on pop=0 tree returns empty bucket without error.
        subroutine rnnRadIdsEmptyTree()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64], [2, 2])
            real(real64)                    :: q(2, 1)  = reshape([0.5_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)     = [100.0_real64]
            integer(int64)                  :: ids(1)   = [1_int64]
            type(KdNodeBucket), allocatable :: res(:)
            integer                         :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=q, radii=r)
            res    = t%rNN_RadIds(q, r, ids)

            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_IDS_EMPTY_TREE: expected 0, got: ', &
                    size(res(1)%nodes)
                stop 1
            end if
        end subroutine rnnRadIdsEmptyTree
end program Testv050_RNN_RAD_IDS_EMPTY_TREE
