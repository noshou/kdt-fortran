program Testv050_RNN_RAD_IDS_SINGLE_QUERY
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rnnRadIdsSingleQuery()
    contains
        !> Radius captures 3 nodes; ids set contains only the id of one —
        !! result bucket has exactly 1 node.
        subroutine rnnRadIdsSingleQuery()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)                    :: q(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)    = [1.1_real64]
            type(KdNodePtr), allocatable    :: centre(:)
            integer(int64)                  :: ids(1)
            type(KdNodeBucket), allocatable :: res(:)

            call t%build(coords)
            centre = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            if (size(centre) .ne. 1) then
                write(*, '(A)') '--- Testv050_RNN_RAD_IDS_SINGLE_QUERY: setup failed'; stop 1
            end if
            ids(1) = centre(1)%p%getId()

            res = t%rNN_RadIds(q, r, ids)

            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_IDS_SINGLE_QUERY: expected 1, got: ', &
                    size(res(1)%nodes)
                stop 1
            end if
            if (res(1)%nodes(1)%p%getId() .ne. ids(1)) then
                write(*, '(A)') '--- Testv050_RNN_RAD_IDS_SINGLE_QUERY: wrong node returned'
                stop 1
            end if
        end subroutine rnnRadIdsSingleQuery
end program Testv050_RNN_RAD_IDS_SINGLE_QUERY
