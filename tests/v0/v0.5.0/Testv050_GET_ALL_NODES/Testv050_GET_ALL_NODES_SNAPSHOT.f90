program Testv050_GET_ALL_NODES_SNAPSHOT
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call getAllNodesSnapshot()
    contains
        !> getAllNodes stamps numRemovesSnapshot so isMember takes the fast path.
        !! Observable effect: after a removal, every node from getAllNodes must
        !! be a member without needing a pool scan. Verified by calling isMember
        !! immediately after getAllNodes -> all must return true.
        !!
        !! Also verifies that getAllNodes after addNodes reflects the updated pop.
        subroutine getAllNodesSnapshot()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            real(real64)                 :: extra(2, 2)  = reshape( &
                [9.0_real64, 0.0_real64, 0.0_real64, 9.0_real64], [2, 2])
            real(real64)                 :: rmvQuery(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(KdNodePtr), allocatable :: nodes(:)
            integer                      :: numRmv, i

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=rmvQuery)
            call t%addNodes(extra)

            ! pop is now 4 (3 - 1 + 2)
            nodes = t%getAllNodes()

            if (size(nodes) .ne. 4) then
                write(*, '(A)')    '--- Testv050_GET_ALL_NODES_SNAPSHOT ---'
                write(*, '(A,I0)') 'expected 4 nodes after rmv+add cycle, got: ', size(nodes)
                stop 1
            end if

            do i = 1, size(nodes)
                if (.not. t%isMember(nodes(i)%p)) then
                    write(*, '(A)')    '--- Testv050_GET_ALL_NODES_SNAPSHOT ---'
                    write(*, '(A,I0)') 'node ', i, ' not a member after rmv+add cycle'
                    stop 1
                end if
            end do
        end subroutine getAllNodesSnapshot
end program Testv050_GET_ALL_NODES_SNAPSHOT
