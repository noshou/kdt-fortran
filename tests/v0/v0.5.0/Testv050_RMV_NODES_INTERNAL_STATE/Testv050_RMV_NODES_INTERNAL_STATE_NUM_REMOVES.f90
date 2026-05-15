program Testv050_RMV_NODES_INTERNAL_STATE_NUM_REMOVES
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesInternalStateNumRemoves()
    contains
        !> getNumRemoves is 0 after build, and increments by numRmv after rmvNodes.
        subroutine rmvNodesInternalStateNumRemoves()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)   :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            integer        :: numRmv
            integer(int64) :: numRemoves

            call t%build(coords)
            numRemoves = t%getNumRemoves()
            if (numRemoves .ne. 0_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_INTERNAL_STATE_NUM_REMOVES ---'
                write(*, '(A,I0)') 'expected getNumRemoves=0 after build, got: ', numRemoves
                stop 1
            end if

            numRmv     = t%rmvNodes(coordsList=query)
            numRemoves = t%getNumRemoves()
            if (numRemoves .ne. 1_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_INTERNAL_STATE_NUM_REMOVES ---'
                write(*, '(A,I0)') 'expected getNumRemoves=1 after removing 1 node, got: ', numRemoves
                stop 1
            end if
        end subroutine rmvNodesInternalStateNumRemoves
end program Testv050_RMV_NODES_INTERNAL_STATE_NUM_REMOVES
