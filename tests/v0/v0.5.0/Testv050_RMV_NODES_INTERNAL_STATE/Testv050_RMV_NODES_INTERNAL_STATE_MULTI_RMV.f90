program Testv050_RMV_NODES_INTERNAL_STATE_MULTI_RMV
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesMultiRmv()
    contains
        !> Multiple sequential rmvNodes calls accumulate into getNumRemoves.
        !! Build 9 nodes. Remove 3 (numRemoves=3). Remove 2 more (numRemoves=5).
        subroutine rmvNodesMultiRmv()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)   :: q1(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64], [2, 3])
            real(real64)   :: q2(2, 2) = reshape( &
                [0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 2])
            integer        :: numRmv
            integer(int64) :: numRemoves

            call t%build(coords)

            numRmv     = t%rmvNodes(coordsList=q1)
            numRemoves = t%getNumRemoves()
            if (numRmv .ne. 3 .or. numRemoves .ne. 3_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_INTERNAL_STATE_MULTI_RMV ---'
                write(*, '(A,I0)') 'after first rmv: expected numRmv=3, got: ', numRmv
                write(*, '(A,I0)') 'after first rmv: expected numRemoves=3, got: ', numRemoves
                stop 1
            end if

            numRmv     = t%rmvNodes(coordsList=q2)
            numRemoves = t%getNumRemoves()
            if (numRmv .ne. 2 .or. numRemoves .ne. 5_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_INTERNAL_STATE_MULTI_RMV ---'
                write(*, '(A,I0)') 'after second rmv: expected numRmv=2, got: ', numRmv
                write(*, '(A,I0)') 'after second rmv: expected numRemoves=5, got: ', numRemoves
                stop 1
            end if
        end subroutine rmvNodesMultiRmv
end program Testv050_RMV_NODES_INTERNAL_STATE_MULTI_RMV
