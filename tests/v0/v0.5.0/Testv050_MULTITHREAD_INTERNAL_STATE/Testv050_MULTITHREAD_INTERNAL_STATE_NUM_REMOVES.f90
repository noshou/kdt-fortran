program Testv050_MULTITHREAD_INTERNAL_STATE_NUM_REMOVES
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call multithreadInternalStateNumRemoves()
    contains
        !> 4 threads each remove 1 node concurrently from a 9-node tree.
        !! After the parallel region getNumRemoves must equal 4.
        subroutine multithreadInternalStateNumRemoves()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)   :: queries(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 4])
            integer(int64) :: numRemoves
            integer        :: i

            call t%build(coords)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(t, queries)
            do i = 1, 4
                block
                    real(real64) :: q(2, 1)
                    integer      :: numRmv
                    q(:, 1) = queries(:, i)
                    numRmv  = t%rmvNodes(coordsList=q)
                end block
            end do
            !$OMP END PARALLEL DO

            numRemoves = t%getNumRemoves()
            if (numRemoves .ne. 4_int64) then
                write(*, '(A)')    '--- Testv050_MULTITHREAD_INTERNAL_STATE_NUM_REMOVES ---'
                write(*, '(A,I0)') 'expected numRemoves=4 after 4 concurrent removals, got: ', numRemoves
                stop 1
            end if
        end subroutine multithreadInternalStateNumRemoves
end program Testv050_MULTITHREAD_INTERNAL_STATE_NUM_REMOVES
