program Testv050_LIN_SCAN_NO_MATCH
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call linScanNoMatch()
    contains
        !> linScan with id=0 returns empty — no node ever has nodeId=0
        !! (ids start at 1 after build).
        subroutine linScanNoMatch()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            integer(int64)               :: ids(1) = [0_int64]
            type(KdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%linScan(ids)

            if (size(res) .ne. 0) then
                write(*, '(A)')    '--- Testv050_LIN_SCAN_NO_MATCH ---'
                write(*, '(A,I0)') 'expected size=0 for non-existent id, got: ', size(res)
                stop 1
            end if
        end subroutine linScanNoMatch
end program Testv050_LIN_SCAN_NO_MATCH
