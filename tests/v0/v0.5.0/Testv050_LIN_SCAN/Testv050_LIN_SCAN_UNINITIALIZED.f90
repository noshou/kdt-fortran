program Testv050_LIN_SCAN_UNINITIALIZED
    use KdTreeFortran
    use iso_fortran_env, only: int64
    implicit none
    call linScanUninitialized()
    contains
        !> linScan on an uninitialized tree must error stop.
        subroutine linScanUninitialized()
            type(KdTree)                 :: t
            integer(int64)               :: ids(1) = [1_int64]
            type(KdNodePtr), allocatable :: res(:)
            res = t%linScan(ids)
        end subroutine linScanUninitialized
end program Testv050_LIN_SCAN_UNINITIALIZED
