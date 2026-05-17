program Testv050_LIN_SCAN_UNINITIALIZED
    use KdTreeFortran
    implicit none
    call linScanUninitialized()
    contains
        !> linScan on an uninitialized tree must error stop.
        subroutine linScanUninitialized()
            type(KdTree)                 :: t
            type(NodeId)                 :: ids(1)
            type(KdNodePtr), allocatable :: res(:)
            res = t%linScan(ids)
        end subroutine linScanUninitialized
end program Testv050_LIN_SCAN_UNINITIALIZED
