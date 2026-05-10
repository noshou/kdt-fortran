!> Expected-fail: rNN_Centroid with an unrecognised metric must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_SINGLE_NODE_QUERY_RNN_CENTROID_INVALID_METRIC

    use KdTree
    use iso_fortran_env, only: real64
    implicit none

    call singleNodeQuery_rNN_Centroid_InvalidMetric()
    contains

        subroutine singleNodeQuery_rNN_Centroid_InvalidMetric()
            type(Tree)                 :: t
            real(real64)               :: coords(2, 2) = reshape( &
                [1.0_real64, 0.0_real64,  &
                0.0_real64, 1.0_real64], [2, 2])
            real(real64)               :: centroid(2) = [0.0_real64, 0.0_real64]
            type(NodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid(centroid, 1.0_real64, metric='invalid')
            write(*, '(A)') '--- singleNodeQuery_rNN_Centroid_InvalidMetric ---'
            write(*,*) 'expected program to fail!'

        end subroutine singleNodeQuery_rNN_Centroid_InvalidMetric

end program Testv020_SINGLE_NODE_QUERY_RNN_CENTROID_INVALID_METRIC
