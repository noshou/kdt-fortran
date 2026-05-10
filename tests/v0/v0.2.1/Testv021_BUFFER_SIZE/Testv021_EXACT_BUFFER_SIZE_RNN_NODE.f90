program Testv021_EXACT_BUFFER_SIZE_RNN_NODE
    use KdTree
    use iso_fortran_env, only: real64
    implicit none

    call exactBufferSize_rNN_Node()
    contains

        !> bufferSize equals the result count exactly — no resize should occur.
        subroutine exactBufferSize_rNN_Node()
            type(Tree)                 :: t
            real(real64)               :: coords(3, 10) = reshape( &
                [0.1_real64, 0.0_real64, 0.0_real64, &
                 0.2_real64, 0.0_real64, 0.0_real64, &
                 0.3_real64, 0.0_real64, 0.0_real64, &
                 0.4_real64, 0.0_real64, 0.0_real64, &
                 0.5_real64, 0.0_real64, 0.0_real64, &
                 0.0_real64, 0.1_real64, 0.0_real64, &
                 0.0_real64, 0.2_real64, 0.0_real64, &
                 0.0_real64, 0.3_real64, 0.0_real64, &
                 0.0_real64, 0.4_real64, 0.0_real64, &
                 0.0_real64, 0.5_real64, 0.0_real64], [3, 10])
            type(NodePtr), allocatable :: res(:)
            type(Node), pointer        :: target
            integer                    :: i

            call t%build(coords)
            res    = t%rNN_Centroid([0.1_real64, 0.0_real64, 0.0_real64], 0.0_real64)
            target => res(1)%p

            do i = 1, 1000
                res = t%rNN_Node(target, 1.0_real64, bufferSize=10)
                if (size(res) .ne. 10) then
                    write(*, '(A)') '--- exactBufferSize_rNN_Node ---'
                    write(*, *) 'expected 10 nodes, got:', size(res)
                    stop 1
                end if
            end do

        end subroutine exactBufferSize_rNN_Node

end program Testv021_EXACT_BUFFER_SIZE_RNN_NODE
