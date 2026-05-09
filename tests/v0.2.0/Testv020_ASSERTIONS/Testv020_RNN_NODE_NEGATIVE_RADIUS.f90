!> Expected-fail: rNN_Node with a negative radius must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_RNN_NODE_NEGATIVE_RADIUS

    use KdTree
    use iso_fortran_env, only: real64
    implicit none

    call rnnNode_NegativeRadius()
    contains

        subroutine rnnNode_NegativeRadius()
            type(Tree)                 :: t
            real(real64)               :: coords(2, 3) = reshape( &
                [1.0_real64, 1.0_real64,  &
                 2.0_real64, 1.0_real64,  &
                 3.0_real64, 1.0_real64], [2, 3])
            type(NodePtr), allocatable :: res(:)
            type(Node), pointer        :: target

            call t%build(coords)
            res    = t%rNN_Centroid([1.0_real64, 1.0_real64], 0.01_real64)
            target => res(1)%p

            res = t%rNN_Node(target, -1.0_real64)
            write(*, '(A)') '--- rnnNode_NegativeRadius ---'
            write(*,*) 'expected error stop, but rNN_Node returned normally'
        end subroutine rnnNode_NegativeRadius

end program Testv020_RNN_NODE_NEGATIVE_RADIUS
