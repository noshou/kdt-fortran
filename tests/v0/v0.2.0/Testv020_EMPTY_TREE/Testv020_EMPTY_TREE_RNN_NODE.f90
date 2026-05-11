!> Expected-fail: rNN_Node on an empty tree must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_EMPTY_TREE_RNN_NODE
    use KdTree
    use iso_fortran_env, only: real64
    implicit none

    call emptyTree()
    contains

        !> rNN_Node on an empty tree must error stop.
        subroutine emptyTree()
            type(Tree)                 :: t, tHelper
            real(real64)               :: coords(2, 0)
            real(real64)               :: helperCoords(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)               :: r = 0.9
            type(NodePtr), allocatable :: res(:)
            type(Node), pointer        :: node1

            call t%build(coords)
            call tHelper%build(helperCoords)

            res   = tHelper%rNN_Centroid([0.0_real64, 0.0_real64], 1000.0_real64)
            node1 => res(1)%p

            res = t%rNN_Node(node1, r) ! expected to fail here (empty tree check fires first)
            write(*, '(A)') '--- Testv020_EMPTY_TREE_RNN_NODE ---'
            write(*, '(A)') 'expected program to fail, but ran successfully!'
        end subroutine emptyTree

end program Testv020_EMPTY_TREE_RNN_NODE