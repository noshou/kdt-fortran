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
            type(Tree)                 :: t
            type(Node), target         :: dummyNode
            real(real64)               :: coords(2, 0),  centroid(2) = [0.0_real64, 0.0_real64], r=0.9
            type(NodePtr), allocatable :: res(:)
            call t%build(coords)
            allocate(dummyNode%coords(size(centroid)), source=centroid)
            res = t%rNN_Node(dummyNode, r) ! expected to fail here
            write(*, '(A)') '--- emptyTree (rNN_Node) ---'
            write(*, '(A)') 'expected program to fail, but ran successfully!'
        end subroutine emptyTree

end program Testv020_EMPTY_TREE_RNN_NODE