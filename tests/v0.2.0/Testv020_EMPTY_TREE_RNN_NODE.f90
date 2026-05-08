! program is expected to fail (tree is empty except root)
program Testv020_EMPTY_TREE_RNN_NODE
    use KdTree
    use iso_fortran_env, only: real64
    implicit none

    call emptyTree()
    contains 
        
        subroutine emptyTree()
            type(Tree)                 :: t
            type(Node), target         :: dummyNode
            real(real64)               :: coords(2, 0),  centroid(2) = [0.0_real64, 0.0_real64], r=0.9
            type(NodePtr), allocatable :: res(:)
            write(*, '(A)') '--- emptyTree (rNN_Node) ---'
            call t%build(coords)
            allocate(dummyNode%coords(size(centroid)), source=centroid)
            res = t%rNN_Node(dummyNode, r)
        end subroutine emptyTree

end program Testv020_EMPTY_TREE_RNN_NODE