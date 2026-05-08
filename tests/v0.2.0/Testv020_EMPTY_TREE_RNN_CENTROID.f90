! program is expected to fail (tree is empty except root)
program Testv020_EMPTY_TREE_RNN_CENTROID
    use KdTree
    use iso_fortran_env, only: real64
    implicit none

    call emptyTree()
    contains 
        
        subroutine emptyTree()
            type(Tree)                 :: t
            real(real64)               :: coords(2, 0),  centroid(2) = [0.0_real64, 0.0_real64], r=0.9
            type(NodePtr), allocatable :: res(:)
            write(*, '(A)') '--- emptyTree (rNN_Centroid) ---'
            call t%build(coords)
            res = t%rNN_Centroid(centroid, r)
        end subroutine emptyTree

end program Testv020_EMPTY_TREE_RNN_CENTROID