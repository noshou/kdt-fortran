program Testv021_TREE_GETTERS_GET_DIM_EMPTY 
    use KdTree
    use iso_fortran_env, only: real64, int64
    call treeGettersGetDimEmpty()
    contains 

        !> should be zero for an empty tree
        subroutine treeGettersGetDimEmpty()
            type(Tree)      :: t
            integer(int64)  :: d
            if ()
        end subroutine treeGettersGetDimEmpty 

end program Testv021_TREE_GETTERS_GET_DIM_EMPTY
