program Testv021_ZERO_RADIUS_POPULATED_TREE_RNN_NODE_DEFAULT
    use KdTree
    use iso_fortran_env, only: real64
    implicit none
    call zeroRadiusPopulatedTree_rNN_Node_Default()
    contains 

        !> Query should return no nodes; tree is populated
        subroutine zeroRadiusPopulatedTree_rNN_Node_Default()
            type(Tree)                 :: t
            real(real64)               :: coords(3, 6) = reshape( &
                [5.0_real64, 1.0_real64,  0.92_real64,            &
                4.0_real64, 2.0_real64,  0.42_real64,             &
                3.0_real64, 3.0_real64,  0.00003_real64,          &
                0.0_real64, 0.0_real64,  0.00000031_real64,       &
                1.0_real64, 5.0_real64, -93131913.0_real64,       &
                0.0_real64, 0.0_real64,  0.0_real64], [3, 6])
            type(NodePtr), allocatable :: res(:)
            type(Node), pointer        :: target
            real(real64)               :: r
            
            call t%build(coords)
            r = sqrt(sum(([0.0_real64, 0.0_real64,  0.00000031_real64] - [0.0_real64, 0.0_real64,  0.0_real64])**2))
            res = t%rNN_Centroid([0.0_real64, 0.0_real64, 0.0_real64], r)
            target => res(1)%p
            
            res = t%rNN_Node(target, 0.0_real64, excludeTarget=.true.)
            if (size(res) .ne. 0) then
                write(*, '(A)') '--- zeroRadiusPopulatedTree_rNN_Node_Default ---'
                write(*,*) 'expected 0 nodes, got:', size(res)
                stop 1
            end if

        end subroutine zeroRadiusPopulatedTree_rNN_Node_Default

end program Testv021_ZERO_RADIUS_POPULATED_TREE_RNN_NODE_DEFAULT