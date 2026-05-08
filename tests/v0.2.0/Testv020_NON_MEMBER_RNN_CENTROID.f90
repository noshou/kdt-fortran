! test expected to fail (adding non-member node)

program Testv020_NON_MEMBER_RNN_CENTROID

    use KdTree
    use iso_fortran_env, only: real64
    implicit none

    call nonMemberRnnCentroid()
    contains 
        subroutine nonMemberRnnCentroid()
            type(Tree)                 :: t1, t2
            real(real64)               :: coords(2, 2) = reshape([1.0_real64, 2.0_real64, -2.0_real64, -32.3_real64], [2, 2])
            real(real64)               :: centroid(2)  = [0.0_real64, 0.0_real64], r
            type(NodePtr), allocatable :: res1(:), res2(:)
            type(Node), pointer        :: node1, node2 

            call t1%build(coords)
            call t2%build(coords)
            r = sqrt(sum([-2.0_real64, -32.3_real64] - [-2.0_real64, -32.3_real64])**2)
            res1 = t1%rNN_Centroid(centroid, r)
            res2 = t2%rNN_Centroid(centroid, r)

        end subroutine nonMemberRnnCentroid

end program Testv020_NON_MEMBER_RNN_CENTROID