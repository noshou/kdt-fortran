!> rNN_Centroid and rNN_Node tests with known result counts.
program Testv020_SINGLE_NODE_QUERY

    use KdTree
    use iso_fortran_env, only: real64
    implicit none

    call singleNodeQuery_rNN_Centroid()
    call singleNodeQuery_rNN_Node()

    contains

        !> One-point tree, radius covers it: rNN_Centroid returns 1 node.
        subroutine singleNodeQuery_rNN_Centroid()
            type(Tree)                 :: t
            real(real64)               :: coords(2, 1) = reshape([1.0_real64, 2.0_real64], [2, 1])
            real(real64)               :: centroid(2)  = [0.0_real64, 0.0_real64], r
            type(NodePtr), allocatable :: res(:)

            call t%build(coords)

            r = ceiling(sqrt(5.0_real64))
            res = t%rNN_Centroid(centroid, r)

            if (size(res) .ne. 1) then
                write(*, '(A)') '--- singleNodeQuery_rNN_Centroid ---'
                write(*,*) 'expected one node to match, got: ', size(res)
                stop 1
            end if
        end subroutine singleNodeQuery_rNN_Centroid

        !> Two-point tree, radius = inter-point distance: rNN_Node returns
        !! both (target itself and the other) since rNN uses <=.
        subroutine singleNodeQuery_rNN_Node()
            type(Tree)                 :: t
            real(real64)               :: coords(2, 2) = reshape([1.0_real64, 2.0_real64, -2.0_real64, -32.3_real64], [2, 2])
            real(real64)               :: centroid(2)  = [0.0_real64, 0.0_real64], r
            type(NodePtr), allocatable :: res(:)
            type(Node), pointer        :: node

            call t%build(coords)

            ! expected to get exactly one node back from this
            r = ceiling(sqrt(5.0_real64))
            res = t%rNN_Centroid(centroid, r)
            node => res(1)%p

            ! expected to get exactly two nodes
            r = sqrt(sum(([-2.0_real64, -32.3_real64] - res(1)%p%coords)**2))
            res = t%rNN_Node(node, r)
            if (size(res) /= 2) then
                write(*, '(A)') '--- singleNodeQuery_rNN_Node ---'
                write(*,*) 'expected 2 nodes, got:', size(res)
                stop 1
            end if

        end subroutine singleNodeQuery_rNN_Node

end program Testv020_SINGLE_NODE_QUERY