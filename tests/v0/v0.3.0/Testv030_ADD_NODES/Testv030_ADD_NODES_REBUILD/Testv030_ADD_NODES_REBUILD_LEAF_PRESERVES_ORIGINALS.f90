program Testv030_ADD_NODES_REBUILD_LEAF_PRESERVES_ORIGINALS
    use KdTree
    use iso_fortran_env, only: real64, int64
    implicit none
    call leafPreservesOriginals()
    contains
        !> Build 8 nodes; add 1 node as a leaf insert (0+1 <= ceil(0.25*8)=2).
        !! rNN_Centroid with a radius wide enough to enclose all 9 must return 9.
        !! This differs from IS_MEMBER (which only checks isMember) and LEAF_RNN_*
        !! (which only find the newly added leaf) — here we verify original nodes
        !! remain reachable via search after a leaf-structural change.
        subroutine leafPreservesOriginals()
            type(Tree)                 :: t
            real(real64)               :: init_coords(2, 8) = reshape( &
                [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, &
                4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [2, 8])
            real(real64)               :: new_coords(2, 1) = reshape([3.5_real64, 0.0_real64], [2, 1])
            type(NodePtr), allocatable :: res(:)
            integer(int64)             :: numMods

            call t%build(init_coords)
            call t%addNodes(new_coords)

            numMods = t%getNumMods()
            if (numMods .ne. 1_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_REBUILD_LEAF_PRESERVES_ORIGINALS ---'
                write(*, '(A,I0)') 'expected leaf insert (numMods=1), got: ', numMods
                stop 1
            end if

            ! euclidean: centroid (3.5, 0), r=4 encloses x=0..7 at y=0 (max dist=3.5) and (3.5,0)
            res = t%rNN_Centroid([3.5_real64, 0.0_real64], 4.0_real64, metric='euclidean')
            if (size(res) .ne. 9) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_REBUILD_LEAF_PRESERVES_ORIGINALS ---'
                write(*, '(A,I0)') 'expected all 9 nodes (8 originals + leaf), got: ', size(res)
                stop 1
            end if
        end subroutine leafPreservesOriginals
end program Testv030_ADD_NODES_REBUILD_LEAF_PRESERVES_ORIGINALS
