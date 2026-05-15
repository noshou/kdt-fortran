program Testv040_FIND_NODES_IDS_AFTER_ADD_NODES
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call findNodesIdsAfterAdd()
    contains
        !> rNN_Ids must find a node inserted via addNodes when given its id.
        !! A 1-node tree assigns nodeId=1; addNodes adds nodeId=2.
        !! Querying at the added coord with id=2 must return 1 node.
        !! Querying at the same coord with id=1 (wrong node) must return 0 nodes.
        subroutine findNodesIdsAfterAdd()
            type(KdTree) :: t
            type(KdNodeBucket), allocatable :: res(:)
            real(real64) :: init_coord(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64) :: new_coord(2, 1)  = reshape([5.0_real64, 5.0_real64], [2, 1])
            integer(int64) :: id_new(1) = [2_int64]
            integer(int64) :: id_wrong(1) = [1_int64]

            call t%build(init_coord)
            call t%addNodes(new_coord)

            ! correct id for the added node (currNodeId: 0->1 at build, 1->2 at addNodes)
            res = t%rNN_Ids(new_coord, id_new, epsilon=0.5_real64)
            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_IDS_AFTER_ADD_NODES ---'
                write(*, '(A,I0)') 'expected 1 node for correct id=2, got: ', size(res(1)%nodes)
                stop 1
            end if

            ! wrong id: coords match but id=1 belongs to the original node at (0,0), not (5,5)
            res = t%rNN_Ids(new_coord, id_wrong, epsilon=0.5_real64)
            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_IDS_AFTER_ADD_NODES ---'
                write(*, '(A,I0)') 'expected 0 nodes for wrong id=1 at added coord, got: ', &
                    size(res(1)%nodes)
                stop 1
            end if
        end subroutine findNodesIdsAfterAdd
end program Testv040_FIND_NODES_IDS_AFTER_ADD_NODES
