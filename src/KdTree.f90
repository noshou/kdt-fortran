module KdTree 
    
    use iso_fortran_env, only: int64, real64, output_unit
    implicit none
    private
    public                  :: Tree, Node, NodePtr
    integer(int64), save    :: nextTreeId = 0_int64

    include "node/NodeType.inc"
    include "node/NodePtrType.inc"
    include "tree/TreeType.inc"

    interface
        
        include "node/NodeGetters/NodeGettersInterface.inc"
        include "node/NodeDistance/NodeDistanceInterface.inc"
        include "node/NodeUtils/NodeUtilsInterface.inc"

        include "tree/TreeGetters/TreeGettersInterface.inc"
        include "tree/TreeUtils/TreeUtilsInterface.inc"
        include "tree/BuildSubmod/BuildSubmodInterface.inc"
        include "tree/TreeModder/TreeModderInterface.inc"
        include "tree/SearchSubmod/SearchSubmodInterface.inc"
    
    end interface

end module KdTree