module KdTree
    use iso_fortran_env, only: real64
    implicit none

    include "node/node.inc"
    include "tree/tree.inc"

    integer, save :: nextTreeId = 0 !> saves tree ids for node membership checks

    contains
        include "node/methods.inc"
        include "tree/methods.inc"
end module KdTree
