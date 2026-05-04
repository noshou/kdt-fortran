module KdTree
    use iso_fortran_env, only: real64
    implicit none

    include "node/node.inc"
    include "tree/tree.inc"
    contains
        include "node/methods.inc"
        include "tree/methods.inc"
end module KdTree
