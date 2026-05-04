type :: tree
    private
    integer                         :: dim = 0, pop = 0
    type(node), pointer :: nodePool(:) => null()
    type(node), pointer             :: root => null()
        contains
            procedure :: getDim
            procedure :: getPop
            procedure :: build
            procedure :: printTree
            procedure :: destroy
            final     :: finalizer
end type tree