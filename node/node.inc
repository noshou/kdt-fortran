type :: node
    class(*), allocatable           :: data
    real(kind=real64), allocatable  :: coords(:)
    integer                         :: splitAxis ! tracks which index in coords is the splitting plane
    type(node), pointer             :: leftChild => null(), rightChild => null()
    integer                         :: treeId
    contains    
        procedure                   :: eucDist
        procedure                   :: printNode
end type node

!> pointer to a node
type :: nodePtr
    type(node), pointer :: p => null()
end type nodePtr