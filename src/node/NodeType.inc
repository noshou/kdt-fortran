type :: Node
    private
    logical                         :: hasData = .false.    !> flagged true if node contains data
    class(*), allocatable           :: data
    real(kind=real64), allocatable  :: coords(:)
    integer(int64)                  :: splitAxis            !> tracks which index in coords is the splitting plane
    integer(int64)                  :: numRemovesSnapshot = 0_int64, nodeId = 0_int64
    integer(int64)                  :: lch = 0_int64, rch = 0_int64  !> indices into nodePool; 0 = no child
    integer(int64)                  :: treeId
    contains    
        procedure                   :: euclideanDist
        procedure                   :: euclideanDistPoint
        procedure                   :: manhattanDist
        procedure                   :: manhattanDistPoint
        procedure                   :: chebyshevDist 
        procedure                   :: chebyshevDistPoint
        procedure                   :: printNode
        procedure                   :: printNodeSingle
        procedure                   :: getData
        procedure                   :: getCoords
        procedure                   :: getSplitAxis
end type Node
