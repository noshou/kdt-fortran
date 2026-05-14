type :: Tree
private
integer(int64)       :: dim = 0_int64, pop = 0_int64, TreeId = 0_int64
integer(int64)       :: currNodeId = 0_int64, numRemoves = 0_int64
logical              :: initialized = .false.       !> true iff tree%build() is called successfully
type(node), pointer  :: nodePool(:) => null()       !> pool of allocated nodes
integer(int64)       :: rootIdx = 0_int64           !> index into nodePool for the root; 0 = empty
integer(int64)       :: modifications = 0_int64     !> total number of insertions/deletions on tree
real(real64)         :: rebuildRatio = 0.25_real64  !> if modifications > rebuildRatio * pop, trigger rebuild 

    contains
        procedure    :: getDim
        procedure    :: getPop
        procedure    :: build
        procedure    :: printTree
        procedure    :: rNN_Node
        procedure    :: rNN_Centroid
        procedure    :: isMember
        procedure    :: getInitState
        procedure    :: getTreeId
        procedure    :: setRebuildRatio
        procedure    :: getRebuildRatio
        procedure    :: associatedNodePool
        procedure    :: associatedRoot
        procedure    :: addNodes
        procedure    :: getNumMods
        procedure    :: assert
        procedure    :: destroy
        final        :: finalizer
end type Tree
