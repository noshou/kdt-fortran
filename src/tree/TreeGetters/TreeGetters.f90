submodule(KdTree) TreeGetters 
    implicit none 
    contains 
        module procedure getDim    
            k = this%dim
        end procedure getDim

        module procedure getPop
            n = this%pop
        end procedure getPop

        module procedure getInitState
            isInit = this%initialized
        end procedure getInitState

        module procedure getTreeId
            id = this%treeId
        end procedure getTreeId

        module procedure getRebuildRatio
            rebuildRatio = this%rebuildRatio
        end procedure getRebuildRatio

        module procedure getNumMods
            numMods = this%modifications
        end procedure getNumMods

end submodule TreeGetters