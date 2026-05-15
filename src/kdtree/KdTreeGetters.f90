submodule(KdTreeFortran) KdTreeGetters 
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

        module procedure getNumRemoves
            numRemoves = this%numRemoves
        end procedure getNumRemoves

        module procedure getAllNodes
            type(KdNode), pointer  :: copy       
            integer(int64)         :: i
            logical                :: init

            call this%getInitState(init)
            if (.not. init) then 
                error stop "getAllNodes: tree is not initialized (call build first?)"
            end if
            
            allocate(nodes(this%pop))
            do i = 1, this%pop
                allocate(copy, source=this%nodePool(i))
                copy%numRemovesSnapshot = this%numRemoves
                nodes(i)%p => copy
            end do
        end procedure getAllNodes

        module procedure getAllCoords
            integer(int64)         :: i
            logical                :: init
            
            call this%getInitState(init)
            if (.not. init) then 
                error stop "getAllCoords: tree is not initialized (call build first?)"
            end if

            allocate(coords(this%dim, this%pop))
            do i = 1, this%pop
                coords(:, i) = this%nodePool(i)%coords
            end do
        end procedure getAllCoords

end submodule KdTreeGetters