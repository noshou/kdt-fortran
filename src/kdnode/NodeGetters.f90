submodule(KdTreeFortran) NodeGetters
    implicit none 
    contains 

        module procedure getData 
            data = this%data
        end procedure getData
    
        module procedure getCoords
            allocate(coords, source=this%coords)
        end procedure getCoords

        module procedure getSplitAxis
            splitAxs = this%splitAxis
        end procedure getSplitAxis

end submodule NodeGetters