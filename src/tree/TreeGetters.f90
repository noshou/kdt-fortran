submodule(KdTree) TreeGetters 
    implicit none 
    contains 
        module procedure getDim    
            k = this%dim
        end procedure getDim

        module procedure getPop
            n = this%pop
        end procedure getPop
end submodule TreeGetters