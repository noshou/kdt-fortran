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
            state = this%initialized
        end procedure getInitState

end submodule TreeGetters