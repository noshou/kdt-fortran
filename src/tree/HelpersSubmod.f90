submodule(KdTreeModule) HelpersSubmod
    implicit none 
    contains
        
        module procedure getDim    
            k = this%dim
        end procedure getDim

        module procedure getPop
            integer                 :: n
            n = this%pop
        end procedure getPop

        module procedure printTree
            if (associated(this%root)) then 
                call this%root%printNode(0)
            else 
                write(*, '(A)') '**empty tree**'
            end if
        end procedure printTree

        module procedure isMember
            if (.not. associated(target)) then 
                res = .false.
            else 
                res = this%treeId .eq. target%treeId
            end if
        end procedure isMember

        module procedure destroy
            if (associated(this%nodePool)) deallocate(this%nodePool)
            this%root   => null()
            this%dim    = 0
            this%pop    = 0
            this%treeId = 0
        end procedure destroy

        module procedure finalizer
            call destroy(this)
        end procedure finalizer

end submodule HelpersSubmod





