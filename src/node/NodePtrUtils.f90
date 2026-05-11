submodule(KdTree) NodePtrUtils
    implicit none
    contains

        module procedure destroyNodePtr
            if (associated(this%p)) deallocate(this%p)
            this%p    => null()
            this%src_ => null()
        end procedure destroyNodePtr

        module procedure finalizerNodePtr
            call destroyNodePtr(this)
        end procedure finalizerNodePtr

end submodule NodePtrUtils
