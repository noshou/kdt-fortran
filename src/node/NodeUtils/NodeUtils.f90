submodule(KdTree) NodeUtils
    implicit none
    contains

        module procedure printNode
            integer :: i, u

            u = output_unit
            if (present(unit)) u = unit

            ! indentation: 2 spaces per depth level
            do i = 1, depth
                write(u, '(A)', advance='no') '  '
            end do

            ! print this Node's coords and split axis
            write(u, '(A,I0,A)', advance='no') '[axis=', this%splitAxis, '] ('
            do i = 1, size(this%coords)
                if (i .gt. 1) write(u, '(A)', advance='no') ', '
                write(u, '(G0.4)', advance='no') this%coords(i)
            end do
            write(u, '(A)') ')'

            if (this%lch .ne. 0_int64) call nodePool(this%lch)%printNode(depth + 1, nodePool, unit)
            if (this%rch .ne. 0_int64) call nodePool(this%rch)%printNode(depth + 1, nodePool, unit)
        end procedure printNode

        module procedure printNodeSingle 
            integer                       :: i, u

            u = output_unit
            if (present(unit)) u = unit

            ! print this Node's coords and split axis
            write(u, '(A,I0,A)', advance='no') '[axis=', this%splitAxis, '] ('
            do i = 1, size(this%coords)
                if (i .gt. 1) write(u, '(A)', advance='no') ', '
                write(u, '(G0.4)', advance='no') this%coords(i)
            end do
            write(u, '(A)') ')'

        end procedure printNodeSingle
        
        module procedure destroyNodePtr
            if (associated(this%p)) deallocate(this%p)
            this%p => null()
        end procedure destroyNodePtr

        module procedure finalizerNodePtr
            call destroyNodePtr(this)
        end procedure finalizerNodePtr


end submodule NodeUtils