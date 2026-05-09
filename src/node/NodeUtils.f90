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

            if (associated(this%leftChild))  call this%leftChild%printNode(depth + 1, unit)
            if (associated(this%rightChild)) call this%rightChild%printNode(depth + 1, unit)
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
        
end submodule NodeUtils