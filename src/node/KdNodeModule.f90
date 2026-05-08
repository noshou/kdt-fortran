module KdNodeModule
    use iso_fortran_env, only: real64, output_unit
    private 
    public :: Node, NodePtr 

    type :: Node
        class(*), allocatable           :: data
        real(kind=real64), allocatable  :: coords(:)
        integer                         :: splitAxis ! tracks which index in coords is the splitting plane
        type(Node), pointer             :: leftChild => null(), rightChild => null()
        integer                         :: treeId
        contains    
            procedure                   :: eucDist
            procedure                   :: printNode
            procedure                   :: printNodeSingle
    end type Node

    !> pointer to a Node
    type :: NodePtr
        type(Node), pointer :: p => null()
    end type NodePtr

    contains 

        !> Calculates the euclidean distance between two Nodes
        !! @param[in] that The Node to calculate distance from          
        !!
        !! @return The euclidean distance between two Nodes
        function eucDist(this, that) result(dist)
            class(Node), intent(in) :: this
            type(Node), intent(in)  :: that
            real(kind=real64)       :: dist
            if (.not. allocated(this%coords) .or. .not. allocated(that%coords)) then
                error stop "eucDist: coords not allocated"
            else if ((size(this%coords).eq. 0) .or. (size(that%coords) .eq. 0)) then 
                error stop "eucDist: axis size must be g.t zero"
            else if (size(this%coords) .ne. size(that%coords)) then  
                error stop "eucDist: axis size mismatch"
            else 
                dist = sqrt(sum((that%coords - this%coords)**2))
            end if
        end function eucDist

        !> Recursively prints this Node and its subtree in pre-order.
        !! @param[in] depth the depth of this Node
        !! @param[in] unit  optional output unit (defaults to stdout)
        recursive subroutine printNode(this, depth, unit)
            class(Node), intent(in)       :: this
            integer,     intent(in)       :: depth
            integer, intent(in), optional :: unit
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
                if (i > 1) write(u, '(A)', advance='no') ', '
                write(u, '(G0.4)', advance='no') this%coords(i)
            end do
            write(u, '(A)') ')'

            if (associated(this%leftChild))  call this%leftChild%printNode(depth + 1, unit)
            if (associated(this%rightChild)) call this%rightChild%printNode(depth + 1, unit)
        end subroutine printNode

        !> Prints the current node, but not its subtree
        !!
        !! Use: printNode(depth) for the full subtree
        !! @param[in] unit  optional output unit (defaults to stdout)
        subroutine printNodeSingle(this, unit)
            class(Node), intent(in)       :: this
            integer, intent(in), optional :: unit
            integer                       :: i, u

            u = output_unit
            if (present(unit)) u = unit

            ! print this Node's coords and split axis
            write(u, '(A,I0,A)', advance='no') '[axis=', this%splitAxis, '] ('
            do i = 1, size(this%coords)
                if (i > 1) write(u, '(A)', advance='no') ', '
                write(u, '(G0.4)', advance='no') this%coords(i)
            end do
            write(u, '(A)') ')'

        end subroutine printNodeSingle

end module KdNodeModule