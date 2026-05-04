
!> Returns the dimension (number of splitting axis) of the tree
function getDim(this) result(k)
    class(tree), intent(in) :: this
    integer                 :: k
    k = this%dim
end function getDim

!> Returns the number of nodes in the tree
function getPop(this) result(n)
    class(tree), intent(in) :: this
    integer                 :: n
    n = this%pop
end function getPop

include "quickSelect.inc"

! coords is a kxn array where n is # points, k is dimensionality of axis
! data is an n sized parallel array to coords' stored data
subroutine build(this, coords, data)
    
    class(tree), intent(inout)      :: this
    real(kind=real64), intent(in)   :: coords(:,:)
    class(*), intent(in), optional  :: data(:)
    integer, allocatable            :: indices(:)
    integer                         :: i

    ! initialize dimension and population size
    this%dim = size(coords, 1)
    this%pop = size(coords, 2) 


    ! ensure number of data points is equal to number of coordinates 
    if (present(data)) then
        if (size(data) .ne. this%pop) then 
            error stop "data array length must equal number of points"
        end if 
    end if

    ! initialize node pool
    allocate(this%nodePool(this%pop))
    do i = 1, this%pop
        allocate(this%nodePool(i)%coords(this%dim))
        this%nodePool(i)%coords(:) = coords(:, i)
        if (present(data)) then
            allocate(this%nodePool(i)%data, source=data(i))
        end if
    end do

    ! allocate indices; that way we don't have to modify the list of nodes
    allocate(indices(this%pop))
    indices = [(i, i=1, this%pop)]

    ! build tree
    call buildSubtree(this, this%root, 0, indices, 1, this%pop)

    deallocate(indices)
end subroutine build

!> Recursively builds the tree
recursive subroutine buildSubtree(this, root, depth, indices, lowerIdx, upperIdx)

    type(tree), intent(inout)          :: this
    type(node), pointer, intent(out)   :: root
    integer, intent(inout)             :: indices(:)
    integer, intent(in)                :: lowerIdx, upperIdx, depth
    integer                            :: axis, median, middleBounds(2), targetIdx

    ! base case: we are at a leaf (or tree is empty)
    if (lowerIdx > upperIdx) then 
        root => null()
    else 
        axis = mod(depth, this%dim) + 1
        targetIdx = (lowerIdx + upperIdx) / 2
        median = quickSelect( &
            this%nodePool,    &
            indices,          &
            lowerIdx,         &
            upperIdx,         &
            axis,             &
            middleBounds,     &
            targetIdx         &
        )    
        root => this%nodePool(indices(median))
        root%splitAxis = axis
        call buildSubtree(this, root%leftChild,  depth+1, indices, lowerIdx, median-1)
        call buildSubtree(this, root%rightChild, depth+1, indices, median+1, upperIdx)
    end if  
end subroutine buildSubtree

subroutine printTree(this)
    class(tree), intent(in) :: this
    if (associated(this%root)) then 
        call this%root%printNode(0)
    else 
        write(*, '(A)') '**empty tree**'
    end if
end subroutine printTree

!> Frees the node pool and resets tree state.
subroutine destroy(this)
    class(tree), intent(inout) :: this
    if (associated(this%nodePool)) deallocate(this%nodePool)
    this%root => null()
    this%dim = 0
    this%pop = 0
end subroutine destroy

! frees up tree when it goes out of scope
subroutine finalizer(this)
    type(tree), intent(inout) :: this
    call destroy(this)
end subroutine finalizer