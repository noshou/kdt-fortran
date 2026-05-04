!> Rearranges indices so that indices(targetIdx) holds the
!! median element along the given axis, with smaller values to its left and
!! larger values to its right. Returns targetIdx.
!!
!! @param[in] nodes             array of kd-tree nodes
!! @param[inout] indices        index permutation array, modified in-place
!! @param[in] lowerIdx          lower bound of the current subarray
!! @param[in] upperIdx          upper bound of the current subarray
!! @param[in] axis              coordinate axis to compare on
!! @param[inout] middleBounds   bounds of the equal-to-pivot region
!! @param[in] targetIdx         rank being searched for (fixed across all recursive calls)
!!
!! @return the index of the median value
recursive function quickSelect( &
    nodes,                      & 
    indices,                    &
    lowerIdx,                   &
    upperIdx,                   &
    axis,                       &
    middleBounds,               &
    targetIdx                   &
    ) result(median)
    
    type(node), intent(in)   :: nodes(:)
    integer, intent(inout)   :: indices(:)
    integer, intent(in)      :: lowerIdx, upperIdx, axis, targetIdx
    integer, intent(inout)   :: middleBounds(2)
    integer                  :: pivotIdx, median
    real(kind=real64)        :: pivotVal, randomNumber

    if (lowerIdx .eq. upperIdx) then 
        median = lowerIdx
    else 

        call random_number(randomNumber)
        pivotIdx = lowerIdx + floor(randomNumber * (upperIdx - lowerIdx + 1))
        pivotVal = nodes(indices(pivotIdx))%coords(axis)
        call quickSelectPartition(  &
            nodes,                  &
            indices,                &
            lowerIdx,               &
            upperIdx,               &
            middleBounds,           &
            axis,                   &
            pivotVal                &
        )

        if (targetIdx .lt. middleBounds(1)) then 
            median = quickSelect(   &
                nodes,              &
                indices,            &
                lowerIdx,           &
                middleBounds(1)-1,  &
                axis,               &
                middleBounds,       &
                targetIdx           &
            )
        else if (targetIdx .gt. middleBounds(2)) then 
            median = quickSelect(   &
                nodes,              &
                indices,            &
                middleBounds(2)+1,  &
                upperIdx,           &
                axis,               &
                middleBounds,       &
                targetIdx           &
            )
        else 
            median = targetIdx
        end if
    end if
end function quickSelect

!> Three-way partition of indices(lowerIdx:upperIdx) around pivot
!! On exit, the subarray is split into three contiguous regions
!!  [ lowerIdx ... middleBounds(1)-1 | middleBounds(1) ... middleBounds(2) | middleBounds(2)+1 ... upperIdx ]
!!  [ < pivot .......................| = pivot ............................| > pivot .......................]
!! @param[in] nodes             array of kd-tree nodes
!! @param[inout] indices        index permutation array, modified in-place
!! @param[in] lowerIdx          lower bound of the subarray to partition
!! @param[in] upperIdx          upper bound of the subarray to partition
!! @param[inout] middleBounds   on exit: (1) first index of equal region, (2) last index
!! @param[in] axis              coordinate axis to compare on
!! @param[in] pivot             pivot value
recursive subroutine quickSelectPartition(  &
    nodes,                                  & 
    indices,                                &
    lowerIdx,                               &
    upperIdx,                               &
    middleBounds,                           &
    axis,                                   &
    pivot                                   &
    )

    type(node), intent(in)         :: nodes(:)
    integer, intent(inout)         :: indices(:), middleBounds(2)
    integer,    intent(in)         :: lowerIdx, upperIdx, axis
    integer                        :: i, tmp, lowerMiddleIdx, upperMiddleIdx
    real(kind=real64), intent(in)  :: pivot

    i = lowerIdx
    lowerMiddleIdx = lowerIdx
    upperMiddleIdx = upperIdx
    do while(i .le. upperMiddleIdx)
        if (nodes(indices(i))%coords(axis) < pivot) then 
            tmp = indices(i)
            indices(i) = indices(lowerMiddleIdx)
            indices(lowerMiddleIdx) = tmp
            i = i + 1
            lowerMiddleIdx = lowerMiddleIdx + 1
        else if (nodes(indices(i))%coords(axis) > pivot) then
            tmp = indices(i)
            indices(i) = indices(upperMiddleIdx)
            indices(upperMiddleIdx) = tmp
            upperMiddleIdx = upperMiddleIdx - 1
        else
            i = i + 1
        end if
    end do 

    middleBounds(1) = lowerMiddleIdx; middleBounds(2) = upperMiddleIdx

end subroutine quickSelectPartition
