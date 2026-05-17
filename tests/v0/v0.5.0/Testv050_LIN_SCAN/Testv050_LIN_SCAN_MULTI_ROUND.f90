program Testv050_LIN_SCAN_MULTI_ROUND
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call linScanMultiRound()
    contains
        !> Multiple add/remove/search cycles.
        !!
        !! Round 1: build(A,B) → linScan([id_A, id_B]) → expect 2
        !! Round 2: addNodes(C,D) → rmvNodes(A) → linScan([id_A]) → expect 0
        !!                       → linScan([id_B,id_C,id_D]) → expect 3
        !! Round 3: addNodes(E) → rmvNodes(B) → linScan([id_E]) → expect 1
        !!                      → linScan([id_B]) → expect 0
        subroutine linScanMultiRound()
            type(KdTree)                 :: t
            real(real64)                 :: coordsAB(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64], [2, 2])
            real(real64)                 :: coordsCD(2, 2) = reshape( &
                [2.0_real64, 0.0_real64, 3.0_real64, 0.0_real64], [2, 2])
            real(real64)                 :: coordsE(2, 1)  = reshape( &
                [9.0_real64, 9.0_real64], [2, 1])
            real(real64)                 :: rmvA(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                 :: rmvB(2, 1) = reshape([1.0_real64, 0.0_real64], [2, 1])
            type(KdNodePtr), allocatable :: pool(:), res(:)
            integer(int64)               :: idA, idB, idC, idD, idE
            integer                      :: numRmv, i

            ! --- Round 1 ---
            call t%build(coordsAB)
            pool = t%getAllNodes()
            idA  = pool(1)%p%getNodeId()
            idB  = pool(2)%p%getNodeId()

            res = t%linScan([idA, idB])
            if (size(res) .ne. 2) then
                write(*, '(A,I0)') '--- MULTI_ROUND r1: expected 2, got: ', size(res); stop 1
            end if

            ! --- Round 2: add C,D; remove A ---
            call t%addNodes(coordsCD)
            pool = t%getAllNodes()
            ! C and D are the 2 nodes with ids beyond idA,idB
            idC = 0_int64; idD = 0_int64
            do i = 1, size(pool)
                if (pool(i)%p%getNodeId() .ne. idA .and. pool(i)%p%getNodeId() .ne. idB) then
                    if (idC == 0_int64) then
                        idC = pool(i)%p%getNodeId()
                    else
                        idD = pool(i)%p%getNodeId()
                    end if
                end if
            end do

            numRmv = t%rmvNodes(coordsList=rmvA)

            res = t%linScan([idA])
            if (size(res) .ne. 0) then
                write(*, '(A,I0)') '--- MULTI_ROUND r2 removed A: expected 0, got: ', size(res); stop 1
            end if

            res = t%linScan([idB, idC, idD])
            if (size(res) .ne. 3) then
                write(*, '(A,I0)') '--- MULTI_ROUND r2 survivors: expected 3, got: ', size(res); stop 1
            end if

            ! --- Round 3: add E; remove B ---
            call t%addNodes(coordsE)
            pool = t%getAllNodes()
            idE  = 0_int64
            do i = 1, size(pool)
                if (pool(i)%p%getNodeId() .ne. idB .and. pool(i)%p%getNodeId() .ne. idC .and. &
                    pool(i)%p%getNodeId() .ne. idD) then
                    idE = pool(i)%p%getNodeId()
                end if
            end do

            numRmv = t%rmvNodes(coordsList=rmvB)

            res = t%linScan([idE])
            if (size(res) .ne. 1) then
                write(*, '(A,I0)') '--- MULTI_ROUND r3 new E: expected 1, got: ', size(res); stop 1
            end if
            res = t%linScan([idB])
            if (size(res) .ne. 0) then
                write(*, '(A,I0)') '--- MULTI_ROUND r3 removed B: expected 0, got: ', size(res); stop 1
            end if
        end subroutine linScanMultiRound
end program Testv050_LIN_SCAN_MULTI_ROUND
