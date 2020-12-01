program part_2
    implicit none
    character(len=*), parameter :: fname = "expenses.input"
    integer, allocatable :: numbers(:)
    integer :: funit, io, n, i, j, k

    open(newunit=funit, file=fname, action='read')
    n = 0
    do  
        read(funit,*,iostat=io)
        if ( io /= 0 ) exit
        n = n + 1
    end do

    allocate( numbers(n) )

    rewind(funit)

    read(funit,*) numbers(1) 
    read(funit,*) numbers(2)

    do i = 3, n
        read(funit,*) numbers(i) 
        do j = i-1, 1, -1
            do k = j-1, 1, -1
                associate( ni=>numbers(i), nj=>numbers(j), nk=>numbers(k) )
                if ( ni + nj + nk == 2020 ) then
                    write(*,*) i, j, k, ni, nj, nk, ni * nj *nk
                    stop
                end if 
                end associate
            end do
        end do 
    end do 
end program part_2