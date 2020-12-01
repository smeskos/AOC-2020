program part_1
    implicit none
    character(len=*), parameter :: fname = "expenses.input"
    integer, allocatable :: numbers(:)
    integer :: funit, io, n, i, j

    open(newunit=funit, file=fname, action='read')
    n = 0
    do  
        read(funit,*,iostat=io)
        if ( io /= 0 ) exit
        n = n +1
    end do

    allocate( numbers(n) )

    rewind(funit)

    read(funit,*) numbers(1)

    do i = 2, n
        read(funit,*) numbers(i)
        do j = i-1, 1, -1
            associate( ni=>numbers(i), nj=>numbers(j) )
            if ( ni + nj == 2020 ) then
                write(*,*) i, j, ni, nj, ni * nj 
                stop
            end if 
            end associate
        end do 
    end do 
end program part_1