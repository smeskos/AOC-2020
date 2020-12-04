program part_1
    use iso_fortran_env, only:int64
    implicit none
    character(len=*), parameter :: fname = "geology.input"
    character(len=*), parameter :: sharp = "#"
    character(len=100) :: text 
    integer :: funit, io, i, encounters, length, start, end
    integer :: patterns(5) = [ 2, 4, 6, 8, 2]
    integer(int64) :: all_product
 
    open(newunit=funit, file=fname, action='read', access='sequential')
    all_product = 1
    do i = 1, 5
        read(funit,'(a)',iostat=io)text 
        length = len_trim(text)
        encounters = 0
        start = 1 
        end = patterns(i)    
        do  
            if ( i == 5 ) read(funit,'(a)',iostat=io)
            read(funit,'(a)',iostat=io)text 
            if ( io /= 0 )  then
                write(*,'(i1,1x,a,1x,i3)')i,'encounters=', encounters 
                exit
            end if 
    
            if ( start > length - (patterns(i) - 1) ) then 
                end     = patterns(i) -(length - start + 1)
                start   = 1   
            end if 
            if ( text(end:end) == sharp )encounters = encounters + 1
            start   = end 
            end     = end + patterns(i) - 1
        end do
        all_product = all_product * encounters
        rewind(funit)
    end do 
    write(*,'(1x,a,1x,i10)')'all_product=',all_product
end program part_1