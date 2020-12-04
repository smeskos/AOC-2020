program part_1
    implicit none
    character(len=*), parameter :: fname = "geology.input"
    character(len=1), parameter :: sharp = "#"
    character(len=100) :: text 
    integer :: funit, io, encounters, length, start, end 
 
    open(newunit=funit, file=fname, action='read', access='sequential')
    encounters = 0
    start = 1 
    end = 4
    read(funit,'(a)',iostat=io)text 
    length = len_trim(text)
    do  
        read(funit,'(a)',iostat=io)text 
        if ( io /= 0 )  then
            write(*,*)encounters 
            exit
        end if 
   
        if ( start > length - 3 ) then 
            end     = 4 -(length - start + 1)
            start   = 1   
        end if 
        if ( text(end:end) == sharp )encounters = encounters + 1
        start   = end 
        end     = end + 3
    end do
end program part_1