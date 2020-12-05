program part_1
    implicit none
    character(len=*), parameter :: fname = "passports.input"
    character(len=3),dimension(7) :: key = [character(len=3) :: 'byr', 'iyr', &
                                'eyr', 'hgt', 'hcl', 'ecl', 'pid']
    integer :: key_exists(7)
    character(len=200) :: text 
    integer :: funit, io, valid, i, n
    valid   = 0
    n = 0
    key_exists = 0
    open(newunit=funit, file=fname, action='read', access='sequential')
    do  
        read(funit,'(a)',iostat=io)text 
        if ( io /= 0 ) then 
            n = n + 1
            if ( sum(key_exists) == 7 )valid = valid + 1
            write(*,'(a,1x,i3,a,i3)')'valid passports:',valid,' out of ',n 
            exit
        end if 
        text = trim(text)
        if ( text == '' ) then 
            n = n + 1
            if ( sum(key_exists) == 7 )valid = valid + 1 
            key_exists = 0
            cycle 
        end if     
        do i = 1, 7
            if ( index(text, key(i)) /= 0 ) key_exists(i) = 1
        end do 
    end do 
end program part_1