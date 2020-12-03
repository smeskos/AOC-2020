program part_1
    implicit none
    character(len=*), parameter :: fname = "passwords.input"
    character(len=100) :: text, password
    character(len=1) :: letter
    integer :: funit, io, n, i1, i2, valid
 
    open(newunit=funit, file=fname, action='read', access='sequential')
    valid = 0
    do  
        read(funit,'(3a)',iostat=io)text 
        if ( io /= 0 )  then
            write(*,*)valid 
            exit
        end if 

        associate( id1=>index(text,'-'), id2=>index(text,':') )
            read(text(1:id1-1)    ,*)i1
            read(text(id1+1:id2-3),*)i2
            letter   = trim( text(id2-1:id2-1) )
            password = trim( adjustl( text(id2+1:) ) ) 
        end associate

        n = count( transfer(password, 'a', len(password)) == letter )
        
        if ( n >= i1 .and. n <=i2 ) valid = valid + 1   
    end do
end program part_1