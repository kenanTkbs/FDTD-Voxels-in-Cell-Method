

   include 'read-materials.f90'

    open(unit=81,             &
           file='move_object', &
           status="old",        &
           iostat=err)

      if (err.NE.0) then
        print *, "Error reading file: 'move_object'"
        stop
      endif

      read(81, *, iostat=err)  chr, movex
      read(81, *, iostat=err)  chr, movey
      
      close(unit=81)


      print *, "move", movex, movey
      
       do j = 1+8+movey, nyh-8+movey
        do i = 1+8+movex, nxh-8+movex
    
           m_code(i,j) = 1
           eps_r(i,j) = 50 !mspecs(m_code(i,j),1)
         
      end do
      end do

