
  ke=1001

  iop = (/596/)
  jop = (/634,636,638/)


  WRITE(rdirname, '(a,I1.1,a,I1.1)') 'outfiles-fine-',movex,'-',movey

  do j = 1, size(jop)
      do i = 1, size(iop)

           op1=iop(i)
           op2=jop(j)

          WRITE(filename, '(a,I3.3,a,I3.3,a,I3.3)') 'outE-',op1,'-',op2

          ke = ke+5
          
         open(unit=ke, &
     &           file=trim(trim(rdirname)//'/'//filename), &
     &           access='append', status='unknown')
 

         ax=(ex(op1,op2)+ex(op1+1,op2))/2.0
         ay=(ey(op1,op2)+ey(op1,op2+1))/2.0
    
          write(ke, *) ax, ay, t

          close(unit=ke)
         
    end do
    end do
