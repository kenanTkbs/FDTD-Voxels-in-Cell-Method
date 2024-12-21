!
!
!----------------------------------------------------------------
       print *, "reading pre-defined material specifications..."
!----------------------------------------------------------------

	nmat = 3
	mcode = 0

     allocate ( mspecs(0:nmat, 1), stat=err )
     allocate ( mspecs_read(1), stat=err ) 
     allocate ( mdesc(0:nmat),    stat=err )

      mspecs = 0.0
      mspecs_read = 0.0
      mdesc = " no data "

      open(unit=53, file='mat_specs', status="old", iostat=err)

      if (err.NE.0) then
        print *, "Error reading file: 'mat_specs'"
        stop
      endif

  LOOP_READ : DO 

     read(53, *, iostat=err) mcode, mdesc_read, mspecs_read(1)

     print *, mcode, mdesc_read, mspecs_read(1)
        mspecs(mcode, 0) = mcode
        mdesc(mcode) = mdesc_read 
        mspecs(mcode, 1) = mspecs_read(1) 


    IF (mcode .eq. nmat) EXIT LOOP_READ

  END DO LOOP_READ

      close(unit=53)


      OPEN(UNIT = 54, FILE ='log-mspecs')

  DO i=0, nmat

    write(54,"(I3, A34, E16.8)") INT(mspecs(i,0)), mdesc(i), mspecs(i,1) 


  end do 
  close(unit=54)

!----------------------------------------------------------------
       print *, "end reading pre-defined material specifications..."
!----------------------------------------------------------------
