
print *, "start calculation of VIC constants"
        DO J=jobj1+1, jobj2-1
           DO I=iobj1+1, iobj2-1

       ia=i-iobj1 + 1
       ja=j-jobj1 + 1
     
! for x components
       if(flagx(i,j).ne.0) then

          epsr_x(i,j) = 0.0
          
       DO jj=ja-1, ja    

                epsr_x(i,j) = epsr_x(i,j) + eps_r(ia,jj)/2.0

             END DO

  !           write(50, *) i, j, epsr_x(i,j)
    end if 

! for y components      

    if(flagy(i,j).ne.0) then
       epsr_y(i,j) = 0.0 
          DO ii=ia-1, ia 

               epsr_y(i,j) = epsr_y(i,j) + eps_r(ii,ja)/2.0
               
           END DO   
end if
                     
           END DO
        END DO
print *, "end calculation of VIC constants"
