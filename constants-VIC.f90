
print *, "start calculation of VIC constants"
        DO J=jobj1+1, jobj2-1
           DO I=iobj1+1, iobj2-1

       ia=i-iobj1+1
       ja=j-jobj1+1
     
! for x components
       if(flagx(i,j).ne.0) then
          epsr_x(i,j) = 0
       DO ii=ia*rate-rate+1, ia*rate
          alpha = 0
          DO jj=ja*rate-rate+1-h_rate, ja*rate-h_rate 
     
              alpha = alpha + eps_r(ii,jj)/2.0
          END DO
          epsr_x(i,j) = epsr_x(i,j) + 1.0/alpha 
       END DO

       epsr_x(i,j) = epsr_x(i,j)/2.0
       epsr_x(i,j) = 1.0/epsr_x(i,j)
   end if   


! for y components      

   if(flagy(i,j).ne.0) then
      epsr_y(i,j) = 0
    DO jj=ja*rate-rate+1, ja*rate
       alpha = 0
             DO ii=ia*rate-rate+1-h_rate, ia*rate-h_rate

                alpha = alpha + eps_r(ii,jj)/2.0
                
           END DO  
       epsr_y(i,j) = epsr_y(i,j) + 1.0/alpha 
    END DO
    epsr_y(i,j) = epsr_y(i,j)/2.0
     epsr_y(i,j) = 1.0/epsr_y(i,j)
end if
                
   

        END DO
     END DO
print *, "end calculation of VIC constants"
