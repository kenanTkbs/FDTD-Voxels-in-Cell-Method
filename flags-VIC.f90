   print *, "start to define VIC flags"

  flagx = 0; flagy = 0
  
        DO J=jobj1+2, jobj2-2
           DO I=iobj1+2, iobj2-2

       ia=i-iobj1+1
       ja=j-jobj1+1
     
! for x components

          ii=ia*rate                 
          jj=ja*rate-h_rate 

         if(m_code(ii,jj).eq.m_code(ii,jj-1)) then
                temps = 1
                else 
                temps = 0
          end if

         if(m_code(ii-1,jj).eq.m_code(ii-1,jj-1)) then
            tempf = 1
            else 
            tempf = 0
         end if
      
        
         if((temps.eq.1).and.(tempf.eq.1)) then
            if(m_code(ii,jj).eq.m_code(ii-1,jj-1)) then
               flagx(i,j) = 1
            else 
               flagx(i,j) = 11
            end if
         else if ((tempf.eq.1).and.(temps.eq.0)) then
            flagx(i,j) = 12
         else if ((tempf.eq.0).and.(temps.eq.1)) then
            flagx(i,j) = 21
         else 
            flagx(i,j) = 22
         end if  

          
! for y components      
      
              jj=ja*rate                 
              ii=ia*rate-h_rate

          if(m_code(ii,jj).eq.m_code(ii-1,jj)) then
                  temps = 1
               else 
                  temps = 0
          end if

         
          if(m_code(ii,jj-1).eq.m_code(ii-1,jj-1)) then
                  tempf = 1
               else 
                  tempf = 0
          end if         

          if((temps.eq.1).and.(tempf.eq.1)) then
            if(m_code(ii,jj).eq.m_code(ii-1,jj-1)) then
               flagy(i,j) = 1
            else 
               flagy(i,j) = 11
            end if
          else if ((tempf.eq.1).and.(temps.eq.0)) then
             flagy(i,j) = 12
          else if ((tempf.eq.0).and.(temps.eq.1)) then
             flagy(i,j) = 21
            else 
               flagy(i,j) = 22
             end if    
            
        END DO
     END DO
     
 print *, "end to define VIC flags"
