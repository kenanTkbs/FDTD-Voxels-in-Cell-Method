   print *, "start to define flags"

  flagx = 0; flagy = 0
  
        DO J=jobj1+1, jobj2-1
           DO I=iobj1+1, iobj2-1

       ia=i-iobj1+1
       ja=j-jobj1+1
     
! for x components
              
         if(m_code(ia,ja).eq.m_code(ia,ja-1)) then
                temp = 1
                else 
                temp = 0
          end if
     
        
         if((temp.eq.1)) then 
             flagx(i,j) = 1
         else 
             flagx(i,j) = 2
         end if

          
! for y components      
                   

          if(m_code(ia,ja).eq.m_code(ia-1,ja)) then
                  temp = 1
               else 
                  temp = 0
          end if
    

         if((temp.eq.1)) then
            flagy(i,j) = 1
         else 
            flagy(i,j) = 2
         end if    
               

           END DO
        END DO

     print *, "finish flags"
