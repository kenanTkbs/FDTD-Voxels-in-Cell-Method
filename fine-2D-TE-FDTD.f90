!------------------------------------------------------------------
!
!     Project: 2D FDTD implementation (TE Mode, Ex, Ey and Hz)
!     Author: Kenan Tekbas
!     Created: 10.05.2023
!     Last Modified: 02.09.2024
!     Description: This codes an example fine mesh implementation.
!------------------------------------------------------------------

      PROGRAM FD_FDTD

      IMPLICIT NONE

!----------------------------------------------------------------
!    Variables for input_params
!----------------------------------------------------------------
!----------------------------------------------------------------
!     Number of elements in each direction
!----------------------------------------------------------------
      INTEGER :: nx, ny

!----------------------------------------------------------------
!     Maximum number of steps
!----------------------------------------------------------------  
      INTEGER :: t_max

!----------------------------------------------------------------
!     Data output directory
!----------------------------------------------------------------
      CHARACTER(len=128) :: dirname, rdirname

      CHARACTER :: outformat*3, chr

      integer, dimension(1) :: iop
      integer, dimension(3) :: jop
      character     :: filename*128
!----------------------------------------------------------------
!     Time step skip
!----------------------------------------------------------------
      REAL :: timeskip

!----------------------------------------------------------------
!     Length of the time step
!----------------------------------------------------------------
      REAL :: dt

!----------------------------------------------------------------
!     Distance between 2 cells
!----------------------------------------------------------------
      REAL :: dx,dy,dz

!----------------------------------------------------------------
!     Epsilon0, permittivity of free space (in farad/meter)
!----------------------------------------------------------------
      REAL :: eps_0

!----------------------------------------------------------------
!     Light speed (v in m/s)
!----------------------------------------------------------------
      REAL, parameter :: C = 3.0*10.0**8

!----------------------------------------------------------------
!     mu0, permeability of free space (in henry/meter)
!----------------------------------------------------------------
      REAL :: mu_0

!----------------------------------------------------------------
!     Pi
!----------------------------------------------------------------
      REAL :: pi 

!----------------------------------------------------------------
!    Variables for loop
!----------------------------------------------------------------
      INTEGER :: i, j, k, t, err
      INTEGER :: i1, i2, j1, j2
      INTEGER :: op1, op2
      REAL :: ax, ay
!----------------------------------------------------------------
!    field values 
!----------------------------------------------------------------

      REAL, DIMENSION(:,:), ALLOCATABLE :: ex, ey 
      REAL, DIMENSION(:,:), ALLOCATABLE :: hz
      REAL, DIMENSION(:,:), ALLOCATABLE :: epsr_x     
      REAL, DIMENSION(:,:), ALLOCATABLE :: epsr_y, eps_r
      INTEGER, dimension(:,:), allocatable :: flagx, flagy
      integer :: iobj1, iobj2, jobj1, jobj2
      integer :: nxh, nyh, movex, movey
      INTEGER, dimension(:,:), allocatable :: m_code
       
!---------------------------------------------------------------
!    Variables for source excitation
!----------------------------------------------------------------

      INTEGER :: temp, fine, islide, istart, sum 
      REAL :: anormalize, isourcecase 
      REAL :: amplite, amplith, decal, tau, t1, t2
      REAL :: teta, steta, cteta, dxcteta, dysteta
      REAL :: to, tt2, te, th, raise, einc, hinc
      REAL :: eix, eiy, hix, hiy
      INTEGER :: ihuyg1, ihuyg2, jhuyg1, jhuyg2, ihuyg22, jhuyg22, io, jo, ii, jj
      REAL :: evalue, tgauss

      INTEGER :: imin, imax, jmin, jmax, ia, ja, temps, tempf, h_rate

        REAL, allocatable :: mspecs(:,:)
        REAL, allocatable :: mspecs_read(:)
        character(len=32), allocatable :: mdesc(:)
        character(len=32) :: mdesc_read
        character(len=2) :: magic       ! magic number                                
        character(len=64) :: comment
        integer :: width, height, maxv  ! maximum value                               
        integer :: ke, mcode, nmat, rate
!---------------------------------------------------------------
!   Reading input parameters
!----------------------------------------------------------------
      open(unit=51,             &
           file='input_params_fine', &
           status="old",        &
           iostat=err)

      if (err.NE.0) then
        print *, "Error reading file: 'input_params'"
        stop
      endif

      read(51, *, iostat=err) chr, nx, ny
      read(51, *, iostat=err) chr, t_max
      read(51, *, iostat=err) chr, dx
      read(51, *, iostat=err) chr, ihuyg1, ihuyg2, jhuyg1, jhuyg2
      read(51, *, iostat=err) chr, iobj1, iobj2, jobj1, jobj2
      read(51, *, iostat=err)  chr, nxh, nyh

      close(unit=51)

!----------------------------------------------------------------
!     Allocate field arrays 
!----------------------------------------------------------------

      allocate(ex(1:nx, 1:ny), stat=err)
      allocate(ey(1:nx, 1:ny), stat=err)
      
      allocate(hz(1:nx, 1:ny), stat=err)
      allocate(epsr_x(1:nx, 1:ny), stat=err)      
      allocate(epsr_y(1:nx, 1:ny), stat=err)

      allocate(flagx(1:nx,1:nx), stat=err)
      allocate(flagy(1:ny,1:ny), stat=err)
      allocate( m_code(1:nxh,1:nyh), stat=err)
      allocate( eps_r(1:nxh,1:nyh), stat=err)

!----------------------------------------------------------------
!     Initialising the fields with 0
!----------------------------------------------------------------

      ex(1:nx,1:ny) = 0.0
      ey(1:nx,1:ny) = 0.0
	
      hz(1:nx,1:ny) = 0.0
     
      m_code = 0
      eps_r = 1
      epsr_x = 1
      epsr_y = 1
      
        
!----------------------------------------------------------------
!     pi 
!----------------------------------------------------------------
      pi = acos(-1.0)

!----------------------------------------------------------------
!     setting dx and dt
!----------------------------------------------------------------

      dy=dx

      timeskip = 0.95
      dt = 1.0d0 * timeskip / (C*sqrt(1.0d0/(dx**2)+1.0d0/(dy**2)))

!     dt = 3.6e-12/2.
! in jean pierre code- (dt about 3D stability condition => representative of applications) formula:  dt = 1.0d0 * timeskip / (C*sqrt(3.0d0/(dx**2))) 

      imin=50
      imax=nx-50
      jmin=50
      jmax=ny-50
!--------------------------------------------------------------
!     setting huygens excitation parameters
!----------------------------------------------------------------
        
      amplite=100.
      amplith=amplite/376.99

!       tt2=tau**2
!       to=5*tau
      tgauss = 134.5e-12 !30*dt*2
      to=3*tgauss
      
      teta=0
      teta=teta*pi/180.
      steta=sin(teta)
      cteta=cos(teta)
      if(steta.gt.-0.01.and.steta.lt.0.01) steta=0.
      if(cteta.gt.-0.01.and.cteta.lt.0.01) cteta=0.

      dxcteta=dx*cteta/3e8
      dysteta=dy*steta/3e8
     
      eix=-amplite*steta
      eiy=amplite*cteta

      print *, eix, eiy
      io = ihuyg1-2
      jo = jhuyg1-2
      ihuyg22=ihuyg2-1
      jhuyg22=jhuyg2-1
      
!----------------------------------------------------------------
      ! mu_0
!----------------------------------------------------------------
      mu_0 = 4*pi*10**(-7.0)

!----------------------------------------------------------------
      ! eps_0
!----------------------------------------------------------------
      eps_0 = 1.0/(mu_0*C*C)

!----------------------------------------------------------------
!     print out settings
!----------------------------------------------------------------

      print *," "
      print *,"FDTD algorithm"
      print *," "
      print *,"Meshing parameters:"
      print *," ",nx,ny,"x,y"," cells"
      print *," dx=",dx," meters", dt
      print *," "
      print *,"Time simulated will be ",dt*t_max," seconds"
      print *," ",t_max," timesteps"
      print *," timesteps of ",dt," seconds"
      print *," "
      print *, "huygens surface",ihuyg1, ihuyg2, jhuyg1, jhuyg2


      include 'set_materials.f90'
      include 'flags-fine.f90'
      include 'constants-fine.f90'
        
!------------------------------------------------------------------------------
!      main loop 
!------------------------------------------------------------------------------

        print *, "Starting time step iteration"
        
      DO t=1, t_max 

       t1=dt*(t-1)+2*dt

!----------------------------------------------------------------
!     Updating Hz
!----------------------------------------------------------------

          do j=jmin,jmax
	    do i=imin,imax
!
               hz(i,j)=hz(i,j)-dt/(mu_0*dx)*(ey(i+1,j)-ey(i,j))+dt/(mu_0*dy)*(ex(i,j+1)-ex(i,j))
!
             end do
            end do

!----------------------------------------------------------------
!     source excitation for Hz
!----------------------------------------------------------------
     
      do j=jhuyg1,jhuyg22
      te=t1-(ihuyg1-io)*dxcteta-(j-jo+0.5)*dysteta
      einc=eiy*exp(-((te-to)/tgauss)**2)
      if(te.lt.0) einc=0.
      hz(ihuyg1-1,j)=hz(ihuyg1-1,j)+dt/(mu_0*dx)*einc      
      end do

      
      do j=jhuyg1,jhuyg22
      te=t1-(ihuyg2-io)*dxcteta-(j-jo+0.5)*dysteta
      einc=eiy*exp(-((te-to)/tgauss)**2)
      if(te.lt.0) einc=0.
      hz(ihuyg2,j)=hz(ihuyg2,j)-dt/(mu_0*dx)*einc     
      end do 

      do i=ihuyg1,ihuyg22
      te=t1-(i-io+0.5)*dxcteta-(jhuyg1-jo)*dysteta
      einc=eix*exp(-((te-to)/tgauss)**2)
      if(te.lt.0) einc=0.
      hz(i,jhuyg1-1)=hz(i,jhuyg1-1)-dt/(mu_0*dy)*einc  
      end do 

      do i=ihuyg1,ihuyg22
      te=t1-(i-io+0.5)*dxcteta-(jhuyg2-jo)*dysteta
      einc=eix*exp(-((te-to)/tgauss)**2)
      if(te.lt.0) einc=0.
      hz(i,jhuyg2)=hz(i,jhuyg2)+dt/(mu_0*dy)*einc
      end do 

!----------------------------------------------------------------
!     Updating Ex
!----------------------------------------------------------------

          do j=jmin,jmax
            do i=imin,imax

               ex(i,j)=ex(i,j) &
     &                  +dt/(eps_0*dy)/epsr_x(i,j)*(hz(i,j)-hz(i,j-1))  
            end do
         end do


!----------------------------------------------------------------
!     Updating Ey
!----------------------------------------------------------------

          do j=jmin,jmax
            do i=imin,imax

               ey(i,j)=ey(i,j) &
     &                  -dt/(eps_0*dx)/epsr_y(i,j)*(hz(i,j)-hz(i-1,j))  
            end do
         end do

         
!----------------------------------------------------------------
!      source excitation for Ey
!----------------------------------------------------------------
      

       do j=jhuyg1,jhuyg22
      th=t1+dt*0.5-(ihuyg1-io-0.5)*dxcteta-(j-jo+0.5)*dysteta
      hinc=amplith*exp(-((th-to)/tgauss)**2)
      if(th.lt.0) hinc=0.
      ey(ihuyg1,j)=ey(ihuyg1,j)+dt/(eps_0*dx)*hinc
      end do

      
      do j=jhuyg1,jhuyg22
      th=t1+dt*0.5-(ihuyg2-io+0.5)*dxcteta-(j-jo+0.5)*dysteta
      hinc=amplith*exp(-((th-to)/tgauss)**2)
      if(th.lt.0) hinc=0.
      ey(ihuyg2,j)=ey(ihuyg2,j)-dt/(eps_0*dx)*hinc
   end do
   
!----------------------------------------------------------------
!      source excitation for Ex
!----------------------------------------------------------------
   
      do i=ihuyg1,ihuyg22
      th=t1+dt*0.5-(i-io+0.5)*dxcteta-(jhuyg1-jo-0.5)*dysteta
      hinc=amplith*exp(-((th-to)/tgauss)**2)
      if(th.lt.0) hinc=0.
      ex(i,jhuyg1)=ex(i,jhuyg1)-dt/(eps_0*dy)*hinc
      end do 

      do i=ihuyg1,ihuyg22
      th=t1+dt*0.5-(i-io+0.5)*dxcteta-(jhuyg2-jo+0.5)*dysteta
      hinc=amplith*exp(-((th-to)/tgauss)**2)
      if(th.lt.0) hinc=0.
      ex(i,jhuyg2)=ex(i,jhuyg2)+dt/(eps_0*dy)*hinc   
      end do 

!----------------------------------------------------------------
!     Output in the function of time step
!----------------------------------------------------------------
 
!	write(100,*) t, (ex(596,636)+ex(597,636))/2.0, t*dt*1e9 !A
!        write(200,*) t, (ey(596,634)+ey(596,635))/2.0, t*dt*1e9 !B
!        write(300,*) t, (ey(596,636)+ey(596,637))/2.0, t*dt*1e9 !C
!        write(400,*) t, (ex(596,638)+ex(597,638))/2.0, t*dt*1e9 !D
    
!----------------------------------------------------------------
!     Output in the function of space
!----------------------------------------------------------------
       
     include 'auto_observation-fine.f90'
   
     END DO  ! DO t=0, t_max

!------------------------------------------------------------------------------
!      end main loop 
!------------------------------------------------------------------------------

!----------------------------------------------------------------
!     deallocate variables 
!----------------------------------------------------------------

      deallocate(ex, stat=err)
      deallocate(ey, stat=err)

      deallocate(hz, stat=err)

      stop


      END PROGRAM FD_FDTD

