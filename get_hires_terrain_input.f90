program terraingrad

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! This program calculates the terrain gradient and then puts it on the WRF  !!
!! grid. Variables starting with "w" are on the WRF grid.
!!!!!!!!!!!! Tom Robinson University of Hawaii 2015 ter@hawaii.edu !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
implicit none

integer, parameter :: hx=2000
integer, parameter :: hy=1200

real, allocatable :: h(:,:), rr(:,:), in_data(:,:)

integer :: ix,iy


!! Get the terrain heights from the oahu.dat file
allocate (in_data(hy,hx), h(hx,hy), rr(hx,hy))
 open (29,file='/share/huina/ter/disertation/operational/full_model/oahu.txt',status="old")
!open (29,file='/share/huina/ter/disertation/operational/full_model/oahu.dat', &   
!               status="old",access='direct',recl=4*hx*hy)
 read (29,*)in_data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 rr = transpose(in_data) ! Transpose the terrain data (may invert y)       !!
 deallocate(in_data)
  write (6,*) 'Terrain data ingested (nom nom nom)'

 close (29)


 do iy = 1,hy ; do ix = 1,hx
   h(ix,(hy-(iy-1))) = rr (ix,iy) 
 enddo ; enddo

 open (28,file="hires_oahu.dat", status="unknown", access = "direct", recl = hx*hy*4)
 write (28,rec=1) h


 end program terraingrad
