program center

integer, parameter :: nx = 220
integer, parameter :: ny = 170
integer, parameter :: fnum = 2

real, dimension (ny) :: lat
real, dimension (nx) :: lon

real :: meanlat, meanlon

open (29,file='lats.txt',status='old')
read(29,*) lat
 close (29)
 
open (29,file='lons.txt',status='old')
read(29,*) lon
 close (29)

 meanlat = sum(lat)/size(lat)
! meanlat = (lat(ny/2) + lat(ny/2+1))/2.0

 meanlon = sum(lon)/size(lon)

   write (6,*) meanlat, meanlon,size(lat),size(lon)

do iy=1,ny
 if (lat(iy) == 19.93252 .or. lat(iy) == 20.06744) write (6,*) iy    
enddo
do ix=1,nx
 if (lon(ix) == -157.1411 .or. lon(ix) == -156.9975) write (6,*) ix    
enddo
 
!write (6,*) (19.93252+20.06744)/2.0 , (-157.1411+-156.9975)/2.0      
   
stop
end
