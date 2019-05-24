subroutine dxdy  (dx, dy, nx, ny)
implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! The subroutine is used to calculate the distance between points of the     !!
!! terrain data.                                                              !!
!!!!!!!!!!!! Tom Robinson University of Hawaii 2013 ter@hawaii.edu !!!!!!!!!!!!!

real,parameter :: minlat=21.2000000,maxlat=21.800000000  !Minimum and maximum lats
real,parameter :: minlon=-158.500000,maxlon=-157.5000000 !Minimum and maximum lons
!real,parameter :: minlat=18.25491,maxlat=22.77174  !Minimum and maximum lats
!real,parameter :: minlon=-160.6300,maxlon=-154.3700 !Minimum and maximum lons
real,parameter :: lat_dist=111320
real,parameter :: re=6378100
integer,intent(in)     ::nx,ny
!real                  :: minlat, maxlat, minlon,maxlon
real   ,intent (inout) :: dx, dy  ! Distance between data points in x and y
real                   :: lon_dist! Distance for one degree longitude at given lat 
real                   :: rlat    ! Average latitude 
!! Make sure the lat and lon are in the right order
if (minlat > maxlat .or. minlon > maxlon) then
   write (6,*) "Latititudes or longitude error.  Min>MAX"
   stop
elseif  (minlat == maxlat .or. minlon == maxlon) then
   write (6,*) "Lat/lon vales are nonvarying"
   stop
endif
!! Calculate the resolution of the data (dx and dy)
dy=((maxlat-minlat)*lat_dist)/ny !distance between points in y

rlat=(maxlat+minlat)/2 !average latitiude to calculate distance of one degree
lon_dist=(2.0*acos(-1.0)*re*cos(rlat*acos(-1.0)/180))/360.0 !Distance of one degree in 
                                                  !longitude at rlat
dx=((maxlon-minlon)*lon_dist)/nx !distance between points in x
write (6,*) dx,dy



return
end
  
