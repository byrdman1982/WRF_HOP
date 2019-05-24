SUBROUTINE latlon (lat, lon, nx, ny)
implicit none

real,parameter :: minlat=21.2000000,maxlat=21.800000000  !Minimum and maximum lats
real,parameter :: minlon=-158.500000,maxlon=-157.5000000 !Minimum and maximum lons
real,parameter :: lat_dist=111320
real,parameter :: re=6378100

integer, intent (in) ::nx,ny
real, dimension (nx,ny) :: lat, lon
!real,parameter       :: dlat=0.0005000000000!(maxlat-minlat)/ny  !Distance lat deg b/t points
!real,parameter       :: dlon=0.0005000000000!(maxlon-minlon)/nx  !Distance lon deg b/t points
double precision :: dlat=0.0005d0, dlon=0.0005d0
integer :: ix,iy

do ix = 1,nx ; do iy=1,ny
  if (ix .eq. 1) then
    lon(ix,iy) = -158.5000000000000000000000000000000000
  else
    lon(ix,iy) = lon(ix-1,iy) + real(dlon)
  endif
!
  if (iy .eq. 1) then
    lat (ix,iy) = 21.200000000000000000000000000000000000
  else
    lat (ix,iy) = lat (ix, iy-1) + real(dlat)
  endif   
enddo ; enddo

!    write (6,*) maxlon, lon (nx,ny), lon (1,1), minlon, dlon
!    write (6,*) maxlat, lat (nx,ny), lat (1,1), minlat, dlat, ny

return
end
