program terraingrad

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! This program calculates the terrain gradient and then puts it on the WRF  !!
!! grid. Variables starting with "w" are on the WRF grid.
!!!!!!!!!!!! Tom Robinson University of Hawaii 2015 ter@hawaii.edu !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
implicit none

integer, parameter :: hx= 220
integer, parameter :: hy= 170
integer, parameter :: wx= 220
integer, parameter :: wy= 170

real, allocatable :: h(:,:), dhdx(:,:),dhdy(:,:), lat(:,:), lon(:,:)
real, dimension (wx) :: wlon
real, dimension (wy) :: wlat
real, dimension (wx,wy) :: wdhdx=0.0, wdhdy=0.0
integer, dimension (wx,wy) :: wpts_box=0

real :: dx,dy
integer :: ix,iy
integer :: iw,jw


!! Get the terrain heights from the oahu.dat file
allocate (h(hx,hy), dhdx(hx,hy), dhdy(hx,hy), lat(hx,hy) ,lon(hx,hy))
 open (29,file='wrf_original_heights.dat', &   
!               status="old")
!read (29) h
                status="old",access='direct',recl=4*hx*hy)
 read(29,rec=1) h
 close (29)
! write (6,*) h
 CALL dxdy (dx,dy,hx,hy)
 CALL gradient(dx,dy,h,dhdx,dhdy,hx,hy)
!CALL latlon(lat,lon,hx,hy)
 
 write (6,*) dx,dy
 write (6,*) h(hx/2,hy/2),dhdx(hx/2,hy/2),dhdy(hx/2,hy/2)
 write (6,*) hx/2,hy/2

 do iy=1,hy ; do ix=1,hx
    if (h(ix,iy) .gt. 10.0) write (6,*) h(ix,iy),ix,iy
 enddo ; enddo
!
!
!open(30,file='lats.txt',status='old')
!open(31,file='lons.txt',status='old')
!ead(30,*) wlat
!ead(31,*) wlon
!close(31)
!close(30)
!  write (6,*) wlat(wy/2),wlon(wx/2),wx*wy
!
!
!   top: do ix=1,hx ; do iy=1,hy
!     if (mod(ix,200) == 0 .and. mod(iy,200) == 0)write (6,*) ix,iy
!     if (dhdx(ix,iy) == 0.0 .or. dhdy(ix,iy) == 0.0) then
!       cycle
!     endif
!     do iw=1,wx ; do jw=1,wy
!      if  (  lat(ix,iy) > (wlat(jw)+wlat(jw-1))/2.0 &
!     .AND.   lat(ix,iy) < (wlat(jw)+wlat(jw+1))/2.0 &
!     .AND.   lon(ix,iy) > (wlon(iw)+wlon(iw-1))/2.0 &
!     .AND.   lon(ix,iy) < (wlon(iw)+wlon(iw+1))/2.0 )then
!       wpts_box(iw,jw) = wpts_box(iw,jw) + 1
!       wdhdx   (iw,jw) = wdhdx   (iw,jw) + dhdx (ix,iy)
!       wdhdy   (iw,jw) = wdhdy   (iw,jw) + dhdy (ix,iy) 
!      endif
!
!     enddo ; enddo 
!
!   enddo ; enddo top
!
!forall (iw=1:wx , jw=1:wy,  wpts_box(iw,jw) /=0 )
!   wdhdx(iw,jw) = wdhdx(iw,jw) / wpts_box(iw,jw)
!   wdhdy(iw,jw) = wdhdy(iw,jw) / wpts_box(iw,jw)
!    
!end forall
!
!     do iw=1,wx ; do jw=1,wy
!   if (wdhdx(iw,jw) /= 0.0) write (6,*) wdhdx(iw,jw),wdhdy(iw,jw),wlat(jw),wlon(iw)
!     enddo ; enddo 
!
  open (60,file="wrf_orig_oahu_dhdx_dhdy.dat",access='direct',status='unknown',&
        recl=4*wx*wy)!, convert='big_endian')
  write(60,rec=1)dhdx
  write(60,rec=2)dhdy
  write(60,rec=3)h
  close (60)
!
!  open (69,file='dhdx.txt',status='unknown')
!    write(69,'(f8.6,", ")')wdhdx  
!  close (69)
!37011
 write (6,*) "set lat ",wlat(1)," ",wlat(wy)
 write (6,*) "set lon ",wlon(1)," ",wlon(wx)
stop
end




