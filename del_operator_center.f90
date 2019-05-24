subroutine gradient(dx,dy,h,delHx,delHy,nx,ny)
implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! This subroutine is used to calculate a horizontal gradient using a center  !!
!! spacial differencing scheme.  For x and y:                                 !!
!!  dh/dx = (h(x+1)-h(x-1)) / 2dx                                             !!
!!  dh/dy = (h(y+1)-h(y-1)) / 2dy                                             !!
!!!!!!!!!!!! Tom Robinson University of Hawaii 2013 ter@hawaii.edu !!!!!!!!!!!!!
!include 'params.h'
integer                       :: nx,ny
integer                       :: ix,iy
real                          :: dx, dy 
real, dimension (nx,ny)       :: h 
real, dimension (nx,ny) :: delHx ,delHy 
 
do ix=1,nx
   do iy=1,ny
!! Check that the value of h exists for a given point
                IF (h(ix,iy) .gt. -1.                                        &
          .AND. h(ix+1,iy) .gt. -1. .AND.  h(ix-1,iy ) .gt. -1. &
          .AND.  h(ix,iy+1 ) .gt. -1. .AND.  h(ix,iy-1 ) .gt. -1.) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!             in y              !!!!!!!!!!!!  
 if (iy==1)then !Forward difference at 1
  delHy(ix,iy )=( h(ix,2 )- h(ix,iy ))/dy
 elseif (iy==ny) then !Forward difference at ny
  delHy(ix,iy )=( h(ix,iy )- h(ix,iy-1 ))/dy
 else !Center difference 
   delHy(ix,iy ) = (  h(ix,iy+1 )- h(ix,iy-1 ) ) / (2.0*dy)
! write (6,*)delHy(ix,iy ), h(ix,iy+1 ),  h(ix,iy-1 ), dy! (  h(ix,iy+1 )- h(ix,iy-1 ) ) / (2.0*dy)
 endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!             in x              !!!!!!!!!!!!  
 if (ix==1)then !Forward difference at 1
  delHx(ix,iy )=( h(2,iy )- h(ix,iy ))/dx
 elseif (ix==nx) then !Forward difference at nx
  delHx(ix,iy )=( h(ix,iy )- h(ix-1,iy ))/dx
 else !Center difference 
   delHx(ix,iy ) = (  h(ix+1,iy )- h(ix-1,iy ) ) / (2.0*dx)  
 endif
                ELSE
                   delHx(ix,iy ) = 0.0 !undefined value if no gradient on level
                   delHy(ix,iy ) = 0.0 !undefined value if no gradient on level
                ENDIF
    enddo
enddo
                            
!write (6,*) h(1,2),delH(1,2)
!write (6,*) (h(ix,2),ix=4,6),delh(5,2),dx

return
end
  
