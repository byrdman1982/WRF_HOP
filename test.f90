program test

integer, parameter :: nx = 219, ny=169
real, dimension (nx,ny) :: dhdx,dhdy


     open (unit=29,file='/share/huina/ter/disertation/' // &
               'WRF_HOP_combo/wrf_grid_oahu_dhdx_dhdy.dat', &
            status='old',access='direct',recl=4*219*169)
        read (29,rec=1)dhdx
        read (29,rec=2)dhdy  
      close (29)      

!     write (6,*)dhdx(5,182),dhdy(5,182)
!     write (6,*)dhdx(6,182),dhdy(6,182)
!write (5,*)

     DO j = 1,ny
     DO i = 1,nx
!write (6,*)i,j,dhdx(i,j),dhdy(i,j)
          if(dhdx(i,j) .ne. 0.0 .AND. dhdy(i,j) .ne. 0.0) then

               write (6,*) dhdx(i,j),dhdy(i,j),i,j
          endif
      ENDDO
!               write (6,*)i,j
      ENDDO

stop
end
