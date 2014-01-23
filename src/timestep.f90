!real(kind=prec) function timestep()
function timestep()

  use shared 
  use inputvar, only : nTime, time, dt1

  implicit none

  integer  :: i
  real(kind=prec), dimension(0:nTime) :: timestep

  !---time step in seconds---
  timestep(0) = dt1*3.6e3
  do i=1,nTime - 1
     timestep(i) = 3.6e3*(time(i+1) - time(i))      
  enddo
  timestep(nTime) = dt1*3.6e3

end function timestep
