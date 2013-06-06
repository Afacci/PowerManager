real(kind=prec) function timestep()

  use shared 
  use plantvar
  use inputvar

  implicit none

  integer  :: i

  !---time step in seconds---
  dt(0) = dt1*3.6e3
  do i=1,nTime - 1
     dt(i) = 3.6e3*(time(i+1) - time(i))      
  enddo
  dt(nTime) = dt1*3.6e3

  return

end function timestep
