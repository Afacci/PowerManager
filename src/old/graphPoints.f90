!---------------------------------------------------------------------------
!  
!   PowerManager.  
!
!---------------------------------------------------------------------------
!License
!    This file is part of PowerManager.
!
!    PowerManager is free software; you can redistribute it and/or modify it
!    under the terms of the GNU General Public License as published by the
!    Free Software Foundation; either version 2 of the License, or (at your
!    option) any later version.
!
!    PowerManager is distributed in the hope that it will be useful, but WITHOUT
!    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!    for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with PowerManager; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!Application
!    limCombin
!
!Description
!    limit the number of feasible combinations according to the constraints and
!    associate a cost to each point
!Author 
!     Andrea Facci.
!
!---------------------------------------------------------------------------

subroutine graphPoints(nComb)

!---Declare Module usage---

use interfaces
use inputVar
use plantVar
use graphVar

!---Declare Local Variables---
implicit none

!integer, dimension(nComb,nm), intent(in)      :: c
integer                     , intent(in)      :: nComb 
real(kind(1.d0)), allocatable, dimension(:,:) :: cl_
real(kind(1.d0)), allocatable, dimension(:)   :: cost_, time_
integer :: i,j,n
integer , allocatable, dimension(:) :: load
logical :: v

allocate(load(nm),nt(0:nTime+1))
n = nComb*nTime
allocate(cost_(n),cl_(n,nm),time_(n))

nt(0)   = 1
n = 0
do i=1,nTime
   nt(i) = 0
   do j=1,nComb
      load = cRef(j,:)
      v    = constraints(load,i)
      if(v) then
         n        = n + 1
         time_(n) = i
         nt(i)    = nt(i) + 1
         cl_(n,:) = load
         cost_(n) = objFunction(load,i,obj)
      endif
   enddo
enddo
nt(nTime+1) = 1

allocate(pointCost(0:n+1),pointLoad(0:n+1,nm),pointTime(0:n+1))

nPoint           = n
pointCost(0)     = 0.d0
pointCost(1:n)   = cost_(1:n)
pointCost(n+1)   = 0.d0 
pointLoad(0,:)   = startPoint(:)
pointLoad(1:n,:) = cl_(1:n,:)
pointLoad(n+1,:) = startPoint(:)
pointTime(0)     = 0
pointTime(1:n)   = time_(1:n)
pointTime(n+1)   = nTime + 1

deallocate(load)
deallocate(cost_,cl_,time_)

return

end subroutine graphPoints
