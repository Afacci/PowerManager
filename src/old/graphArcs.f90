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

subroutine graphArcs(nComb)

!---Declare Module usage---

use inputVar
use graphVar
use plantVar
use economy
use myArithmetic

!---Declare Local Variables---
implicit none

integer , intent(in) :: nComb
integer :: i,j,t,n1,n2,ni,nf,k
integer, parameter :: zero = 0.d0
integer, allocatable, dimension(:) :: cNew, cOld

allocate(nPre(nPoint+1), predCost(nPoint+1,nComb))
allocate(predList(0:nPoint+1,nComb))

predList(:,:) = inan(1.d0)
predCost(:,:) = rnan(1.d0)

n1 = 1
ni  = 0

!---detect the predecessors for each node---
do t=1,nTime+1
   n2 = n1 + nt(t)   - 1
   nf = ni + nt(t-1) - 1 
   do i=n1,n2
      nPre(i) = nt(t-1)
      predList(i,1:nPre(i)) = (/ (k, k=ni,nf) /)
   enddo
   ni = nf + 1
   n1 = n2 + 1
enddo

!do i=1,nPoint+1
!   print*, 'predlIST', predlist(i,:)
!enddo
!print*, 'nPre', nPre

!---associate the cost to each arc---
allocate(cNew(nm), cOld(nm))
do i=1,nPoint+1
   cNew = pointLoad(i,:)
   do j=1,nPre(i)
      k = predList(i,j)
      cOld = pointLoad(k,:)
      predCost(i,j) = pointCost(k) + fireCost(cNew,cOld)
!      write(*,*)  predCost(i,j)
   enddo
!   write(*,*) nPre(i),  predCost(i,:)
enddo

deallocate(cNew,cOld)

return

end subroutine graphArcs

