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
!    along with OpenFOAM; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!>\file constraints.f90
!>\brief
!> static constraints.
!>\details
!> This file contains a subroutine that checks if the plant respects the energy constraints for a given
!> set-point and time-step.
!\author 
!     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief
!> static constraints
!>\details
!> This function checks if the plant respects the energy constraints for a given
!> set-point and time-step. The constraints are:\n
!> - Thermal Power: \f$ \sum U_{th} + U_{th}^{self} \le \sum P_{th}\f$
!> - Chilling Power:\f$ \sum U_{ch}  \le \sum P_{ch}\f$
!> - Electrical Power : \f$ \sum U_{el} + U_{el}^{self} \le \sum P_{el}\f$ only
!> if the pant is bnot grid connected
!>\param[in] c index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t time step index. Note t=x meas the x'th time step from the
!>\author Andrea Facci
logical function constraints(c,t)

!---Declare Module usage---
use plantVar
use inputVar
use energy

implicit none
integer, dimension(nm), intent(in) :: c
integer,                intent(in) :: t
real(kind(1.d0))                   :: p,u
integer                            :: i,j,kr,ks

constraints = .true.
!---Declare Local Variables---

!---eliminate all the combination where the top plant is off and the bottom
!on....
do i=is(iB),ie(iB)
   kr = c(i)
   if(pes(i).ne.'fuel') then
      j = eSource(i)
      ks = c(j)
      if(sp(ks,j).eq.0.d0.and.sp(kr,j).gt.0.d0) then 
         constraints = .false.
         return
      endif
   endif
enddo

p = chProd(c)
u = sum(uCh(t,:))
if(p.lt.u)  then
   constraints = .false.
   return
endif

p = thProd(c) 
u = sum(uTh(t,:)) + thSelfCons(c)
if(p.lt.u) constraints = .false.


end function constraints

!=============================================================

logical function timeConstr(cindex,tState)

use inputVar
use plantVar

implicit none
integer, dimension(nm), intent(in) :: cindex
real(kind(1.d0)), dimension(2*nm),intent(in) :: tState
integer                            :: i,j
real(kind(1.d0))                   :: cc

timeConstr = .true. 

do i=1,nm
   cc = sp(cindex(i),i)
   if(cc.eq.0.d0.and.tState(i).gt.0.d0) then
      if(tState(i).lt.minUpTime(i)) then
         timeConstr = .false.
         exit
      endif
   endif
   j = nm + i
   if(cc.gt.0d0.and.tState(j).gt.0.d0) then
      if(tState(j).lt.minDownTime(i)) then
         timeConstr = .false.
         exit
      endif
   endif
enddo

end function timeConstr
