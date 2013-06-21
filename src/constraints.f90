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

module constr

contains

logical function constraints(c,t)

!---Declare Module usage---

use shared
use plantVar
use inputVar
use energy

implicit none
integer, dimension(nm), intent(in) :: c
integer,                intent(in) :: t
real(kind = prec)                   :: p,u
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
      if(sp(ks,j).eq.zero.and.sp(kr,j).gt.zero) then 
         constraints = .false.
         return
      endif
   endif
enddo

p = chProd(c,t)
u = sum(uCh(t,:))
if(p.lt.u)  then
   constraints = .false.
   return
endif

p = thProd(c,t) 
u = sum(uTh(t,:)) + thSelfCons(c,t)
if(p.lt.u) constraints = .false.

!print*, 'constraints', thProd(c,t), c


end function constraints

!=============================================================

logical function timeConstr(cindex,tState)

use shared
use inputVar
use plantVar

implicit none
integer, dimension(nm), intent(in) :: cindex
real(kind = prec), dimension(2*nm0),intent(in) :: tState
integer                            :: i,j
real(kind = prec)                   :: cc

timeConstr = .true. 

do i=1,nm0
   cc = sp(cindex(i),i)
   if(cc.eq.zero.and.tState(i).gt.zero) then
      if(tState(i).lt.minUpTime(i)) then
         timeConstr = .false.
         exit
      endif
   endif
   j = nm0 + i
   if(cc.gt.zero.and.tState(j).gt.zero) then
      if(tState(j).lt.minDownTime(i)) then
         timeConstr = .false.
         exit
      endif
   endif
enddo

end function timeConstr


!=============================================================

logical function thStorageConstr(oldLevel,c,t)


!---Declare Module usage---

use shared
use plantVar
use inputVar
use energy

implicit none
integer, dimension(nm), intent(in) :: c
integer,                intent(in) :: t
real(kind=prec),        intent(in) :: oldLevel
real(kind=prec)                    :: newLevel

thStorageConstr = .true.

if(capacityTS.le.zero.or.PmaxTS.le.zero) return

newLevel = thStorageLevelUpdate(oldLevel,c,t)

if(newLevel.lt.zero)       thStorageConstr = .false.
if(newLevel.gt.capacityTS) thStorageConstr = .false.
!if(t.eq.nTime) then
!   if(newLevel.ne.eSocTh)  thStorageConstr = .false.
!endif

return

end function thStorageConstr


!=============================================================

logical function elStorageConstr(oldLevel,c,t)

!---Declare Module usage---

use shared
use plantVar
use inputVar
use energy

implicit none
integer, dimension(nm), intent(in) :: c
integer,                intent(in) :: t
real(kind=prec),        intent(in) :: oldLevel
real(kind=prec)                    :: newLevel

elStorageConstr = .true.

if(capacityES.le.zero.or.PmaxES.le.zero) return

newLevel = elStorageLevelUpdate(oldLevel,c,t)

if(newLevel.lt.zero)       elStorageConstr = .false.
if(newLevel.gt.capacityES) elStorageConstr = .false.
if(t.eq.nTime) then
   if(newLevel.ne.eSocEl)  elStorageConstr = .false.
endif

return

end function elStorageConstr

end module constr
