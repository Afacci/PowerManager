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
!>\file performances.f90
!>\brief calculates the performances of a single equipment. 
!>\details calculates the performances of a single equipment.  .
!>\author 
!>  Andrea Facci.
!
!---------------------------------------------------------------------------

subroutine performances(c, cOld, equip, num,t, pEl, pTh, pCh, eIn, mf, cfu,cm,cOn) 

!---Declare Module usage---

use shared
use inputVar
use plantVar

!---Declare Local Variables---
implicit none

integer, intent(in) :: c, num, t
integer, intent(in), optional :: cOld
character(len=*), intent(in) :: equip
real(kind = prec), intent(out), optional :: pEl, pTh, pCh, eIn, mf, cfu,cm,cOn

integer :: i
real(kind = prec) :: pEl_, pIn_, mf_, pTh_, pCh_
real(kind = prec) :: cc, ccO

!--- function body

if(present(cOn).and.(.not.present(cOld))) then  
   print*, 'Fatal Error in  performances call. Set-point at the previoius time step needed to &
            compute starting cost.'
   stop
endif

select case(equip)
   case('Trigeneration')
     i = num + is(iT) - 1
     cc = sp(c, i)
     pEl_ = pMax(i)*cc*envCorr(t,i,4)
     if(present(pTh).or.present(pCh).or.present(eIn).or.present(mf).or.present(cfu)) then
        pIn_ = pEl_/(etaEl(c,i)*envCorr(t,i,1))
     endif
     if(present(pEl)) pEl = pEl_
     if(present(pTh)) pTh = pIn_*etaTh(c,i)*envCorr(t,i,2)
     if(present(pCh)) pCh = pIn_*etaCh(c,i)*envCorr(t,i,3)
     if(present(eIn)) eIn = Pin_
     if(present(mf))  mf  = pIn_/lhv(i)
     if(present(cfu))  cfu  = pIn_*cf(i)/lhv(i)*dt(t)
     if(present(cm)) then
        if(cc.gt.zero) then
           cm = OeMcost(i)*dt(t)
        else
           cm = zero
        endif
      endif
      if(present(cOn)) then
         ccO = sp(cOld,i)
         if(cc.gt.zero.and.ccO.eq.zero) then
           cOn = onOffCost(i)
         else
           cOn = zero
         endif
      endif
   case('Boiler')
     i = num + is(iB) - 1
     if(present(pEl)) then 
        print*, 'Fatal Error in performaces call. Boilers do not support Electrical Power'
        stop
     endif
     if(present(pCh)) then 
        print*, 'Fatal Error in performaces call. Boilers do not support Chilling Power' 
        stop
     endif
     if(present(pCh)) then 
        print*, 'Fatal Error in performaces call. Boilers do not support Chilling efficiency'
        stop
     endif
     cc = sp(c, i)
     pTh_ = pMax(i)*cc*envCorr(t,i,4)
     if(present(eIn).or.present(mf).or.present(cfu)) then
        pIn_ = pTh_/(etaTh(c,i)*envCorr(t,i,2))
     endif
     if(present(mf).or.present(cfu))  then 
        if(pes(i).eq.'fuel') then
           mf_  = pIn_/lhv(i)
        else
           mf_ = zero
        endif
     endif
     if(present(pTh)) pTh = pTh
     if(present(eIn)) eIn = Pin_
     if(present(mf))  mf  = mf_
     if(present(cfu)) cfu = mf_*cf(i)*dt(i)
     if(present(cm)) then
        if(cc.gt.zero) then
           cm = OeMcost(i)*dt(t)
        else
           cm = zero
        endif
      endif
      if(present(cOn)) then
         ccO = sp(cOld,i)
         if(cc.gt.zero.and.ccO.eq.zero) then
           cOn = onOffCost(i)
         else
           cOn = zero
         endif
      endif
   case('Chiller')
     i = num + is(iC) - 1
     if(present(pEl)) then 
        print*, 'Fatal Error in performaces call. Chillers do not support Electrical Power'
        stop
     endif
     if(present(pTh)) then 
        print*, 'Fatal Error in performaces call. Chillers do not support Thermal Power'   
        stop
     endif
     if(present(pTh)) then 
        print*, 'Fatal Error in performaces call. Chillers do not support Thermal efficiency'
        stop
     endif
     if(present(mf))  then
        print*, 'Fatal Error in performaces call. Chillers does not support fuel mass flow'
        stop
     endif
     pCh_ = pMax(i)*cc*envCorr(t,i,4)
     if(present(pCh)) pCh = pCh_
     if(present(eIn)) eIn = pCh_/(etaCh(c,i)*envCorr(t,i,3))
     if(present(cm)) then
        if(cc.gt.zero) then
           cm = OeMcost(i)*dt(t)
        else
           cm = zero
        endif
      endif
      if(present(cOn)) then
         ccO = sp(cOld,i)
         if(cc.gt.zero.and.ccO.eq.zero) then
           cOn = onOffCost(i)
         else
           cOn = zero
         endif
      endif
end select

 end subroutine 
