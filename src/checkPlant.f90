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
!>\file checkPlant.f90
!>\brief Check the power plant coherence with the energy demand.
!>\author 
!  Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief 
!> Check the power plant coherence with the energy demand.
!>\details
!> Check the power plant coherence with the energy demand. Specifically the
!> maximum thermal and chilling power must be higher than the relative energy
!> demand. Thermal load includes also themal energy needed by absorption
!> chillers. If the plant is not grid connected also rated electrical power must
!> be grater than maximum electrical demand.
!>\author 
!  Andrea Facci.

subroutine checkPlant

!---Declare Module usage---
use shared
use inputVar
use plantVar
use interfaces

!---Declare Local Variables---
implicit none

real(kind = prec)  :: pEl, pCh, pTh, pThT, uEmax, uCmax, uTmax, &
                     uTf, uEf, cMax, pIn
real(kind = prec), allocatable,dimension(:) :: uTht,uElt,uCht
integer           :: n,  i

!--subroutine body-----

allocate(uTht(nTime),uElt(nTime),uCht(nTime))

!-evaluation of maximum power for each energy vector.
pEl = zero
if(nTrig.gt.0) then
   do i=1,nTrig
      cMax = maxval(spT(:,i))
      pEl  = PmaxT(i)*cMax + pEl
   enddo
endif
pCh = zero
if(nChi.gt.0) then
   do i=1,nChi
      cMax = maxval(spC(:,i))
      pCh  = pCh + PmaxC(i)*cMax
   enddo
endif
pThT = zero
if(nTrig.gt.0) then
   do i=1,nTrig
      n    = nSpT(i)
      cMax = maxval(spT(:,i))
      pIn  = cMax*PmaxT(i)/etaElT_(n,i)
      pThT = pThT + pIn*etaThT_(n,i)
   enddo
endif
pTh = zero
if(nBoi.gt.0) then
   do i=1,nBoi
      cMax = maxval(spB(:,i))
      pTh = pTh + PmaxB(i)*cMax 
   enddo
endif
pTh = pTh + pThT

!-evaluation of maximum load for each vector.
do i=1,Ntime
    uCht(i) = sum(uCh(i,:))
enddo
uCmax = maxval(uCht)
uTf   = zero
uEf   = zero
if(nChi.gt.0) then
   do i=1,nChi
      n = nSpC(i)
      if(tecC(i).eq.'Absorption') uTf = uTf + uCmax/etaC_(n,i)
      if(tecC(i).eq.'Mechanical') ueF = uEf + uCmax/etaC_(n,i)
   enddo
endif
do i=1,nTime
    uTht(i) = sum(uTh(i,:))
    uElt(i) = sum(uEl(i,:))
enddo
uTmax = uTf + maxval(uTht)
uEmax = uEf + maxval(uElt)

!-abort the execution when one of the loads cannot be satisfied.
if(uTmax.gt.pTh) call abortExecution(i=7,j=1,r1=pTh, r2=uTmax )
if(uCmax.gt.pCh) call abortExecution(i=7,j=2,r1=pCh, r2=uCmax)
if(uEmax.gt.pEl.and.gridConnection.eq.'StandAlone') call abortExecution(i=7,j=2,r1=pEl, r2=uEmax)

do i=1,nTrig+nBoi+nChi
   if(upTime0(i).eq.0.and.startPoint(i).gt.zero) call abortExecution(15,i,word='upTime',r1=upTime0(i),r2=startPoint(i))
   if(upTime0(i).gt.0.and.startPoint(i).eq.zero) call abortExecution(15,i,word='upTime',r1=upTime0(i),r2=startPoint(i))
   if(downTime0(i).eq.0.and.startPoint(i).eq.zero) call abortExecution(15,i,word='DownTime',r1=downTime0(i),r2=startPoint(i))
   if(downTime0(i).gt.0.and.startPoint(i).gt.zero) call abortExecution(15,i,word='DownTime',r1=downTime0(i),r2=startPoint(i))
enddo

deallocate(uTht,uElt,uCht)

return

end subroutine checkPlant
