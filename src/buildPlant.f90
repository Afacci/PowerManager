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
!>\file buildPlant.f90
!>\brief 
!> Collects all the informations relative to the power plant.
!>\author 
!  Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief 
!> Collects all the informations relative to the power plant.
!>\details
!> Collects all the informations relative to the power plant starting from the 
!> Inputs of the single machinery. Calculates the efficiencies for each given
!> set-point and store them in a single array.
!> Perform units of measure conversions when necessary.
!>\author 
!  Andrea Facci.
subroutine buildPlant

!---Declare Module usage---

use cmdVar
use inputVar
use plantVar
use mathTools
use interfaces
use myArithmetic

!---Declare Local Variables---
implicit none
integer :: i,j, maxsp, k, k1, k2
integer, dimension(2)  :: ext1,ext2,ext3, ext
character(len=100)     :: word, cdummy
real(kind = prec)       :: kJ_kWh
real(kind = prec), allocatable, dimension(:,:) :: upTimeVinc, downTimeVinc
integer,dimension(nm):: dtv, utv
real(kind = prec), dimension(1) :: rdummy1, rdummy2
real(kind = prec), dimension(:,:,:), allocatable :: tCorr, pCorr
real(kind = prec), dimension(:,:), allocatable :: aCorr

!--subroutine body-----

call allocateVar(16)

!-- calculate all the efficiencies for the given set points.
if(nTrig.ge.0) then
   do i=1,nTrig
      etaElT_(:,i) = interpolation(etaElT(:,1,i), etaElT(:,2,i), nEtaElT(i), spT(:,i),nSpT(i), ext1)     
      if(ext1(1).eq.1.and.(.not.silent)) call warning(2,1,i, word='Electrical')
      if(ext1(2).eq.1.and.(.not.silent)) call warning(3,1,i, word='Electrical')
      etaThT_(:,i) = interpolation(etaThT(:,1,i), etaThT(:,2,i), nEtaThT(i), spT(:,i),nSpT(i), ext2) 
      if(ext2(1).eq.1.and.(.not.silent)) call warning(2,1,i, word='Thermal')
      if(ext2(2).eq.1.and.(.not.silent)) call warning(3,1,i, word='Thermal')
      etaChT_(:,i) = interpolation(etaChT(:,1,i), etaChT(:,2,i), nEtaChT(i), spT(:,i),nSpT(i), ext3)
      if(ext3(1).eq.1.and.(.not.silent)) call warning(2,1,i, word='Chilling')
      if(ext3(2).eq.1.and.(.not.silent)) call warning(3,1,i, word='Chilling')
   enddo
endif

if(nBoi.gt.0) then
   do i=1,nBoi
      etaB_(:,i) = interpolation(etaB(:,1,i), etaB(:,2,i), nEtaB(i), spB(:,i),nSpB(i), ext) 
      if(ext(1).eq.1.and.(.not.silent)) call warning(2,2,i, word='Thermal')
      if(ext(2).eq.1.and.(.not.silent)) call warning(3,2,i, word='Thermal')
   enddo
endif

if(nChi.gt.0) then
   do i=1,nChi
      etaC_(:,i) = interpolation(etaC(:,1,i), etaC(:,2,i), nEtaC(i), spC(:,i),nSpC(i), ext) 
      if(ext(1).eq.1.and.(.not.silent)) call warning(2,3,i, word='Chilling')
      if(ext(2).eq.1.and.(.not.silent)) call warning(3,3,i, word='Chilling')
   enddo
endif

!--- check that the power plant is able to satisfie the required loads
call checkPlant

nm  = nTrig + nBoi + nChi                    !total number of machinery
nSpTot = 0
if(nBoi.gt.0) nSpTot = nSpTot + sum(nSpT)
if(nBoi.gt.0) nSpTot = nSpTot + sum(nSpB)
if(nChi.gt.0) nSpTot = nSpTot + sum(nSpC)
call allocateVar(17)
!-starting index for each kind of equip. in the set-point vector.
iT = 1
iB = 2
iC = 3
is(iT) = 1                   !Trigeneration
ie(iT) = nTrig
is(iB) = is(iT) + nTrig      !Boilers
ie(iB) = ie(iT) + nBoi
is(iC) = is(iB) + nBoi       !Chillers
ie(iC) = ie(iB) + nChi

!--- build the power plant "connections", that is the set-point, power, and
!--- efficiencies vectors of the global power plant.
j = 0
do i=is(iT),ie(iT)
   j = j + 1 
   nSp(i)  = nSpT(j)
enddo
j=0
do i=is(iB),ie(iB)
   j = j + 1 
   nSp(i)  = nSpB(j)
enddo
j=0
do i=is(iC),ie(iC)
   j = j + 1 
   nSp(i)  = nSpC(j)
enddo

call allocateVar(18)

sp(:,:) = rNan(rVal)
eSource(:) = -1

!---Estimate environmental condition corrections for each time-step---
call allocateVar(32)

allocate(pCorr(nTime,nm,4), tCorr(nTime,nm,4), aCorr(nm,4))
pCorr = rNan(rVal)
tCorr = rNan(rVal)
aCorr = rNan(rVal)
do i=1,nTime
   k = 0
   do j=is(iT),ie(iT)
      k = k + 1
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,2,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,1) = rdummy2(1)
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,3,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,4,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,5,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,4) = rdummy2(1)

      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,2,k), npcT(k), pAmb(i),1)
      pCorr(i,j,1) = rdummy2(1)
      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,3,k), npcT(k), pAmb(i),1)
      pCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,4,k), npcT(k), pAmb(i),1)
      pCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,5,k), npcT(k), pAmb(i),1)
      pCorr(i,j,4) = rdummy2(1)
   enddo
   k = 0
   do j=is(iB),ie(iB)
      k = k + 1
      rdummy2= interpolation(tempCorrB(:,1,k), tempCorrB(:,2,k), ntcB(k), tAmb(i),1)
      tCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(tempCorrB(:,1,k), tempCorrB(:,3,k), ntcB(k), tAmb(i),1)
      tCorr(i,j,4) = rdummy2(1)

      rdummy2= interpolation(presCorrB(:,1,k), presCorrB(:,2,k), npcB(k), pAmb(i),1)
      pCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(presCorrB(:,1,k), presCorrB(:,3,k), npcB(k), pAmb(i),1)
      pCorr(i,j,4) = rdummy2(1)
   enddo
   k = 0
   do j=is(iC),ie(iC)
      k = k + 1
      rdummy2= interpolation(tempCorrC(:,1,k), tempCorrC(:,2,k), ntcC(k), tAmb(i),1)
      tCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(tempCorrC(:,1,k), tempCorrC(:,3,k), ntcC(k), tAmb(i),1)
      tCorr(i,j,4) = rdummy2(1)

      rdummy2= interpolation(presCorrC(:,1,k), presCorrC(:,2,k), npcC(k), pAmb(i),1)
      pCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(presCorrC(:,1,k), presCorrC(:,3,k), npcC(k), pAmb(i),1)
      pCorr(i,j,4) = rdummy2(1)
   enddo
enddo

k = 0
do j=is(iT),ie(iT)
   k = k + 1
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,2,k), nacT(k), altitude,1)
   aCorr(j,1) = rdummy2(1)
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,3,k), nacT(k), altitude,1)
   aCorr(j,2) = rdummy2(1)
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,4,k), nacT(k), altitude,1)
   aCorr(j,3) = rdummy2(1)
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,5,k), nacT(k), altitude,1)
   aCorr(j,4) = rdummy2(1)
enddo
k = 0
do j=is(iB),ie(iB)
   k = k + 1
   rdummy2= interpolation(altCorrB(:,1,k), altCorrB(:,2,k), nacB(k), altitude,1)
   aCorr(j,2) = rdummy2(1)
   rdummy2= interpolation(altCorrB(:,1,k), altCorrB(:,3,k), nacB(k), altitude,1)
   aCorr(j,4) = rdummy2(1)
enddo
k = 0
do j=is(iC),ie(iC)
   k = k + 1
   rdummy2= interpolation(altCorrC(:,1,k), altCorrC(:,2,k), nacC(k), altitude,1)
   aCorr(j,3) = rdummy2(1)
   rdummy2= interpolation(altCorrC(:,1,k), altCorrC(:,3,k), nacC(k), altitude,1)
   aCorr(j,4) = rdummy2(1)
enddo

envCorr = rNaN(rVal)
do i=1,nTime
   do j = 1,nm
      do k = 1,4 
         envCorr(i,j,k) = tCorr(i,j,k)*pCorr(i,j,k)*aCorr(j,k)
      enddo
   enddo
enddo
deallocate(aCorr,pCorr,tCorr)

j = 0
do i=is(iT),ie(iT)
   j = j + 1 
   sp(1:nSp(i),i) = spT(:,j)
   Pmax(i)  = PmaxT(j)
   lhv(i)   = fuelLhvT(j)
   cf(i)    = fuelCostT(j)
   pes(i)   = 'fuel'
   etaEl(1:nSp(i),i) = etaElT_(1:nSp(i),j)
   etaTh(1:nSp(i),i) = etaThT_(1:nSp(i),j)
   etaCh(1:nSp(i),i) = etaChT_(1:nSp(i),j)
   onOffCost(i) = fireCostT(j)
   OeMCost(i)   = maintCostT(j)/3.6e3
   minUpTime(i)   = minUpTimeT(j)*3.6e3
   minDownTime(i) = minDownTimeT(j)*3.6e3
   tec(i) = tecT(j)
   if(strategy.ne.'Optimized') then
      if(trigPriority(j).gt.nTrig) call abortExecution(21,j)
      TrigPriority(j) = TrigPriority(j) + is(iT) - 1
   endif
   do k=1,nSpT(j)
      cr(k,i) = k
   enddo
enddo
j=0
do i=is(iB),ie(iB)
   j = j + 1 
   sp(1:nSp(i),i)    = spB(:,j)
   Pmax(i)           = PmaxB(j)
   lhv(i)            = fuelLhvB(j)
   cf(i)             = fuelCostB(j)
   onOffCost(i)      = fireCostB(j)
   OeMCost(i)        = maintCostB(j)/3.6e3
   etaEl(1:nSp(i),i) = -1
   etaTh(1:nSp(i),i) = etaB_(1:nSp(i),j)
   etaCh(1:nSp(i),i) = -1
   minUpTime(i)      = minUpTimeB(j)*3.6e3
   minDownTime(i)    = minDownTimeB(j)*3.6e3
   do k=1,nSpB(j)
      cr(k,i) = k
   enddo
   cdummy = adjustl(tecB(j))
   k      = index(cdummy,'@')
   if(cdummy(1:k-1).eq.'Trig') then
      pes(i) = 'heat'
      tec(i) = 'Recovery'
      read(cdummy(k+1:),*) eSource(i) 
   else
      pes(i) = 'fuel'
      eSource(i) = -1
      tec(i) = 'Fuel'
   endif
   if(strategy.ne.'Optimized') then
      if(BoiPriority(j).gt.nBoi) call abortExecution(22,j)
      BoiPriority(j) = BoiPriority(j) + is(iB) - 1
   endif
enddo
j=0
do i=is(iC),ie(iC)
   j = j + 1 
   sp(1:nSp(i),i) = spC(1:nSp(i),j)
   Pmax(i) = PmaxC(j)
   lhv(i)  = -1
   cf(i)   = -1
   etaEl(1:nSp(i),i) = -1
   etaTh(1:nSp(i),i) = -1
   onOffCost(i)      = fireCostC(j)
   OeMCost(i)        = maintCostC(j)/3.6e3
   minUpTime(i)      = minUpTimeC(j)*3.6e3
   minDownTime(i)    = minDownTimeC(j)*3.6e3
   etaCh(1:nSp(i),i) = etaC_(1:nSp(i),j)
   if(tecC(j).eq.'Absorption') pes(i) = 'heat'
   if(tecC(j).eq.'Mechanical') pes(i) = 'elec'
   do k=1,nSpC(j)
      cr(k,i) = k
   enddo
   tec(i) = tecC(j)
   if(strategy.ne.'Optimized') then
      if(ChiPriority(j).gt.nChi) call abortExecution(23,j)
      ChiPriority(j) = chiPriority(j) + is(iC) - 1
   endif
enddo

!---time-dependent constraints---
call allocateVar(21)
k1 = maxval(minUpTime)/3.6e3 + 1
k2 = maxval(minDownTime)/3.6e3 + 1
allocate(upTimeVinc(max(k1,k2),nm), downTimeVinc(max(k1,k2),nm))
timeVinc(:,:) = zero
do i=1,nm
   nTv(i) = minUpTime(i)/3.6e3 + 1
   do j=1,ntv(i)
      timeVinc(j,i) = (j - 1)*3.6e3
   enddo
enddo
do i=1,nm
   k = nm + i
   nTv(k) = minDownTime(i)/3.6e3 + 1
   do j=1,ntv(k)
      timeVinc(j,k) = (j - 1)*3.6e3
   enddo
enddo

!---time step in seconds---
dt(0) = dt1*3.6e3
do i=1,nTime - 1
   dt(i) = 3.6e3*(time(i+1) - time(i))      
enddo
dt(nTime) = dt1*3.6e3
upTime0 = upTime0*3.6e3
downTime0 = downTime0*3.6e3

!--- convert all the prices in €/kJ---
kJ_kWh = 1.0/3.6e3
gridBuyCost(:)  = gridBuyCost(:)*kJ_kWh
gridSellCost(:) = gridSellCost(:)*kJ_kWh
cEl(:,:)        = cEl(:,:)*kJ_kWh
cTh(:,:)        = cTh(:,:)*kJ_kWh
cCh(:,:)        = cCh(:,:)*kJ_kWh
c_st = c_st*kJ_kWh
c_tr = c_tr*kJ_kWh

call deallocateVar(1)

end subroutine buildPlant
