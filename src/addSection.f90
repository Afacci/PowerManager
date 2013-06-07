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
!>\file 
!>\brief File  prototype. 
!>\details this is the prototype for all the files of the PowerManger project.
!> Copy, rename, and modify this this file to create a new procedure or module.
!>\Author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

module addSection

!---Declare Module usage---

use shared
use plantVar
use inputVar
use cmdVar
use interfaces
use mathTools

contains

   subroutine addTrigenerative


      implicit none
      integer :: i, j, k, l, kk
      integer, dimension(2)  :: ext1,ext2,ext3
      character(len=100)     :: word
      real(kind=prec), dimension(4) :: tCorr, pCorr
      real(kind=prec), dimension(ntime,4) ::aCorr


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
      
      j = 0
      do i=is(iT),ie(iT)
         j = j + 1 
         nSp(i)  = nSpT(j)
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
         if(iPEC) then
            pef(i) = pefT(j)
            pecOn(i) = pecOnT(j)
         endif
!         !---time constraints
!         nTv(j) = minUpTime(j)/3.6e3 + 1
!         do k=1,ntv(j)
!            timeVinc(k,j) = (k - 1)*3.6e3
!         enddo
!         !min non-running time
!         kk = nm0 + j
!         nTv(kk) = minDownTime(j)/3.6e3 + 1
!         do k=1,ntv(kk)
!            timeVinc(k,kk) = (k - 1)*3.6e3
!         enddo
      enddo


      k = 0
      do j=is(iT),ie(iT)
         k = k + 1
         aCorr(i,1) = scalarInterp(altCorrT(:,1,k), altCorrT(:,2,k), nacT(k), altitude(1))
         aCorr(i,2) = scalarInterp(altCorrT(:,1,k), altCorrT(:,3,k), nacT(k), altitude(1))
         aCorr(i,3) = scalarInterp(altCorrT(:,1,k), altCorrT(:,4,k), nacT(k), altitude(1))
         aCorr(i,4) = scalarInterp(altCorrT(:,1,k), altCorrT(:,5,k), nacT(k), altitude(1))
      enddo

      do i=1,nTime
         k = 0
         do j=is(iT),ie(iT)
            k = k + 1
            tCorr(1) = scalarInterp(tempCorrT(:,1,k), tempCorrT(:,2,k), ntcT(k), tAmb(i))
            tCorr(2) = scalarInterp(tempCorrT(:,1,k), tempCorrT(:,3,k), ntcT(k), tAmb(i))
            tCorr(3) = scalarInterp(tempCorrT(:,1,k), tempCorrT(:,4,k), ntcT(k), tAmb(i))
            tCorr(4) = scalarInterp(tempCorrT(:,1,k), tempCorrT(:,5,k), ntcT(k), tAmb(i))
  
            pCorr(1) = scalarInterp(presCorrT(:,1,k), presCorrT(:,2,k), npcT(k), pAmb(i))
            pCorr(2) = scalarInterp(presCorrT(:,1,k), presCorrT(:,3,k), npcT(k), pAmb(i))
            pCorr(3) = scalarInterp(presCorrT(:,1,k), presCorrT(:,4,k), npcT(k), pAmb(i))
            pCorr(4) = scalarInterp(presCorrT(:,1,k), presCorrT(:,5,k), npcT(k), pAmb(i))

            do l = 1,4 
               envCorr(i,j,l) = tCorr(l)*pCorr(l)*aCorr(i,l)
            enddo
         enddo

      enddo

   end subroutine addTrigenerative

!=========================================================================================

   subroutine addBoilers
     
      implicit none

      integer :: i, j, k, l, kk
      character(len=100)     :: cdummy
      integer, dimension(2)  :: ext
      character(len=100)     :: word
      real(kind=prec), dimension(2) :: tCorr, pCorr
      real(kind=prec), dimension(ntime,2) ::aCorr


      if(nBoi.gt.0) then
         do i=1,nBoi
            etaB_(:,i) = interpolation(etaB(:,1,i), etaB(:,2,i), nEtaB(i), spB(:,i),nSpB(i), ext) 
            if(ext(1).eq.1.and.(.not.silent)) call warning(2,2,i, word='Thermal')
            if(ext(2).eq.1.and.(.not.silent)) call warning(3,2,i, word='Thermal')
         enddo
      endif

      j=0
      do i=is(iB),ie(iB)
         j = j + 1 
         nSp(i)  = nSpB(j)
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
         if(iPEC) then
            pef(i) = pefB(j)
            pecOn(i) = pecOnB(j)
         endif
 !        !---time constraints
 !        nTv(j) = minUpTime(j)/3.6e3 + 1
 !        do k=1,ntv(j)
 !           timeVinc(k,j) = (k - 1)*3.6e3
 !        enddo
 !        !min non-running time
 !        kk = nm0 + j
 !        nTv(kk) = minDownTime(j)/3.6e3 + 1
 !        do k=1,ntv(kk)
 !           timeVinc(k,kk) = (k - 1)*3.6e3
 !        enddo
      enddo

      k = 0
      do j=is(iB),ie(iB)
         k = k + 1
         aCorr(i,1) = scalarInterp(altCorrB(:,1,k), altCorrB(:,2,k), nacB(k), altitude(1))
         aCorr(i,2) = scalarInterp(altCorrB(:,1,k), altCorrB(:,3,k), nacB(k), altitude(1))
      enddo

      do i=1,nTime
         k = 0
         do j=is(iB),ie(iB)
            k = k + 1
            tCorr(1) = scalarInterp(tempCorrB(:,1,k), tempCorrB(:,2,k), ntcB(k), tAmb(i))
            tCorr(2) = scalarInterp(tempCorrB(:,1,k), tempCorrB(:,3,k), ntcB(k), tAmb(i))
  
            pCorr(1) = scalarInterp(presCorrB(:,1,k), presCorrB(:,2,k), npcB(k), pAmb(i))
            pCorr(2) = scalarInterp(presCorrB(:,1,k), presCorrB(:,3,k), npcB(k), pAmb(i))

            envCorr(i,j,2) = tCorr(1)*pCorr(1)*aCorr(i,1)
            envCorr(i,j,4) = tCorr(2)*pCorr(2)*aCorr(i,2)
         enddo
      enddo
   
   end subroutine addBoilers

!=========================================================================================
  
   subroutine addChillers

      implicit none

      integer :: j, i, k, l, kk
      integer, dimension(2)  :: ext
      character(len=100)     :: word
      real(kind=prec), dimension(2) :: tCorr, pCorr
      real(kind=prec), dimension(ntime,2) ::aCorr


      if(nChi.gt.0) then
         do i=1,nChi
            etaC_(:,i) = interpolation(etaC(:,1,i), etaC(:,2,i), nEtaC(i), spC(:,i),nSpC(i), ext) 
            if(ext(1).eq.1.and.(.not.silent)) call warning(2,3,i, word='Chilling')
            if(ext(2).eq.1.and.(.not.silent)) call warning(3,3,i, word='Chilling')
         enddo
      endif

      j=0
      do i=is(iC),ie(iC)
         j = j + 1 
         nSp(i)  = nSpC(j)
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
 !        !---time constraints
 !        nTv(j) = minUpTime(j)/3.6e3 + 1
 !        do k=1,ntv(j)
 !           timeVinc(k,j) = (k - 1)*3.6e3
 !        enddo
 !        !min non-running time
 !        kk = nm0 + j
 !        nTv(kk) = minDownTime(j)/3.6e3 + 1
 !        do k=1,ntv(kk)
 !           timeVinc(k,kk) = (k - 1)*3.6e3
 !        enddo
      enddo


      k = 0
      do j=is(iC),ie(iC)
         k = k + 1
         aCorr(i,1) = scalarInterp(altCorrC(:,1,k), altCorrC(:,2,k), nacC(k), altitude(1))
         aCorr(i,2) = scalarInterp(altCorrC(:,1,k), altCorrC(:,3,k), nacC(k), altitude(1))
      enddo

      do i=1,nTime
         k = 0
         do j=is(iC),ie(iC)
            k = k + 1
            tCorr(1) = scalarInterp(tempCorrC(:,1,k), tempCorrC(:,2,k), ntcC(k), tAmb(i))
            tCorr(2) = scalarInterp(tempCorrC(:,1,k), tempCorrC(:,3,k), ntcC(k), tAmb(i))
  
            pCorr(1) = scalarInterp(presCorrC(:,1,k), presCorrC(:,2,k), npcC(k), pAmb(i))
            pCorr(2) = scalarInterp(presCorrC(:,1,k), presCorrC(:,3,k), npcC(k), pAmb(i))

            envCorr(i,j,3) = tCorr(1)*pCorr(1)*aCorr(i,1)
            envCorr(i,j,4) = tCorr(2)*pCorr(2)*aCorr(i,2)
         enddo
      enddo


   end subroutine addChillers

!=========================================================================================

   subroutine addThermalStorage

     implicit none
     
     integer :: i, j ,k 
     real(kind=prec) :: dsoc, dsp, dtmin

     if(capacityTS.gt.zero) then
         j = is(iTS) 
         nSp(is(iTS)) = 2*nSpTS + 1
         Pmax(j) = PmaxTs
         sp(1,j) = -1
         cr(1,j) = 1
         dsp = 1.0/nSpTs
         do i=2,nSpTS
            sp(i,j) = sp(i-1,j) + dsp
            cr(i,j) = i
         enddo
         sp(nSpTS + 1,j) = zero
         cr(nSpTS + 1,j) = nSpTs + 1
         do i= nSpTS + 2, nSp(j)
            sp(i,j) = sp(i-1,j) + dsp
            cr(i,j) = i
         enddo
         iSocTh = iSocTh*capacityTS
         eSocTh = eSocTh*capacityTS
         dtmin  = minval(dt)
         dsoc   = dsp*dtmin*pMaxTS
         nsoc   = floor(capacityTs/(dsoc)) + 1
         allocate(soc(nsoc), socTh(0:nTime+1))
         soc(1) = zero
         do i=2,nsoc
            soc(i) = soc(i-1) + dsoc
         enddo
     else
         nsoc = 1
         allocate(soc(nsoc), socTh(0:nTime+1))
         soc(1) = 0
     endif

!     !Thermal storage capacity constraint
!     k = 2*nm0 + 1
!     nTv(k) = nSoc
!     do j=1,nsoc
!        timeVinc(j,k) = soc(j)
!     enddo

   end subroutine addThermalStorage

!=========================================================================================

   subroutine addElectricalStorage

     implicit none

     integer :: i, j ,k 
     real(kind=prec) :: dsocEl, dspEl, dtmin

     if(capacityES.gt.zero.and.pMaxEs.gt.zero) then
         j = is(iES) 
         nSp(is(iES)) = 2*nSpES + 1
         Pmax(j) = PmaxEs
         sp(1,j) = -1
         cr(1,j) = 1
         dspEl = 1.0/nSpEs
         do i=2,nSpES
            sp(i,j) = sp(i-1,j) + dspEl
            cr(i,j) = i
         enddo
         sp(nSpES + 1,j) = zero
         cr(nSpES + 1,j) = nSpEs + 1
         do i= nSpTS + 2, nSp(j)
            sp(i,j) = sp(i-1,j) + dspEl
            cr(i,j) = i
         enddo
         iSocEl = iSocTh*capacityES
         eSocEl = eSocTh*capacityES
         dtmin    = minval(dt)
         dsocEL   = dspEl*dtmin*pMaxES
         nsocEl   = floor(capacityEs/(dsocEl)) + 1
         allocate(Esoc(nsocEl), socEl(0:nTime+1))
         Esoc(1) = zero
         do i=2,nsocEL
            Esoc(i) = Esoc(i-1) + dsocEl
         enddo
     else
         nsocEl = 1
         allocate(Esoc(nsoc), socEl(0:nTime+1))
         Esoc(1) = 0
     endif

 !    k = 2*nm0 + 2
 !    nTv(k) = nSocEl
 !    do j=1,nSocEL
 !       timeVinc(j,k) = Esoc(j)
 !    enddo
 !
   end subroutine addElectricalStorage

!=========================================================================================

end module addSection
