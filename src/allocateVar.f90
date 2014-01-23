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
!>\file allocateVar.f90
!>\brief variable allocation.
!>\author 
!     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief variable allocation
!>\details Allocates the veraibles according to the inputs. All the allocation
!> of "global" variables should be done here.
!>\param[in] what defines the variables to be allocated
!>\param[in] num  defines the dimnesion of the array
!>\author Andrea Facci

subroutine allocateVar(what,num)

!---Declare Unit usage---
use shared
use inputVar
use plantVar 

implicit none

!---Declare Local Variables---
integer, intent(in) :: what
integer, intent(in) , optional :: num
integer :: nMax, maxSize, np, nMax1, nMax2

select case(what)
    !readTrigenration
    case(0)
      allocate(startPoint(num), uptime0(num), downTime0(num))
    case(1)
        allocate(pMaxT(nTrig), fuelCostT(nTrig), fuelLHVT(nTrig), fireCostT(nTrig), maintCostT(nTrig), nSpT(nTrig))
        nSpT(1:nTrig) = 0
!        allocate(nSizeT(nTrig))
!        nSizeT(1:nTrig) = 0
        allocate(nEtaElT(nTrig), nEtaThT(nTrig), nEtaChT(nTrig), tecT(nTrig), minUpTimeT(nTrig),minDownTimeT(nTrig))
        allocate(ntcT(nTrig), npcT(nTrig), nacT(nTrig), TrigPriority(nTrig),pefT(nTrig), & 
                 pecOnT(nTrig))
   case(2)
        nMax = maxval(nSpT)
        allocate(spT(nMax,nTrig))
        spT(1:nmax,1:nTrig) = -1
!   case(3)
!        maxSize = maxval(nSizeT)
!        allocate(kSizeT(nTrig,maxSize))
!        kSizeT(1:nTrig,1:maxSize) = -1
   case(4) 
        nMax = maxval(nEtaElT)
        allocate(etaElT(nMax,2,nTrig))
   case(5) 
        nMax = maxval(nEtaThT)
        allocate(etaThT(nMax,2,nTrig))
   case(6) 
        nMax = maxval(nEtaChT)
        allocate(etaChT(nMax,2,nTrig))
   !--readBoilers
   case(7)
        allocate(pMaxB(nBoi), fuelCostB(nBoi), fuelLHVB(nBoi), fireCostB(nBoi), maintCostB(nBoi), &
                 nSpB(nBoi), minUpTimeB(nBoi), minDownTimeB(nBoi))
        allocate(ntcB(nBoi), npcB(nBoi), nacB(nBoi), pefB(nBoi), pecOnB(nBoi))
        nSpB(1:nBoi) = 0
!        allocate(nSizeB(nBoi))
!        nSizeB(1:nBoi) = 0
        allocate(nEtaB(nBoi), tecB(nTrig), BoiPriority(nBoi))
   case(8)
        nMax = maxval(nSpB)
        allocate(spB(nMax,nBoi))
        spB(1:nMax,1:nBoi) = -1
!   case(9)   
!        maxSize = maxval(nSizeB)
!        allocate(kSizeB(nBoi,maxSize))
!        kSizeB(1:nBoi,1:maxSize) = -1
   case(10) 
        nMax = maxval(nEtaB)
        allocate(etaB(nMax,2,nBoi))
        !---readChillers
   case(11)
        allocate(pMaxC(nChi), fireCostC(nChi), maintCostC(nChi), nSpC(nChi), minUpTimeC(nChi),minDownTimeC(nChi))
        nSpC(1:nChi) = 0
!        allocate(nSizeC(nChi))
!        nSizeC(1:nChi) = 0
        allocate(nEtaC(nChi), tecC(nChi))        
        allocate(ntcC(nChi), npcC(nChi), nacC(nChi), ChiPriority(nChi))
   case(12)
        nMax = maxval(nSpC)
        allocate(spC(nMax,nChi))
        spC(1:nMax,1:nChi) = -1
!   case(13)
!        maxSize = maxval(nSizeC)
!        allocate(kSizeC(nChi,maxSize))
!        kSizeC(1:nChi,1:maxSize) = -1
   case(14) 
        nMax = maxval(nEtaC)
        allocate(etaC(nMax,2,nChi))
   case(19)
        allocate(nld(nLoad))
   case(15)
        allocate(time(nTime), uEl(nTime,nld(iEl)), uTh(nTime,nld(iTh)), uCh(nTime,nld(iCh)))    
   !---buildPlant
   case(16)
        if(nTrig.gt.0) then
            nMax = maxval(nSpT)
            allocate(etaElT_(nMax,nTrig),etaThT_(nMax,nTrig), etaChT_(nMax,nTrig))
        endif
        if(nBoi.gt.0) then
            nMax = maxval(nSpB)
            allocate(etaB_(nMax,nBoi))
        endif
        if(nChi.gt.0) then
            nMax = maxval(nSpC)
            allocate(etaC_(nMax,nChi))
        endif
        allocate(dt(0:nTime))
   case(17)
        allocate(nSp(nm))
   case(18)
        !nMax = maxval(nSp)
        nMax = num
        allocate(sp(nMax,nm),cr(nMax,nm))
        allocate(minUpTime(nm0), minDownTime(nm0), eSource(nm))
        !np   = nMax**nm 
        !allocate(cRef(np,nm))
        allocate(etaEl(nMax,nm), etaCh(nMax,nm), etaTh(nMax,nm))
        allocate(Pmax(nm),pes(nm),lhv(nm),cf(nm), onOffCost(nm), OeMCost(nm),tec(nm))
        if(iPEC) then 
          allocate(pef(nm), pecOn(nm))
          pef = zero
          pecOn = zero
        endif
   case(20)
        allocate(cEl(nTime,nlp(iElp)), cTh(nTime,nlp(iThp)), cCh(nTime,nlp(iChp)))    
        allocate(gridBuyCost(nTime), gridSellCost(nTime)) 
   case(21)
        nMax1 = maxval(minUpTime  )/3.6e3 + 1
        nMax2 = maxval(minDownTime)/3.6e3 + 1
        nMax  = max(nMax1,nMax2,nSoc, nSocEl, nSocIce)
        allocate(timeVinc(nMax,2*nm0 + 3))
        allocate(nTv(2*nm0 + 3))
   case(22)
        allocate(tAmb(nTime), pAmb(nTime))
   case(23)
        allocate(tempCorrT(num,5,nTrig))
   case(24)
        allocate(presCorrT(num,5,nTrig))
   case(25)
        allocate(altCorrT(num,5,nTrig))
   case(26)
        allocate(tempCorrB(num,3,nBoi))
   case(27)
        allocate(presCorrB(num,3,nBoi))
   case(28)
        allocate(altCorrB(num,3,nBoi))
   case(29)
        allocate(tempCorrC(num,3,nChi))
   case(30)
        allocate(presCorrC(num,3,nChi))
   case(31)
        allocate(altCorrC(num,3,nChi))
   case(32)
        allocate(envCorr(nTime,nm,4))
   case(33)
        allocate(pvCorr(ntPV,2))
   case(34)
        allocate(etaSC(nEtaSC,2))
   case(35)
        allocate(nwt(nwf), wSurf(nwf), nCpW(nwf), minWind(nwf), maxWind(nwf), hPale(nwf))
   case(36)
        allocate(cpw(maxval(nCpw),2,nwf))
   case(37)
        allocate(wind(nWind,2))
   case(38)
        allocate(preset(nTime,nTrig), cpred(nTime,nTrig))
   case(39)
        allocate(etaEsIn(nEtaEsIn,2))
   case(40)
        allocate(etaEsOut(nEtaEsOut,2))
end select

end subroutine 
