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
        allocate(pMaxT(nTrig), degRateT(nTrig), fuelCostT(nTrig), fuelLHVT(nTrig), invT(nTrig) &
                ,lifeT(nTrig), fireCostT(nTrig), maintCostT(nTrig), nSpT(nTrig))
        nSpT(1:nTrig) = 0
        allocate(nSizeT(nTrig))
        nSizeT(1:nTrig) = 0
        allocate(nEtaElT(nTrig), nEtaThT(nTrig), nEtaChT(nTrig), tecT(nTrig), minUpTimeT(nTrig),minDownTimeT(nTrig))
   case(2)
        nMax = maxval(nSpT)
        allocate(spT(nMax,nTrig))
        spT(1:nmax,1:nTrig) = -1
   case(3)
        maxSize = maxval(nSizeT)
        allocate(kSizeT(nTrig,maxSize))
        kSizeT(1:nTrig,1:maxSize) = -1
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
        allocate(pMaxB(nBoi), degRateB(nBoi), fuelCostB(nBoi), fuelLHVB(nBoi), invB(nBoi), lifeB(nBoi), &
                 fireCostB(nBoi), maintCostB(nBoi), nSpB(nBoi), minUpTimeB(nTrig), minDownTimeB(nTrig))
        nSpB(1:nBoi) = 0
        allocate(nSizeB(nBoi))
        nSizeB(1:nBoi) = 0
        allocate(nEtaB(nBoi), tecB(nTrig))
   case(8)
        nMax = maxval(nSpB)
        allocate(spB(nMax,nBoi))
        spB(1:nMax,1:nBoi) = -1
   case(9)   
        maxSize = maxval(nSizeB)
        allocate(kSizeB(nBoi,maxSize))
        kSizeB(1:nBoi,1:maxSize) = -1
   case(10) 
        nMax = maxval(nEtaB)
        allocate(etaB(nMax,2,nBoi))
        !---readChillers
   case(11)
        allocate(pMaxC(nChi), degRateC(nChi), invC(nChi), lifeC(nChi), fireCostC(nChi), & 
                 maintCostC(nChi), nSpC(nChi), minUpTimeC(nChi),minDownTimeC(nTrig))
        nSpC(1:nChi) = 0
        allocate(nSizeC(nChi))
        nSizeC(1:nChi) = 0
        allocate(nEtaC(nChi), tecC(nChi))
   case(12)
        nMax = maxval(nSpC)
        allocate(spC(nMax,nChi))
        spC(1:nMax,1:nChi) = -1
   case(13)
        maxSize = maxval(nSizeC)
        allocate(kSizeC(nChi,maxSize))
        kSizeC(1:nChi,1:maxSize) = -1
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
        nMax = maxval(nSp)
        allocate(sp(nMax,nm),cr(nMax,nm))
        allocate(minUpTime(nm), minDownTime(nm), eSource(nm))
        np   = nMax**nm 
        allocate(cRef(np,nm))
        allocate(etaEl(nMax,nm), etaCh(nMax,nm), etaTh(nMax,nm))
        allocate(Pmax(nm),pes(nm),lhv(nm),cf(nm), onOffCost(nm), OeMCost(nm))
   case(20)
        allocate(cEl(nTime,nlp(iElp)), cTh(nTime,nlp(iThp)), cCh(nTime,nlp(iChp)))    
        allocate(gridBuyCost(nTime), gridSellCost(nTime)) 
   case(21)
        nMax1 = maxval(minUpTime  )/3.6e3 + 1
        nMax2 = maxval(minDownTime)/3.6e3 + 1
        nMax  = max(nMax1,nMax2)
        allocate(timeVinc(nMax,2*nm))
        allocate(nTv(2*nm))
   case(22)
        allocate(tAmb(nTime), pAmb(nTime))
end select

end subroutine 
