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

subroutine deallocateVar(what,num)

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
    case(1)
        !readTrigenration
        deallocate(pMaxT, fuelCostT, fuelLHVT, fireCostT,                          &
                   maintCostT, nSpT, nEtaElT, nEtaThT, nEtaChT, tecT,      &
                   minUpTimeT, minDownTimeT, ntcT, npcT, nacT, spT,etaElT, &
                   etaThT,etaChT)
        !--readBoilers
        deallocate(pMaxB, fuelCostB, fuelLHVB, fireCostB,                         & 
                   maintCostB, nSpB, minUpTimeB, minDownTimeB, ntcB, npcB, nacB,  &
                   nEtaB, tecB, spB, etaB)
        !---readChillers
        deallocate(pMaxC, fireCostC, maintCostC, nSpC, minUpTimeC, & 
                   minDownTimeC,nEtaC, tecC, ntcC, npcC, nacC, spC, etaC)
   !---buildPlant
        deallocate(etaElT_, etaThT_, etaChT_, etaB_, etaC_)
        deallocate(presCorrT,altCorrT,tempCorrB,presCorrB,altCorrB,tempCorrC,presCorrC,altCorrC)
end select

end subroutine deallocateVar
