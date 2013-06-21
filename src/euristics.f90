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
!>\file euristics.f90
!>\brief Module that contains the function to apply an euristics to the graph. 
!>\details 
!> Module that contains the function to apply an euristics to the graph. in order to reduce the number of points and arcs of the graph.
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Module that contains the function to apply an euristics to the graph. 
!>\details 
!> Module that contains the function to apply an euristics to the graph. in order to reduce the number of points and arcs of the graph.
!>\author Andrea Facci

module euristics

use shared  
use energy
use plantVar
use inputVar
use interfaces
use constr

!interface
!    logical function constraints(c,t)
!        use shared        
!        use plantVar
!        use interfaces
!        use inputVar
!        implicit none
!        integer, dimension(nm), intent(in) :: c
!        integer,                intent(in) :: t
!    end function constraints
!end interface


contains
  
!>\brief Rejects solutions with excess of thermal production.
!>\details Rejects the points where an excess thermal production is achieved
!> using fuel boilers. If there is a constraint on the duration of the on or
!> off intervals the possibility to operate the boilers at minimum load even when
!> not strictly necessary is considered.
!>\param[in] c  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t  time-step index
!>\author Andrea Facci

  logical function thRedundant(c,t)

  !---Declare Module usage---

  !---Declare Local Variables---
  implicit none

  integer,dimension(nm), intent(in) :: c
  integer              , intent(in) :: t
  integer                           :: i, j, ii
  real(kind = prec)                 :: uTermal 
  integer, dimension(nBoi)          :: minSetPoint 
  logical                           :: v
  integer,dimension(nm)             :: cStar

  thRedundant = .false.
  
  uTermal = sum(uTh(t,:)) + thSelfCons(c,t)

  ii = 0
  do i=is(iB),ie(iB)
     ii = ii + 1
     if(minUpTime(i).gt.zero.or.minDownTime(i).gt.zero) then
        minSetPoint(ii) = 2
     else
        minSetPoint(ii) = 1
     endif
  enddo

  ii = 0
  if(cogThProd(c,t).ge.uTermal) then
     do i=is(iB),ie(iB)
        ii = ii + 1
        j = c(i)
        if(pes(i).eq.'fuel') then
           if(c(i).gt.minSetPoint(ii)) then 
             thRedundant = .true.
             return
           endif
        endif
     enddo
  endif

  ii = 0
  cStar = c
  do i=is(iB),ie(iB)
     ii = ii + 1
     if(c(i).gt.minSetPoint(ii)) then
        cStar(i) = c(i) - 1
        v = constraints(cStar,t)
        if(v) then
            thRedundant = .true.
            return
        endif
     endif
  enddo

  end function thRedundant

!==================================================================
!>\brief Rejects solutions with excess of chilling production.
!>\details Rejects the points where an excess chilling production is achieved
!> using mechanical chillers. If there is a constraint on the duration of the on or
!> off intervals the possibility to operate the chillers at minimum load even when
!> not strictly necessary is considered.
!>\param[in] c  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t  time-step index
!>\author Andrea Facci
  logical function chRedundant(c,t)

  !---Declare Module usage---

  !---Declare Local Variables---
  implicit none

  integer,dimension(nm), intent(in) :: c
  integer              , intent(in) :: t
  integer                           :: i, j, ii
  real(kind = prec)                 :: uChilling
  integer, dimension(nChi)          :: minSetPoint 
  logical                           :: v
  integer,dimension(nm)             :: cStar

  chRedundant = .false.
  
  uChilling = sum(uCh(t,:))
  
  ii = 0
  do i=is(iC),ie(iC)
     ii = ii + 1
     if(minUpTime(i).gt.zero.or.minDownTime(i).gt.zero) then
        minSetPoint(ii) = 2
     else
        minSetPoint(ii) = 1
     endif
  enddo

  ii = 0
  cStar = c
  do i=is(iC),ie(iC)
     ii = ii + 1
     if(c(i).gt.minSetPoint(ii)) then
        cStar(i) = c(i) - 1
        v = constraints(cStar,t)
        if(v) then
            chRedundant = .true.
            return
        endif
     endif
  enddo

  end function chRedundant

end module euristics
