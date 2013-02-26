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
!>\file myArithmetic.f90
!>\brief creates and detects NaN 
!>\author 
!     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Creates and detects NaN 
!>\details This module collects some subroutine useful to create and detect
!> NaNs. It trys to imitate the IEEE_ARITHMETIC module that unfortunately is
!> still not available for the gfortran compiler.
!>\author 
!     Andrea Facci.
module myArithmetic

use shared

contains

!>\brief Creates NaN.
!>\details Creates a NaN to be associated to a real, double precition variable.
!>\param[in] x random double precision number.
!>\author Andrea Facci.

real(kind = prec) function rnan(x)
  !dir$ optimize:0
  real(kind = prec), intent(in) :: x

  rnan = (x-x)/(x-x)
end function rnan

!===================================================================================

!>\brief Creates NaN.
!>\details Creates a NaN to be associated to an integer, variable.
!>\param[in] x random  integer number.
!>\author Andrea Facci.

integer function inan(x)
  !dir$ optimize:0
  integer, intent(in) :: x
  
  inan = (x-x)/(x-x)
end function inan

!>\brief Detects NaNs
!>\details This subroutine determine if a value is NaN.
!>\param[in] x the value to be tested.
!>\author Andrea Facci
logical function isNaN(x)
  
  if(x.ne.x) then 
     isNaN = .true.
  else
     isNaN = .false.
  endif

end function isNaN  

!real(kind(1.d0)) function  infinity()
!
!    INTEGER :: inf
!    real(kind(1.d0)) :: rinf
!    EQUIVALENCE (inf,rinf) !Stores two variable at the same address
!    DATA inf/z'7f800000'/  !Hex for +Infinity

!    infinity = rinf
!    WRITE(*,*) infinity

!end function infinity

end module myArithmetic
