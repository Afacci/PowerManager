
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
!>\file aiuto.f90
!!\brief prints a very short help.
!!
!!\details prints a very short help.
!!\author 
!!     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief prints a very short help.
!!
!!\details prints a very short help. This is called if -help option is given from the
!!command line.
!!\author 
!!     Andrea Facci.
subroutine aiuto

use shared        

print*
print*,'+++++++++++++++++++++++++++++Help++++++++++++++++++++++++++++++'
print*,' Program "PowerManager" optimization of distributed poly-generation'
print*,' patforms. Finds the optimal set point of a power plant according to'
print*,' loads and enegy prices, using dynamic programming.'
print*,' The power plant configuartion and laod are difined in the input files:'
print*,'   --> ./Input/General.inp'
print*,'   --> ./Input/Trigeneration.inp'
print*,'   --> ./Input/Boilers.inp'
print*,'   --> ./Input/Chillers.inp'
print*,'   --> ./Input/Load.inp'
print*
print*,' Known Options are:'
print*,'    -help       : print this help page'
print*,'    -verysilent : print nothing to standard output '
print*,'    -silent     : minmal standard output '
print*,' For source code docunmentation refer to the Doxygen documentation in'
print*,' doc/ folder.'
call endExecution

end subroutine aiuto
