
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
!>\file cmdVar.f90
!>\brief 
!> Collects the variable read from command line.
!>\details
!> Collects the variable read from command line.
!>\author
!
!---------------------------------------------------------------------------

!>\brief 
!> Collects the variable read from command line.
!>\details
!> Collects the variable read from command line.
!>\author
!> Andrea Facci
module cmdVar

!> Controls the input to the screen
logical :: silent,vsilent,verb, debug
character(len=100)  :: out

end module cmdVar
