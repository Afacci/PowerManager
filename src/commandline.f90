
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
!>\file 
!>\brief Reads the commad line.
!>\details subroutine to read the execution options from command line.
!>\author Andrea Facci
!---------------------------------------------------------------------------

!>\file 
!>\brief Reads the commad line.
!>\details subroutine to read the execution options from command line.
!>\author Andrea Facci

subroutine commandline

use cmdVar

implicit none

integer:: numarg, i
character(len=200) :: riga
integer ::  iargc

numarg = iargc()

silent  = .false.
vsilent = .false.
verb    = .false.
out     = 'Rename'

i=0
do while(i.lt.numarg)
   i = i + 1
   call getarg(i,riga)
   riga = trim(riga)
   select case(riga)
      case('-verbose')
          verb = .true.
      case('-silent') 
          silent = .true.
      case('-verysilent') 
          vsilent = .true. 
          silent=.true.
      case('-help') 
         call aiuto
      case('-overwrite')
         out = 'Overwrite'
      case('-rename')
         out = 'Rename'
      case default 
      print*,'-------------------------------------------------------------'
      write(*,100)trim(riga)
      print*,' use -help option for the known options                      '
      print*,'-------------------------------------------------------------'
   end select    
enddo

100 format(' Unknown option',2X,A,2X,' will be ignored')

end subroutine commandline


!=============================================================================
!                    OPZIONI CONSIDERATE                                     !
!=============================================================================
!
!-silent: non stampa riepilogo sottomodelli la solo riepilogo generale.
!-verysilent: non stampa nulla a schermo
!-help: stampa un help devo ancora scrivere.
!==============================================================================
