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
!    You should have received a copy of the GNU General Public License; 
!    if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!>\file findEntry2.f90
!>\brief Collection of procetures to find a specific entry inside a file.
!>\author 
!  Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Finds an entry with integer value.
!>\details 
!> Use this subroutine to find a specific entry in a file, when an integer value is
!> expected in the value field, that is between the two "|". The string in the
!> value field is associated to an integer array of dimension "n"
!> Optionally it returns also the number of lines from the current
!> cursor position to locate the required entry, and a logical for the precence
!> of the required field. Optionally it is also possible to rewind the file.
!>\param[in]  entry     the keyword to be located
!>\param[in]  n         the number of elements expected for the value field
!>\param[in]  theUnit   the unit corresponding to the file to be searched 
!>\param[in]  rew       wether the unit is to be rewinded or not
!>\param[out] valore    the vaule correspondind to keyword that is searched for
!>\param[out] isPresent logical that tells if the entry is present or not
!>\param[out] nRow      number of rows needed to located the entry.
!>\author Andrea Facci

subroutine iFindEntry(entry,n,theUnit,rew,valore,isPresent, nRow)

use shared
implicit none
!---Declare Module usage---

interface
    subroutine findEntry(entry,theUnit,rew,valore,isPresent,nRow)
        use shared
        implicit none
        integer, intent(in)            :: theUnit
        integer, intent(out), optional :: nRow
        character(len=100), intent(out), optional :: valore
        logical, intent(in), optional  :: rew
        logical, intent(out), optional :: isPresent
        character(len=*), intent(in)   :: entry
    end subroutine findEntry
end interface

!---Declare local variables---

integer, intent(in)            :: theUnit
integer, intent(in)            :: n
integer, intent(out), optional :: valore(n)
integer, intent(out), optional :: nRow
logical, intent(in), optional  :: rew
logical, intent(out), optional :: isPresent
character(len=*), intent(in)   :: entry
logical :: isPresent_
integer :: nRow_, i
logical :: rew_

character(len=100) :: keyword, value, buffer
integer            :: ifirst, ilast

if(present(rew)) then 
    rew_ = rew
else
    rew_ = .false.
endif

call findEntry(entry,theUnit,rew_,value,isPresent_,nRow_)
if(present(isPresent)) isPresent = isPresent_
if(present(valore)) read(value,*) (valore(i), i=1,n)

!---Rewind the input file
!if(rew_) call rewUnit(theUnit,nRow_)
if(present(nRow)) nRow = nRow_

100 format(A100)

end subroutine iFindEntry

!>\brief Finds an entry with double precision value.
!>\details 
!> Use this subroutine to find a specific entry in a file, when a double precision value is
!> expected. in the value field, that is between the two "|". The string in the
!> value field is associated to  double precision array of dimension "n"
!> Optionally it returns also the number of lines from the current
!> cursor position to locate the required entry, and a logical for the precence
!> of the required field. Optionally it is also possible to rewind the file.
!>\param[in]  entry     the keyword to be located
!>\param[in]  n         the number of elements expected for the value field
!>\param[in]  theUnit   the unit corresponding to the file to be searched 
!>\param[in]  rew       wether the unit is to be rewinded or not
!>\param[out] valore    the vaule correspondind to keyword that is searched for
!>\param[out] isPresent logical that tells if the entry is present or not
!>\param[out] nRow      number of rows needed to located the entry.
!>\author Andrea Facci

subroutine dFindEntry(entry,n,theUnit,rew,valore,isPresent,nRow)

!---Declare Module usage---
use shared
implicit none

interface
    subroutine findEntry(entry,theUnit,rew,valore,isPresent,nRow)
        implicit none
        integer, intent(in)            :: theUnit
        integer, intent(out), optional :: nRow
        character(len=100), intent(out), optional :: valore
        logical, intent(in), optional  :: rew
        logical, intent(out), optional :: isPresent
        character(len=*), intent(in)   :: entry
    end subroutine findEntry
end interface

!---Declare local variables---

integer, intent(in)            :: theUnit
integer, intent(in)            :: n
integer, intent(out), optional :: nRow
real(kind = prec), intent(out), optional  :: valore(n)
logical, intent(in), optional  :: rew
logical, intent(out), optional :: isPresent
character(len=*), intent(in)   :: entry
logical :: isPresent_
integer :: nRow_, i
logical :: rew_

character(len=100) :: keyword, value, buffer
integer            :: ifirst, ilast

if(present(rew)) then 
    rew_ = rew
else
    rew_ = .false.
endif

call findEntry(entry,theUnit,rew_,value,isPresent_,nRow_)
if(present(isPresent)) isPresent = isPresent_
if(present(valore)) read(value,*) (valore(i), i=1,n)

!---Rewind the input file
!if(rew_) call rewUnit(theUnit,nRow)
if(present(nRow)) nRow = nRow_

100 format(A100)

end subroutine dFindEntry

!>\brief Finds an entry with character value.
!>\details 
!> Use this subroutine to find a specific entry in a file, when an character value is
!> expected. in the value field, that is between the two "|". The string in the
!> value field is associated to a character array of dimension "n"
!> Optionally it returns also the number of lines from the current
!> cursor position to locate the required entry, and a logical for the precence
!> of the required field. Optionally it is also possible to rewind the file.
!> Note that the maximum length of each element of the "value" vector is 100.
!>\param[in]  entry     the keyword to be located
!>\param[in]  n         the number of elements expected for the value field
!>\param[in]  theUnit   the unit corresponding to the file to be searched 
!>\param[in]  rew       wether the unit is to be rewinded or not
!>\param[out] valore    the vaule correspondind to keyword that is searched for
!>\param[out] isPresent logical that tells if the entry is present or not
!>\param[out] nRow      number of rows needed to located the entry.
!>\author Andrea Facci

subroutine cFindEntry(entry,n,theUnit,rew,valore,isPresent, nRow)

!---Declare Module usage---
use shared

implicit none

interface
    subroutine findEntry(entry,theUnit,rew,valore,isPresent,nRow)
        implicit none
        integer, intent(in)            :: theUnit
        integer, intent(out), optional :: nRow
        character(len=100), intent(out), optional :: valore
        logical, intent(in), optional  :: rew
        logical, intent(out), optional :: isPresent
        character(len=*), intent(in) :: entry
    end subroutine findEntry
end interface

!---Declare local variables---

integer, intent(in)            :: theUnit
integer, intent(in)            :: n
integer, intent(out), optional :: nRow
character(len=100), intent(out), optional :: valore(n)
logical, intent(in), optional  :: rew
logical, intent(out), optional :: isPresent
character(len=*), intent(in) :: entry
logical :: isPresent_
integer :: nRow_, i
logical :: rew_

character(len=100) :: keyword, value, buffer
integer            :: ifirst, ilast

if(present(rew)) then 
    rew_ = rew
else
    rew_ = .false.
endif

call findEntry(entry,theUnit,rew_,value,isPresent_,nRow_)
if(present(isPresent)) isPresent = isPresent_
if(present(valore)) read(value,*) (valore(i), i=1,n)

if(present(nRow)) nRow = nRow_

100 format(A100)

end subroutine cFindEntry

!>\brief Finds an entry and returns the vaule as it is.
!>\details 
!> Use this subroutine to find a specific entry in a file. If given as parameter
!> returns the "value" field, that is the value comprised between the two "|" in
!> the input file. This subroutine does not associate the vaule fied to any
!> array but returns a single strig axactly as it is in the input file.
!> Optionally it returns also the number of lines from the current
!> cursor position to locate the required entry, and a logical for the precence
!> of the required field. Optionally it is also possible to rewind the file.
!>\param[in]  entry     the keyword to be located
!>\param[in]  n         the number of elements expected for the value field
!>\param[in]  theUnit   the unit corresponding to the file to be searched 
!>\param[in]  rew       wether the unit is to be rewinded or not
!>\param[out] valore    the vaule correspondind to keyword that is searched for
!>\param[out] isPresent logical that tells if the entry is present or not
!>\param[out] nRow      number of rows needed to located the entry.
!>\author Andrea Facci

subroutine findEntry(entry,theUnit,rew,valore,isPresent,nRow)

!---Declare Module usage---

use shared
!---Declare local variables---
implicit none

integer, intent(in)            :: theUnit
integer, intent(out), optional :: nRow
character(len=100), intent(out), optional :: valore
logical, intent(in), optional  :: rew
logical, intent(out), optional :: isPresent
character(len=*), intent(in)   :: entry
integer :: nRow_, i
logical :: rew_

character(len=100) :: keyword, buffer
integer            :: ifirst, ilast

if(present(rew)) then 
    rew_ = rew
else
    rew_ = .false.
endif

if(present(isPresent)) isPresent = .false.
nRow_ = 0
do  
    nRow_ = nRow_ + 1
    read(theUnit,100) buffer
    buffer = adjustl(buffer)
    if(buffer(1:3).eq.'end') exit
    if(buffer(1:1).ne.'!')then
        ifirst  = index(buffer,'|')
        ilast   = index(buffer(ifirst+1:),'|') + ifirst
        keyword = trim(buffer(1:ifirst-1))
        if(keyword.eq.entry) then 
            if(present(valore)) valore = trim(buffer(ifirst+1 : ilast-1))
            if(present(isPresent)) isPresent = .true.
            exit
        endif
    endif
enddo

!---Rewind the input file
if(rew_) call rewUnit(theUnit,nRow_)
if(present(nRow)) nRow = nRow_

100 format(A100)

end subroutine findEntry

