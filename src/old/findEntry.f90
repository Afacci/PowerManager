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
!Application
!    The application
!
!Description
!    very simple description.
!Author 
!     Andrea Facci.
!
!---------------------------------------------------------------------------

subroutine iFindEntry(entry,n,theUnit,rew,valore,isPresent, nRow)

!---Declare Module usage---

!---Declare local variables---
implicit none

integer, intent(in)            :: theUnit
integer, intent(in)            :: n
integer, intent(out), optional :: valore(n)
integer, intent(out), optional :: nRow
logical, intent(in), optional  :: rew
logical, intent(out), optional :: isPresent
character(len=100), intent(in) :: entry
integer :: nRow_, i
logical :: rew_

character(len=100) :: keyword, value, buffer
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
        value   = trim(buffer(ifirst+1 : ilast-1))
        if(keyword.eq.entry) then 
            if(present(valore)) read(value,*) (valore(i), i=1,n)
            if(present(isPresent)) isPresent = .true.
            exit
        endif
    endif
enddo

!---Rewind the input file
if(rew_) call rewUnit(theUnit,nRow_)
if(present(nRow)) nRow = nRow_

100 format(A100)

end subroutine iFindEntry


subroutine dFindEntry(entry,n,theUnit,rew,valore,isPresent,nRow)

!---Declare Module usage---

!---Declare local variables---
implicit none

integer, intent(in)            :: theUnit
integer, intent(in)            :: n
integer, intent(out), optional :: nRow
real(kind(1.d0)), intent(out), optional  :: valore(n)
logical, intent(in), optional  :: rew
logical, intent(out), optional :: isPresent
character(len=100), intent(in) :: entry
integer :: nRow_, i
logical :: rew_

character(len=100) :: keyword, value, buffer
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
        value   = trim(buffer(ifirst+1 : ilast-1))
        if(keyword.eq.entry) then 
            if(present(valore)) read(value,*) (valore(i), i=1,n)
            if(present(isPresent)) isPresent = .true.
            exit
        endif
    endif
enddo

!---Rewind the input file
if(rew_) call rewUnit(theUnit,nRow)
if(present(nRow)) nRow = nRow_

100 format(A100)

end subroutine dFindEntry


subroutine cFindEntry(entry,n,theUnit,rew,valore,isPresent, nRow)

!---Declare Module usage---

!---Declare local variables---
implicit none

integer, intent(in)            :: theUnit
integer, intent(in)            :: n
integer, intent(out), optional :: nRow
character(len=100), intent(out), optional :: valore(n)
logical, intent(in), optional  :: rew
logical, intent(out), optional :: isPresent
character(len=100), intent(in) :: entry
integer :: nRow_, i
logical :: rew_

character(len=100) :: keyword, value, buffer
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
        value   = trim(buffer(ifirst+1 : ilast-1))
        if(keyword.eq.entry) then 
            if(present(valore)) read(value,*) (valore(i), i=1,n)
            if(present(isPresent)) isPresent = .true.
            exit
        endif
    endif
enddo

!---Rewind the input file
if(rew_) call rewUnit(theUnit,nRow)
if(present(nRow)) nRow = nRow_

100 format(A100)

end subroutine cFindEntry


subroutine findEntry(entry,theUnit,rew,valore,isPresent,nRow)

!---Declare Module usage---

!---Declare local variables---
implicit none

integer, intent(in)            :: theUnit
integer, intent(out), optional :: nRow
character(len=100), intent(out), optional :: valore
logical, intent(in), optional  :: rew
logical, intent(out), optional :: isPresent
character(len=100), intent(in) :: entry
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
if(rew_) call rewUnit(theUnit,nRow)
if(present(nRow)) nRow = nRow_

100 format(A100)

end subroutine findEntry
