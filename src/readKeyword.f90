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
!>\file readKeyword.f90
!>\brief Reads a line of a text file.
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Reads a line of a text file.
!>\details 
!> This procedure reads a line of a unit and returns the "keyword" and "value"
!> fileds as single value charaters of maximum lenght = 100. The Unit needs to
!> be already opened and associated to the desired file. This subroutine reads
!> the line corresponding to the actual position of te cursor.
!> The structure expected for the input text file is
!> clarified in the following example along with the meaning of "keyword" and
!> "value" field.\n
!> \verbatim
!> A Lot of usless stuff because only text between begin and end is read.
!>    begin 
!>       !Commented line
!>       KeywordField  | ValueField | !Comment two
!>
!>       KeywordField  | ValueField |
!>       ScalarValue   | 1          |
!>       Vector        | 1 2 n      |
!>       VectorSeries  |(a b c ) (d e f)|
!>       Matrix        |(11 12 13  |
!>                     | 21 22 23  |
!>                     | 31 32 33) |
!>    end
!> A lof really usless text
!>\endverbatim
!> Note that only the text between "begin" and "end" is read. Blank lines are
!> automatically discarded, as well as lines beginning with "!" that are
!> considered comments. If only one delimiter "|" is present the line is considered
!> mispelled, no value are is associated to keyword nor to value, and an optional
!> error code is returned. The number of lines that were read before the
!> first valid line can be returned with the optional argument "nRow". If the
!> optional argument "rew" is set to true the unit is rewinded exactly of nRow
!> lines at the end of the procedure.
!>\param[out] Keyword, value keyword and value fields
!>\param[out] error          Error code = 1 in case of mispelled lines
!>\param[out] nRow           The number of lines that were read before the first valid line
!>\param[in]  theUnit        Unit number associated to the desired file.
!>\param[in]  rew            Wether to rewind or not the unit. Default is false.
!>\author
!>     Andrea Facci.

subroutine readKeyword (theUnit,rew, keyword, value, error,nRow)

!---Declare Module usage--- 
use shared

!---Declare local variables---
implicit none

character(len=*), intent(out)  :: Keyword, value
integer, intent(out), optional :: error
integer, intent(out), optional :: nRow 
integer, intent(in)            :: theUnit
logical, intent(in), optional  :: rew
character(len=100)             :: buffer
integer                        :: ifirst, ilast, nRow_
logical                        :: rew_

if(present(rew)) then 
    rew_ = rew
else
    rew_ = .false.
endif
if(present(error)) error = 0

nRow_ = 0
do  
    nRow_ = nRow_ + 1
    read(theUnit,100) buffer
    buffer = adjustl(buffer)
    if(buffer(1:1).ne.'!')then
        if(buffer(1:3).eq.'end') then  !last useful line --> associate a dummy parater to value
            keyword    = trim(buffer(1:3))
            value      = ' '
            exit
        else
            ifirst    = index(buffer,'|')
            ilast     = index(buffer(ifirst+1:),'|') + ifirst
            if(buffer(1:).eq.' ') then                    !the line is blank --> read another line
               continue
            elseif ((ifirst.eq.ilast).or.ilast.eq.0) then !missing one of the "|"
               if(present(error)) error   = 1             !that should delimit the table
               keyword = ' '                              !dummy values
               value   = ' '
               exit
            else
               keyword   = trim(buffer(1:ifirst-1))
               value     = trim(buffer(ifirst+1 : ilast-1))
               exit
            endif
         endif
    endif
enddo
!---Rewind the input file
if(rew_) call rewUnit(theUnit,nRow_, .false.)
if(present(nRow)) nRow = nRow_

return

100 format(A100)

end subroutine readKeyword
