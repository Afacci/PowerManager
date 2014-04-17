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
!>\file readLoad.f90
!>\brief Reads Load.inp file
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Reads Load.inp file
!>\details 
!> This subroutine reads the file "Load.inp". The procedure looks for each specific entry
!> in the "keyword field", and associates the value in the "value" field, to the
!> corresponding variable. If the desired entry is not present returns an error
!> message and aborts the execution. The structure of the input file is
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
!> automatically discarded, while any unrecognized entry is discarded returning
!> a warning code. Line beginning with "!" are considered comments and
!> discarded.
!>\author 
!>     Andrea Facci.

subroutine readEnv

!---Declare Unit usage---
use shared
use inputVar
use fileTools
use interfaces
use cmdVar
use myArithmetic

implicit none

!---Declare Local Variables---
integer              :: genUnit = 110, idummy
character(len=100)    :: inputFile = './Input/Environment.inp'
logical              :: filePresent
character(len=500)   :: buffer, keyword, value, cdummy
integer              :: i, j, nl, error, line, n, firstLine, nb, nd, nInp
logical,dimension(2) :: isPresent = .false.
character(len=100),dimension(1,3)   :: env
real(kind = prec), allocatable, dimension(:,:) :: matrix
logical ,dimension(5)  :: ljModel = .false.
logical                :: simpleModel = .false.
character(len=20), dimension(3)                :: param


!---Check File Presence---
call openUnit(inputFile,genUnit,filePresent)
if(.not.filePresent) call abortExecution(1,7)

!---Skip all the lines before the keyword "begin".
Line = 0;
do 
    read(genUnit,100) buffer
    buffer = adjustl(buffer)
    Line = Line + 1
    if(buffer(1:5).eq.'begin') exit
enddo

firstLine = line

do 
    call readKeyword(genUnit,.false., keyword,value,error, nl)
    if (error.eq.1) call abortExecution(0,6,line)
    line = line + nl
    select case(keyword)
       case('end')
          exit
!       case('Latitude','Day','SummerTime','Cloudiness','Radiation','RadiationModel')
!          continue
       case('Climate')
          isPresent(1) = .true.
          call rewUnit(genUnit,1)
          env = cMatrixRead(theUnit=genUnit, nline=1, nCol=3)
          n = vCount(genUnit,.true.)
          if(n.ne.nTime) call abortExecution(17)
          allocate(matrix(nTime,3))
          matrix =  dmatrixRead(genUnit,n,3)
          call allocateVar(22)
             do i=1,3
                buffer = env(1,i)
                select case(buffer)
                   case('Time')
                      iTime = i
                      if(iTime.ne.1) call abortExecution(8)
                   case('Temperature')
                      tAmb = matrix(:,i)
                   case('Pressure')
                      pAmb = matrix(:,i)
                end select
           end do
           deallocate(matrix)
       case('Altitude')
          isPresent(2) = .true.
          read(value,*) altitude
!    if(surfSC.gt.zero.or.surfPV.gt.zero) then
!        select case(keyword)
!          case('end')
!             exit
!          case('Climate','Altitude', 'Radiation')
!             continue
       case('Latitude')
          read(value,*) latitude
          ljModel(1) = .true.
       case('Day')
          read(value,*) Day
          ljModel(2) = .true.
       case('SummerTime')
          read(value,*) summerTime
          ljModel(3) = .true.
!       case('Cloudiness')
!          read(value,*) clouds
!          ljModel(4) = .true.
!       case('RadiationModel') 
!          read(value,*) radMod
!          ljModel(5) = .true.
!          print*, 'radMod', radMod
       case('Radiation')
           ljModel(5) = .true.
!       simpleModel = .true.
       read(value,*) (param(i), i=1,2)
       do i=1,2
           select case(param(i))
              case('Beam')
                 nb = i
              case('Diffused')
                 nd = i
           end select
       enddo
       allocate(matrix(24,2))
       matrix  = rNaN(rVal)
       matrix  = dmatrixRead(genUnit,24,2)
       BeamRad = matrix(:,nb)
       DiffRad = matrix(:,nd)
       deallocate(matrix)
       case(' ') 
          if(verb) call warning(4,3,line=line)
       case default
          if(.not.silent) call warning(1,7,line=line,word=keyword)
    end select
!      endif
enddo

!if(radMod.eq.'Simple') then
!  call rewUnit(genUnit,line-firstLine)
!  line = firstLine
!  do 
!    call readKeyword(genUnit,.false., keyword,value,error, nl)
!    if (error.eq.1) call abortExecution(0,6,line)
!    line = line + nl
!    select case(keyword)
!       case('end')
!          exit
!       case('Radiation')
!         simpleModel = .true.
!         read(value,*) (param(i), i=1,2)
!         do i=1,2
!             select case(param(i))
!                case('Beam')
!                   nb = i
!                case('Diffused')
!                   nd = i
!             end select
!         enddo
!         allocate(matrix(24,2))
!         matrix  = rNaN(rVal)
!         matrix  = dmatrixRead(genUnit,24,2)
!         BeamRad = matrix(:,nb)
!         DiffRad = matrix(:,nd)
!         deallocate(matrix)
!       case default
!         continue
!    end select
!  enddo
!endif


if(surfSC.gt.zero.or.surfPV.gt.zero) then
!   if(radMod.eq.'Simple') then
!      if(.not.simpleModel) call abortExecution(30,1)
!   endif
!   nInp = size(ljModel)
   do i=1,nInp-1
      if(.not.ljModel(i)) call abortExecution(31,i)
   enddo
   if(.not.ljModel(5)) call abortExecution(30,1)
endif

close(genUnit)
100 format(A500)

end subroutine readEnv
