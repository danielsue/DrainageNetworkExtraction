!!  Project
!!  drainage analysis
!!  Author
!!  Danyang Su
!!  Email: 
!!  danyang.su@gmail.com
!!  License:
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License as published by
!!  the Free Software Foundation, either version 3 of the License, or
!!  (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!  Note: this code a modification of Carlos Otero & Davide Santos's code 
!!
!! + DXFortran Standart Module 
!! 
module DXFortranStd
! 
! Description: 
!   Module of the Standart Librarie of DXFortran. 
! 
! Current Code Owner:
! Carlos Otero & Davide Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! www.ualg.pt/est/adec/csc/dxfortran
! 
! History: 
!  
! Version   Date     Comment 
! -------   -----    ------- 
! 0.1       00.11    Carlos Otero & Davide Santos
! 0.2       00.12    Carlos Otero & Davide Santos
! 
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!  Documenting Exchangeable Fortran 90 Code". 
 
Implicit none 
 
Contains 
! Define procedures contained in this module. 

!
!+ Subroutine to begin a DXF File.
!
Subroutine dfBegin(FileNum,layerDef) 
! Description: 
!   Begin a DXF file <FileNum>.
! 
! Method: 
!   No comment...
! 
! Current Code Owner: 
! Carlos Otero & Davide Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! www.ualg.pt/est/adec/csc/dxfortran
! 
! History: 
! Version   Date     Comment 
! -------   ----     ------- 
!     0.1   00.11    Carlos Otero & Davide Santos
! 
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!                        Documenting Exchangeable Fortran 90 Code". 
! 
Implicit None 
 
! Include statements: 

! Declarations must be of the form: 
 
! Subroutine arguments 

! Scalar arguments with intent(in): 

integer ,intent(in):: FileNum           ! File Number
integer            :: l,              & ! num. of layers
                      i
character (len=*), intent(in)   :: layerDef(:,:) 

l = size(layerDef, dim=1)


!- Begin of header ------------------------------------------------------------

   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "SECTION"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "HEADER"
   write (FileNum,fmt='(A)') "  999"
   write (FileNum,fmt='(A)') "Generator:DXFortran"
   write (FileNum,fmt='(A)') "  999"
   write (FileNum,fmt='(A)') "http://www.ualg.pt/est/adec/dxfortran/"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "ENDSEC"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "SECTION"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "TABLES"
   write (FileNum,fmt='(A)') "  0"
!- Style Def        ------------------------------------------------------------
   write (FileNum,fmt='(A)') "TABLE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "STYLE"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     2"
   !---- STANDARD
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "STYLE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "STANDARD"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') "41"
   write (FileNum,fmt='(A)') "1.0"
   write (FileNum,fmt='(A)') " 50"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 71"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') " 42"
   write (FileNum,fmt='(A)') "2.5"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "txt"
   write (FileNum,fmt='(A)') "  4"
   write (FileNum,fmt='(A)') ""
   !---- ROMANS
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "STYLE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "ROMANS"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 41"
   write (FileNum,fmt='(A)') "1.0"
   write (FileNum,fmt='(A)') " 50"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 71"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') " 42"
   write (FileNum,fmt='(A)') "2.5"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "romans.shx"
   write (FileNum,fmt='(A)') "  4"
   write (FileNum,fmt='(A)') ""
      !---- ROMANC
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "STYLE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "ROMAND"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 41"
   write (FileNum,fmt='(A)') "1.0"
   write (FileNum,fmt='(A)') " 50"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 71"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') " 42"
   write (FileNum,fmt='(A)') "2.5"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "romand.shx"
   write (FileNum,fmt='(A)') "  4"
   write (FileNum,fmt='(A)') ""
   !---- ROMANC
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "STYLE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "ROMANT"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 41"
   write (FileNum,fmt='(A)') "1.0"
   write (FileNum,fmt='(A)') " 50"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 71"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') " 42"
   write (FileNum,fmt='(A)') "2.5"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "romant.shx"
   write (FileNum,fmt='(A)') "  4"
   write (FileNum,fmt='(A)') ""
   !---- 
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "ENDTAB"
   write (FileNum,fmt='(A)') "  0"
!- LType Def        ------------------------------------------------------------
   write (FileNum,fmt='(A)') "TABLE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "    16"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "CONTINUOUS"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Solid line"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "CENTER"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Center ____ _ ____ _ ____ _ ____ _ ____ _ ____"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     4"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "50.8"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "31.75"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-6.35"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "6.35"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-6.35"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "CENTER2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Center (.5x) ___ _ ___ _ ___ _ ___ _ ___ _ ___"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     4"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "28.575"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "19.05"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-3.175"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "3.175"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-3.175"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "CENTERX2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Center (2x) ________  __  ________  __  _____"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     4"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "101.6"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "63.5"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-12.7"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "12.7"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-12.7"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DASHDOT"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Dash dot __ . __ . __ . __ . __ . __ . __ . __"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     4"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "25.4"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "12.7"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-6.35"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-6.35"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DASHDOT2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Dash dot (.5x) _._._._._._._._._._._._._._._."
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     4"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "12.7"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "6.35"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-3.175"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-3.175"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DASHDOTX2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Dash dot (2x) ____  .  ____  .  ____  .  ___"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     4"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "50.8"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "25.4"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-12.7"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-12.7"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DASHED"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Dashed __ __ __ __ __ __ __ __ __ __ __ __ __ _"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     2"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "19.05"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "12.7"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-6.35"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DASHED2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Dashed (.5x) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     2"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "9.525"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "6.35"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-3.175"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DASHEDX2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Dashed (2x) ____  ____  ____  ____  ____  ___"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     2"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "38.1"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "25.4"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-12.7"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DIVIDE"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Divide ____ . . ____ . . ____ . . ____ . . ____"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     6"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "31.75"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "12.7"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-6.35"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-6.35"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-6.35"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DIVIDE2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Divide (.5x) __..__..__..__..__..__..__..__.._"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     6"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "15.875"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "6.35"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-3.175"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-3.175"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-3.175"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DIVIDEX2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Divide (2x) _______  .  .  _______  .  .  _____"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     6"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "63.5"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "25.4"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-12.7"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-12.7"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-12.7"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DOT"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Dot . . . . . . . . . . . . . . . . . . . . . ."
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     2"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "6.35"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-6.35"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DOT2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Dot (.5x) ....................................."
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     2"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "3.175"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-3.175"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "DOTX2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Dot (2x) .  .  .  .  .  .  .  .  .  .  .  .  . "
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     2"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "12.7"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.0"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-12.7"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "HIDDEN"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Hidden __ __ __ __ __ __ __ __ __ __ __ __ __ _"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     2"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "9.525"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "6.35"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-3.175"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "HIDDEN2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Hidden (.5x) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     2"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "0.1875"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.125"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-0.0625"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "LTYPE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "HIDDENX2"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(A)') "     0"
   write (FileNum,fmt='(A)') "  3"
   write (FileNum,fmt='(A)') "Hidden (2x) ____ ____ ____ ____ ____ ____ ____"
   write (FileNum,fmt='(A)') " 72"
   write (FileNum,fmt='(A)') "    65"
   write (FileNum,fmt='(A)') " 73"
   write (FileNum,fmt='(A)') "     2"
   write (FileNum,fmt='(A)') " 40"
   write (FileNum,fmt='(A)') "0.75"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "0.5"
   write (FileNum,fmt='(A)') " 49"
   write (FileNum,fmt='(A)') "-0.25"
   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "ENDTAB"
   write (FileNum,fmt='(A)') "  0"
!- Layer Def       ------------------------------------------------------------
   write (FileNum,fmt='(A)') "TABLE"
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "LAYER"
   write (FileNum,fmt='(A)') " 70"
   write (FileNum,fmt='(I6)')            l              !+ num. of layer's
   write (FileNum,fmt='(A)') "  0"

   do i=1,l
       write (FileNum,fmt='(A)') "LAYER"
       write (FileNum,fmt='(A)') "  2"
       write (FileNum,fmt='(A)')            trim(layerDef(i,1))  !+ layer name
       write (FileNum,fmt='(A)') " 70"
       write (FileNum,fmt='(A)') "     0"
       write (FileNum,fmt='(A)') " 62"
       write (FileNum,fmt='(A)')            trim(layerDef(i,3))
       write (FileNum,fmt='(A)') "  6"
       write (FileNum,fmt='(A)')            trim(layerDef(i,2)) !+ layer linetype def
       write (FileNum,fmt='(A)') "  0"
   end do
   write (FileNum,fmt='(A)') "ENDTAB"
   write (FileNum,fmt='(A)') "  0"

!- End of header --------------------------------------------------------------

   write (FileNum,fmt='(A)') "ENDSEC"
!==========================================================

   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)') "SECTION"   
   write (FileNum,fmt='(A)') "  2"
   write (FileNum,fmt='(A)') "ENTITIES"
  
 return
 end subroutine dfBegin


!
!+ End the DXF File.
!
Subroutine dfEnd(FileNum) 
 
! Description: 
!   End the DXF file <FileNum>
! 
! Method: 
! No comment...
!
! Current Code Owner: 
! Carlos Otero & Davide Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! www.ualg.pt/est/adec/csc/dxfortran
! 
! History: 
! Version   Date     Comment 
! -------   ----     ------- 
!     0.1  00.11     Carlos Otero & Davide Santos
! 
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!                        Documenting Exchangeable Fortran 90 Code". 
! 
 
Implicit None 
 
! Declarations must be of the form: 
 
! Subroutine arguments 

! Scalar arguments with intent(in): 
 
integer ,intent(in):: FileNum         ! File number.

!- End of header ---------------------------------------------------------------  

   write (FileNum,fmt='(A)') "  0"
   write (FileNum,fmt='(A)')"ENDSEC"
   write (FileNum,fmt='(A)')"  0"
   write (FileNum,fmt='(A)')"EOF"

   close(unit=FileNum)

 return
 end subroutine dfEnd

!
!Draw a Arc with a center point and a radius
!
subroutine dfArc				&
               (FileNum,		&
               Layer,			&
               Color,			&
               X,				&
               Y,				&
               Z,				&
               Radius,			&
			   SAngle,			&
			   EAngle,			&
			   Thick)          
!
! Description: 
! 
! Draw a ARC in the in the file <FileNum> in the layer <Layer>
! and in the color <Color> with coordenates (X,Y,Z) with radius <Radius> 
! and thikness Thick
! 
! Method: 
! No comment...
!
! Current Code Owner: 
! David Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! www.ualg.pt/est/adec/csc/dxfortran
! 
! History: 
! Version   Date        Comment 
! -------   --------    ------- 
! 1.0		04.02.23	Davide Santos
!
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!                        Documenting Exchangeable Fortran 90 Code". 
! 

Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
 
! Subroutine arguments 
! Scalar arguments with intent(in): 

Real, Intent (in)        ::             &
                              X,        &		!+ Coordenate XX
                              Y,        &		!+ Coordenate YY
                              Z,        &		!+ Coordenate ZZ
							  SAngle,	&		!+ Start Angle
							  EAngle,	&		!+ End Angle
                              Radius,   &		!+ Radius of the Arc							  
							  Thick					!+ Thickness		

Integer   ,Intent (in)   ::   FileNum			!+ File number
                          
Character , Intent(in)   ::               &
                              Color *(*), &		!+ Arc Color
                              Layer *(*)		!+ Arc Layer

!- End of header --------------------------------------------------------------- 
      write (FileNum,fmt='(A)')  "  0"			!+ Begin of ARC
      write (FileNum,fmt='(A)')  "ARC"		!
      write (FileNum,fmt='(A)')  "  8"			!+ Layer code
      write (FileNum,fmt='(A)')  TRIM(Layer)	!+ Layer's name

      if (Color=="bylayer" .or. &                
          Color=="Bylayer" .or. &
          Color=="BYLAYER")  then
         ! Nothing to do if color is bylayer
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(Color)  !
      end if
      write (FileNum,fmt='(A)')  " 39"			 !+ Thickness of the ARC (3D)
      write (FileNum,fmt= *    )  Thick				 ! 
     
      write (FileNum,fmt='(A)')  " 10"           !+ Point codes
      write (FileNum,fmt= *   )  X               ! 
      write (FileNum,fmt='(A)')  " 20"           !
      write (FileNum,fmt= *   )  Y               !
      write (FileNum,fmt='(A)')  " 30"           !
      write (FileNum,fmt= *   )  Z               !
      write (FileNum,fmt='(A)')  " 40"           !
      write (FileNum,fmt= *   )  Radius          !+ Radius of the Arc
      write (FileNum,fmt='(A)')  " 50"           !
      write (FileNum,fmt= *   )  SAngle          !+ Start Angle
      write (FileNum,fmt='(A)')  " 51"           !
      write (FileNum,fmt= *   )  EAngle          !+ End Angle
return
end subroutine dfArc

!
!Draw a circle with a center point and a radius
!
subroutine dfCircle			&
               (FileNum,	&
               Layer,		&
               Color,		&
               X,			&
               Y,			&
               Z,			&
               Radius,		&
			   Thick)          


!
! Description: 
! 
! Draw a circle in the in the file <FileNum> in the layer <Layer>
! and in the color <Color> with coordenates (X,Y,Z) and radius <Radius>
!
! 
! Method: 
! No comment...
!
! Current Code Owner: 
! David A. B.Pereira
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! www.ualg.pt/est/adec/csc/dxfortran
! 
! History: 
! Version   Date        Comment 
! -------   --------    ------- 
!           02.01.29    David A. B. Pereira
!			04.02.23	Davide Santos
! 
!
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!                        Documenting Exchangeable Fortran 90 Code". 
! 

Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
 
! Subroutine arguments 
! Scalar arguments with intent(in): 

Real, Intent (in)        ::              &
                              X,         &		!+ Coordenate XX
                              Y,         &		!+ Coordenate YY
                              Z,         &		!+ Coordenate ZZ
                              Radius,    &		!+ Radius of the circle
							  Thick					!+ Thickness		

Integer   ,Intent (in)   ::   FileNum			!+ File number
                          
Character , Intent(in)   ::               &
                              Color *(*), &		!+ Circle Color
                              Layer *(*)		!+ Circle Layer

!- End of header --------------------------------------------------------------- 
      write (FileNum,fmt='(A)')  "  0"			!+ Begin of Circle
      write (FileNum,fmt='(A)')  "CIRCLE"		!
      write (FileNum,fmt='(A)')  "  8"			!+ Layer code
      write (FileNum,fmt='(A)')  TRIM(Layer)	!+ Layer's name

      if (Color=="bylayer" .or. &                
          Color=="Bylayer" .or. &
          Color=="BYLAYER")  then
         ! Nothing to do if color is bylayer
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(Color)  !
      end if
      write (FileNum,fmt='(A)')  " 39"			 !+ Thickness of the circle (3D)
      write (FileNum,fmt= *    )  Thick				 ! 
     
      write (FileNum,fmt='(A)')  " 10"           !+ Point codes
      write (FileNum,fmt= *   )  X               ! 
      write (FileNum,fmt='(A)')  " 20"           !
      write (FileNum,fmt= *   )  Y               !
      write (FileNum,fmt='(A)')  " 30"           !
      write (FileNum,fmt= *   )  Z               !
      write (FileNum,fmt='(A)')  " 40"           !
      write (FileNum,fmt= *   )  Radius          !+ Radius of the circle

 return
end subroutine dfCircle


!
! Draw a DONUT in DXF format.
! 
Subroutine dfDonut(FileNum, LayerName, Color,Dint, Dext, X, Y)

!
! Description: 
! 
! To draw a DONUT in the in DXF with coordenates <X>, <Y> and
! with inside diameter <DINT> and the outside diameter <DEXT>
!
!     Input variables:
!       the file number <FILENUM>
!       the file name <FILENAME>
!       the layer name <LAYERNAME>
!       the inside diameter <DINT> 
!       the outside diameter <DEXT>
!       the coordenates of the center of the donut <X> and <Y>

! Method: 
! No comment...
!
! Current Code Owner: 
! David A. B. Pereira
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! 
! History: 
! Version     Date      Comment 
! -------   --------    ------- 
!           02.04.12    David A. B. Pereira
! 
!
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!                        Documenting Exchangeable Fortran 90 Code". 
! 

Implicit None

CHARACTER, INTENT(IN) :: LayerName *(*),Color*(*)

INTEGER, INTENT(IN) :: FileNum

REAL,INTENT(IN) :: DINT, Dext, X, Y

    write (FileNum,fmt='(A)')  "  0"
    write (FileNum,fmt='(A)')  "POLYLINE"
    write (FileNum,fmt='(A)')  "  8"
    write (FileNum,fmt='(A)')  TRIM(LAYERNAME)

      if (COLOR=="bylayer" .or. &                
          COLOR=="Bylayer" .or. &
          COLOR=="BYLAYER")  then
         ! Nothing to do if color is bylayer
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(COLOR)  !
      end if

    write (FileNum,fmt='(A)')  "  66"
    write (FileNum,fmt='(A)')  "  1"
    write (FileNum,fmt='(A)')  "  10"
    write (FileNum,fmt='(F10.6)') DINT
    write (FileNum,fmt='(A)')  "  20"
    write (FileNum,fmt='(F10.6)') DINT
    write (FileNum,fmt='(A)')  "  30"
    write (FileNum,fmt='(F10.6)') 0.0
    write (FileNum,fmt='(A)')  "  70"
    write (FileNum,fmt='(A)')  "  1"
    write (FileNum,fmt='(A)')  "  40"
    write (FileNum,fmt='(F10.6)') 0.50*DEXT
    write (FileNum,fmt='(A)')  "  41"
    write (FileNum,fmt='(F10.6)')  0.5*DEXT
    write (FileNum,fmt='(A)')  "  0"
    write (FileNum,fmt='(A)')  "VERTEX"
    write (FileNum,fmt='(A)')  "  8"
    write (FileNum,fmt='(A)')  TRIM(LAYERNAME)

      if (COLOR=="bylayer" .or. &                
          COLOR=="Bylayer" .or. &
          COLOR=="BYLAYER")  then
         ! Nothing to do if color is bylayer
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(COLOR)  !
      end if

    write (FileNum,fmt='(A)')  "  10"
    write (FileNum,fmt='(F10.6)')  X-0.25*DEXT
    write (FileNum,fmt='(A)')  "  20"
    write (FileNum,fmt='(F10.6)')  Y
    write (FileNum,fmt='(A)')  "  30"
    write (FileNum,fmt='(F10.6)')  0.0
    write (FileNum,fmt='(A)')  "  42"
    write (FileNum,fmt='(A)')  "  1.0"
    write (FileNum,fmt='(A)')  "  0"
    write (FileNum,fmt='(A)')  "VERTEX"
    write (FileNum,fmt='(A)')  "  8"
    write (FileNum,fmt='(A)')  TRIM(LAYERNAME)

      if (COLOR=="bylayer" .or. &                
          COLOR=="Bylayer" .or. &
          COLOR=="BYLAYER")  then
         ! Nothing to do if color is bylayer
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(COLOR)  !
      end if

    write (FileNum,fmt='(A)')  "  10"
    write (FileNum,fmt='(F10.6)')  X+0.25*DEXT
    write (FileNum,fmt='(A)')  "  20"
    write (FileNum,fmt='(F10.6)')  Y
    write (FileNum,fmt='(A)')  "  30"
    write (FileNum,fmt='(F10.6)')  0.0
    write (FileNum,fmt='(A)')  "  42"
    write (FileNum,fmt='(A)')  "  1.0"
    write (FileNum,fmt='(A)')  "  0"
    write (FileNum,fmt='(A)')  "SEQEND"
    write (FileNum,fmt='(A)')  "  8"
    write (FileNum,fmt='(A)')  TRIM(LAYERNAME)

      if (COLOR=="bylayer" .or. &                
          COLOR=="Bylayer" .or. &
          COLOR=="BYLAYER")  then
         ! Nothing to do if color is bylayer
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(COLOR)  !
      end if

RETURN
!
! The End of DONUT Subroutine
!
End Subroutine dfDonut

!
!
!+ Draw a line in DXF format.
! 
Subroutine dfLine   & 
          (FileNum, &
           Layer,   &
           Ltype,   &
           Color,   &
           X1,      &
           Y1,      &
           Z1,      &
           X2,      &
           Y2,      &
           Z2,      &
           Thick)   
! Description: 
!   Draw a line in the in the file <FileNum> in the layer <Layer>
! and in the color <Color> with coordenates of 1st note (X1, Y1),
! and the 2nd node coordenate (X2, Y2) and with the height <Thick>
! 
! Method: 
! No comment...
!
! Current Code Owner: 
! Carlos Otero & Davide Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! 
! History: 
! Version   Date     Comment 
! -------   -----    ------- 
! 0.1       00.11    Carlos Otero & Davide Santos
! 0.2       00.12    Carlos Otero & Davide Santos
! 0.3       02.02    David A. B. Pereira
!
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!                        Documenting Exchangeable Fortran 90 Code". 
! 

Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
 
! Subroutine arguments 
! Scalar arguments with intent(in): 

Real,Intent (in)       ::         &
                              X1, &           !+ Coordenate XX of the 1st node
                              Y1, &           !+ Coordenate YY of the 1st node   
                              Z1, &           !+ Coordenate ZZ of the 1st node 
                              X2, &           !+ Coordenate XX of the 2nd node
                              Y2, &           !+ Coordenate YY of the 2nd node
                              Z2, &           !+ Coordenate ZZ of the 1st node
                              Thick               !+ THICKNESS
    
Integer   ,Intent (in) :: FileNum !+ File number
                          
Character , Intent(in) ::            &
                         Color *(*), &        !+ Line's Color
                         Layer *(*), &        !+ Line's layer
                         Ltype *(*)           !+ Line's "LineType"

!- End of header --------------------------------------------------------------- 
      write (FileNum,fmt='(A)')  "  0"        !+ Begin of line
      write (FileNum,fmt='(A)')  "LINE"       !
      write (FileNum,fmt='(A)')  "  8"        !+ Layer code
      write (FileNum,fmt='(A)')  TRIM(Layer)  !+ Layer's name
      write (FileNum,fmt='(A)')  "  6"        !
      write (FileNum,fmt='(A)')  TRIM(Ltype)  !


      if (Color=="bylayer" .or. Color=="Bylayer" .or. Color=="BYLAYER") then
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(Color)  !
      end if
      
      write (FileNum,fmt='(A)')  " 39"        !+ THICKNESS OF THE LINE (3D)
      write (FileNum,fmt= *    )  Thick           ! 
      

      write (FileNum,fmt='(A)')  " 10"        !+ Begin of line coordenate X1
      write (FileNum,fmt= *    )  X1          ! 
      write (FileNum,fmt='(A)')  " 20"        !+ Begin of line coordenate Y1
      write (FileNum,fmt= *    )  Y1          !
      write (FileNum,fmt='(A)')  " 30"        !+ Begin of line coordenate Z1
      write (FileNum,fmt= *    )  Z1          !
      write (FileNum,fmt='(A)')  " 11"        !+ End of line coordenate Y2
      write (FileNum,fmt= *    )  X2          !
      write (FileNum,fmt='(A)')  " 21"        !+ End of line coordenate Y2
      write (FileNum,fmt= *    )  Y2          !
      write (FileNum,fmt='(A)')  " 31"        !+ End of line coordenate Z2
      write (FileNum,fmt= *    )  Z2          !
 return
 end subroutine dfLine

!
!+ Begin of a polyline
! 
Subroutine dfBeginPline & 
          (FileNum,     &
           Layer,       &
           Color) 
 
! Description: 
!   Begin a polyline in the file <FileNum>, in the layer <Layer> with
! the color <Color>.
! 
! Method: 
! No comment...
! 
! Current Code Owner: 
! Carlos Otero & Davide Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! www.ualg.pt/est/adec/csc/dxfortran
! 
! History: 
! Version   Date     Comment 
! -------   ----     ------- 
! 0.1  00.11  Carlos Otero & Davide Santos
! 
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
 
Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
! 
! Subroutine arguments 
! Scalar arguments with intent(in): 
 
Integer, Intent (in) :: FileNum !+ File number
                           
Character , Intent (in)  ::                &
                                Layer*(*), &     !+ Line's layer
                                Color*(*)        !+ Line's Color

!- End of header --------------------------------------------------------------- 
      write (FileNum,fmt='(A)') "  0"            !+ Begin of polyline
      write (FileNum,fmt='(A)') "POLYLINE"       !
      write (FileNum,fmt='(A)') "  8"            !+ Layer Code
      write (FileNum,fmt='(A)')      trim(Layer)       !+ Layer's name
      if (Color=="bylayer" .or. Color=="Bylayer" .or. Color=="BYLAYER") then
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  trim(Color)  !
      end if
      write (FileNum,fmt='(A)') " 66"            !
      write (FileNum,fmt='(A)') "    1"          !
      write (FileNum,fmt='(A)') "  0"            !
 return
end subroutine dfBeginPline
!
!+ End of a polyline
! 
Subroutine dfEndPline          & 
                     (FileNum, &
                      Layer,   &
                      Color) 
! Description: 
!   End of a polyline in the file <FileNum> in the layer <Layer> with the
! color <Color>.
! 
! Method: 
! No comment...
!
! Current Code Owner: 
! Carlos Otero & Davide Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! www.ualg.pt/est/adec/csc/dxfortran
! 
! History: 
! Version   Date     Comment 
! -------   ----     ------- 
!   0.1     00.11    Carlos Otero & Davide Santos (University of Algarve - Portugal)
! 
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 

Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
 
! Subroutine arguments 
! Scalar arguments with intent(in): 

Integer   ,Intent (in)  :: FileNum        !+ File number
                           
Character , Intent(in)  ::              &
                           Layer*(*),   & !+ Polyline's layer
                           Color*(*)      !+ Polyline's color
 
!- End of header --------------------------------------------------------------- 

      write (FileNum,fmt='(A)') "SEQEND"    !
      write (FileNum,fmt='(A)') "  8"       !+ Layer's code
      write (FileNum,fmt='(A)') trim(Layer) !+ Layer's name
      if (Color=="bylayer" .or. Color=="Bylayer" .or. Color=="BYLAYER") then
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(Color)  !
      end if
 return
 end subroutine dfEndPline
!
!+ Definition of the polyline vertex.
!
subroutine dfPlVertex          & 
                     (FileNum, &
                      Layer,   &
                      Color,   &
                      X,       &
                      Y,       &
                      Z)
! Description: 
!   Vertex of the polyline in the file <FileNum> in the layer <Layer>,
!  with the color <Color> and coordenates (X,Y,Z).
!
! Method: 
! No comment...
!
! Current Code Owner: 
! Carlos Otero & Davide Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! www.ualg.pt/est/adec/csc/dxfortran
! 
! History: 
! Version   Date     Comment 
! -------   ----     ------- 
! 0.1       00.11    Carlos Otero & Davide Santos
! 
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!   Documenting Exchangeable Fortran 90 Code". 
! 
 
Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
! 
! Subroutine arguments 
! Scalar arguments with intent(in): 

real, intent (in)          :: X,   &          !+ coordenate XX of the polyline's vertex
                              Y,   &          !+ coordenate YY of the polyline's vertex
                              Z               !+ coordenate ZZ of the polyline's vertex
integer, Intent (in)       :: FileNum         !+ File's number

Character , Intent (in)    ::        &        !+ File's number
                           Layer*(*),&        !+ Layer's name
                           Color*(*)          !+ Color's number
!- End of header --------------------------------------------------------------- 

      write (FileNum,fmt='(A)')   "VERTEX"       !
      write (FileNum,fmt='(A)')   "  8"          !+ Layer's code
      write (FileNum,fmt='(A)')       trim(Layer)!+ Layer's name
      if (Color=="bylayer" .or. Color=="Bylayer" .or. Color=="BYLAYER") then
      else
         write (FileNum,fmt='(A)')  " 62"         !+ Color code
         write (FileNum,fmt='(A)')  TRIM(Color)   !
      end if
      write (FileNum,fmt='(A)')   " 10"       !+ Code of the coordenate X of the polyline vertex
      write (FileNum,fmt= *)   X              !+ Coordenate X of the vertex
      write (FileNum,fmt='(A)')   " 20"       !+ Code of the coordenate Y of the polyline vertex
      write (FileNum,fmt= *)   Y              !+ Coordenate Y of the vertex
      write (FileNum,fmt='(A)')   " 30"       !+ Code of the coordenate Z of the polyline vertex
      write (FileNum,fmt= *)   Z              !+ Coordenate Z of the vertex
      write (FileNum,fmt='(A)')  "  0"        !        

      return
 end subroutine dfPlVertex
!
!+ Write text to file.
!
Subroutine dfText           & 
                 (FileNum,  &
                  Layer,    &
                  Color,    &
                  X,        &
                  Y,        &
                  Z,        &
                  Text,     &
                  Height,   &
                  Factor,   &
                  Angle,    &
                  Alignh,    &
                  Alignv,    &
                  Style)  
! Description: 
!   Write text to the file <FileNum>, color <Color> and with the
! coordenates (X,Y). The message is stored in the variable <Text>,
! with de height <Height>, angle <Angle> and horizontal aligned with <Alignh),
! and vertical aligned with <alignv>:
! - Align left   : <Alignh> = 0         - Align none   : <Alignv> = 0
! - Align center : <Alignh> = 1         - Align bottom : <Alignv> = 1
! - Align right  : <Alignh> = 2         - Align middle : <Alignv> = 2
!                                       - Align top    : <Alignv> = 3
!
!
! Method: 
! No comment... 
!
! Current Code Owner: 
! Carlos Otero & Davide Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! 
! History: 
! Version   Date     Comment 
! -------   ----     ------- 
! 0.1       00.11    Carlos Otero & Davide Santos
! 0.2       02.04    David Pereira
! 
!
!
! V 0.2: inserts the vertical alignment options for the text position.
!
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 

Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
! Subroutine arguments 
! Scalar arguments with intent(in): 

Real, Intent (in)         ::        &
                             X,     &           !+ Coordenate X of the text.
                             Y,     &           !+ Coordenate Y of the text.
                             Z,     &           !+ Coordenate Z of the text.
                             Height, &          !+ Height of the text.
                             Factor, &          !+ Factor of the text.
                             Angle              !+ Angle of the text. 

Integer ,Intent (in)      ::         & 
                             Alignh,  &         !+ Text horizontal alignment.
                             Alignv,  &         !+ Text vertical alignment.
                             FileNum            !+ File's number. 

Character , Intent (in)   ::       &
                             Layer*(*), &       !+ Layer of the text.
                             Color*(*), &       !+ Color of the text.
                             Text*(*),  &       !+ Text.
                             Style*(*)          !+ Text style
 
!- End of header --------------------------------------------------------------- 

      write (FileNum,fmt='(A)') "  0"
      write (FileNum,fmt='(A)') "TEXT"
      write (FileNum,fmt='(A)') "  8"
      write (FileNum,fmt='(A)')     trim(Layer)
      if (Color=="bylayer" .or. Color=="Bylayer" .or. Color=="BYLAYER") then
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt=*     )  TRIM(Color) !
      end if
      write (FileNum,fmt='(A)') " 10"            !+ X code coordenate of the text.
      write (FileNum,*)    X                     !+ X coordenate.
      write (FileNum,fmt='(A)') " 20"            !+ Y code coordenate of the text.
      write (FileNum,*)    Y                     !+ Y coordenate.
      write (FileNum,fmt='(A)') " 30"            !+ Z code coordenate of the text.
      write (FileNum,*)    Z                     !+ Z coordenate.
      write (FileNum,fmt='(A)') " 40"
      write (FileNum,*)    Height
      write (FileNum,fmt='(A)') " 41"
      write (FileNum,*)    Factor                !+ Scale Factor
      write (FileNum,fmt='(A)') "  1"
      write (FileNum,*)    TRIM(text)
      write (FileNum,fmt='(A)') " 50"
      write (FileNum,*)    Angle
      write (FileNum,fmt='(A)') "  7"
      write (FileNum,fmt='(A)') TRIM(style)
      write (FileNum,fmt='(A)') " 72"
      write (FileNum,fmt='(I1)') alignh
      write (FileNum,fmt='(A)') " 11"
      write (FileNum,*)    X
      write (FileNum,fmt='(A)') " 21"
      write (FileNum,*)    Y                    !+ Height / 2.0 (Removed in version 0.2)
      write (FileNum,fmt='(A)') " 73"
      write (FileNum,fmt='(I1)') alignv

      return
 End Subroutine dfText
!
!+ Draw a 3dfACE in DXF format.
! 
Subroutine df3DFace   & 
          (FileNum,   &
           Layer,     &
           Ltype,     &
           Color,     &
           X1,        &
           Y1,        &
           Z1,        &
           X2,        &
           Y2,        &
           Z2,        &
           X3,        &
           Y3,        &
           Z3,        &
           X4,        &
           Y4,        &
           Z4)   
!
! Description: 
! 
! Draw a 3DFace in the in the file <FileNum> in the layer <Layer>
! and in the color <Color> with coordenates 
! 
! Node 1 - (X1, Y1, Z1)      1------2
! Node 2 - (X2, Y2, Z2)      |      |
! Node 3 - (X3, Y3, Z3)      |      |
! Node 4 - (X4, Y4, Z4)      4------3
!
! 
! Method: 
! No comment...
!
! Current Code Owner: 
! Carlos Otero & Davide Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! www.ualg.pt/est/adec/csc/dxfortran
! 
! History: 
! Version   Date     Comment 
! -------   -----    ------- 
!           01.06    Carlos Otero & Davide Santos
! 
!
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 

Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
 
! Subroutine arguments 
! Scalar arguments with intent(in): 

Real,Intent (in)         ::               &
                              X1,         &     !+ Coordenate XX - node 1
                              Y1,         &     !+ Coordenate YY
                              Z1,         &     !+ Coordenate ZZ
                              X2,         &     !+ Coordenate XX - node 2
                              Y2,         &     !+ Coordenate YY
                              Z2,         &     !+ Coordenate ZZ
                              X3,         &     !+ Coordenate XX - node 3
                              Y3,         &     !+ Coordenate YY
                              Z3,         &     !+ Coordenate ZZ
                              X4,         &     !+ Coordenate XX - node 4
                              Y4,         &     !+ Coordenate YY
                              Z4                !+ Coordenate ZZ

    
Integer   ,Intent (in) ::     FileNum           !+ File number
                          
Character , Intent(in) ::                 &
                              Color *(*), &     !+ 3DFace Color
                              Layer *(*), &     !+ 3DFace layer
                              Ltype *(*)        !+ 3DFace "LineType"

!- End of header --------------------------------------------------------------- 
      write (FileNum,fmt='(A)')  "  0"          !+ Begin of 3DFace
      write (FileNum,fmt='(A)')  "3DFACE"         !
      write (FileNum,fmt='(A)')  "  8"          !+ Layer code
      write (FileNum,fmt='(A)')  TRIM(Layer)    !  Layer's name
      write (FileNum,fmt='(A)')  "  6"          !+ LType code
      write (FileNum,fmt='(A)')  TRIM(Ltype)    !  LType name

      if (Color=="bylayer" .or. &                
          Color=="Bylayer" .or. &
          Color=="BYLAYER")  then
         ! Nothing to do if color is bylayer
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(Color)  !
      end if
      
      write (FileNum,fmt='(A)')  " 10"           !+ Begin of 3Dface - Node 1
      write (FileNum,fmt= *   )  X1              ! 
      write (FileNum,fmt='(A)')  " 20"           !
      write (FileNum,fmt= *   )  Y1              !
      write (FileNum,fmt='(A)')  " 30"           !
      write (FileNum,fmt= *   )  Z1              !

      write (FileNum,fmt='(A)')  " 11"           !+ Begin of 3Dface - Node 2
      write (FileNum,fmt= *   )  X2              ! 
      write (FileNum,fmt='(A)')  " 21"           !
      write (FileNum,fmt= *   )  Y2              !
      write (FileNum,fmt='(A)')  " 31"           !
      write (FileNum,fmt= *   )  Z2              !

      write (FileNum,fmt='(A)')  " 12"           !+ Begin of 3Dface - Node 3
      write (FileNum,fmt= *   )  X3              ! 
      write (FileNum,fmt='(A)')  " 22"           !
      write (FileNum,fmt= *   )  Y3              !
      write (FileNum,fmt='(A)')  " 32"           !
      write (FileNum,fmt= *   )  Z3              !

      write (FileNum,fmt='(A)')  " 13"           !+ Begin of 3Dface - Node 4
      write (FileNum,fmt= *   )  X4              ! 
      write (FileNum,fmt='(A)')  " 23"           !
      write (FileNum,fmt= *   )  Y4              !
      write (FileNum,fmt='(A)')  " 33"           !
      write (FileNum,fmt= *   )  Z4              !

 return
 end subroutine df3DFace

!
!+ Draw a Ponit in DXF format.
! 
Subroutine dfpoint    & 
          (FileNum,   &
           Layer,     &
           Color,     &
           X,         &
           Y,         &
           Z,         &
		   Thick)   
!
! Description: 
! 
! Draw a 2D or 3D Point in the in the file <FileNum> in the layer <Layer>
! and in the color <Color> with coordenates (X,Y,Z).
!
! 
! Method: 
! No comment...
!
! Current Code Owner: 
! Carlos Otero & Davide Santos
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! www.ualg.pt/est/adec/csc/dxfortran
! 
! History: 
! Version   Date     Comment 
! -------   -----    ------- 
!           01.06    Carlos Otero & Davide Santos
! 
!
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 

Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
 
! Subroutine arguments 
! Scalar arguments with intent(in): 

Real, Intent (in)        ::              &
                              X,         &     !+ Coordenate XX
                              Y,         &     !+ Coordenate YY
                              Z,         &     !+ Coordenate ZZ
							  Thick			   !+ Thickness of the Point

    
Integer   ,Intent (in)   ::   FileNum          !+ File number
                          
Character , Intent(in)   ::               &
                              Color *(*), &     !+ 3DFace Color
                              Layer *(*)        !+ 3DFace layer

!- End of header --------------------------------------------------------------- 
      write (FileNum,fmt='(A)')  "  0"          !+ Begin of 3DFace
      write (FileNum,fmt='(A)')  "POINT"        !
      write (FileNum,fmt='(A)')  "  8"          !+ Layer code
      write (FileNum,fmt='(A)')  TRIM(Layer)    !  Layer's name

      if (Color=="bylayer" .or. &                
          Color=="Bylayer" .or. &
          Color=="BYLAYER")  then
         ! Nothing to do if color is bylayer
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(Color)  !
      end if
      write (FileNum,fmt='(A)')  " 39"			 !+ Thickness of the Point
      write (FileNum,fmt= *    ) Thick			  ! 
      
      write (FileNum,fmt='(A)')  " 10"           !+ Point codes
      write (FileNum,fmt= *   )  X               ! 
      write (FileNum,fmt='(A)')  " 20"           !
      write (FileNum,fmt= *   )  Y               !
      write (FileNum,fmt='(A)')  " 30"           !
      write (FileNum,fmt= *   )  Z               !

 return
 end subroutine dfpoint
!
! Draw a SOLID in DXF format.
! 
Subroutine dfSolid    & 
          (FileNum,   &
           Layer,     &
           Ltype,     &
           Color,     &
           X1,        &
           Y1,        &
           X2,        &
           Y2,        &
           X3,        &
           Y3,        &
           X4,        &
           Y4,        &
           Z4,        &
           Thick)   
!
! Description: 
! 
! Draw a SOLID in the in the file <FileNum> in the layer <Layer>
! and in the color <Color> with coordenates <X1,Y1><X2,Y2><X3,Y3>
! <X4,Y4,Z4> and the thickness of the SOLID.
! 
! Node 1 - (X1, Y1)           1------2
! Node 2 - (X2, Y2)           |      |
! Node 3 - (X3, Y3)           |      |
! Node 4 - (X4, Y4, Z4)       3------4
!
! 
! Method: 
! No comment...
!
! Current Code Owner: 
! David A. B. Pereira
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! 
! History: 
! Version     Date      Comment 
! -------   --------    ------- 
!           02.02.18    David A. B. Pereira
! 
!
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 

Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
 
! Subroutine arguments 
! Scalar arguments with intent(in): 

Real,Intent (in)         ::               &
                              X1,         &     !+ Coordenate XX - node 1
                              Y1,         &     !+ Coordenate YY
                              X2,         &     !+ Coordenate XX - node 2
                              Y2,         &     !+ Coordenate YY
                              X3,         &     !+ Coordenate XX - node 3
                              Y3,         &     !+ Coordenate YY
                              X4,         &     !+ Coordenate XX - node 4
                              Y4,         &     !+ Coordenate YY
                              Z4,         &     !+ Coordenate ZZ
                              Thick             !+ Thickness

    
Integer   ,Intent (in) ::     FileNum           !+ File number
                          
Character , Intent(in) ::                 &
                              Color *(*), &     !+ SOLID Color
                              Layer *(*), &     !+ SOLID layer
                              Ltype *(*)        !+ SOLID "LineType"

!- End of header --------------------------------------------------------------- 
      write (FileNum,fmt='(A)')  "  0"          !+ Begin of SOLID
      write (FileNum,fmt='(A)')  "SOLID"        !
      write (FileNum,fmt='(A)')  "  8"          !+ Layer code
      write (FileNum,fmt='(A)')  TRIM(Layer)    !  Layer's name
      write (FileNum,fmt='(A)')  "  6"          !+ LType code
      write (FileNum,fmt='(A)')  TRIM(Ltype)    !  LType name

      if (Color=="bylayer" .or. &                
          Color=="Bylayer" .or. &
          Color=="BYLAYER")  then
         ! Nothing to do if color is bylayer
      else
         write (FileNum,fmt='(A)')  " 62"        !+ Color code
         write (FileNum,fmt='(A)')  TRIM(Color)  !
      end if
      
      write (FileNum,fmt='(A)')  " 10"           !+ Begin of SOLID - Node 1
      write (FileNum,fmt= *   )  X1              ! 
      write (FileNum,fmt='(A)')  " 20"           !
      write (FileNum,fmt= *   )  Y1              !
      write (FileNum,fmt='(A)')  " 30"           !
      write (FileNum,fmt= *   )  Z4              !

      write (FileNum,fmt='(A)')  " 11"           !+ Begin of SOLID - Node 2
      write (FileNum,fmt= *   )  X2              ! 
      write (FileNum,fmt='(A)')  " 21"           !
      write (FileNum,fmt= *   )  Y2              !
      write (FileNum,fmt='(A)')  " 31"           !
      write (FileNum,fmt= *   )  Z4              !

      write (FileNum,fmt='(A)')  " 12"           !+ Begin of SOLID - Node 3
      write (FileNum,fmt= *   )  X3              ! 
      write (FileNum,fmt='(A)')  " 22"           !
      write (FileNum,fmt= *   )  Y3              !
      write (FileNum,fmt='(A)')  " 32"           !
      write (FileNum,fmt= *   )  Z4              !

      write (FileNum,fmt='(A)')  " 13"           !+ Begin of SOLID - Node 4
      write (FileNum,fmt= *   )  X4              ! 
      write (FileNum,fmt='(A)')  " 23"           !
      write (FileNum,fmt= *   )  Y4              !
      write (FileNum,fmt='(A)')  " 33"           !
      write (FileNum,fmt= *   )  Z4              !
       write (FileNum,fmt='(A)')  " 39"          !
      write (FileNum,fmt= *   )  Thick           !+ Thickness
       
 return
 end subroutine dfSolid


!
! Draw a Solid Box
!

Subroutine dfSSolidBox & 
          (FileNum,   &
           Layer,     &
           Ltype,     &
           Color,     &
           X1,        &
           Y1,        &
           Z1,        &
           H1,		  &
		    B,        &
			L)
           
! Description: 
! Draw a Solid Box giving the center point <X1, Y1, Z1>,
! the width <B>, the length <L> and the thickness of the box.
!
! Method: 
! No comment...
!
! Current Code Owner: 
! David Pereira
! (Department of Civil Engineering of the University of Algarve - Portugal)     
! 
! History: 
! Version   Date     Comment 
! -------   -----    ------- 
! 0.1       02.02    David Pereira
!
! Code Description: 
!   Language:  Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 

Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
 
! Subroutine arguments 
! Scalar arguments with intent(in): 

Real,Intent (in)       ::         &
                              X1, &  !+ Coordenate XX of the 1st node
                              Y1, &  !+ Coordenate YY of the 1st node   
                              Z1, &  !+ Coordenate ZZ of the 1st node 
							  H1, &	 !+ THICKNESS
							   B, &  !+ WIDTH OF THE BOX
							   L     !+ LENGHT OF THE BOX

Integer   ,Intent (in) :: FileNum !+ File number
                          
Character , Intent(in) ::            &
                         Color *(*), &  !+ Line's Color
                         Layer *(*), &  !+ Line's layer
                         Ltype *(*)     !+ Line's "LineType"

REAL :: X(4),Y(4),Z(4)
INTEGER :: I

!+
!+ Definition of the Box corners
!+

X(1)=X1-0.50*B
X(2)=X1+0.50*B
X(3)=X(1)
X(4)=X(2)
Y(1)=Y1+0.50*L
Y(2)=Y(1)
Y(3)=Y1-0.50*L
Y(4)=Y(3)


CALL dfSolid(FileNum, Layer,Ltype,Color,X(1),Y(1),X(2),Y(2),X(3),Y(3),X(4),Y(4),Z1,H1)

  return
  end subroutine dfSSolidBox

End Module DXFortranStd
