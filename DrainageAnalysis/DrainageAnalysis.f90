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

!****************************************************************************
!
!  PROGRAM: DrainageAnalysis
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program DrainageAnalysis

    use Tinsurface
    
    implicit none

    ! Variables
    
    integer,parameter::fileId = 10 , logFileID = 20
    
    logical::alive     !检查文件是否存在
    
    character(len = 256)::fileName = "MeshData\mesh.2dm"
    
    character(len = 256)::fileNameIn
    
    real::typicalElevation = 260.0 , areaThreshold = 0

    
    character(len=1)::ExitOrNot = 'N'
    
    real(8)::startTime,endTime
    
    
     !!Body of DrainageAnalysis
     write(*,*) "**********************************************************************" 
     write(*,*) "*****              Drainage extraction for TIN DEM.              *****" 
     write(*,*) "*****                      Author: Danyang Su.                   *****" 
     write(*,*) "*****                Email: danyang.su@gmail.com                 *****" 
     write(*,*) "*****                           License                          *****" 
     write(*,*) "***** This program is free software under the terms of the GNU   *****" 
     write(*,*) "***** General Public License. This program is distributed in the *****" 
     write(*,*) "***** hope that it will be useful, but WITHOUT ANY WARRANTY;     *****" 
     write(*,*) "***** without even the implied warranty of MERCHANTABILITY or    *****" 
     write(*,*) "***** FITNESS FOR A PARTICULAR PURPOSE.                          *****" 
     write(*,*) "***** See the GNU General Public License for more details.       *****"    
     write(*,*) "**********************************************************************"
     write(*,*) ""
     
    do while(.true.)
        
        write(*,*) "Please press Y if you want to exit, or any other key (except enter)"
        write(*,*) "to continue..."
        
        read(*,*) ExitOrNot
        
        if(ExitOrNot == 'Y' .OR. ExitOrNot == 'y') then
        
            exit
            
        end if
        
        write(*,*) "Type in mesh file name located in MeshData folder, without extension"
        
        read(*,*) fileNameIn
        
        write(*,*) "Type in drainage area threshold (m^2)"
        
        read(*,*) areaThreshold
        
!        write(*,*) "Type in typical contour line elevation, eg. 260"
!        
!        read(*,*) typicalElevation
        
        fileName = "MeshData\" // trim(fileNameIn) // ".2dm"
        
        inquire (file = fileName,exist = alive)
        
        if(.not.alive) then
        
            write(*,*) trim(fileName)," file do not exist,please check!"
            
            cycle
        
        end if
        
        !!Reading data:Required
        write(*,*) "Reading data ..."
        
        call CPU_TIME(startTime)
        
        call GetData(fileID,fileName)
        
        call CPU_TIME(endTime)
        
        write(*,*) "Time elapse:",endTime-startTime,"seconds"
        
        
        !!Making Tin topology:Required
        write(*,*) "Making Tin topology..."
        
        call CPU_TIME(startTime)
        
        call MakeTinTopology()
        
        call CPU_TIME(endTime)
        
        write(*,*) "Time elapse:",endTime-startTime,"seconds"
        
        
        
!        !!Checking Tin :not required if no tin error exist
!        write(*,*) "Checking Tin topology..."
!        
!        call CPU_TIME(startTime)
!        
!        call CheckTinTopology()
!        
!        call CPU_TIME(endTime)
!        
!        write(*,*) "Time elapse:",endTime-startTime,"seconds"
        
        
        !!Revise flat area:Requirdd
        write(*,*) "Revise flat area ..."
        
        call CPU_TIME(startTime)
        
        call RevistDepression()
        
        call CPU_TIME(endTime)
        
        write(*,*) "Time elapse:",endTime-startTime,"seconds"
        
        
        !!Mark derpession bottom:Required
        write(*,*) "Mark derpession bottom ..."
        
        call CPU_TIME(startTime)
        
        call MarkDepressionBottomNode()
        
        call CPU_TIME(endTime)
        
        write(*,*) "Time elapse:",endTime-startTime,"seconds"
        
        
        !!Calculating water flow simulation:Required
        write(*,*) "Calculating water flow simulation ..."
        
        call CPU_TIME(startTime)
        
        call CalculateFlowValue()
        
        call CPU_TIME(endTime)
        
        write(*,*) "Time elapse:",endTime-startTime,"seconds"
        
       
        !!Making drainage networks:Required
        write(*,*) "Making drainage networks..."
        
        call CPU_TIME(startTime)
        
        call CalculateDrainage()
        
        call CPU_TIME(endTime)
        
        write(*,*) "Time elapse:",endTime-startTime,"seconds"
        
 
!        !!Check drainage network net:Not Required
!        write(*,*) "正在检验..."
!        
!        call CPU_TIME(startTime)
!        
!        call CheckTinNet()
!        
!        call CPU_TIME(endTime)
!        
!        write(*,*) "耗时：",endTime-startTime,"秒" 

        !!Revise Drainage networks:Required
        write(*,*) "Revise Drainage networks..."
        
        call CPU_TIME(startTime)
        
        call ReviseAbnormalDrainages(areaThreshold)
        
        call CPU_TIME(endTime)
        
        write(*,*) "Time elapse:",endTime-startTime,"seconds"  
        
!        !!Mark outerboundary nodes and nodes that span typical contour line:Required
!        write(*,*) "Mark outerboundary nodes and nodes that span typical contour line..."
!        
!        call CPU_TIME(startTime)
!        
!        call MarkOuterboundaryNodes()
!        
!        call MarkTypicalLineNodes(typicalElevation)
!        
!        call CPU_TIME(endTime)
!        
!        write(*,*) "Time elapse:",endTime-startTime,"seconds" 
        
                
        !! Output splitted meshes files:optional
        write(*,*) "Output splitted meshe files..."
        
        call CPU_TIME(startTime)
        
        call OutputSplittedMeshes(fileNameIn)
        
        call CPU_TIME(endTime)
        
        write(*,*) "Time elapse:",endTime-startTime,"seconds"  

        !! output to one file test
        
        call OutputSplittedMeshesOneFile(fileNameIn)
        
        
        !!output drainage networks to DXF File
        call OutputDrainageToDXF(fileNameIn)
        
        
        !!output drainage networks to DXF File
        !!call OutputLocalDrainageToDXF(fileNameIn,360)
        !!
        
        write(*,*) "Finish"
        
        write(*,*) ""
        
    end do
        
    end program DrainageAnalysis
    

!    program Test
!    
!        use Geometry
!        
!        implicit none
!        
!        real::xa,ya,za,xb,yb,zb,xc,yc,zc,vala,valb,valc 
!        
!        character(len=1)::ExitOrNot = 'N'
!        
!        do while(.true.)
!                
!            write(*,*) "退出操作请输入Y，继续操作请输入其它任意字符"
!            
!            read(*,*) ExitOrNot
!            
!            if(ExitOrNot == 'Y' .OR. ExitOrNot == 'y') then
!            
!                exit
!                
!            end if
!        
!!            write(*,*)"输入：xa,ya,za,xb,yb,zb,xc,yc,zc"
!!            
!!            read(*,*) xa,ya,za,xb,yb,zb,xc,yc,zc
!!            
!!            call calculateFlowToLineValue(xa,ya,za,xb,yb,zb,xc,yc,zc,vala,valb,valc)
!!            
!!            write(*,*) "水流值：",vala,valb,valc
!!            
!!            call calculateFlowDirection(xa,ya,za,xb,yb,zb,xc,yc,zc,vala,valb)
!!            
!!            write(*,*)"水流方向：",vala,"i + ",valb,"j"
!
!!            write(*,*)"输入：xa,ya"
!!            
!!            read(*,*) xa,ya
!!            
!!            write(*,*) xa == ya
!
!            read(*,*) xa,ya,xb,yb,xc,yc
!            
!            write(*,*) "area:" , triangleArea2D(xa,ya,xb,yb,xc,yc)
!            
!        end do
!    
!    end program Test

