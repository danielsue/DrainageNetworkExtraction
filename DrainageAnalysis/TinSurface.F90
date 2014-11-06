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
!!
!!
!!三角网曲面
!!

Module TinSurface

    use TinSturcture
    
    use InputMeshFile
    
    use Geometry
    
    use DXFortranStd
    
    implicit none
    
    type(Triangle), allocatable::Triangles(:)     !!三角形链表
    
    type(Node), allocatable::Nodes(:)             !!节点链表
    
    type(Line),allocatable::Lines(:)              !!边链表
    
    integer, allocatable::BoundaryNodesID(:)     !!边界节点ID序列
    
    integer,allocatable::DepressionBottomNodesID(:)      !!洼地底点ID
    
    integer,allocatable::NormalDrainageOutletNodesID(:)  !!正常流域出口点ID
    
    integer,allocatable::PseudoDrainageOutletNodesID(:)  !!伪洼地流域出口点ID
    
    integer,allocatable::TriID_Index(:)          !!三角形ID与三角形索引的差值列表，辅助提高访问速率用
    
    integer,allocatable::NodesID_Index(:)        !!节点ID与节点索引的差值列表，辅助提高访问速率用
    
    type(Drainage),allocatable::Drainages(:)          !!所有流域
    
    type(LinesStatistics) :: LinesStat
    
    public Triangles,Nodes,BoundaryNodesID,DepressionBottomNodesID,Lines
    
    private TriID_Index,NodesID_Index
    
    public GetData,MakeTinTopology,CheckTinTopology,RevistDepression,MarkDepressionBottomNode,CalculateFlowValue
    
    public CalculateDrainage,CheckTinNet,ReviseAbnormalDrainages, MarkOuterboundaryNodes, MarkTypicalLineNodes
    
    public OutputSplittedMeshes,OutputSplittedMeshesOneFile,OutputDrainageToDXF,OutputLocalDrainageToDXF
    
    private NodeID2Index,TriID2Index,makeNodeID_IndexRelation,makeTriID_IndexRelation,CalculateSubDrainage,trackFlowTriangles
    
    private mergeDrainage
    
    
    contains
        
        !!
        !!获取原始2DM三角网数据
        !!
        subroutine GetData(fileId,fileName)
        
            implicit none
            
            integer,intent(in)::fileId
            
            character(len=*) , intent(in)::fileName
            
            call GetTinData(fileId,fileName,Triangles,Nodes)
            
            !!write(*,*) "文件扫描：",ScanStatus,"数据读取：", InputStatus
            
            if(InputStatus) then
            
                write(*,*) "Triangles Amount:",size(Triangles),"Nodes Amount:",size(Nodes)
                
                call makeNodeID_IndexRelation()
                
                call makeTriID_IndexRelation()
            
            end if
            
        
        end subroutine
        
        !!
        !!三角形拓扑生成
        !!
        subroutine MakeTinTopology()
        
            implicit none
            
            integer :: i , j , k ,ii
            
            integer::nodeTemp(NodesAmount)
            
            integer::nodeIndexTemp(4)
            
            integer,allocatable::intArrayTemp(:)
            
            type(Line),allocatable:: linesTemp(:)

            if(allocated(linesTemp)) then
            
                deallocate(linesTemp)
            
            end if
            
            allocate(linesTemp(2 * TrianglesAmount + 1))
           
            !!节点－与节点相连的三角形ID
             
            !!统计节点所连的三角形个数
            do i = 1 , TrianglesAmount
            
                do j = 1 , 3
                
                    k = NodeID2Index(Triangles(i)%NodesID(j))
                    
                    Nodes(k)%LinkedTriAmount = Nodes(k)%LinkedTriAmount + 1
                    
                end do
            
            end do
            
            !!分配内存
            do i = 1 , NodesAmount
            
                if(allocated(Nodes(i)%LinkedTriID)) then
                
                    deallocate(Nodes(i)%LinkedTriID)
                
                end if
                
                allocate(Nodes(i)%LinkedTriID(Nodes(i)%LinkedTriAmount))
            
            end do
            
            !!节点所连三角形赋值
            nodeTemp = 1

            do i = 1 , TrianglesAmount
            
                do j = 1 , 3
                
                    k = NodeID2Index(Triangles(i)%NodesID(j))
                    
                    Nodes(k)%LinkedTriID(nodeTemp(k)) = Triangles(i)%ID
                    
                    nodeTemp(k) = nodeTemp(k) + 1
                    
                end do
            
            end do
            
            !!节点－节点，节点－边，三角形－边属性
            
            do i = 1 , NodesAmount
            
                if(allocated(Nodes(i)%LinkedNodesID)) then
                
                    deallocate(Nodes(i)%LinkedNodesID)
                
                end if
            
                allocate(Nodes(i)%LinkedNodesID(Nodes(i)%LinkedTriAmount + 1))
                
                Nodes(i)%LinkedNodesID = -1

                if(allocated(Nodes(i)%LinkedLinesID)) then
                
                    deallocate(Nodes(i)%LinkedLinesID)
                
                end if
                
                allocate(Nodes(i)%LinkedLinesID(Nodes(i)%LinkedTriAmount + 1))
                
                Nodes(i)%LinkedLinesID = -1
                
            end do
            
            nodeTemp = 0
            
            ii = 0  !!临时表示边的索引及有效个数
            
            do i = 1 , TrianglesAmount
            
                nodeIndexTemp = -1
            
                do j = 1 , 3
                
                    nodeIndexTemp(j) = NodeID2Index(Triangles(i)%NodesID(j))
                
                end do
                
                nodeIndexTemp(4) = NodeID2Index(Triangles(i)%NodesID(1))
                
                do j = 1 , 3
                
                    if(any(Nodes(nodeIndexTemp(j))%LinkedNodesID == Nodes(nodeIndexTemp(j + 1))%ID )) then
                    
                        do k = 1 , nodeTemp(nodeIndexTemp(j))
                        
                            if(linesTemp(Nodes(nodeIndexTemp(j))%LinkedLinesID(k))%StartNodeID == Nodes(nodeIndexTemp(j + 1))%ID) then
                            
                                linesTemp(Nodes(nodeIndexTemp(j))%LinkedLinesID(k))% RightTriID = Triangles(i)%ID
                                
                                if(j > 1) then
                                
                                    Triangles(i)%LinesID(j-1) = Nodes(nodeIndexTemp(j))%LinkedLinesID(k)
                                    
                                else
                                
                                    Triangles(i)%LinesID(3) = Nodes(nodeIndexTemp(j))%LinkedLinesID(k)
                                
                                end if
                                
                                exit
                            
                            end if
                        
                        end do
                        
                    else
                    
                        ii = ii + 1
                        
                        nodeTemp(nodeIndexTemp(j)) = nodeTemp(nodeIndexTemp(j)) + 1
                    
                        Nodes(nodeIndexTemp(j))%LinkedNodesID(nodeTemp(nodeIndexTemp(j))) = Nodes(nodeIndexTemp(j + 1))%ID
                        
                        Nodes(nodeIndexTemp(j))%LinkedLinesID(nodeTemp(nodeIndexTemp(j))) = ii
                        
                        nodeTemp(nodeIndexTemp(j + 1)) = nodeTemp(nodeIndexTemp(j + 1)) + 1
                        
                        Nodes(nodeIndexTemp(j +1))%LinkedNodesID(nodeTemp(nodeIndexTemp(j + 1))) = Nodes(nodeIndexTemp(j))%ID
                        
                        Nodes(nodeIndexTemp(j + 1))%LinkedLinesID(nodeTemp(nodeIndexTemp(j + 1))) = ii
                            
                        linesTemp(ii)%ID = ii
                        
                        linesTemp(ii)%StartNodeID = Nodes(nodeIndexTemp(j))%ID
                        
                        linesTemp(ii)%EndNodeID = Nodes(nodeIndexTemp(j + 1))%ID
                        
                        linesTemp(ii)%LeftTriID = Triangles(i)%ID
                        
                        if(j>1) then
                        
                            Triangles(i)%LinesID(j - 1) = ii
                            
                        else 
                        
                            Triangles(i)%LinesID(3) = ii
                        
                        end if
                    
                    end if
                
                end do
                            
            end do
            
            
            if(allocated(Lines)) then
            
                deallocate(Lines)
            
            end if
            
            allocate(Lines(ii))
            
            Lines = linesTemp(1:ii)
            
            deallocate(linesTemp)
            
            do i = 1 , NodesAmount
            
                if(allocated(intArrayTemp)) then
                
                    deallocate(intArrayTemp)
                
                end if
                
                allocate(intArrayTemp(nodeTemp(i)))
                
                intArrayTemp = Nodes(i)%LinkedNodesID(1:nodeTemp(i))
                
                deallocate(Nodes(i)%LinkedNodesID)
                
                allocate(Nodes(i)%LinkedNodesID(nodeTemp(i)))
                
                Nodes(i)%LinkedNodesID = intArrayTemp
                
                Nodes(i)%LinkedNodesAmount = nodeTemp(i)
                
                intArrayTemp = Nodes(i)%LinkedLinesID(1:nodeTemp(i))
                
                deallocate(Nodes(i)%LinkedLinesID)
                
                allocate(Nodes(i)%LinkedLinesID(nodeTemp(i)))
                
                Nodes(i)%LinkedLinesID = intArrayTemp
            
            end do
            
            
            
            !!生成边界节点
            
            j = 0 
            
            nodeTemp = -1
        
            do i=1 , NodesAmount
            
                if(Nodes(i)%LinkedNodesAmount == Nodes(i)%LinkedTriAmount + 1) then
                
                    j = j + 1
                    
                    nodeTemp(j) = Nodes(i)%ID
                
                end if
            
            end do
            
            if(allocated(BoundaryNodesID)) then
            
                deallocate(BoundaryNodesID)
            
            end if
            
            allocate(BoundaryNodesID(j))
            
            BoundaryNodesID = nodeTemp(1:j)
            
        !!do i = 1 , j
        !!
        !!    k = NodeID2Index(BoundaryNodesID(i))
        !!
        !!    write(*,*)Nodes(k)%Location%X,Nodes(k)%Location%Y,Nodes(k)%Location%Z
        !!
        !!end do
        
        end subroutine
        
        !!
        !!检查Tin网格的拓扑属性
        !!
        subroutine CheckTinTopology()
        
            implicit none
            
            integer::i , j , k ,m ,n

            write(*,*)"Triangle－Line－Triangle Nodes Check ..."
            
            do i = 1 , TrianglesAmount
            
                do j = 1 , 3
                
                    k = Triangles(i)%LinesID(j)
                    
                    if(k <= 0) then
                    
                        write(*,*)"Triangles ID:",i,"ID：",Triangles(i)%ID,"Line:",j,"Not exist Error"
                        
                        cycle
                        
                    end if    
                    
                    if(Lines(k)%LeftTriID == Triangles(i)%ID) then
                    
                        if(j == 1) then
                        
                            if(Triangles(i)%NodesID(2) /= Lines(k)%StartNodeID .OR. Triangles(i)%NodesID(3) /= Lines(k)%EndNodeID) then
                            
                                write(*,*)"Triangles ID:",i,"ID：",Triangles(i)%ID,"Line",j,"Index Error"
                            
                            end if
                        
                        end if
                        
                        if(j == 2) then
                        
                            if(Triangles(i)%NodesID(3) /= Lines(k)%StartNodeID .OR. Triangles(i)%NodesID(1) /= Lines(k)%EndNodeID) then
                            
                                write(*,*)"Triangles ID:",i,"ID：",Triangles(i)%ID,"Line",j,"Index Error"
                            
                            end if
                        
                        end if   
                        
                        if(j == 3) then
                        
                            if(Triangles(i)%NodesID(1) /= Lines(k)%StartNodeID .OR. Triangles(i)%NodesID(2) /= Lines(k)%EndNodeID) then
                            
                                write(*,*)"Triangles ID:",i,"ID：",Triangles(i)%ID,"Line",j,"Index Error"
                            
                            end if
                        
                        end if 
                                                                         
                    end if
                    
                    if(Lines(k)%RightTriID == Triangles(i)%ID) then
                    
                        if(j == 1) then
                        
                            if(Triangles(i)%NodesID(3) /= Lines(k)%StartNodeID .OR. Triangles(i)%NodesID(2) /= Lines(k)%EndNodeID) then
                            
                                write(*,*)"Triangles ID:",i,"ID：",Triangles(i)%ID,"Line",j,"Index Error"
                            
                            end if
                        
                        end if
                        
                        if(j == 2) then
                        
                            if(Triangles(i)%NodesID(1) /= Lines(k)%StartNodeID .OR. Triangles(i)%NodesID(3) /= Lines(k)%EndNodeID) then
                            
                                write(*,*)"Triangles ID:",i,"ID：",Triangles(i)%ID,"Line",j,"Index Error"
                            
                            end if
                        
                        end if   
                        
                        if(j == 3) then
                        
                            if(Triangles(i)%NodesID(2) /= Lines(k)%StartNodeID .OR. Triangles(i)%NodesID(1) /= Lines(k)%EndNodeID) then
                            
                                write(*,*)"Triangles ID:",i,"ID：",Triangles(i)%ID,"Line",j,"Index Error"
                            
                            end if
                        
                        end if 
                                                                         
                    end if
                
                end do
            
            end do
            
            write(*,*)"Boundary Lines－Boundary Triangles Check..."
            
            j = 0
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%LeftTriID == -1) then
                
                    write(*,*) "Line ID:" , i ,"has no left triangle"
            
                end if
                
                if(Lines(i)%RightTriID == -1) then
                
                    j = j + 1
            
                end if
                            
            end do
            
            write(*,*)"Boundary Nodes－Boundary Lines Check..."
            
            if(size(BoundaryNodesID) /= j) then
            
                write(*,*) "boundary error, boundary nodes amount does not match boundary lines amount"
            
            end if
            
            write(*,*) "Node－Node,Node－Line Check..."
            
            m = 0
            
            n = 0
            
            do i = 1 , NodesAmount
            
                do j = 1 , Nodes(i)%LinkedNodesAmount
                
                    m = m + Nodes(i)%ID + Nodes(i)%LinkedNodesID(j)
                    
                    n = n + Lines( Nodes(i)%LinkedLinesID(j))%StartNodeID + Lines( Nodes(i)%LinkedLinesID(j))%EndNodeID
                
                end do
                
                if(m /= n) then
                
                    write(*,*)"Node ID：",Nodes(i)%ID,"link not matched!"
                
                end if
                
                m = 0
                
                n = 0
            
            end do
            
            write(*,*)"Finish"
            
        end subroutine
        
        
        !!
        !!修正水平段数据, in this section debug mode result doesnot match release mode result
        !!
        subroutine RevistDepression()
        
            implicit none
            
            integer::i , j , k , m , n ,i2,ii
            
!            logical::flagLines(size(Lines))
!            
!            real::elevationMark(size(Lines))
            
            logical,allocatable::flagLines(:)
            
            real,allocatable::elevationMark(:)       
            
            real::elevationMin,elevationMax,elevationError
            
            i = size(Lines)
            
            if(allocated(flagLines)) then
            
                deallocate(flagLines)
            
            end if
            
            allocate(flagLines(i))
            
            if(allocated(elevationMark)) then
            
                deallocate(elevationMark)
            
            end if
            
            allocate(elevationMark(i))
            
            flagLines = .false.
            
            elevationMin = Nodes(1)%Location%Z
            
            elevationMax = Nodes(1)%Location%Z
            
            do i = 2 , NodesAmount
            
                if(Nodes(i)%Location%Z < elevationMin) then
                
                    elevationMin = Nodes(i)%Location%Z
                
                end if
                
                if(Nodes(i)%Location%Z > elevationMax) then
                
                    elevationMax = Nodes(i)%Location%Z
                
                end if
                            
            end do
            
            elevationError = (elevationMax - elevationMin) * 0.000001
            
            if(elevationError == 0) then
            
                elevationError = 0.0001
                
            end if


            ii = 0
            
            do i = 1 , size(Lines)
            
                j = NodeID2Index(Lines(i)%StartNodeID)
                
                k = NodeID2Index(Lines(i)%EndNodeID)
                
                if(Nodes(j)%Location%Z + Nodes(j)%DZ /= Nodes(k)%Location%Z + Nodes(k)%DZ) then
                
                    flagLines(i) = .true.

                    cycle
                
                end if
                
                n = 1
                
                elevationMark(n) = Nodes(k)%Location%Z + Nodes(k)%DZ
                
                do m = 1 , Nodes(k)%LinkedNodesAmount
                
                    if(flagLines(Nodes(k)%LinkedLinesID(m))) then
                    
                        n = n + 1
                        
                        if(Lines(Nodes(k)%LinkedLinesID(m))%StartNodeID == Nodes(k)%ID) then
                        
                            i2 = NodeID2Index(Lines(Nodes(k)%LinkedLinesID(m))%EndNodeID)
                        
                        else
                        
                            i2 = NodeID2Index(Lines(Nodes(k)%LinkedLinesID(m))%StartNodeID)
                            
                        end if
                        
                        elevationMark(n) = Nodes(i2)%Location%Z + Nodes(i2)%DZ
                        
                    end if
                    
                end do
                
                !!对Nodes(k)进行高程校正
                m = 1
                
                do while(.true.)
                
                    if(n == 1) then
                    
                        Nodes(k)%DZ = elevationError * m
                        
                        if(Nodes(j)%Location%Z + Nodes(j)%DZ /= Nodes(k)%Location%Z + Nodes(k)%DZ) then
                        
                            exit
                            
                        end if
                            
                        m = m + 1
                                                        
                        cycle
                    
                    end if
                
                    if(all(elevationMark(1:n) /= Nodes(k)%Location%Z + Nodes(k)%DZ + elevationError * m)) then
                    
                        Nodes(k)%DZ =Nodes(k)%DZ + elevationError * m
                        
                        exit
                        
                    end if
                        
                    if(all(elevationMark(1:n) /= Nodes(k)%Location%Z + Nodes(k)%DZ - elevationError * m)) then
                    
                        Nodes(k)%DZ =Nodes(k)%DZ -elevationError * m
                        
                        exit
                    
                    end if
                    
                    m = m + 1
                
                end do
                
                flagLines(i) = .true.
                
                if(Nodes(k)%DZ > 0) then
                
                    ii = ii + 1
                
                end if
                
            end do
            
            write(*,*) "Elevation provisionally adjusted counts:",ii
            
        
        end subroutine
        
        !!
        !!标记洼地谷底点
        !!
        subroutine MarkDepressionBottomNode()
        
            implicit none
            
            integer i , j , k , m
            
            logical:: flag
            
            m = 0
            
            do i = 1 , NodesAmount
            
                if(Nodes(i)%LinkedNodesAmount == Nodes(i)%LinkedTriAmount + 1) then
                
                    cycle
                
                end if
                
                flag = .true.
                
                do j = 1 , Nodes(i)%LinkedNodesAmount
                
                    k = NodeID2Index(Nodes(i)%LinkedNodesID(j))
                
                    if(Nodes(i)%Location%Z + Nodes(i)%DZ > Nodes(k)%Location%Z + Nodes(k)%DZ) then
                    
                        flag = .false.
                        
                        exit
                        
                    end if

                end do
                
                if(flag) then
                
                    Nodes(i)%NodeType = 1
                        
                    m = m + 1
                
                end if
                                    
            end do

            if(allocated(DepressionBottomNodesID)) then
            
                deallocate(DepressionBottomNodesID)
            
            end if

            
            if(m < 1) then
            
                return
            
            end if
            
            allocate(DepressionBottomNodesID(m))
            
            j = 0
            
            do i = 1 ,NodesAmount
            
                if(Nodes(i)%NodeType == 1) then
                
                    j = j + 1
                    
                    DepressionBottomNodesID(j) = Nodes(i)%ID
                    
                end if
            
            end do
            
            write(*,*) "There exist",m,"depression bottom points"
        
        end subroutine
        
        
        !!
        !!计算三角形上水流方向，并生成流域出口点，同时对三角形面积进行赋值
        !!
        subroutine CalculateFlowValue()
        
            implicit none
            
            integer :: i , j , k
            
            integer::nodesIndex(3)
            
            real,allocatable::tempDH(:)
            
            real :: x1,y1,x2,y2,x3,y3,z1,z2,z3
            
            !integer::tempNodesID(NodesAmount),tempNodesID2(NodesAmount)
            
            integer , allocatable::tempNodesID(:),tempNodesID2(:)
            
            if(allocated(tempNodesID)) then
            
                deallocate(tempNodesID)
            
            end if
            
            allocate(tempNodesID(NodesAmount))
            
            if(allocated(tempNodesID2)) then
            
                deallocate(tempNodesID2)
            
            end if
            
            allocate(tempNodesID2(NodesAmount))
            
                        
            do i = 1 , TrianglesAmount
            
                do j = 1 , 3
                
                    nodesIndex(j) = NodeID2Index(Triangles(i)%NodesID(j))
                
                end do
                
                x1 = Nodes(nodesIndex(1))%Location%X
                y1 = Nodes(nodesIndex(1))%Location%Y
                z1 = Nodes(nodesIndex(1))%Location%Z + Nodes(nodesIndex(1))%DZ
                x2 = Nodes(nodesIndex(2))%Location%X
                y2 = Nodes(nodesIndex(2))%Location%Y
                z2 = Nodes(nodesIndex(2))%Location%Z + Nodes(nodesIndex(2))%DZ
                x3 = Nodes(nodesIndex(3))%Location%X
                y3 = Nodes(nodesIndex(3))%Location%Y
                z3 = Nodes(nodesIndex(3))%Location%Z + Nodes(nodesIndex(3))%DZ
                
                !!calculate flow direction
                call calculateFlowToLineValue(x1,y1,z1,x2,y2,z2,x3,y3,z3,&
                Triangles(i)%FlowToLineValue(1),Triangles(i)%FlowToLineValue(2),Triangles(i)%FlowToLineValue(3))
                
                !!calculate area
                Triangles(i)%Area = triangleArea2D(x1,y1,x2,y2,x3,y3)
                
                Triangles(i)%CenterLocation%X = (x1 + x2 + x3) / 3.0
                Triangles(i)%CenterLocation%Y = (y1 + y2 + y3) / 3.0
                Triangles(i)%CenterLocation%Z = (z1 + z2 + z3) / 3.0

                !!write(*,*) Triangles(i)%FlowToLineValue(1),Triangles(i)%FlowToLineValue(2),Triangles(i)%FlowToLineValue(3)
            end do
            
            
            do i = 1 , TrianglesAmount
                
                k = minloc(Triangles(i)%FlowToLineValue,1)
            
                do j = 1 , 3
                
                    if(Triangles(i)%FlowToLineValue(j) > 0) then
                    
                        Triangles(i)%FlowToLine(j) = 1
                    
                    else if(Triangles(i)%FlowToLineValue(j) < 0) then
                    
                        if(j == k) then
                        
                            Triangles(i)%FlowToLine(j) = -1
                            
                        else
                        
                            Triangles(i)%FlowToLine(j) = 0
                        
                        end if
                        
                    else
                    
                        Triangles(i)%FlowToLine(j) = 0
                    
                    end if
                
                end do
            
            end do
            
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%LeftTriID > 0) then
                
                    k = TriID2Index(Lines(i)%LeftTriID)
                
                    do j = 1 , 3
                    
                        if(Triangles(k)%LinesID(j) == i) then
                        
                            Lines(i)%LeftFlowToLine = Triangles(k)%FlowToLine(j)
                            
                            exit
                        
                        end if
                    
                    end do
                
                end if
                
                if(Lines(i)%RightTriID > 0) then
                
                    k = TriID2Index(Lines(i)%RightTriID)
                
                    do j = 1 , 3
                    
                        if(Triangles(k)%LinesID(j) == i) then
                        
                            Lines(i)%RightFlowToLine = Triangles(k)%FlowToLine(j)
                            
                            exit
                        
                        end if
                    
                    end do
                
                end if
            
            end do            
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%RightTriID == -1) then
                
                    if(Lines(i)%LeftFlowToLine == 0) then
                    
                        Lines(i)%FlowCharacter = 1
                        
                    else
                    
                        Lines(i)%FlowCharacter = Lines(i)%LeftFlowToLine
                        
                    end if
                
                else if(Lines(i)%RightFlowToLine == 0 .AND. Lines(i)%LeftFlowToLine == 0) then
                
                    Lines(i)%FlowCharacter = 1
                    
                else
                
                    Lines(i)%FlowCharacter = Lines(i)%LeftFlowToLine + Lines(i)%RightFlowToLine
                
                end if
                
            end do
            
            j = 0
            
            k = 0

            do i = 1 , size(Lines)
            
                if(Lines(i)%FlowCharacter > 0) then
                
                    j = j + 1
                    
                else if(Lines(i)%FlowCharacter < 0) then
                
                    k = k + 1
                    
                end if
                
            end do
            
            !write(*,*) "共有" , j ,"条分水边，", k ,"条汇水边，",size(Lines) - j - k , "条过水边"
            
            LinesStat%SeparateLinesAmount = j
            
            LinesStat%JoinLinesAmount = k
            
            LinesStat%PassLinesAmount = size(Lines) - j - k
            
            LinesStat%LinesAmount = size(Lines)
            
           
            !!节点的汇水边和出水边
            do i = 1 , NodesAmount
            
                if(Nodes(i)%LinkedNodesAmount < 1) then
                
                    cycle
                
                end if
                
                if(allocated(tempDH)) then
                
                    deallocate(tempDH)
                    
                end if
                
                allocate(tempDH(Nodes(i)%LinkedNodesAmount))
                
                
                do j = 1 , Nodes(i)%LinkedNodesAmount
                
                    if(Lines(Nodes(i)%LinkedLinesID(j))%FlowCharacter >= 0) then
                    
                        tempDH(j) = 0
                        
                        cycle
                    
                    end if
                
                    k = Lines(Nodes(i)%LinkedLinesID(j))%StartNodeID
                    
                    if(k == Nodes(i)%ID) then
                    
                        k = Lines(Nodes(i)%LinkedLinesID(j))%EndNodeID
                    
                    end if
                    
                    tempDH(j) =  Nodes(i)%Location%Z + Nodes(i)%DZ -Nodes(NodeID2Index(k))%Location%Z - Nodes(NodeID2Index(k))%DZ
                    
                end do
                
                j = maxloc(tempDH,1)
                
                if(tempDH(j) > 0) then
                
                    Nodes(i)%FlowToLineID = Nodes(i)%LinkedLinesID(j)
                
                end if
                
                k = count(tempDH < 0)
                
                if(k < 1) then
                
                    if(allocated(Nodes(i)%FlowFromLineID)) then
                
                        deallocate(Nodes(i)%FlowFromLineID)
                
                    end if
                    
                    cycle
                
                end if
                
                if(allocated(Nodes(i)%FlowFromLineID)) then
                
                    deallocate(Nodes(i)%FlowFromLineID)
                
                end if
                
                allocate(Nodes(i)%FlowFromLineID(k))
                
                k = 0
                
                do j = 1 , Nodes(i)%LinkedNodesAmount
                
                    if(tempDH(j) < 0) then
                    
                        k = k + 1
                        
                        Nodes(i)%FlowFromLineID(k) = Nodes(i)%LinkedLinesID(j)
                    
                    end if
                
                end do
                
            end do
            
            j = 0
            
            k = 0

            do i = 1 , NodesAmount
            
                if(allocated(Nodes(i)%FlowFromLineID) .AND. Nodes(i)%FlowToLineID == -1 .AND. Nodes(i)%NodeType /= 1 ) then
                
                    if(Nodes(i)%LinkedNodesAmount ==  Nodes(i)%LinkedTriAmount + 1) then
                    
                        j = j + 1
                        
                        tempNodesID(j) = Nodes(i)%ID
                        
                        Nodes(i)%NodeType = -1
                        
                    else
                    
                        k = k + 1
                        
                        tempNodesID2(k) = Nodes(i)%ID
                    
                        Nodes(i)%NodeType = 2
                    
                    end if
                
                end if
            
            end do
            
            if(j > 0) then
            
                if(allocated(NormalDrainageOutletNodesID)) then
                
                    deallocate(NormalDrainageOutletNodesID)
                
                end if
                
                allocate(NormalDrainageOutletNodesID(j))
                
                NormalDrainageOutletNodesID = tempNodesID(1:j)
                            
            end if
            
                       
            if(k > 0) then
            
                if(allocated(PseudoDrainageOutletNodesID)) then
                
                    deallocate(PseudoDrainageOutletNodesID)
                
                end if
                
                allocate(PseudoDrainageOutletNodesID(k))
                
                PseudoDrainageOutletNodesID = tempNodesID2(1:k)
                            
            end if            
                  
        end subroutine
        
        !!
        !!检查Tin网格，仅供检验用
        !!
        subroutine CheckTinNet()
        
            implicit none
            
            integer::i
            
            do i = 1 , NodesAmount

                if(Nodes(i)%NodeType == 1) then
                
                    if(.NOT. allocated(Nodes(i)%FlowFromLineID)) then
                    
                        write(*,*) "Error,depression bottom does not contains any inflow line,Node ID：",Nodes(i)%ID
                        
                    else
                    
                        write(*,*) "depression inflow lines amount:",size(Nodes(i)%FlowFromLineID),"outflow line ID：",Nodes(i)%FlowToLineID
                    
                    end if
                                
                end if    
                       
            end do
            
           
            do i = 1 , NodesAmount
            
                write(*,*) "NODE ID:",Nodes(i)%ID,Nodes(i)%Location%X,Nodes(i)%Location%Y,Nodes(i)%Location%Z+Nodes(i)%DZ
            
            end do
            
            do i = 1 , size(Lines)
            
                write(*,*) "LineID-StartEndNode-LeftRightTri:",Lines(i)%ID,Lines(i)%StartNodeID,Lines(i)%EndNodeID,Lines(i)%LeftTriID,Lines(i)%RightTriID
                
                write(*,*) "LineID-LineFlow:",Lines(i)%ID,Lines(i)%LeftFlowToLine,Lines(i)%RightFlowToLine,Lines(i)%FlowCharacter
            
            end do
            
            do i = 1 , TrianglesAmount
            
                write(*,*) "TriID-NodesID:",Triangles(i)%ID,Triangles(i)%NodesID(1),Triangles(i)%NodesID(2),Triangles(i)%NodesID(3)
                
                write(*,*) "TriID-LinesID:",Triangles(i)%ID,Triangles(i)%LinesID(1),Triangles(i)%LinesID(2),Triangles(i)%LinesID(3)
                
                write(*,*) "TriID-FlowToLineValue:",Triangles(i)%ID,Triangles(i)%FlowToLineValue(1),Triangles(i)%FlowToLineValue(2),Triangles(i)%FlowToLineValue(3)
                
                write(*,*) "TriID-FlowToLine:",Triangles(i)%ID,Triangles(i)%FlowToLine(1),Triangles(i)%FlowToLine(2),Triangles(i)%FlowToLine(3)
            
            end do
        
        end subroutine
        
        !!
        !!计算流域
        !!        
        subroutine CalculateDrainage()
        
            implicit none
            
            integer :: i , j , k , m , joinAmount , trackTrianglesAmount
            
            integer,allocatable::joinLines(:) , trackedTrianglesID(:)
            
            joinLines = -1
            
            trackedTrianglesID = -1
            
            i = 0
            
            if(allocated(DepressionBottomNodesID)) then
            
                i = i + size(DepressionBottomNodesID)
            
            end if
            
            if(allocated(NormalDrainageOutletNodesID)) then
            
                i = i + size(NormalDrainageOutletNodesID)
            
            end if
            
            
            if(allocated(PseudoDrainageOutletNodesID)) then
            
                i = i + size(PseudoDrainageOutletNodesID)
            
            end if
            
            if (i < 1) then
            
                return
            
            end if
                       
            if(allocated(Drainages)) then
            
                deallocate(Drainages)
            
            end if
            
            allocate(Drainages(i))
            
            
            i = 0
            
            do j = 1 ,NodesAmount
            
                if(Nodes(j)%NodeType == 0) then
                
                    cycle
                
                end if
                
                i = i + 1
                    
                joinAmount = 0
                
                if(size(Nodes(j)%FlowFromLineID) < 1) then
                
                    write(*,*) "NodeID:",Nodes(j)%ID,"NodeType:",Nodes(j)%NodeType,"No FlowFromLine"
                    
                    cycle
                
                end if
                
                call calculateSubDrainage(Nodes(j)%ID,joinLines,joinAmount)
                
                if(joinAmount < 1) then
                
                    write(*,*) "Node ID:",Nodes(j)%ID,"making drainage network error"
                    
                    cycle
                    
                else
                    
                    Drainages(i)%ID = i
                    
                    Drainages(i)%OutLetNodeID = Nodes(j)%ID
                    
                    Drainages(i)%DrainageType = Nodes(j)%NodeType
                
                    if(allocated(Drainages(i)%JoinLinesID)) then
                    
                        deallocate(Drainages(i)%JoinLinesID)
                    
                    end if
                    
                    allocate(Drainages(i)%JoinLinesID(joinAmount))
                    
                    Drainages(i)%JoinLinesID = joinLines(1:joinAmount)

                end if
                
                trackTrianglesAmount = 0
                
                do m = 1 , joinAmount
                
                    k = Drainages(i)%JoinLinesID(m)
                
                    if(Lines(k)%LeftFlowToLine < 0) then
                    
                        call trackFlowTriangles(Lines(k)%LeftTriID,trackedTrianglesID,trackTrianglesAmount)
                    
                    end if
                    
                    if(Lines(k)%RightFlowToLine < 0) then
                    
                        call trackFlowTriangles(Lines(k)%RightTriID,trackedTrianglesID,trackTrianglesAmount)
                    
                    end if 
                                   
                end do
                
                if(trackTrianglesAmount > 0) then
                
                    if(allocated(Drainages(i)%PassTrianglesID)) then
                    
                        deallocate(Drainages(i)%PassTrianglesID)
                    
                    end if
                    
                    allocate(Drainages(i)%PassTrianglesID(trackTrianglesAmount))
                    
                    Drainages(i)%PassTrianglesID = trackedTrianglesID(1:trackTrianglesAmount)
                    
!                    write(*,*) "Drainage ID:", Drainages(i)%ID,"Type:",Drainages(i)%DrainageType,"OutLet Node ID:",Drainages(i)%OutLetNodeID
!                    
!                    write(*,*) "Inflow Lines Amount:",joinAmount,"Inflow Triangles Amount:", trackTrianglesAmount
                                                       
                else
                
                    write(*,*) "Error:Drainage ID:", Drainages(i)%ID,"has no inflow triangles!"
                
                end if
                    
            end do
            
            do i = 1 , size(Drainages)
            
                Drainages(i)%Area = 0
                
                do j = 1 , size(Drainages(i)%PassTrianglesID)
                
                    k = TriID2Index(Drainages(i)%PassTrianglesID(j))
                    
                    Triangles(k)%ToDrainageID = Drainages(i)%ID
                    
                    Drainages(i)%Area = Drainages(i)%Area + Triangles(k)%Area
                
                end do
            
            end do
            
            
            !!result test
            j = 0
            
            k = 0
            
            m = 0
            
            do i = 1 , size(Drainages)
            
                if(Drainages(i)%DrainageType == -1) then
                
                    j = j + size(Drainages(i)%PassTrianglesID)
                    
                    m = m + 1
                    
                else if(Drainages(i)%DrainageType == 1 .OR. Drainages(i)%DrainageType == 2) then
                
                    k = k + size(Drainages(i)%PassTrianglesID)
                
                end if
            
            end do
            
            write(*,*) "Normal Drainages:",m," ,Triangles:",j
            
            write(*,*) "Depressions(pseudo):",size(Drainages) - m," ,triangles:" , k
            
            write(*,*) "Total Drainages:",size(Drainages), " ,Total Triangles:",j + k
            
                       
        end subroutine
        
        !!
        !!计算子流域
        !!
        recursive subroutine calculateSubDrainage(nodeID,tempJoinLines,joinAmount)
        
            implicit none
            
            integer,intent(in)::nodeID
            
            integer::joinAmount
            
            integer,allocatable::tempJoinLines(:)
            
            integer::i , k , m
            
            if(.NOT. allocated(tempJoinLines)) then
            
                allocate(tempJoinLines(LinesStat%JoinLinesAmount))
            
            end if
            
            k = NodeID2Index(nodeID)
            
            if(allocated(Nodes(k)%FlowFromLineID)) then
                       
                do i = 1 , size(Nodes(k)%FlowFromLineID)

                    joinAmount = joinAmount + 1
                    
                    m =Nodes(k)%FlowFromLineID(i)
                    
                    tempJoinLines(joinAmount) = m
                    
                    if(Lines(m)%StartNodeID /= Nodes(k)%ID .AND. Nodes(NodeID2Index(Lines(m)%StartNodeID))%FlowToLineID == m) then
                    
                        call calculateSubDrainage(Lines(m)%StartNodeID,tempJoinLines,joinAmount)
                        
                    end if
                    
                    if(Lines(m)%EndNodeID /= Nodes(k)%ID .AND. Nodes(NodeID2Index(Lines(m)%EndNodeID))%FlowToLineID == m) then
                    
                        call calculateSubDrainage(Lines(m)%EndNodeID,tempJoinLines,joinAmount)
                        
                    end if
                    
                end do
            
            end if
            
        end subroutine
        
        
        !!
        !!Track all triangles that flow into the appointed triangle
        !!
        recursive subroutine trackFlowTriangles(triangleID,trackedTrianglesID,trackedTrianglesAmount)
        
            implicit none
            
            integer,intent(in)::triangleID
            
            integer :: trackedTrianglesAmount
            
            integer,allocatable::trackedTrianglesID(:)
            
            integer::i , j
            
            if(.NOT. allocated(trackedTrianglesID)) then
            
                allocate(trackedTrianglesID(TrianglesAmount))
            
            end if
            
            trackedTrianglesAmount = trackedTrianglesAmount + 1
            
            trackedTrianglesID(trackedTrianglesAmount) = triangleID
            
            do i = 1 , 3
            
                j = Triangles(TriID2Index(triangleID))%LinesID(i)

                if(Lines(j)%FlowCharacter == 0) then
                
                    if(Lines(j)%LeftTriID == triangleID .AND. Lines(j)%LeftFlowToLine > 0) then
                    
                        call trackFlowTriangles(Lines(j)%RightTriID,trackedTrianglesID,trackedTrianglesAmount)
                    
                    end if
                    
                    if(Lines(j)%RightTriID == triangleID .AND. Lines(j)%RightFlowToLine > 0) then
                    
                        call trackFlowTriangles(Lines(j)%LeftTriID,trackedTrianglesID,trackedTrianglesAmount)
                    
                    end if
                
                end if
            
            end do
        
        end subroutine
        
        !!
        !!合并洼地流域与伪洼地流域到正常流域中
        !!
        subroutine ReviseAbnormalDrainages(thresholdArea)
        
            implicit none 
            
            real,intent(in) ::thresholdArea
            
            integer::i , j , k , m , n ,ii,jj
            
            real :: tempArea, tempDepthA, tempDepthB
            
            integer,allocatable::tempDrainagesID(:)          !tempTrianglesID(:)
            
            real,allocatable::tempDepth(:)
            
            integer,allocatable::tempNodesID(:)    
            
            if(.NOT. allocated(NormalDrainageOutletNodesID)) then
            
                write(*,*) "No normal drainage exist! All the small drainages make up a large basin!"
                
                return
            
            end if
            
            do i = 1 , size(Drainages)
            
                if(Drainages(i)%DrainageType == -1 .AND. Drainages(i)%Area > thresholdArea) then
                
                    cycle
                
                end if
                
                !!find depression's outlet drainage id
                ii = size(Drainages(i)%PassTrianglesID)
                
                if(allocated(tempDepth)) then
                
                    deallocate(tempDepth)
                
                end if
                
                allocate(tempDepth(3 * ii))
                
                if(allocated(tempNodesID)) then
                
                    deallocate(tempNodesID)
                
                end if
                
                allocate(tempNodesID(3 * ii))
                
                if(allocated(tempDrainagesID)) then
                
                    deallocate(tempDrainagesID)
                
                end if
                
                allocate(tempDrainagesID(3 * ii))
                
                jj = 0
                    
                do j = 1 , ii

                    k = TriID2Index(Drainages(i)%PassTrianglesID(j))
                    
                    do m = 1 , 3
                    
                        n = Triangles(k)%LinesID(m)
                        
                        if(Lines(n)%LeftTriID > 0 .AND. Lines(n)%RightTriID > 0) then
                        
                            if(Lines(n)%RightTriID == Drainages(i)%PassTrianglesID(j) .AND. &
                            Triangles(TriID2Index(Lines(n)%LeftTriID))%ToDrainageID /= Drainages(i)%ID) then
                            
                                jj = jj + 1
                                
                                tempDepthA = Nodes(NodeID2Index(Lines(n)%StartNodeID))%Location%Z + &
                                Nodes(NodeID2Index(Lines(n)%StartNodeID))%DZ
                                
                                tempDepthB = Nodes(NodeID2Index(Lines(n)%EndNodeID))%Location%Z + &
                                Nodes(NodeID2Index(Lines(n)%EndNodeID))%DZ
                                
!                                tempDepth(jj) = Nodes(NodeID2Index(Lines(n)%StartNodeID))%Location%Z + &
!                                Nodes(NodeID2Index(Lines(n)%StartNodeID))%DZ + Nodes(NodeID2Index(Lines(n)%EndNodeID))%Location%Z + &
!                                Nodes(NodeID2Index(Lines(n)%EndNodeID))%DZ
                                tempDepth(jj) = tempDepthA + tempDepthB
                                
                                tempDrainagesID(jj) = Triangles(TriID2Index(Lines(n)%LeftTriID))%ToDrainageID
                                
                                if(tempDepthA < tempDepthB) then
                                    tempNodesID(jj) = Lines(n)%StartNodeID
                                else
                                    tempNodesID(jj) = Lines(n)%EndNodeID
                                end if
                            
                            end if
                            
                            if(Lines(n)%LeftTriID == Drainages(i)%PassTrianglesID(j) .AND. &
                            Triangles(TriID2Index(Lines(n)%RightTriID))%ToDrainageID /= Drainages(i)%ID) then
                            
                                jj = jj + 1
                                
                                tempDepthA = Nodes(NodeID2Index(Lines(n)%StartNodeID))%Location%Z + &
                                Nodes(NodeID2Index(Lines(n)%StartNodeID))%DZ
                                
                                tempDepthB = Nodes(NodeID2Index(Lines(n)%EndNodeID))%Location%Z + &
                                Nodes(NodeID2Index(Lines(n)%EndNodeID))%DZ
                                
!                                tempDepth(jj) = Nodes(NodeID2Index(Lines(n)%StartNodeID))%Location%Z + &
!                                Nodes(NodeID2Index(Lines(n)%StartNodeID))%DZ + Nodes(NodeID2Index(Lines(n)%EndNodeID))%Location%Z + &
!                                Nodes(NodeID2Index(Lines(n)%EndNodeID))%DZ
                                
                                tempDepth(jj) = tempDepthA + tempDepthB
                                
                                tempDrainagesID(jj) = Triangles(TriID2Index(Lines(n)%RightTriID))%ToDrainageID
                                
                                if(tempDepthA < tempDepthB) then
                                    tempNodesID(jj) = Lines(n)%StartNodeID
                                else
                                    tempNodesID(jj) = Lines(n)%EndNodeID
                                end if
                                                                
                            end if     
                                               
                        end if
                    
                    end do
                
                end do
                
                if(jj < 1) then
                
                    write(*,*) "drainage ID:",Drainages(i)%ID,",has no outlet.Triangles amount:",ii
                    
                    cycle
                
                end if
                
                !!find the lowest line for outlet line
                n = minLoc(tempDepth(1:jj),1)
                
                m = Triangles(TriID2Index( Drainages(tempDrainagesID(n))%PassTrianglesID(1)))%ToDrainageID
                !!call makeDepressionFlowPath(nodeID,drainageIDFrom,drainageIDTo)
                
                if(Drainages(i)%DrainageType == 1) then
                
                    Nodes(NodeID2Index(tempNodesID(n)))%NodeType = -2
                
                end if
                
                call makeDepressionFlowPath(tempNodesID(n),Drainages(i)%ID)
                
                call mergeDrainage(m,i)
                
                Drainages(i)%IsValid = .false.
                
            end do
            
            j = 0
            
            k = 0 
            
            tempArea = 0
            
            do i = 1 , size(Drainages)
            
                if(Drainages(i)%DrainageType == -1 .AND. Drainages(i)%IsValid) then
                
                    !write(*,*) "drainageID:",i,"Area:",Drainages(i)%Area
                
                    j = j + size(Drainages(i)%PassTrianglesID)
                    
                    k = k + 1
                    
                    tempArea = tempArea + Drainages(i)%Area
                
                end if
            
            end do
            
            write(*,*) "Normal Drainages:",k ," ,Total Triangles:" , j ,"Total Area(sq.m):",tempArea
            
        
        end subroutine
        
        
        !!
        !!add drainages(B) to drainages(A),drainages(B) still exist,but the trianges it contained is not belong to it any more
        !!
        subroutine mergeDrainage(iA,iB)
        
            implicit none
            
            integer,intent(in) :: iA , iB
            
            integer,allocatable::tempTrianglesID(:)
            
            integer :: i , ii , k
            
            ii = size(Drainages(iB)%PassTrianglesID)
            
            do i = 1 , ii
            
                k = TriID2Index(Drainages(iB)%PassTrianglesID(i))
                
                Triangles(k)%ToDrainageID = iA
            
            end do
            
            !!Merge these triangles to drainages(m)
            if(allocated(tempTrianglesID)) then
            
                deallocate(tempTrianglesID)
            
            end if
            
            allocate(tempTrianglesID(size(Drainages(iA)%PassTrianglesID)))
            
            tempTrianglesID = Drainages(iA)%PassTrianglesID
            
            deallocate(Drainages(iA)%PassTrianglesID)
            
            allocate(Drainages(iA)%PassTrianglesID(ii + size(tempTrianglesID)))
            
            Drainages(iA)%PassTrianglesID(1:size(tempTrianglesID)) = tempTrianglesID
            
            Drainages(iA)%PassTrianglesID(size(tempTrianglesID) + 1 : size(Drainages(iA)%PassTrianglesID)) = Drainages(iB)%PassTrianglesID
            
            Drainages(iA)%Area = Drainages(iA)%Area + Drainages(iB)%Area
            
            if(Drainages(iB)%DrainageType == -1) then
            
                Drainages(iA)%DrainageType = -1
            
            end if
                
        end subroutine
        
        !!
        !!Makr facet flow path for depressions and pseudo drainages
        !!
        subroutine makeDepressionFlowPath(nodeID,drainageIDFrom)
            
            implicit none
            
            integer,intent(in) :: nodeID,drainageIDFrom
            
            real,allocatable :: tempDepths(:)
            
            integer,allocatable :: tempTriIndex(:)
                     
            integer :: i , j , k , m , n , k1 , k2
            
            j = NodeID2Index(nodeID)
            
            k = Nodes(j)%LinkedTriAmount
            
            if(allocated(tempDepths)) then
            
                deallocate(tempDepths)
            
            end if
            
            allocate(tempDepths(k))
            
            if(allocated(tempTriIndex)) then
            
                deallocate(tempTriIndex)
            
            end if
            
            allocate(tempTriIndex(k))
                        
            tempDepths = 0
            tempTriIndex = -1
            
            n = 0

           
            do i = 1 , k
            
                m = TriID2Index(Nodes(j)%LinkedTriID(i))
                
                if(Triangles(m)%ToDrainageID == drainageIDFrom) then
                    cycle
                end if
                
                n = n + 1
                
                tempDepths(n) = Triangles(m)%CenterLocation%Z
                
                tempTriIndex(n) = m
                
            
            end do
            
            if(n > 0) then
            
                k1 = minloc(tempDepths(1:n),1)
                
                call trackTriangleFlowPath(Triangles(tempTriindex(k1))%ID)
            
            end if
        
        end subroutine
                
        !!
        !!Track facet flow path for depressions and pseudo drainages
        !!        
        recursive subroutine trackTriangleFlowPath(triID)
        
            implicit none
            
            integer,intent(in) :: triID
            
            integer :: i , j , k , m
            
            if(triID < 0) then
            
                return
                
            end if
            
            j = TriID2Index(triID)
            
            if(Triangles(j)%IsFlowFacet == .true.) then
            
                return
            
            end if
            
           
           
            Triangles(j)%IsFlowFacet = .true.
            
            do i = 1 , 3
            
                if(Triangles(j)%FlowToLine(i) < 0) then
                
                    
                
                    if(Lines(Triangles(j)%LinesID(i))%LeftTriID == triID) then
                    
                        !!check next triangel
                        k = Lines(Triangles(j)%LinesID(i))%RightTriID
                        
                    else
                    
                        k = Lines(Triangles(j)%LinesID(i))%LeftTriID
                        
                    end if
                    
                    if(k < 0) then
                    
                        return
                    
                    end if
                    
                    k = TriID2Index(k)
                        
                    if(Lines(Triangles(k)%LinesID(1))%FlowCharacter < 0 .OR. &
                    Lines(Triangles(k)%LinesID(2))%FlowCharacter < 0 .OR. &
                    Lines(Triangles(k)%LinesID(3))%FlowCharacter < 0) then
                    
                        Triangles(k)%IsFlowFacet = .true.
                        
                    else
                    
                        call trackTriangleFlowPath(Triangles(k)%ID)
                        
                    end if
                    
                end if
           
            end do
        
        end subroutine
                
        
      
        !!
        !!mark every drainage outer boundary nodes
        !!
        subroutine MarkOuterboundaryNodes()
        
            implicit none
            
            logical , allocatable::flagNodes(:)
            
            integer , allocatable::collectNodesID(:)
                        
            integer::i , j , k , m , n , ii
            
            if(allocated(flagNodes)) then
            
                deallocate(flagNodes)
            
            end if
            
            allocate(flagNodes(NodesAmount))
            
            if(allocated(collectNodesID)) then
            
                deallocate(collectNodesID)
            
            end if
            
            allocate(collectNodesID(NodesAmount))
                                    
            collectNodesID = -1
            
            do i = 1 , size(Drainages)
            
                if(Drainages(i)%DrainageType /= -1 .OR. Drainages(i)%IsValid == .false.) then
                
                    cycle
                
                end if
                            
                flagNodes = .true.
                
                ii = 0
                
                do j = 1 , size(Drainages(i)%PassTrianglesID)
                
                    m = TriID2Index(Drainages(i)%PassTrianglesID(j))
                
                    do k = 1 , 3
                    
                        n = NodeID2Index(Triangles(m)%NodesID(k))
                        
                        if(flagNodes(n)) then
                        
                            flagNodes(n) = .false.
                        
                            if(Nodes(n)%LinkedNodesAmount == Nodes(n)%LinkedTriAmount + 1) then
                            
                                ii = ii + 1
                                
                                collectNodesID(ii) = Triangles(m)%NodesID(k)
                            
                            end if
                        
                        end if
                    
                    end do
                
                end do
                
                if(allocated(Drainages(i)%OuterBoundaryNodesID)) then
                
                    deallocate(Drainages(i)%OuterBoundaryNodesID)
                
                end if
                
                allocate(Drainages(i)%OuterBoundaryNodesID(ii))
                
                Drainages(i)%OuterBoundaryNodesID = collectNodesID(1:ii)
            
            end do
        
        end subroutine
        
        !!
        !!mark nodes id that span the typical contour line
        !!
        subroutine MarkTypicalLineNodes(typicalElevation)
        
            implicit none
            
!            logical::flagNodes(NodesAmount) , flagLines(size(Lines))
!            
!            integer::collectNodesID(NodesAmount)

            logical , allocatable::flagNodes(:) , flagLines(:)
            
            integer , allocatable::collectNodesID(:)
                        
            real,intent(in)::typicalElevation
            
            integer::i , j , k , m1 , m2 , n1 , n2 , ii
            
            if(allocated(flagNodes)) then
            
                deallocate(flagNodes)
            
            end if
            
            allocate(flagNodes(NodesAmount))
            
             if(allocated(collectNodesID)) then
            
                deallocate(collectNodesID)
            
            end if
            
            allocate(collectNodesID(NodesAmount))
            
                       
            if(allocated(flagLines)) then
            
                deallocate(flagLines)
            
            end if
            
            allocate(flagLines(size(Lines)))
                        
                        
            collectNodesID = -1
            
            do i = 1 , size(Drainages)
            
                if(Drainages(i)%DrainageType /= -1 .OR. Drainages(i)%IsValid == .false.) then
                
                    cycle
                
                end if
                            
                flagNodes = .true.
                
                flagLines = .true.
                
                ii = 0
                
                do j = 1 , size(Drainages(i)%PassTrianglesID)
                
                    m1 = TriID2Index(Drainages(i)%PassTrianglesID(j))
                
                    do k = 1 , 3
                    
                        m2 = Triangles(m1)%LinesID(k)
                    
                        if(flagLines(m2)) then
                        
                            flagLines(m2) = .false.
                        
                            n1 = NodeID2Index(Lines(m2)%StartNodeID)
                            
                            n2 = NodeID2Index(Lines(m2)%EndNodeID)
                            
                            if((Nodes(n1)%Location%Z - typicalElevation )* (Nodes(n2)%Location%Z - typicalElevation) <= 0) then
                            
                                if(flagNodes(n1)) then
                                
                                    flagNodes(n1) = .false.
                                    
                                    ii = ii + 1
                                    
                                    collectNodesID(ii) = Lines(m2)%StartNodeID
                                
                                end if
                                
                                if(flagNodes(n2)) then
                                
                                    flagNodes(n2) = .false.
                                    
                                    ii = ii + 1
                                    
                                    collectNodesID(ii) = Lines(m2)%EndNodeID
                                
                                end if
                                                            
                            end if
                        
                        end if
                    
                    end do
                
                end do
                
                if(allocated(Drainages(i)%SpanTypicalLineNodesID)) then
                
                    deallocate(Drainages(i)%SpanTypicalLineNodesID)
                
                end if
                
                if(ii < 1) then
                
                    cycle
                
                end if
                
                allocate(Drainages(i)%SpanTypicalLineNodesID(ii))
                
                Drainages(i)%SpanTypicalLineNodesID = collectNodesID(1:ii)
            
            end do      
              
        end subroutine
        
        
        !!
        !!output all splitted meshes to files
        !!
        subroutine OutputSplittedMeshes(fileName)
        
            USE IFPORT
        
            implicit none
            
            character(len = 256),intent(in)::fileName
            
            character(len = 256)::fileNameOutput , tempOutputStr
            
            character(len = 30)::catchmentNum
            
!            logical::tempNodesID(NodesAmount),flag
            logical :: flag
            
            logical , allocatable :: tempNodesID(:)
            
            integer :: i , j , k , m , n , ii
            
            if(.NOT. allocated(Drainages)) then
            
                return
            
            end if
            
            if(allocated(tempNodesID)) then
            
                deallocate(tempNodesID)
            
            end if
            
            allocate(tempNodesID(NodesAmount))
            
            !!Creat output folder if not exist
            flag = MAKEDIRQQ("Output")
            
            flag = MAKEDIRQQ("Output\"//trim(fileName))
            
!            if(i == 1) then
!            
!                write(*,*) "Output terminate because result of " ,trim(fileName)," already exist, please delete it first."
!                
!                stop
!            
!            end if
            
            j = 0
            
            do i = 1 , size(Drainages)
            
                !if(.NOT.(Drainages(i)%DrainageType == -1 .AND. Drainages(i)%IsValid)) then
                
                if(Drainages(i)%DrainageType /= -1 .OR. Drainages(i)%IsValid == .false.) then
                
                    cycle
                
                end if
                
                j = j + 1
                
                tempNodesID = .true.
                
                write(catchmentNum,*) j
                
                fileNameOutput ="Output\" // trim(fileName) // "\catchment"// trim(adjustl(catchmentNum))
                
                flag = MAKEDIRQQ(trim(fileNameOutput))
                
                
                
                !!输出Mesh.2dm文件
                open(unit = 20,file = trim(fileNameOutput)//"\catchment"// trim(adjustl(catchmentNum))//".2dm")
                
                write(20,"(a)") "MESH2D"
                
                do k = 1 , size(Drainages(i)%PassTrianglesID)
                
                    m = TriID2Index(Drainages(i)%PassTrianglesID(k))
                
                    write(20,"(a,5(3x,i))") "E3T",Triangles(m)%ID,Triangles(m)%NodesID(1),Triangles(m)%NodesID(2), & 
                    Triangles(m)%NodesID(3),Triangles(m)%MaterialID
                
                end do
                
                do k = 1 , size(Drainages(i)%PassTrianglesID)
                
                    m = TriID2Index(Drainages(i)%PassTrianglesID(k))
                    
                    do n = 1 , 3
                    
                        ii =NodeID2Index(Triangles(m)%NodesID(n))
                        
                        if(tempNodesID(ii)) then
                            
                            write(20,"(a,3x,i,3(3x,f))") "ND",Nodes(ii)%ID,Nodes(ii)%Location%X,Nodes(ii)%Location%Y,Nodes(ii)%Location%Z
                            
                            tempNodesID(ii) = .false.
                        
                        end if
                    
                    end do
                
                end do
                
                close(20)
                
                
                !!输出边界节点
                open(unit = 20,file = trim(fileNameOutput)//"\catchment"// trim(adjustl(catchmentNum))//"-outflow.nds")
                
                do k = 1 , size(Drainages(i)%OuterBoundaryNodesID)
                
                    write(tempOutputStr,*) Drainages(i)%OuterBoundaryNodesID(k)
                
                    write(20,"(a)") trim(adjustl(tempOutputStr))
                    
                end do
                
                close(20) 
                
                !!输出跨越特征等高线的点
                
                open(unit = 20,file = trim(fileNameOutput)//"\catchment"// trim(adjustl(catchmentNum))//"-260.nds")
                
                do k = 1 , size(Drainages(i)%SpanTypicalLineNodesID)
                
                    write(tempOutputStr,*) Drainages(i)%SpanTypicalLineNodesID(k)
                
                    write(20,"(a)") trim(adjustl(tempOutputStr))
                                    
                end do
                
               if(.NOT. allocated(Drainages(i)%SpanTypicalLineNodesID)) then

                    write(20,"(a)") ""
                
                end if
                
                close(20) 
                
            end do            
        
        end subroutine     

        !!
        !!output all splitted meshes to files
        !!
        subroutine OutputSplittedMeshesOneFile(fileName)
        
            USE IFPORT
        
            implicit none
            
            character(len = 256),intent(in)::fileName
            
            character(len = 256)::fileNameOutput , tempOutputStr
            
            character(len = 30)::catchmentNum
            
!            logical::tempNodesID(NodesAmount),flag
            logical :: flag
            
            logical , allocatable :: tempNodesID(:)
            
            integer :: i , j , k , m , n , ii
            
            if(.NOT. allocated(Drainages)) then
            
                return
            
            end if
            
            if(allocated(tempNodesID)) then
            
                deallocate(tempNodesID)
            
            end if
            
            allocate(tempNodesID(NodesAmount))
            
            !!Creat output folder if not exist
            flag = MAKEDIRQQ("Output")
            
            flag = MAKEDIRQQ("Output\"//trim(fileName))
            
            j = 0
            
            fileNameOutput ="Output\" // trim(fileName)
            
            open(unit = 20,file = trim(fileNameOutput)//"\drainageTin.2dm")
            
            write(20,"(a)") "MESH2D"
            
            do i = 1 , size(Drainages)
            
                if(Drainages(i)%DrainageType /= -1 .OR. Drainages(i)%IsValid == .false.) then
                
                    cycle
                
                end if
                
                j = j + 1
                
                tempNodesID = .true. 
                
                !!输出Mesh.2dm文件
                                
                do k = 1 , size(Drainages(i)%PassTrianglesID)
                
                    m = TriID2Index(Drainages(i)%PassTrianglesID(k))
                
                    write(20,"(a,5(3x,i))") "E3T",Triangles(m)%ID,Triangles(m)%NodesID(1),Triangles(m)%NodesID(2), & 
                    Triangles(m)%NodesID(3),i
                
                end do
                
                do k = 1 , size(Drainages(i)%PassTrianglesID)
                
                    m = TriID2Index(Drainages(i)%PassTrianglesID(k))
                    
                    do n = 1 , 3
                    
                        ii =NodeID2Index(Triangles(m)%NodesID(n))
                        
                        if(tempNodesID(ii)) then
                            
                            write(20,"(a,3x,i,3(3x,f))") "ND",Nodes(ii)%ID,Nodes(ii)%Location%X,Nodes(ii)%Location%Y,Nodes(ii)%Location%Z
                            
                            tempNodesID(ii) = .false.
                        
                        end if
                    
                    end do
                
                end do

            end do    
            
            close(20)
            
            if (allocated(tempNodesID)) then
            
                deallocate(tempNodesID)
            
            end if
        
        end subroutine     

        !!
        !!Output drainage networks to DXF file
        !!
        subroutine OutputDrainageToDXF(fileName)
        
            USE IFPORT
            
            implicit none
        
            character(len = 256),intent(in)::fileName
            CHARACTER (len=15), DIMENSION(:,:), ALLOCATABLE  :: layer_def    ! Layers def 
            CHARACTER (len=8 ) :: Layer ,Color ! Other vars 

            INTEGER :: FileID

            REAL ::Height, Factor, Angle, Radius
            INTEGER :: Alignh, Alignv, i , j ,k , m
            CHARACTER (len=8 ):: Style
            character(len = 256)::fileNameOutput
            logical :: flag
            
            character (len = 6) :: textID
            
            Factor  = 1.0
            Angle   = 0.0
            ! - Align left   : <Alignh> = 0         - Align none   : <Alignv> = 0
            ! - Align center : <Alignh> = 1         - Align bottom : <Alignv> = 1
            ! - Align right  : <Alignh> = 2         - Align middle : <Alignv> = 2
            Alignh  = 1
            Alignv  = 2
            ! Text Styles: STANDARD, ROMANS, ROMAND, ROMANT.
            Style   = "ROMANT"
            !-----------------------------------+
            ! Layer defenition
            !-----------------------------------+
            ALLOCATE(layer_def(10,3))

            layer_def(1,1)= "0"              !+ Layer name (0 default layer)
            layer_def(1,2)= "continuous"     !+ Linetype
            layer_def(1,3)= "7"              !+ Color
            layer_def(2,1)= "外边界"
            layer_def(2,2)= "continuous" 
            layer_def(2,3)= "1"
            layer_def(3,1)= "汇水边"
            layer_def(3,2)= "continuous" 
            layer_def(3,3)= "2"
            layer_def(4,1)= "分水边"
            layer_def(4,2)= "continuous" 
            layer_def(4,3)= "3"
            layer_def(5,1)= "内边界"
            layer_def(5,2)= "continuous" 
            layer_def(5,3)= "4"
            layer_def(6,1)= "汇流面"
            layer_def(6,2)= "continuous" 
            layer_def(6,3)= "5"   
            layer_def(7,1)= "洼地底点"
            layer_def(7,2)= "continuous" 
            layer_def(7,3)= "6"  
            layer_def(8,1)= "伪流域点"
            layer_def(8,2)= "continuous" 
            layer_def(8,3)= "6"
            layer_def(9,1)= "洼地出口"
            layer_def(9,2)= "continuous" 
            layer_def(9,3)= "6"                                           
            layer_def(10,1)= "流域ID"
            layer_def(10,2)= "continuous" 
            layer_def(10,3)= "7" 
            
            Layer   = "外边界"
            Color   = "bylayer"
            FileID  = 20

            flag = MAKEDIRQQ("Output")
            flag = MAKEDIRQQ("Output\"//trim(fileName))
            fileNameOutput = "Output\" // trim(fileName)//"\drainageNetworks.dxf"
            
            OPEN (FileID  , FILE = fileNameOutput)
            
            !!+ Begin DXF Example file
            CALL dfBegin(FileID,layer_Def)
            
            !!输出边界线
            Layer = "外边界"
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%RightTriID < 0) then  
                
                    j = NodeID2Index(Lines(i)%StartNodeID)
                    
                    CALL dfBeginPline (FileID, Layer, Color)
            
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    j = NodeID2Index(Lines(i)%EndNodeID)
                    
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    CALL dfEndPline (FileID, Layer, Color) 
                
                end if
            
            end do
            
            !!输出边界线
            Layer = "内边界"
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%LeftTriID > -1 .AND. Lines(i)%RightTriID > -1) then
                 
                    if(Triangles(TriID2Index(Lines(i)%LeftTriID))%ToDrainageID /= &
                    Triangles(TriID2Index(Lines(i)%RightTriID))%ToDrainageID) then  
                    
                        j = NodeID2Index(Lines(i)%StartNodeID)
                        
                        CALL dfBeginPline (FileID, Layer, Color)
                
                        CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                        
                        j = NodeID2Index(Lines(i)%EndNodeID)
                        
                        CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                        
                        CALL dfEndPline (FileID, Layer, Color) 
                    
                    end if
                    
                end if
            
            end do            

            !!输出汇水线
            Layer = "汇水边"
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%FlowCharacter < 0 ) then
                
                    j = NodeID2Index(Lines(i)%StartNodeID)
                    
                    CALL dfBeginPline (FileID, Layer, Color)
            
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    j = NodeID2Index(Lines(i)%EndNodeID)
                    
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    CALL dfEndPline (FileID, Layer, Color) 
                
                end if
            
            end do
            
            !!输出分水线
            Layer = "分水边"
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%FlowCharacter > 0) then
                
                    j = NodeID2Index(Lines(i)%StartNodeID)
                    
                    CALL dfBeginPline (FileID, Layer, Color)
            
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    j = NodeID2Index(Lines(i)%EndNodeID)
                    
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    CALL dfEndPline (FileID, Layer, Color) 
                
                end if
            
            end do
            
            !!输出汇流面
            Layer = "汇流面"
            
            do i = 1 , TrianglesAmount
            
                if(Triangles(i)%IsFlowFacet) then
                
                    j = NodeID2Index(Triangles(i)%NodesID(1))
                    k = NodeID2Index(Triangles(i)%NodesID(2))
                    m = NodeID2Index(Triangles(i)%NodesID(3))
                
                    call df3DFace (FileID, Layer, "bylayer", Color, &
                           Nodes(j)%Location%X, Nodes(j)%Location%Y, Nodes(j)%Location%Z + Nodes(j)%DZ,& 
                           Nodes(k)%Location%X, Nodes(k)%Location%Y, Nodes(k)%Location%Z + Nodes(k)%DZ,&
                           Nodes(m)%Location%X, Nodes(m)%Location%Y, Nodes(m)%Location%Z + Nodes(m)%DZ,&
                           Nodes(j)%Location%X, Nodes(j)%Location%Y, Nodes(j)%Location%Z + Nodes(j)%DZ)
                
                end if
            
            end do
            
            Layer = "洼地底点"
            do i = 1 , NodesAmount
            
                if(Nodes(i)%NodeType == 1) then
                
                    CALL dfCircle(FileID, Layer, Color, Nodes(i)%Location%X, Nodes(i)%Location%Y, &
                     Nodes(i)%Location%Z + Nodes(i)%DZ,4.0,2.0)
                
                end if
            
            end do
       
            
            Layer = "伪流域点"
            do i = 1 , NodesAmount
            
                if(Nodes(i)%NodeType == 2) then
                
                    CALL dfCircle(FileID, Layer, Color, Nodes(i)%Location%X, Nodes(i)%Location%Y, &
                    Nodes(i)%Location%Z + Nodes(i)%DZ,4.0,2.0)
                
                end if
            
            end do  
            

            Layer = "洼地出口"
            do i = 1 , NodesAmount
            
                if(Nodes(i)%NodeType == -2) then
                
                    CALL dfCircle(FileID, Layer, Color, Nodes(i)%Location%X, Nodes(i)%Location%Y, &
                    Nodes(i)%Location%Z + Nodes(i)%DZ,4.0,2.0)
                
                end if
            
            end do 
            
                        
            Layer = "流域ID"
            
            do i = 1 , TrianglesAmount
            
                write(textID,"(i5)")Triangles(i)%ToDrainageID
            
                call dfText  (FileID, Layer,  Color, Triangles(i)%CenterLocation%X,Triangles(i)%CenterLocation%Y, &
                Triangles(i)%CenterLocation%Z,textID, 8.0,1.0,0.0,1,2,"ROMANT")
                
            end do
             
            !
            !+ End DXF Example file
            !
            CALL dfEnd(FileID)

            CLOSE(FileID)
            
            DEALLOCATE(layer_def)
        
        end subroutine     

        !!
        !!Output local drainage networks to DXF file
        !!
        subroutine OutputLocalDrainageToDXF(fileName,drainageID)
        
            USE IFPORT
            
            implicit none
        
            character(len = 256),intent(in)::fileName
            integer, intent(in) :: drainageID
            CHARACTER (len=15), DIMENSION(:,:), ALLOCATABLE  :: layer_def    ! Layers def 
            CHARACTER (len=8 ) :: Layer ,Color ! Other vars 

            INTEGER :: FileID

            REAL ::Height, Factor, Angle, Radius
            INTEGER :: Alignh, Alignv, i , j ,k , m
            CHARACTER (len=8 ):: Style
            character(len = 256)::fileNameOutput
            logical :: flag
            
            Factor  = 1.0
            Angle   = 0.0
            ! - Align left   : <Alignh> = 0         - Align none   : <Alignv> = 0
            ! - Align center : <Alignh> = 1         - Align bottom : <Alignv> = 1
            ! - Align right  : <Alignh> = 2         - Align middle : <Alignv> = 2
            Alignh  = 1
            Alignv  = 2
            ! Text Styles: STANDARD, ROMANS, ROMAND, ROMANT.
            Style   = "ROMANT"
            !-----------------------------------+
            ! Layer defenition
            !-----------------------------------+
            ALLOCATE(layer_def(9,3))

            layer_def(1,1)= "0"              !+ Layer name (0 default layer)
            layer_def(1,2)= "continuous"     !+ Linetype
            layer_def(1,3)= "7"              !+ Color
            layer_def(2,1)= "外边界"
            layer_def(2,2)= "continuous" 
            layer_def(2,3)= "1"
            layer_def(3,1)= "汇水边"
            layer_def(3,2)= "continuous" 
            layer_def(3,3)= "2"
            layer_def(4,1)= "分水边"
            layer_def(4,2)= "continuous" 
            layer_def(4,3)= "3"
            layer_def(5,1)= "内边界"
            layer_def(5,2)= "continuous" 
            layer_def(5,3)= "4"
            layer_def(6,1)= "汇流面"
            layer_def(6,2)= "continuous" 
            layer_def(6,3)= "5"
            layer_def(7,1)= "洼地底点"
            layer_def(7,2)= "continuous" 
            layer_def(7,3)= "6"
            layer_def(8,1)= "伪流域点"
            layer_def(8,2)= "continuous" 
            layer_def(8,3)= "6"
            layer_def(9,1)= "洼地出口"
            layer_def(9,2)= "continuous" 
            layer_def(9,3)= "6"
                        
            Layer   = "外边界"
            Color   = "bylayer"
            FileID  = 20

            flag = MAKEDIRQQ("Output")
            flag = MAKEDIRQQ("Output\"//trim(fileName))
            fileNameOutput = "Output\" // trim(fileName)//"\drainageNetworks.dxf"
            
            OPEN (FileID  , FILE = fileNameOutput)
            
            !!+ Begin DXF Example file
            CALL dfBegin(FileID,layer_Def)
            
            !!输出边界线
            Layer = "外边界"
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%RightTriID < 0 .AND. Triangles(TriID2Index(Lines(i)%LeftTriID))%ToDrainageID == drainageID) then  
                
                    j = NodeID2Index(Lines(i)%StartNodeID)
                    
                    CALL dfBeginPline (FileID, Layer, Color)
            
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    j = NodeID2Index(Lines(i)%EndNodeID)
                    
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    CALL dfEndPline (FileID, Layer, Color) 
                
                end if
            
            end do
            
            !!输出边界线
            Layer = "内边界"
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%LeftTriID > -1 .AND. Lines(i)%RightTriID > -1) then
                 
                    if((Triangles(TriID2Index(Lines(i)%LeftTriID))%ToDrainageID == drainageID .AND. &
                    Triangles(TriID2Index(Lines(i)%RightTriID))%ToDrainageID /= drainageID) .OR. &
                    (Triangles(TriID2Index(Lines(i)%LeftTriID))%ToDrainageID /= drainageID .AND. &
                    Triangles(TriID2Index(Lines(i)%RightTriID))%ToDrainageID == drainageID)) then  
                    
                        j = NodeID2Index(Lines(i)%StartNodeID)
                        
                        CALL dfBeginPline (FileID, Layer, Color)
                
                        CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                        
                        j = NodeID2Index(Lines(i)%EndNodeID)
                        
                        CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                        
                        CALL dfEndPline (FileID, Layer, Color) 
                    
                    end if
                    
                end if
            
            end do            

            !!输出汇水线
            Layer = "汇水边"
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%FlowCharacter < 0 .AND. Triangles(TriID2Index(Lines(i)%LeftTriID))%ToDrainageID == drainageID) then
                
                    j = NodeID2Index(Lines(i)%StartNodeID)
                    
                    CALL dfBeginPline (FileID, Layer, Color)
            
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    j = NodeID2Index(Lines(i)%EndNodeID)
                    
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    CALL dfEndPline (FileID, Layer, Color) 
                
                end if
            
            end do
            
            !!输出分水线
            Layer = "分水边"
            
            do i = 1 , size(Lines)
            
                if(Lines(i)%FlowCharacter > 0 .AND. Triangles(TriID2Index(Lines(i)%LeftTriID))%ToDrainageID == drainageID) then
                
                    j = NodeID2Index(Lines(i)%StartNodeID)
                    
                    CALL dfBeginPline (FileID, Layer, Color)
            
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    j = NodeID2Index(Lines(i)%EndNodeID)
                    
                    CALL dfPlVertex (FileID, Layer,Color,Nodes(j)%Location%X,Nodes(j)%Location%Y,Nodes(j)%Location%Z)
                    
                    CALL dfEndPline (FileID, Layer, Color) 
                
                end if
            
            end do
            
            !!输出汇流面
            Layer = "汇流面"
            Do i=1,3
                CALL df3DFace (FileID, Layer, "bylayer", Color, &
                           50.0+10.0*i, 20.0, 0.0,& 
                           60.0+10.0*i, 20.0, 0.0,&
                           60.0+10.0*i, 10.0, 0.0,&
                           50.0+10.0*i, 10.0, 0.0)
            END DO
            
            do i = 1 , TrianglesAmount
            
                if(Triangles(i)%IsFlowFacet .AND. Triangles(i)%ToDrainageID == drainageID ) then
                
                    j = NodeID2Index(Triangles(i)%NodesID(1))
                    k = NodeID2Index(Triangles(i)%NodesID(2))
                    m = NodeID2Index(Triangles(i)%NodesID(3))
                
                    call df3DFace (FileID, Layer, "bylayer", Color, &
                           Nodes(j)%Location%X, Nodes(j)%Location%Y, Nodes(j)%Location%Z + Nodes(j)%DZ,& 
                           Nodes(k)%Location%X, Nodes(k)%Location%Y, Nodes(k)%Location%Z + Nodes(k)%DZ,&
                           Nodes(m)%Location%X, Nodes(m)%Location%Y, Nodes(m)%Location%Z + Nodes(m)%DZ,&
                           Nodes(j)%Location%X, Nodes(j)%Location%Y, Nodes(j)%Location%Z + Nodes(j)%DZ)
                
                end if
            
            end do
            
            
            Layer = "洼地底点"
            do i = 1 , NodesAmount
            
                if(Nodes(i)%NodeType == 1) then
                
                    CALL dfCircle(FileID, Layer, Color, Nodes(i)%Location%X, Nodes(i)%Location%Y, &
                     Nodes(i)%Location%Z + Nodes(i)%DZ,4.0,2.0)
                
                end if
            
            end do
       
            
            Layer = "伪流域点"
            do i = 1 , NodesAmount
            
                if(Nodes(i)%NodeType == 2) then
                
                    CALL dfCircle(FileID, Layer, Color, Nodes(i)%Location%X, Nodes(i)%Location%Y, &
                    Nodes(i)%Location%Z + Nodes(i)%DZ,4.0,2.0)
                
                end if
            
            end do               
            
            
            Layer = "洼地出口"
            do i = 1 , NodesAmount
            
                if(Nodes(i)%NodeType == -2) then
                
                    CALL dfCircle(FileID, Layer, Color, Nodes(i)%Location%X, Nodes(i)%Location%Y, &
                    Nodes(i)%Location%Z + Nodes(i)%DZ,4.0,2.0)
                
                end if
            
            end do 
            
            !
            !+ End DXF Example file
            !
            CALL dfEnd(FileID)

            CLOSE(FileID)
            
            DEALLOCATE(layer_def)

        end subroutine     

        !!
        !!节点ID查找索引号
        !!
        integer function NodeID2Index(ID)
        
            implicit none
            
            integer,intent(in)::ID
            
            NodeID2Index = ID - NodesID_Index(ID)
        
        end function NodeID2Index
        
        !!
        !!三角形ID查找索引号
        !!
        integer function TriID2Index(ID)
                
            implicit none
            
            integer,intent(in)::ID
            
            TriID2Index = ID - TriID_Index(ID)
        
        end function
        
        !!创建创建节点ID与索引的差值列表
        subroutine makeNodeID_IndexRelation()
        
            implicit none
            
            integer :: IDMax , IDMin , i
            
            IDMax = Nodes(1)%ID
            
            IDMin = Nodes(1)%ID
            
            do i = 2 , NodesAmount
             
                if(Nodes(i)%ID < IDMin) then
                
                    IDMin = Nodes(i)%ID
                
                end if
                
                if(Nodes(i)%ID > IDMax) then
                
                    IDMax = Nodes(i)%ID
                
                end if
            
            end do
            
            if(allocated(NodesID_Index)) then
            
                deallocate(NodesID_Index)
                
            end if
            
            allocate(NodesID_Index(IDMin:IDMax))
            
            NodesID_Index = 0
            
            do i = 1 , NodesAmount
            
                NodesID_Index(Nodes(i)%ID) = Nodes(i)%ID - i
            
            end do
        
        end subroutine
        
        
        !!创建三角形ID与索引的差值列表
        subroutine makeTriID_IndexRelation()
        
            implicit none
            
            integer :: IDMax , IDMin , i
            
            IDMax = Triangles(1)%ID
            
            IDMin = Triangles(1)%ID
            
            do i = 2 , TrianglesAmount
             
                if(Triangles(i)%ID < IDMin) then
                
                    IDMin = Triangles(i)%ID
                
                end if
                
                if(Triangles(i)%ID > IDMax) then
                
                    IDMax = Triangles(i)%ID
                
                end if
            
            end do
            
            if(allocated(TriID_Index)) then
            
                deallocate(TriID_Index)
                
            end if
            
            allocate(TriID_Index(IDMin:IDMax))
            
            TriID_Index = 0
            
            do i = 1 , TrianglesAmount
            
                TriID_Index(Triangles(i)%ID) = Triangles(i)%ID - i
            
            end do
        
        end subroutine
                
end Module