========================================================================
    Fortran Console Application : "DrainageAnalysis" Project Overview
========================================================================

The Intel Fortran Console Application Wizard has created this 
"DrainageAnalysis" project for you as a starting point.

This file contains a summary of what you will find in each of the files 
that make up your project.

DrainageAnalysis.vfproj
    This is the main project file for Fortran projects generated using an 
    Application Wizard.  It contains information about the version of 
    Intel Fortran that generated the file, and information about the 
    platforms, configurations, and project features selected with the 
    Application Wizard.

DrainageAnalysis.f90
    This is the main source file for the Fortran Console application. 
    It contains the program entry point.

/////////////////////////////////////////////////////////////////////////////
Other notes:

1. How to use this program

   Put the source data(*.2dm) into the folder "MeshData" that locates together with the executable program; Run the executable program, follow the introduction given and you will get the result in the folder "Output",which creat automaticly. 

  eg. If you have data abc.2dm located in the MehsData folder, run the program, type in any other character except "Y" or "y" if you want to continue, then type in the source data file name "abc", finally, type in the area threshold such as 10000 which means if the drainage's area is smaller than 10000 square meter, it will merged into the adjacent drainage.

Notice : If you run the same project (the same source data) twice, please remove the former result(output)  or change the former result folder's name first, because the program now does
not delete the exist result.
 
/////////////////////////////////////////////////////////////////////////////
