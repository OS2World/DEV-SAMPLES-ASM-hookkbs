rem Uses IMPLIB to create lib file for dll and Assembles and links
rem asm file and its associated def file to a dll and moves the dll
rem file to c:\os2\dll
implib c:\toolkt20\os2lib\%1.lib %1.def
If errorlevel 1  goto errexit0
rem assemble dll program
ml /c %1.asm
IF errorlevel 1  goto errexit1
link386 /PM:PM /ALIGN:4 /BASE:0x12000000 /E %1.obj,,,c:\toolkt20\os2lib\os2386.lib,%1.def ;
If errorlevel  1 goto errexit2
del *.obj
del *.map
goto exit
:errexit0
Echo Error making ImpLib Library file
goto exit
:errexit1
Echo Assemble Error NOT Linking for DLL
goto exit
:errexit2
Echo Link Error for DLL
:exit
Echo DLL Assemble and Link OK
move %1.dll c:\os2\dll

