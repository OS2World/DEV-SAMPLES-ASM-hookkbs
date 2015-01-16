rem This assembles and links an asm file to an EXE file
ml /c %1.asm
IF errorlevel 1  goto errexit
link386 /PM:PM /ALIGN:4 /BASE:0x10000 /E %1.obj,,,c:\toolkt20\os2lib\os2386.lib;,%1.def ;
del *.obj
del *.map
quit
:errexit
Echo Compile Error NO Link
