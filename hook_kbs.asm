;---------------------------------------------------------------------------;
; M.F.Kaplon  Begun:Fri  06-12-1992  Revised:Fri  09-25-1992
; Title : hook_kbs.asm
;
; "Copyright 1992 M.F. Kaplon"
;
; INITIALIZES,CREATES MESSAGE QUEUE, REGISTERS CLASS,CREATES STANDARD WINDOW
; ESTABLISHES MAIN MESSAGE LOOP, ESTABLISHES MAIN WINDOW PROCEDURE, Reads in
; C:\os2\hook_kbs.dat , Establishes Hook HK_INPUT, Reeived Filtered Messages
; from hook_dls , mp1 = Alt/Ctrl flag, mp2 = scan code and
; takes appropriate action. Uses WM_USER+300h to receive message.
;
; For System Function Calls
;
; 1 - Must use the C Calling Convention which means
;     Arguments Pushed on Stack in Reverse Order than declared in function
;     And the stack pointer reset after call.
;     This is done by the MACRO   $Call   defined in DOSWIN32.MAC
; 2 - Use  .MODEL FLAT. When using a pointer, all you have to do is push
;     the offset of the variable since in FLAT model everyting is NEAR
;     in the 32 bit space
; 3 - Uses an 8K stack
;
; It is assumed that the Developers toolkit for OS/2 2.0 is installed
; on the users C:drive in the default installation
;
; The files needed to create the functioning program are:
; doswin32.mac  Macros and Equates used by Program
; hook_kbs.asm   Source for Executable  Assembled and Linked by mlc-w386.cmd
; hook_dls.def  Define file needed by IMPLIB and LINK386 for DLL
; hook_dls.asm  Source for DLL = Assembled and linked by dll-w386.cmd
; hook_kbs.dat  Text File assigning programs to key strokes -read by hook_kb
;               The user creates this file according to structure outlined in
;               sample and it MUST BE LOCATED IN C:\OS2\DLL
; dll-w386.cmd  Command File to create the hook_dls.dll and copy to c:\os2\dll
; mlc-w386.cmd  Command File to Assemble and Link hook_kbs.asm
;
; Except as noted, all files should be in the same directory.
; To assemble and link use the directory holding the above files as default
; and use the commands
; dll-w386  hook_dls;creates hook_dls.dll from hook_dls.asm,moves to c:\os2\dll
; mlc-w386  hook_kb ;creates hook_kbs.exe from  hook_kbs.asm
;
; kb_hooks.exe is placed in the directory holding the above files
;
; There is one warning message
;
; LINK : warning L4036: no automatic data segment
;
; This message has to do with no DGROUP being defined
; It can be suppressed with DATA NONE in a "DEF" file
;
;---------------------------------------------------------------------------;
;USES HOOK HK_INPUT
;----------------- PRELIMINARIES ----------------

.386             ;preceeding .MODEL makes USE32 default
.MODEL           FLAT,SYSCALL,OS_OS2

;---------- Conditionally required equates -------------
NUMBUFS            equ    1     ; Uncomment if need number routines
DOSERROR           equ    1     ; Uncomment if need DosError Messages

INCL_WINERRORS     equ    1
INCL_WIN           equ    1
INCL_DOSMEMMGR     equ    1
INCL_DOSFILEMGR    equ    1
INCL_WINSWITCHLIST equ    1

INCLUDE        doswin32.mac             ;macros used by *.asm

INCLUDE        c:\toolkt20\asm\os2inc\os2def.inc ;structure defns includes POINTL
INCLUDE        c:\toolkt20\asm\os2inc\pmwin.inc  ;structure defns POINTL defn required
INCLUDE        c:\toolkt20\asm\os2inc\pmerr.inc  ;errors
INCLUDE        c:\toolkt20\asm\os2inc\pmshl.inc
INCLUDE        c:\toolkt20\asm\os2inc\bsememf.inc;memory
INCLUDE        c:\toolkt20\asm\os2inc\bsedos.inc ;files
INCLUDELIB     c:\toolkt20\os2lib\os2386.lib     ;Library

;---------- Prototype Definitions for MASM 6.0 -------------
InitMainWindow Proto Near SYSCALL var1:DWORD,var2:DWORD,var3:DWORD
MainPaint      Proto Near SYSCALL var1:DWORD
MainKeyBoard   Proto Near SYSCALL var1:DWORD,var2:DWORD,var3:DWORD

ExecOnKB     STRUCT      ;Structure used in program-stores data from hook_kb.dat
   Exec     DWORD   ?    ;Address of ASCIIZ str - name of exec program
   CmdLn    DWORD   ?    ;Address of ASCIIZ str - command line parms
   SessT     WORD   ?    ;Session Type
ExecOnKB    ENDS         ;structure length now = 22

LenExec     equ    10    ;length of ExecOn Kb

.STACK    8096   ;8K stack

.DATA

IFDEF NUMBUFS                      ;To use  UNCOMMENT NUMBUFS equate above
  $DefineNumBufs
ENDIF
IFDEF DOSERROR
   $DOSErrorMessages
ENDIF

; Copyright Notice
notice          BYTE   "Copyright 1992 M.F. Kaplon"

;------------- handles --------
hab            DWORD   ?           ;Anchor block Handle
hmq            DWORD   ?           ;Message Queue Handle
hwndMainFrame  DWORD   ?           ;Handle to Main Frame Window
hwndMain       DWORD   ?           ;Handle to Client window

;------------- Text Strings --------
szAppName       BYTE   "Main0",0    ;Class Name of Window Procedure
szWinTitle      BYTE   "** TSR **",0 ;Window Title
szDebugMsg      BYTE   "  SwitchHandle",0
;------------- Text Strings for WinMessageBox calls for Errors --------

msgModuleLoaded         BYTE "hook_KBS Already Loaded",0
msgInfo                 BYTE "Debug Info",0
msgFileErr              BYTE "Incorrect hook_kbs.dat File",0

;------------- Styles --------
msgBoxStyle    DWORD   (MB_YESNO OR MB_DEFBUTTON1)
flStyle        DWORD   (CS_SIZEREDRAW OR CS_HITTEST)  ;Window Style
flCtlData      DWORD   FCF_SYSMENU                    ;Invisible Window

;------------- structures --------
quemsg          QMSG    {,,,,,,}        ;Queue message structure
AltX           ExecOnKB 37 dup({0,0,9}) ;Array for  Alt-# 0-9,A-Z,A-Ins default Initialization
CtrlX          ExecOnKB 36 dup({0,0,9}) ;Array for Ctrl-# 0-9,A-Z default Initialization

;------------- Miscellaneous --------
parm1          DWORD   ?           ;handle of window sending message
parm2          DWORD   ?           ;message id value
parm3          DWORD   ?           ;message mp1
parm4          DWORD   ?           ;message mp2
parm5          DWORD   ?           ;message time
Concanted       BYTE 64 dup(0)     ;buffer to hold concanted strings
Alt_Ctrl       DWORD   ?           ;0/1 if Alt/Ctrl key struck
lookup0         BYTE   'Q'-37h,'W'-37h,'E'-37h,'R'-37h,'T'-37h,'Y'-37h,'U'-37h,'I'-37h,'O'-37h,'P'-37h ;26,32,14,27,29,34,30,18,24,25 ;
lookup1         BYTE   'A'-37h,'S'-37h,'D'-37h,'F'-37h,'G'-37h,'H'-37h,'J'-37h,'K'-37h,'L'-37h         ;10,28,13,15,16,17,19,20,21 ;
lookup2         BYTE   'Z'-37h,'X'-37h,'C'-37h,'V'-37h,'B'-37h,'N'-37h,'M'-37h                         ;35,33,12,31,11,23,22 ;

;------------ Specific to TSR Hook  use -----
DllLoadError    BYTE  100 dup(0)   ;Buffer for name of object contributing to error
DllHandle      DWORD ?             ;Handle of Dynamic LInk Module returned here
DllFullModName  BYTE  "c:\os2\dll\hook_dls.dll",0
DllProcAddr1   DWORD ?             ;address of proc 1 in dynamic link module

;- StartSession structure Offset ------------ Identification --------------
StartData       WORD  32    ; 0  Length of Structure for all but Shift-Alt-W
                WORD  0     ; 2  Related 0 is independent, 1 is child
                WORD  0     ; 4  0/1 Start in Foreground/Background
                WORD  0     ; 6  Trace Option 0 is no trace
               DWORD  0     ; 8  ProgramTitle 0 uses Program Name
               DWORD  ?     ;12  Address of ASCZII string with fully qualified program name
               DWORD  0     ;16  Address of Input Args to Pgm - 0 is none
               DWORD  0     ;20  TermQ 0 is no Queue
               DWORD  0     ;24  Environment - must be 0 for DOS
                WORD  0     ;28  InheritOp 0 Inherits Shell Environment
                WORD  ?     ;30  SessType see p.2-345 of Control Prog Ref.
               DWORD  ?     ;32  ICON File
               DWORD  ?     ;36  PgmHandle
                WORD 8000h  ;40  PgmControl-use specified size for Shift-Alt-W
                WORD 140    ;42  InitXPos  for Shift-Alt-W
                WORD 200    ;44  InitYPos  for Shift-Alt-W
                WORD 350    ;46  InitXSize for Shift-Alt-W
                WORD  60    ;48  InitYSize for Shift-Alt-W

SessID         DWORD  0     ;receives Session ID for Alt-1
ProcID         DWORD  0     ;receives ProcessID  for Alt-1
TitleAlt        BYTE  "Alt- ",0  ;space filled in with 3 or letter
TitleCtl        BYTE  "Ctl- ",0  ;space filled in with 3 or letter
HotKeyID        BYTE   ?         ;Identifies HotKey Selected
InsProg         BYTE  "c:\4os2\4os2.exe",0 ; assigned to Shift-Alt-Ins
DosEnvValue     BYTE  "DPMI_DOS_API=ENABLED",0
;----Shared Memory Variables---
SharedMem      DWORD  ?     ;base address returned
SharedMemName   BYTE  "\SHAREMEM\DATAS.DAT",0
SharedMemFlags DWORD  12h   ;(PAG_COMMIT OR PAG_WRITE)

;-------------- parameters for memory usage and file opening
memAddr        DWORD   ?                        ;address of memory block
memFlags       DWORD  13h ;(PAG_COMMIT OR (PAG_WRITE OR PAG_READ)) ;read-write access required

fName          BYTE    "c:\os2\hook_kbs.dat",0  ;Address of data file
fhandle        DWORD   ?                        ;Address of Handle for File
fActionTaken   DWORD   ?                        ;Address for action taken
fSize          DWORD   ?                        ;Logical size of file
fAttribute     DWORD   0
fOpenFlag      DWORD   OPEN_ACTION_OPEN_IF_EXISTS
fOpenMode      DWORD   OPEN_SHARE_DENYNONE OR OPEN_ACCESS_READWRITE
fExtaBuf       DWORD   0                        ;no extended attributes

fpointer0      DWORD   ?                        ;file pointer start file
fpointer1      DWORD   ?                        ;file pointer end file
EOFflag        DWORD   0                        ;EndOfFile flag
;------------ switch list parameters -------------
numitems       DWORD   ?                        ;#items in list
baseaddr       DWORD   ?                        ;of SWBLOCK {,}

.CODE

startup:                         ;need to do this way with flat model

;----------  GET COMMANDLINE PARMS -------
;$GetCmdLine                    ;macro
;.WHILE byte ptr [esi] != 0     ;go past program name
;      inc    esi
;.ENDW
;inc    esi                     ;now points to  1st space after command name
;mov    eax,0
;.WHILE  byte ptr [esi] != 0    ;go to end of command line
;     .WHILE byte ptr [esi] == ' '
;          inc   esi
;     .ENDW
;     .IF byte ptr [esi] != ' ' && byte ptr [esi] != 0; if a space
;         inc    esi
;         inc    eax
;         .WHILE byte ptr [esi] != ' ' && byte ptr [esi] != 0
;              inc   esi
;         .ENDW
;     .ENDIF
;.ENDW                          ;if eax non 0 has cmd line parm
;
;.IF eax > 0                    ;turn shft flag on
;    mov   shft_flag,1
;.ENDIF

;----------  ESTABLISH WINDOW ------------

;-----Initialize Window -Anchor block handle returned = hab
$Call WinInitialize,0            ;called with argument 0
mov    hab,eax                   ;return value
.IF hab == NULL
    $Call WinTerminate,hab
    $DosExit
.ENDIF

;-----Create MessageQue  QueueHandle returned = hmq
$Call WinCreateMsgQueue,hab,0    ; 0 is default size of queue
mov      hmq,eax                 ;returned queue handle
.IF hmq == NULL
   $WinErrMsg " : WinCreateMsgQueue"
   $DosExit
.ENDIF

;---- Register Window Class Returned value is TRUE or FALSE
$Call WinRegisterClass,hab,offset szAppName,offset MainWinProc,flStyle,0
.IF eax == FALSE
    $WinErrMsg " : WinRegisterClass"
    $Call WinTerminate,hab
    $DosExit
.ENDIF

;---- CreateStandard Window - Returns handle for Main Window Frame and client Window
$Call WinCreateStdWindow,HWND_DESKTOP,WS_VISIBLE,offset flCtlData,offset szAppName,\
                    offset szWinTitle,WS_VISIBLE,0,0,offset hwndMain
mov   hwndMainFrame,eax          ;returned Frame Window handle
.IF hwndMainFrame == 0
   $WinErrMsg " : WinCreateStdWindow"
   Call   ExitWin
.ENDIF

;------------  IS hook_DLL.DLL LOADED ? -------------------

$Call DosQueryModuleHandle,offset DllFullModName,offset DllHandle
.IF  eax == 0    ;module already loaded
     $Call WinMessageBox,HWND_DESKTOP,HWND_DESKTOP,offset msgModuleLoaded,NULL,0,msgBoxErrStyle
     Call   ExitWin
.ENDIF

;----- ALLOCATE SHARED MEM AND PLACE HANDLE THERE ----
$Call DosAllocSharedMem,offset SharedMem,offset SharedMemName,32,SharedMemFlags
.IF eax > 0
   $WinErrMsg " : DosAllocSharedMem"
   Call   ExitWin
.ENDIF
mov    esi,SharedMem
mov    eax,hwndMainFrame
mov    [esi],eax     ;put window handle there

;----------  IS Data File AVAILABLE AND VALID ? ------------

;---------- First see if  hook_kb.dat exists in proper location ------
$Call DosOpen,offset fName,offset fhandle,offset fActionTaken,fsize,fAttribute,fOpenFlag,fOpenMode,fExtaBuf
.IF   eax != 0
     $WinErrMsg " : DosOpen "
     Call   ExitWin
.ENDIF

;------- get file size
$Call DosSetFilePtr,fhandle,0,FILE_BEGIN,offset fpointer0
.IF    eax != 0
     $WinErrMsg " : DosSetFilePtr"
      Call   ExitWin
.ENDIF

$Call DosSetFilePtr,fhandle,0,FILE_END,offset fpointer1
mov    eax,fpointer1
sub    eax,fpointer0
mov    fsize,eax        ;fsize now has file size
add    eax,16           ;allow a little leeway in buffer
;------- now allocate memory for file buffer
$Call DosAllocMem,offset memAddr,eax,memFlags
.IF    eax != 0
     $WinErrMsg " : DosAllocMem FileBuf"
     Call   ExitWin
.ENDIF

;------Read File Into buffer and Close file -------
;reposition to start of file
$Call DosSetFilePtr,fhandle,0,FILE_BEGIN,offset fpointer0
$Call DosRead,fHandle,memAddr,fsize,offset nwritten
$Call DosClose,fHandle

;------ Read Buffer and Initialize Arrays ALtX and CtrlX ----
mov    esi,memAddr
mov    ecx,0               ;Index and Counter for memAddr

.WHILE ecx <=  fsize
    call   SkipSpaces        ;skip over any initial spaces
    .BREAK .IF EOFflag == 1
    call   SkipCommentsToEOL ;skip over comments and spaces to first valid entry
    .BREAK .IF EOFflag == 1
    call SkipSpaces          ;skip over initial spaces
    .BREAK .IF EOFflag == 1
    .IF (byte ptr [esi+ecx] == 'A' || byte ptr [esi+ecx] == 'a' || byte ptr [esi+ecx] == 'C' || byte ptr [esi+ecx] == 'c')
         inc   ecx           ;point to number or letter
         xor   eax,eax
         xor   edx,edx
         mov  al,byte ptr [esi+ecx]  ;get digit or letter ID
        .IF  (al >= '0' && al <= '9')
             sub  al,'0'         ;convert to decimal
        .ENDIF
        .IF  al  >= 'a'
             sub  al,32          ;convert to UpperCase
        .ENDIF
        .IF  (al >= 'A' && al <= 'Z')
             sub  al,37h          ;convert to decimal 10 +
        .ENDIF
         mov      dl,LenExec ;Length of Structure    10
         mul      dl         ;ax has offset into array
         inc      ecx        ;right after digit/letter id A/C number
    .ELSE
         .CONTINUE
    .ENDIF
    .IF byte ptr[esi+ecx - 2] == 'A' || byte ptr[esi+ecx - 2] == 'a'
          mov     edi,offset AltX      ;this is Alt Keys
    .ENDIF
    .IF byte ptr[esi+ecx - 2] == 'C' || byte ptr[esi+ecx - 2] ==  'c'
          mov     edi,offset CtrlX     ;this is a Ctrl Key
    .ENDIF
    .IF edi == offset AltX || edi == offset CtrlX
          mov     edx,eax              ;edx is offset into array
          call    SkipSpaces           ;go to SessID
          .BREAK .IF EOFflag == 1
          xor     eax,eax
          mov     al,byte ptr [esi+ecx] ;get sess type
          sub     al,'0'                ;convert to  number
          mov     word ptr [edi + edx+8],ax
          inc     ecx
          call    SkipSpaces            ;get to Exec string
          .BREAK .IF EOFflag == 1
          mov     eax,esi
          add     eax,ecx               ;offset of exec
          mov     [edi+edx],eax         ;store its address
          ; have to go to end of Exec string and place a numeric 0 there
          .WHILE  byte ptr [esi+ecx] != ' '
               inc  ecx
          .ENDW   ;exits    pointing to end of exec command
          .BREAK .IF byte ptr[esi+ecx] != ' '
             mov     byte ptr [esi+ecx],0
          inc     ecx
          call    SkipSpaces            ;get next parameter
          .BREAK .IF EOFflag == 1
          xor     eax,eax
          .IF     byte ptr [esi+ecx]== '0'  ;no command line
              mov     al,byte ptr [esi+ecx]
              sub     al,'0'
              mov     [edi+edx+4],eax
          .ELSE                         ;its a string - copy its address
              mov eax,esi
              add eax,ecx               ;offset of command line
              mov     [edi+edx+4],eax   ;store command line address
              .IF   byte ptr [esi+ecx] != 22h  ;not a "
                 .WHILE  byte ptr [esi+ecx] != ' ' && ecx < fsize  ;skip until a space
                     .IF byte ptr[esi+ecx] == lf || byte ptr[esi+ecx] == cr  ; LF but no ";"
                          Call DataFormatErr
                     .ENDIF
                      inc  ecx
                 .ENDW                  ;exits  pointing to end of command line
                .BREAK .IF ecx >= fsize
                 mov    byte ptr [esi+ecx],0 ;put 0 at end
              .ELSE                     ;first char is a quote
                 inc  ecx               ;move beyond it
                 .WHILE byte ptr [esi+ecx] != 22h && ecx < fsize ; skip until a "
                      inc  ecx
                 .ENDW                  ;exits pointing to final quote
                 .BREAK .IF ecx >= fsize
                 inc    ecx
                 .BREAK .IF byte ptr[esi+ecx] != ' '
                 mov    byte ptr [esi+ecx],0 ;put 0 at end
              .ENDIF
          .ENDIF
          ;inc    ecx
          .BREAK .IF ecx >= fsize
          xor    edi,edi                ;reset to 0 so test can be made
          .WHILE byte ptr[esi+ecx] != ';' && ecx < fsize  ;go to next comment ;
              .IF byte ptr[esi+ecx] == lf || byte ptr[esi+ecx] == cr  ; LF but no ";"
                   Call DataFormatErr
              .ENDIF
              inc   ecx
          .ENDW
          .BREAK .IF ecx >= fsize
          .CONTINUE
    .ENDIF
.ENDW
; See if Dat File properly read in
.IF ecx < fsize
     Call DataFormatErr
.ENDIF

;---------  ALLOCATE MEMORY FOR SWITCH LIST STRUCTURE -------
mov     ecx,00000002h   ;write access flags for Dos alloc
or      ecx,00000010h   ;page commit
$Call DosAllocMem,offset baseaddr,3000,ecx
.IF  eax != 0
    $WinErrMsg  " : DosAllocMem SwitchList"
    Call ExitWin
.ENDIF

;-----------  ESTABLISH THE HOOK -------------

;The way to proceed is to install a System Hook. This has to go into a DLL
;otherwise it cannot be called by other programs. The procedure in hook_dls
;will inspect WM_CHAR and if the ShiftKey and one of Alt or Ctrl key is down
;and if there is a valid Scan Code THEN
;it redefine the msg ID of that message as WM_USER+cfffh so it goes nowhere
;and then post a message to this program with the mp1 and mp2 parms
;using WM_USER+300h
$Call DosLoadModule,offset DllLoadError,LENGTHOF DllLoadError,offset DllFullModName,offset DllHandle
.IF eax != 0
    $WinErrMsg " DosLoadModule"
.ENDIF
$Call DosQueryProcAddr,DllHandle,1,0,offset DllProcAddr1
.IF eax != 0
    $WinErrMsg " DosQueryProcAddr"
.ENDIF
$Call WinSetHook,hab,NULLHANDLE,HK_INPUT,DllProcAddr1,DllHandle
.IF eax == 0
    $WinErrMsg " WinSetHook"
.ENDIF

;---------  CREATE MAIN MESSAGE LOOP -----------

mml: $Call WinGetMsg,hab,offset quemsg,0,0,0    ;Note: differs from usual
;     .IF eax == TRUE                           ;msg loop - done so that program
         $Call WinDispatchMsg,hab,offset quemsg ;can only be terminated
         jmp   mml                              ;by the keystroke Shift-Alt-Del
;     .ENDIF                                    ;as notified from the DLL

;Normally an Exit routine would go here, as indicated below but which
;is commented out. It is not required since it can never be reached.
;----------  EXIT ---------------
;$Call WinReleaseHook,hab,NULL,HK_INPUT,[DllProcAddr1],DllHandle
;.IF    eax == 0
;     $WinErrMsg " : WinReleaseHook"
;.ENDIF
;$Call DosFreeModule,DllHandle
;.IF    eax != 0
;     $WinErrMsg " : DosFreeModule"
;.ENDIF
;$Call DosFreeMem,memAddr
;$Call WinDestroyWindow,hwndMainFrame
;call  ExitWin

;---------------- End of Main Program -------------------

;-------------  PROCESS MESSAGE QUEUE FOR hook_KB ------------
;------------------ MainWinProc -----------------
;parm1 = hwnd,parm2 = msg,parm3  = mp1,parm4 = mp2
;this is called from System and so has to do everything itself
MainWinProc Proc Near
    ;----------- GET PASSED PARAMETERS FROM STACK ------------
    push   ebp           ;return address is 4 bytes and this push is 4 bytes
    mov    ebp,esp       ;so first parameter is 8 bytes from top
    mov    eax,[ebp+8]
    mov    parm1,eax     ;hwnd
    mov    eax,[ebp+12]
    mov    parm2,eax     ;msg
    mov    eax,[ebp+16]
    mov    parm3,eax     ;mp1
    mov    eax,[ebp+20]
    mov    parm4,eax     ;mp2
    mov    eax,[ebp+24]
    mov    parm5,eax     ;time of message
   ;----- RESTORE STACK POINTER AND STACK STATUS  ---
    mov      esp,ebp     ;restore  stack pointer
    pop      ebp

    ;---------------- WM_CREATE ----------------
    .IF parm2 == WM_CREATE
         Invoke InitMainWindow,parm1,parm3,parm4
    .ENDIF
    ;---------------- WM_PAINT ----------------
    .IF parm2 == WM_PAINT ; && parm1 == hwndMain
       Invoke  MainPaint,parm1
    .ENDIF
    ;---------------- WM_CHAR ----------------
    .IF parm2 == WM_CHAR
       Invoke  MainKeyboard,parm3,parm4,parm1    ;sets return value
    .ENDIF
    ;---------------- WM_USER+0CFFFH ----------------
;    .IF parm2 == WM_USER+0CFFFh                  ;diverted message
;        mov    eax,TRUE
;    .ENDIF                                       ;makes no diff
    ;---------------- WM_USER+300H ----------------
    .IF parm2 == WM_USER+300h                    ;posted message
    ;if here know that Shift Key was down and at least one of Alt or Ctrl
    ;GET SCAN CODE AND ALT/CTRL FLAG
         mov    edx,parm4       ;scan code
         mov    eax,parm3       ;Alt/Ctrl Flag = 0/1
         mov    Alt_Ctrl,eax    ;Alt-Ctrl flag
;         mov    ebx,parm3   ;[esi+8]     ;ebx has mp1
;         mov    ecx,KC_KEYUP
;         test   bx,cx
;         jnz    wm0             ;accept only on down stroke
;         jz    wm0             ;accept only on up strike
;         mov    ecx,KC_ALT       ;? Alt Key down when msg generated
;         test   bx,cx           ;test scan code
;         jz     wm1             ;jump if Alt Key Not Hit
;         mov    Alt_Ctrl,0      ;Alt Key Was Down
;         jmp    wm2
;wm1:     mov    ecx,KC_CTRL     ;? Ctrl-Key down
;         test   bx,cx
;         jz     wm0             ;Was neither
;         mov    Alt_Ctrl,1      ;Was Ctrl Key Down
;wm2:     ;if statement below is on this line gives assemble error
;         xor    edx,edx
;         shld   edx,ebx,8       ;get high 8 bits of mp1 into dl
         .IF edx == 83           ; Alt-Del (White Del key)
            $Call DosFreeMem,memAddr
            $Call WinReleaseHook,hab,NULL,HK_INPUT,[DllProcAddr1],DllHandle
            $Call DosFreeModule,DllHandle
            $Call WinDestroyWindow,hwndMainFrame
            Call  ExitWin
         .ENDIF
;ELSE TEST FOR ASSIGNED KEYS
;SETUP DATA STRUCTURES FOR ACTIVATING HOT KEY
;---- Code in this group places the offset into the array ExecOnKB into EDX
         .IF  edx == 82 && Alt_Ctrl == 0      ;Shift-Alt-Ins(White)
              mov    esi,offset StartData     ;data structure for DosStartSession
              mov    word ptr StartData,50    ;for Alt-Ins
              mov    HotKeyID,'ê'             ;For Program Title
              mov    edi,offset TitleAlt
              mov    cl,HotKeyID
              mov    byte ptr [edi+4],cl
              mov    dword ptr[esi+8],offset TitleAlt  ;program title
              $Call  WinSetFocus,HWND_DESKTOP,hwndMainFrame ;required for program started to be in foreground
              mov    eax,offset InsProg       ;address of Exec program
              mov    [esi+12],eax
              mov    dword ptr[esi+16],0      ;no command line
              mov    word ptr[esi+30],2       ;Session Type
              jmp    wmexe
         .ELSE
              mov    word ptr StartData,32
         .ENDIF
         .IF    edx >= 2 && edx <= 11     ;scan code for 1..9,0' || dl >= 'A' && dl <= 'Z'
             .IF  edx >= 2 && edx <= 10
                 sub   edx,1       ;convert to number 1 - 9
             .ELSE
                 mov   edx,0       ; 0 offset for 0
             .ENDIF
             mov   HotKeyID,dl
             add   HotKeyID,30h    ;convert to ASCII
             jmp   wmok
         .ENDIF
          xor   ecx,ecx
         .IF  edx >= 16 && edx <= 25; && edx != 17 ; q,w,e,r,t,y,u,i,o,p
               mov   esi,offset lookup0
               sub   edx,16
               jmp   wmoj
         .ENDIF
         .IF edx >= 30 && edx <= 38 ; a,s,d,f,g,h,j,k,l
               mov   esi,offset lookup1
               sub   edx,30
               jmp   wmoj
         .ENDIF
         .IF  edx >= 43 && edx <= 50  ;z,x,c,v,b,n,m
               mov   esi,offset lookup2
               sub   edx,44
               jmp   wmoj
         .ELSE
               jmp   wm0
         .ENDIF
wmoj:     mov   cl,byte ptr [esi+edx]  ;cl has value - cl+37h is UC letter
          mov   HotKeyID,cl            ;identifies letter
          add   HotKeyID,37h
          xor   edx,edx
          mov   dl,cl
wmok:     ;- Alt_Ctrl has Alt_Ctrl Flag and dl has offset into array
         .IF  edx >= 0 && edx <= 35
              xor  eax,eax
              mov   al,dl
              mov  esi,offset StartData  ;data structure for DosStartSession
              .IF  Alt_Ctrl == 0
                  mov   ebx,offset AltX  ;Alt Definitions
                  mov   edi,offset TitleAlt
                  mov   cl,HotKeyID
                  mov   byte ptr [edi+4],cl
                  mov   dword ptr[esi+8],offset TitleAlt  ;program title
              .ENDIF
              .IF  Alt_Ctrl == 1
                  mov   ebx,offset CtrlX ;Ctrl Definitions
                  mov   edi,offset TitleCtl
                  mov   cl,HotKeyID
                  mov   byte ptr [edi+4],cl
                  mov   dword ptr[esi+8],offset TitleCtl  ;program title
              .ENDIF
              mov    cl,LenExec
              mul    cl             ;Structure Length ten
              add    ebx,eax        ;element # of structure
              ;[ebx+8] != 9 is flag that keystroke is assigned
              .IF  word ptr [ebx+8] != 9
                   $Call WinSetFocus,HWND_DESKTOP,hwndMainFrame ;required for program started to be in foreground
                   mov  eax,[ebx]              ;address of Exec program
                   mov  [esi+12],eax
                   mov  eax,[ebx+4]            ;address of command line
                   mov  [esi+16],eax
                   mov   ax,word ptr [ebx+8]   ;Session type
                   mov  [esi+30],ax
                  .IF ax == 4 || ax == 7    ;set environment string DPMI_DOS_API for Borland IDE
                      mov dword ptr[esi+24],offset DosEnvValue
                  .ELSE
                      mov  dword ptr[esi+24],0   ;default value reset
                  .ENDIF
;GO THRU SWITCH LIST TO SEE IF HOT KEY ACTIVE
                   ;-- called before DosStartSession to see if program loaded
                   ;-- esi=offset into StartData
                   ;-- go thru Switch List to see if loaded and if so switch to
                   ;-- The test made on 4th char of TitleAlt/TitleCtl which
                   ;-- is the title in the window and Task List
wmexe:             pusha
                   $NumSwitchListEntries
                   mov  numitems,eax      ;number of switch list entries
                   dec numitems
                   Call GetSwitchList     ;activate structure
                   mov  edi,baseaddr      ;address of switch list
                   mov  ecx,0
                   .IF  Alt_Ctrl == 0
                        mov    bl,'A'
                   .ELSE
                        mov    bl,'C'
                   .ENDIF
                   .WHILE  ecx < numitems           ;starts at offset 0
                       mov   al,byte ptr[edi+40]  ;5-th char TaskList title
                       mov   dl,byte ptr[edi+36]  ;1stChar of Title A/C
                       ;IF ACTIVE SWITCH TO
                       .IF al == HotKeyID && bl == dl
                            ;Now Switch to that window - get switch handle
                            mov  eax,[edi+4]
                            $Call WinSwitchToProgram,eax
                            popa
                            jmp  wm0                ;do not reload
                       .ENDIF
                       inc   ecx
                       add   edi,swblksize           ;next element in structure
                   .ENDW
                   popa
                   ;ELSE ACTIVATE HOT KEY PROGRAM
                   $Call DosStartSession,esi,offset SessID,offset ProcID
;                   mov   eax,TRUE   ;USE WITH WinSendMsg
;                   ret
              .ENDIF
         .ENDIF
    .ENDIF                                     ;End WM_USER+300h
   wm0:
   ;----- Default Procedure * Return Value in eax ------
   $Call WinDefWindowProc,parm1,parm2,parm3,parm4
   ret
MainWinProc  endp

;-------------- MainPaint * WM_PAINT --------------
MainPaint  proc Near SYSCALL uses eax, var1:DWORD   ;var1 = hwnd
      mov    eax,TRUE    ;  processed
      ret
MainPaint  endp

;-------------- InitMainWindow * WM_CREATE --------------
;var1 = hwnd, var2 = mp1, var3 = mp2
InitMainWindow Proc Near SYSCALL, var1:DWORD,var2:DWORD,var3:DWORD
      mov    eax,TRUE    ;  processed
       ret
InitMainWindow Endp

;-------------- MainKeyBoard * WM_CHAR --------------
;var1 = mp1,var2 = mp2,var3 = hwnd
MainKeyBoard  Proc Near SYSCALL uses ebx ecx,var1:DWORD,var2:DWORD,var3:DWORD
      mov    eax,FALSE    ; not processed
      ret
MainKeyBoard Endp

;if there are spaces this exits pointing to next non-space else points to non-space
SkipSpaces   proc    ;just skips over spaces
   .WHILE ecx < fsize && byte ptr [esi+ecx] == ' '   ;spaces
        inc ecx
   .ENDW
   .IF ecx >= fsize
        mov EOFflag,1
   .ENDIF
    ret
SkipSpaces   endp

SkipCommentsToEOL proc    ;skip from ; to beginning of next line
    .IF byte ptr[esi+ecx] == ';' && ecx < fsize
        inc    ecx
        .WHILE byte ptr [esi+ecx] != 0ah && ecx < fsize  ;Line feed end of line
            inc ecx
        .ENDW                       ;points to 0ah if it exists if not EOF
    .ELSE
        mov     eax,ecx                  ;bytes processed appears in message
        $WinDebugMessage msgFileErr
        call ExitWin
    .ENDIF
    .IF  byte ptr[esi+ecx] != 0ah   ;must be EOF
         mov    EOFflag,1
    .ELSE                           ;it is end of line
         inc    ecx                 ;goto next line
         .IF    ecx >= fsize
             mov    EOFflag,1
         .ENDIF
    .ENDIF
    ret
SkipCommentsToEOL endp

DataFormatErr   Proc
    mov     eax,ecx            ;bytes processed appears in message
    $WinDebugMessage msgFileErr
    call ExitWin
    ret
DataFormatErr   EndP

ExitWin  Proc
    $Call WinDestroyMsgQueue,hmq
    $Call WinTerminate,hab
    $DosExit    ;so exit
;    ret
ExitWin  Endp

SwitchListAlloc  Proc          ;allocate memory for a switch list of 30
    mov     ecx,00000002h   ;write access flags for Dos alloc
    or      ecx,00000010h   ;page commit
    $Call DosAllocMem,offset baseaddr,3000,ecx
    .IF  eax != 0
       $WinErrMsg  " : DosAllocMem SwitchList"
       Call Exitwin
    .ENDIF
    ret
SwitchListAlloc  Endp

GetSwitchList   Proc
    pusha
    $Call  WinQuerySwitchList,hab,baseaddr,3000
    .IF eax == 0
        $WinErrMsg " : WinQuerySwitchList"
        Call ExitWin
    .ENDIF
    popa
    ret
GetSwitchList   Endp


END   startup                       ;required

