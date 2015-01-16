;---------------------------------------------------------------------------;
; M.F.Kaplon  Begun:Tue  07-14-1992  Revised:Tue  09-15-1992
; hook_dls.asn
;
; "Copyright 1992 M.F. Kaplon"
;
; This is the dll to be used with hook_kbs.asm
;
; Its purpose is to test the system message stream for WM_CHAR and when
; selected keys are struck to post a message to hook_kbs using WM_USER+300h.
;
; The criteria for selection is : KeyStroke Down
;                                 Shift Key and either Alt or Ctrl down
;                                 valid Scan Code indicated.
;
; If those criteria are met then the msg ID is changed to WM_USER+0cfffh
; after mp1 and mp2 of the message are obtained and the mp1 and mp2
; of the message to be posted to hook_kbs are changed to contain
; mp1 = Alt/Ctrl flag = 0/1 if Alt/Ctrl down
; mp2 = Scan Code of Key on Down Stroke
;
; The function InputHook monitors the System message queue and responds
; whenever the WinGetMsg or WinPeekMsg is about to return a message.
;
; This is assembled and linked as dll by calling   dll-w386  hook_dls
;
; ths cmd file dll-w386 also moves the created dll to  c:\os2\dll
;
;---------------------------------------------------------------------------;
;
;------------------ PRELIMINARIES ----------------------

.386             ;preceeding .MODEL makes USE32 default
.MODEL           FLAT,SYSCALL,OS_OS2

INCL_DOSMEMMGR      equ  1
INCL_WINERRORS      equ  1
INCL_WIN            equ  1
INCLUDE      c:\toolkt20\asm\os2inc\os2def.inc  ;structure defns includes POINTL
INCLUDE      c:\toolkt20\asm\os2inc\pmwin.inc   ;structure defns POINTL defn required
INCLUDE      c:\toolkt20\asm\os2inc\bsememf.inc ;memory
INCLUDE      c:\toolkt20\asm\os2inc\pmerr.inc   ;errors
INCLUDELIB   c:\toolkt20\os2lib\os2386.lib      ;Library

INCLUDE      doswin32.mac                       ;macros for calls

.STACK    2048

.DATA
;------------- handles --------
jr_hab            DWORD   ?    ;Anchor block handle
hook              DWORD   ?    ;Handle of hook_kbs
hook_mp1          DWORD   ?    ;mp1 of message
hook_mp2          DWORD   ?    ;mp2 of message
;------------ structures
jr_qmsg           DWORD   ?    ;Address of InputHook Message structure

;----Shared Memory Variables---
SharedMem      DWORD  0     ;base address returned
SharedMemName   BYTE  "\SHAREMEM\DATAS.DAT",0
SharedMemFlags DWORD  12h   ;(PAG_COMMIT OR OBJ_GETTABLE OR PAG_WRITE)

.CODE

;----------  ESTABLISH InputHook --------------
;Has the form BOOL EXPENTRY InputHook(HAB hab, PQMSG pQmsg,ULONG fs)
;QMSG TRUCT  has  following parms as offsets
;offset 0 hwnd ,offset 4 msg ,offset 8 mp1 ,offset 12 mp2 ,etc.
;fs contains flags from WinPeekMsg function
InputHook     proc
;---- GET PARAMETERS FROM STACK ---
    push   ebp                  ;return address = 4 bytes,this push = 4 bytes
    mov    ebp,esp              ;so first parameter is 8 bytes from top
    mov    eax,[ebp+8]          ;hab
    mov    jr_hab,eax           ;anchor block handle
    mov    eax,[ebp+12]         ;address of qmsg truct
    mov    jr_qmsg,eax          ;store address of QMSG STRUCT structure
;---- restore stack pointer
    mov    esp,ebp              ;restore  stack pointer
    pop    ebp                  ;back to  way it was at entrance

;---- GET ADDRESS OF SHARED MEMORY ---
    .IF SharedMem == 0
       $Call DosGetNamedSharedMem,offset SharedMem,offset SharedMemName,1
       .IF eax != 0
          $Alarm    ;$WinErrMsg " : DosAllocSharedMem"
       .ENDIF
       ;---- Get handle of hook_kb and Release Shared Memory ----
       mov   edi,SharedMem
       mov   eax,[edi]
       mov   hook,eax              ;now has handle of hook-kb
       $Call DosFreeMem,SharedMem  ;release since no longer needed
       .IF eax != 0
          $Alarm    ;$WinErrMsg " : DosAllocSharedMem"
       .ENDIF
    .ENDIF
;---- IF WM_CHAR MESSAGE DETECTED -----
    ;save mp1 and mp2 of message
    mov  esi,jr_qmsg              ;point to message
    .IF dword ptr [esi+4] == WM_CHAR
         mov  eax,[esi+8]
         mov  hook_mp1,eax
         mov  eax,[esi+12]
         mov  hook_mp2,eax
     ;Test for Shift Key,Alt/Ctrl Down and Valid Scan Key
     ;------ NOW [esi+8]=mp1  [esi+12]=mp2 ------
         mov    ebx,hook_mp1
         mov    ecx,KC_KEYUP
         test   bx,cx
         jnz    wm0             ;accept only on down stroke
;         jz    wm0             ;accept only on up strike
         test   ebx,KC_SCANCODE ;test scan code
         jz     wm0             ;jump if no valid scan code
         test   ebx,KC_SHIFT
         jz     wm0             ;shift key not down - dont process
         mov    ecx,KC_ALT      ;? Alt Key down when msg generated
         test   bx,cx           ;test scan code
         jz     wm1             ;jump if Alt Key Not Hit & test Ctrl key
         mov    hook_mp1,0      ;Alt/Ctrl flag
         jmp    wm2
wm1:     mov    ecx,KC_CTRL     ;? Ctrl-Key down
         test   bx,cx
         jz     wm0             ;Was neither
         mov    hook_mp1,1      ;Alt/Ctrl flag
         ;if ScanCode & Shift & one of Alt or Ctrl down send original message nowhere
wm2:     mov    dword ptr [esi+4],WM_USER+0cfffh
         xor    edx,edx
         shld   edx,ebx,8       ;get high 8 bits of mp1 into dl
         mov    hook_mp2,edx    ;scan code
         ;WinPostMessage to HOOK_KBS
         $Call WinPostMsg,hook,WM_USER+300h,hook_mp1,hook_mp2
    .ENDIF
wm0:mov    eax,FALSE            ;pass to next hook in chain
    ret
InputHook  endp

END

