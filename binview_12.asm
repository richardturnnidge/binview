;  MOSlet to print out a binary file in hex, dec or bin


; ---------------------------------------------
;
;   MACROS
;
; ---------------------------------------------

    macro MOSCALL afunc
    ld a, afunc
    rst.lil $08
    endmacro

    macro TABTO x,y
    ld a, 31  
    rst.lil $10    
    ld a, x  
    rst.lil $10   
    ld a, y  
    rst.lil $10    
    endmacro

    macro setcol col
    ld a, 17  
    rst.lil $10    
    ld a, col  
    rst.lil $10    
    endmacro


; ---------------------------------------------
;
;   START HERE
;
; ---------------------------------------------


    .assume adl=1   
    .org $0B0000    ; NOTE different assemble address for MOSlets

    jp start        ; We jump over the header

    .align 64      
    .db "MOS"       
    .db 00h         
    .db 01h       

app_name:      .db     "binview.bin", 0         ; The executable name, only used in arg1
max_args:      EQU     16                       ; Maximum number of arguments allowed in argv
arg_pointer:   .blkb   max_args * 3, 0          ; max 16 x 3 bytes each
num_args:      .db     0                        ; the number of arguments entered

; ---------------------------------------------
;
;   INITIAL SETUP CODE HERE
;
; ---------------------------------------------

start:              
    push af                             ; Push all registers to the stack
    push bc
    push de
    push ix
    push iy

    ld a, 'h'
    ld (format), a                      ; default format hex

    ld a, 8
    ld (columns), a                     ; default columns 8

    ld a, 0
    ld (byteCount), a 
    

    ld  IX, arg_pointer                 ; The argv array pointer address
    push IX
    call PARSE_PARAMS                   ; Parse the parameters
    ld a,c                              ; C contains the number of params entered
    ld (num_args),a                     ; store number of arguments entered
    pop IX                              ; IX: argv  

 ;   call printBin


    cp 2  
    jr c, error                         ; if less than 2 args, then exit
    ld de, (ix+3)                       ; address of first arg, is a 0 terminated string for the file

    call sortArgs                       ; get al the startup info we need
    call drawTitle



; Setup UDG character

    ld hl, udgData                      ; address of string to use
    ld bc, endUgdData - udgData         ; length of string
    rst.lil $18                         ; Call the MOS API to send data to VDP 

    jp drawColumns

now_exit:

    ld a, 15
    rst.lil $10                         ; reset page mode

    pop iy                              ; Pop all registers back from the stack
    pop ix
    pop de
    pop bc
    pop af
    ld hl,0                             ; Load the MOS API return code (0) for no errors.

    ret                                 ; Return MOS

; ---------------------------------------------

error:
    ld hl, errorString                  ; location of string
    call printString
    jp now_exit

; ---------------------------------------------

sortArgs:

; arg 0 is the app - at ix+0
; arg 1 is the filename received

    ld hl, (ix + 3)
    ld (fPointer), hl                   ; store pointer to filename

; arg 2 is the number of columns to draw

    ld a, (num_args)
    cp 2
    ret z                               ; ret if only filename given

    ld hl, (ix + 6)                      ; 2nd arg, the number of columns
    push hl 
    pop de 

    call STRING2INT                     ; pass pointer in DE, hl is result

    ld a, l
    ld (columns), a 

; arg 3 is the format - (h)ex, (d)ec, (b)in

    ld a, (num_args)
    cp 3
    ret z  

    ld hl, (ix + 9)                      ; 2nd arg
    ld a, (hl)
    ld (format), a 

    ret 



; ---------------------------------------------

drawTitle:

    ld a, 14
    rst.lil $10                         ; set page mode

    ld hl, titleString                  ; location of string
    call printString

    ret 

titleString:
    .db "Binary File Viewer - ©2024 R.Turnnidge \n\r"
    .db "\n\r"
    .db "Count   Data....\n\r\n\r",0

; ---------------------------------------------

drawColumns:
    ; open the file
    ld hl, 0
    ld (byteCount), hl

    ld hl, (fPointer)                   ; get pointer to file name provided
    ld c, fa_read                       ; only going to read the file
    MOSCALL $0A                         ; file handle A is returned
           
    cp 0                                ; check if file valid
    jp z, error

    ld (fileHandle), a


rowLoop:

    ; print byte counter
    push af

    ld hl, (byteCount)                 ; put byte counter de into hl


    call B2D16
    ld hl, B2DBUF +15
    call printString


    ld a, ' '
    rst.lil $10
    ld a, '-'
    rst.lil $10
    ld a, ' '
    rst.lil $10
    pop af

    ld a, (columns)
    ld b, a                             ; counter for columns
    ld iy, asciiBuffer                  ; store bytes we read

columnLoop:
    push bc                             ; store column counter

    ld a, (fileHandle)
    ld c, a
    MOSCALL $0E                         ; byte A is returned  

    cp 1                                ; 1 is if at eof
    jp z, doneLoop                      ; if at eof


    ld a, (fileHandle)                  ; if not, then read next char
    ld c, a
    MOSCALL $0C                         ; byte A is returned  


    ld (iy), a 
    inc iy                              ; store byte in buffer and ove to next

    push af

; check which mode we are in

    ld a, (format)
    cp 'b'
    jr z, doBinary

    cp 'd'
    jr z, doDecimal

    ; default, do hex
    pop af
    call printHex

    jr didit

doBinary:

    pop af

    call printBin

    jr didit

doDecimal:
    pop af

    ld hl, 0
    ld l, a

    call B2D16
    ld hl, B2DBUF +17
    call printString   

    jr didit

didit:

    ld a, ' '
    rst.lil $10


    ld hl, (byteCount)
    inc hl                              ; inc byte counter
    ld (byteCount), hl

    pop bc                              ; get column counter back

    djnz columnLoop

;   now do ascii chars
    ld a, (columns)
    ld b, a                             ; counter for columns
    ld iy, asciiBuffer                  ; store bytes we read

asciiLoop: 

    ld a, (iy)

    cp 32   
    jr c, nonAscii
    cp 128   
    jr nc, nonAscii


    jr printAscii

nonAscii:
    ld a, 128;'?'                       ; non ascii so use '?'


printAscii:
    rst.lil $10                         ; print it out
    inc iy
    djnz asciiLoop

    ld a, 17
    rst.lil $10
    ld a, 128

    rst.lil $10

;   new line for next row

    ld hl, LINEFEED
    call printString



    MOSCALL $08                         ; get IX pointer to sysvars
    ld a, (ix + 05h)                    ; ix+5h is 'last key pressed'
    cp 27                               ; is it ESC key?
    jp z, doneLoop                      ; if so exit cleanly

    jp rowLoop

doneLoop:
    pop bc


    ld a, (fileHandle)
    ld c, a
    MOSCALL $0B                         ; close file

    ld hl, LINEFEED
    call printString


    jp now_exit 

asciiBuffer:    .blkb 32,'?'
byteCount:  .dl 0                       ; count through the file
fileHandle: .db 0
linebg:     .db 0

; ---------------------------------------------


printString:                            ; print zero terminated string
    ld a,(hl)
    or a
    ret z
    RST.LIL 10h
    inc hl
    jr printString

errorString:
    .db "ERROR - no valid file\n\r",0

LINEFEED:           .asciz "\r\n"

; ---------------------------------------------

; File access modes
;
fa_read:            EQU 01h
fa_write:           EQU 02h
fa_open_existing:   EQU 00h
fa_create_new:      EQU 04h
fa_create_always:   EQU 08h
fa_open_always:     EQU 10h
fa_open_append:     EQU 30h

fPointer:   .dl     0
fileName:   .blkb   64,0
columns:    .db     0
format:     .db     0

; ---------------------------------------------

udgData:
    .db     23, 128              ; define UDG character number ?
    .db     00000000b            ; binary data 0
    .db     00001000b            ; binary data 1
    .db     00010100b            ; binary data 2
    .db     00000100b            ; binary data 3
    .db     00001000b            ; binary data 4
    .db     00000000b            ; binary data 5
    .db     00001000b            ; binary data 6
    .db     00000000b            ; binary data 7

endUgdData:

; ---------------------------------------------
;
;   PARAM PARSING ROUTINE WRITTEN BY OTHERS
;
; ---------------------------------------------

; Parse the parameter string into a C style array
; Parameters
; - HL: Address of parameter string
; - IX: Address for array pointer storage
; Returns:
; -  C: Number of parameters parsed

PARSE_PARAMS:      
    ld BC, app_name
    ld (IX+0), BC                       ; ARGV[0] = the executable name
    inc IX
    inc IX
    inc IX
    call skip_spaces                    ; Skip HL past any leading spaces

    ld BC, 1                            ; C: ARGC = 1 - also clears out top 16 bits of BCU
    ld B, max_args - 1                  ; B: Maximum number of arg_pointer

parse_step_2:    
    push BC                             ; Stack ARGC    
    push HL                             ; Stack start address of token
    call get_token                      ; Get the next token
    ld A, C                             ; A: Length of the token in characters
    pop DE                              ; Start address of token (was in HL)
    pop BC                              ; ARGC
    or  A                               ; Check for A=0 (no token found) OR at end of string
    ret Z

    ld  (IX+0), DE                      ; Store the pointer to the token
    push HL                             ; DE=HL
    pop DE
    call skip_spaces                    ; And skip HL past any spaces onto the next character
    Xor A
    ld (DE), A                          ; Zero-terminate the token
    inc IX
    inc IX
    inc IX                              ; Advance to next pointer position
    inc C                               ; Increment ARGC
    ld A, C                             ; Check for C >= A
    cp B
    jr C, parse_step_2                  ; And loop
    RET

; ---------------------------------------------

; Skip spaces in the parameter string
; Parameters:
; - HL: Address of parameter string
; Returns:
; - HL: Address of next none-space character
;    F: Z if at end of string, otherwise NZ if there are more tokens to be parsed

skip_spaces:       
    ld A, (HL)                  ; Get the character from the parameter string   
    cp ' '                      ; Exit if not space
    ret NZ 
    inc HL                      ; Advance to next character
    jr skip_spaces              ; Increment length


; ---------------------------------------------

; Get the next token
; Parameters:
; - HL: Address of parameter string
; Returns:
; - HL: Address of first character after token
; -  C: Length of token (in characters)

get_token:     
    ld C, 0                     ; Initialise length
token_loop:
    ld A, (HL)                  ; Get the character from the parameter string
    or A                        ; Exit if 0 (end of parameter string in MOS)
    ret Z
    cp 13                       ; Exit if CR (end of parameter string in BBC BASIC)
    ret Z
    cp ' '                      ; Exit if space (end of token)
    ret Z
    inc HL                      ; Advance to next character
    inc C                       ; Increment length
    jr token_loop


; ---------------------------------------------
;
;   STRING TO INTEGER ROUTINE WRITTEN BY OTHERS
;
; ---------------------------------------------

; Takes pointer to a string of ascii representing an integer and converts to 3 byte integer
; hl = result
; de = pointer to ASCII number

STRING2INT:
  Ld hl,0
s2i_loop: 
  ld a,(de)
  Sub 48
  Jr c,s2i_done
  Cp 10
  Jr nc,s2i_done
  Push hl
  Pop bc
  Add hl,hl                     ; x2
  Add hl,hl                     ; x4
  Add hl,bc                     ; x5
  Add hl,hl                     ; x10
  Ld bc,0
  Ld c,a
  Add hl,bc                     ; Add digit
  Inc de                        ; go to next number
  Jr s2i_loop
s2i_done:
  ret 

; ---------------------------------------------
;
;   DEBUG ROUTINES
;
; ---------------------------------------------
    
; ---------------------------------
; print decimal value of 0 -> 255 to screen at current TAB position

printDec:               ; debug A to screen as 3 char string pos

    ld (base),a         ; save

    cp 200              ; are we under 200 ?
    jr c,_under200      ; not 200+
    sub a, 200
    ld (base),a         ; sub 200 and save

    ld a, '2'           ; 2 in ascii
    rst.lil $10         ; print out a '200' digit

    jr _under100

_under200:
    cp 100              ; are we under 100 ?
    jr c,_under100      ; not 200+
    sub a, 100
    ld (base),a         ; sub 100 and save

    ld a, '1'           ; 1 in ascii
    rst.lil $10         ; print out a '100' digit

_under100:
    ld a, (base)        ; get last 2 digits as decimal
    ld c,a              ; store numerator in C
    ld d, 10            ; D will be denominator
    call C_Div_D        ; divide C by 10 to get two parts. 
                        ; A is the remainder, C is the int of C/D

    ld b, a             ; put remainder ascii into B

    ld a, c             ; get int div
    cp 0                ; if 0 (ie, number was <10)
    jr z, _lastBut1    ; just do last digit

    add a, 48           ; add 48 to make ascii of int C/D
    rst.lil $10         ; print out 10s digit
    jr _lastDigit

_lastBut1:
    add a, 48           ; add 48 to make ascii of int C/D
    rst.lil $10         ; print out 10s digit

_lastDigit:
    ld a,b              ; get remainder back
    add a, 48           ; add 48 to remainder to convert to ascii   
    rst.lil $10         ; print out last digit

    ret 

base:   .db     0       ; used in calculations

; ---------------------------------

printHex:                           ; debug A to screen as HEX byte pair at pos BC
    push af 
    push bc
    push hl
    ld (debug_char), a              ; store A
                                    ; first, print 'A=' at TAB 36,0
;     ld a, 31                        ; TAB at x,y
;     rst.lil $10
;     ld a, b                         ; x=b
;     rst.lil $10
;     ld a,c                          ; y=c
;     rst.lil $10                     ; put tab at BC position

    ld a, (debug_char)              ; get A from store, then split into two nibbles
    and 11110000b                   ; get higher nibble
    rra
    rra
    rra
    rra                             ; move across to lower nibble
    add a,48                        ; increase to ascii code range 0-9
    cp 58                           ; is A less than 10? (58+)
    jr c, nextbd1a                   ; carry on if less
    add a, 7                        ; add to get 'A' char if larger than 10
nextbd1a:    
    rst.lil $10                     ; print the A char

    ld a, (debug_char)              ; get A back again
    and 00001111b                   ; now just get lower nibble
    add a,48                        ; increase to ascii code range 0-9
    cp 58                           ; is A less than 10 (58+)
    jp c, nextbd2a                   ; carry on if less
    add a, 7                        ; add to get 'A' char if larger than 10 
nextbd2a:    
    rst.lil $10                     ; print the A char
    
    ld a, (debug_char)
    pop hl
    pop bc
    pop af 
    ret                             ; head back

; ---------------------------------

debugA:                             ; debug A to screen as HEX byte pair at pos BC
    push af 
    push bc
    push hl
    ld (debug_char), a              ; store A
                                    ; first, print 'A=' at TAB 36,0
    ld a, 31                        ; TAB at x,y
    rst.lil $10
    ld a, b                         ; x=b
    rst.lil $10
    ld a,c                          ; y=c
    rst.lil $10                     ; put tab at BC position

    ld a, (debug_char)              ; get A from store, then split into two nibbles
    and 11110000b                   ; get higher nibble
    rra
    rra
    rra
    rra                             ; move across to lower nibble
    add a,48                        ; increase to ascii code range 0-9
    cp 58                           ; is A less than 10? (58+)
    jr c, nextbd1                   ; carry on if less
    add a, 7                        ; add to get 'A' char if larger than 10
nextbd1:    
    rst.lil $10                     ; print the A char

    ld a, (debug_char)              ; get A back again
    and 00001111b                   ; now just get lower nibble
    add a,48                        ; increase to ascii code range 0-9
    cp 58                           ; is A less than 10 (58+)
    jp c, nextbd2                   ; carry on if less
    add a, 7                        ; add to get 'A' char if larger than 10 
nextbd2:    
    rst.lil $10                     ; print the A char
    
    ld a, (debug_char)
    pop hl
    pop bc
    pop af 
    ret                             ; head back

debug_char:     .db 0

; -----------------

C_Div_D:
;Inputs:
;     C is the numerator
;     D is the denominator
;Outputs:
;     A is the remainder
;     B is 0
;     C is the result of C/D
;     D,E,H,L are not changed
;
    ld b,8              ; B is counter = 8
    xor a               ; [loop] clear flags
    sla c               ; C = C x 2
    rla                 ; A = A x 2 + Carry
    cp d                ; compare A with Denominator
    jr c,$+4            ; if bigger go to loop
    inc c               ; inc Numerator
    sub d               ; A = A - denominator
    djnz $-8            ; go round loop
    ret                 ; done 8 times, so return

; not sure how this works, need to get my head around it!


; ---------------------------------------------

printBin:
                ; take A as number and print out as binary, B,C as X,Y position
                ; take D as number of bits to do- fixed 8 in this example
;     push af 

;     ld a, 31        ; TAB at x,y
;     rst.lil $10
;     ld a, b         ; x=b
;     rst.lil $10
;     ld a,c          ; y=c
;     rst.lil $10     ; put tab at BC position

;     pop af 


    ld b, 8 ; number of bits to do
    ld hl, binString
rpt:
    ld (hl), 48     ; ASCII 0 is 48, 1 is 49 ; reset first

    bit 7, a
    jr z, nxt
    ld (hl), 49
nxt:    
    inc hl  ; next position in string
    rla 
    djnz rpt


    ld hl, printStr
    ld bc, endPrintStr - printStr

    rst.lil $18


    ret

            ; print binary
printStr:
binString:  .db     "00000000"
endPrintStr:

; ---------------------------------

; Print ASCII of a BYTE, WORD, LONG, etc

; Combined routine for conversion of different sized binary numbers into
; directly printable ASCII(Z)-string
; Input value in registers, number size and -related to that- registers to fill
; is selected by calling the correct entry:
;
;  entry  inputregister(s)  decimal value 0 to:
;   B2D8             A                    255  (3 digits)
;   B2D16           HL                  65535   5   "
;   B2D24         E:HL               16777215   8   "
;   B2D32        DE:HL             4294967295  10   "
;   B2D48     BC:DE:HL        281474976710655  15   "
;   B2D64  IX:BC:DE:HL   18446744073709551615  20   "
;
; The resulting string is placed into a small buffer attached to this routine,
; this buffer needs no initialization and can be modified as desired.
; The number is aligned to the right, and leading 0's are replaced with spaces.
; On exit HL points to the first digit, (B)C = number of decimals
; This way any re-alignment / postprocessing is made easy.
; Changes: AF,BC,DE,HL,IX
; P.S. some examples below

; by Alwin Henseler


B2D8:    LD H,0
         LD L,A
B2D16:   LD E,0             ; ENTRY point for 16 bit numbers
B2D24:   LD D,0
B2D32:   LD BC,0
B2D48:   LD IX,0          ; zero all non-used bits
B2D64:   LD (B2DINV),HL
         LD (B2DINV+2),DE
         LD (B2DINV+4),BC
         LD (B2DINV+6),IX ; place full 64-bit input value in buffer
         LD HL,B2DBUF
         LD DE,B2DBUF+1
    LD (HL), ' '

B2DFILC: EQU $-1         ; address of fill-character
         LD BC,18
         LDIR            ; fill 1st 19 bytes of buffer with spaces
         LD (B2DEND-1),BC ;set BCD value to "0" & place terminating 0
         LD E,1          ; no. of bytes in BCD value
         LD HL,B2DINV+8  ; (address MSB input)+1
         LD BC,#0909
         XOR A
B2DSKP0: DEC B
         JR Z,B2DSIZ     ; all 0: continue with postprocessing
         DEC HL
         OR (HL)         ; find first byte <>0
         JR Z,B2DSKP0
B2DFND1: DEC C
         RLA
         JR NC,B2DFND1   ; determine no. of most significant 1-bit
         RRA
         LD D,A          ; byte from binary input value
B2DLUS2: PUSH HL
         PUSH BC
B2DLUS1: LD HL,B2DEND-1  ; address LSB of BCD value
         LD B,E          ; current length of BCD value in bytes
         RL D            ; highest bit from input value -> carry
B2DLUS0: LD A,(HL)
         ADC A,A
         DAA
         LD (HL),A       ; double 1 BCD byte from intermediate result
         DEC HL
         DJNZ B2DLUS0    ; and go on to double entire BCD value (+carry!)
         JR NC,B2DNXT
         INC E           ; carry at MSB -> BCD value grew 1 byte larger
         LD (HL),1       ; initialize new MSB of BCD value
B2DNXT:  DEC C
         JR NZ,B2DLUS1   ; repeat for remaining bits from 1 input byte
         POP BC          ; no. of remaining bytes in input value
         LD C,8          ; reset bit-counter
         POP HL          ; pointer to byte from input value
         DEC HL
         LD D,(HL)       ; get next group of 8 bits
         DJNZ B2DLUS2    ; and repeat until last byte from input value
B2DSIZ:  LD HL,B2DEND    ; address of terminating 0
         LD C,E          ; size of BCD value in bytes
         OR A
         SBC HL,BC       ; calculate address of MSB BCD
         LD D,H
         LD E,L
         SBC HL,BC
         EX DE,HL        ; HL=address BCD value, DE=start of decimal value
         LD B,C          ; no. of bytes BCD
         SLA C           ; no. of bytes decimal (possibly 1 too high)
         LD A,'0'
         RLD             ; shift bits 4-7 of (HL) into bit 0-3 of A
         CP '0'          ; (HL) was > 9h?
         JR NZ,B2DEXPH   ; if yes, start with recording high digit
         DEC C           ; correct number of decimals
         INC DE          ; correct start address
         JR B2DEXPL      ; continue with converting low digit
B2DEXP:  RLD             ; shift high digit (HL) into low digit of A
B2DEXPH: LD (DE),A       ; record resulting ASCII-code
         INC DE
B2DEXPL: RLD
         LD (DE),A
         INC DE
         INC HL          ; next BCD-byte
         DJNZ B2DEXP     ; and go on to convert each BCD-byte into 2 ASCII
         SBC HL,BC       ; return with HL pointing to 1st decimal
         RET

B2DINV:  .DS 8            ; space for 64-bit input value (LSB first)
B2DBUF:  .DS 20           ; space for 20 decimal digits
B2DEND:  .DS 1            ; space for terminating 0



