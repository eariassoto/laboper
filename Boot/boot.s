
;*********************************************
;	Boot1.asm
;		- Etapa 1 del bootloader
;
;	Laboratorio Sistemas Operativos 
;*********************************************

bits	16						; we are in 16 bit real mode

org	0						; we will set regisers later

start:	
     jmp	main					; salto al inicio del booteo

;*********************************************
;	BIOS Parameter Block
;*********************************************

; BPB Begins 3 bytes from start. We do a far jump, which is 3 bytes in size.
; If you use a short jump, add a "nop" after it to offset the 3rd byte.

bpbOEM			db "My OS   "			; OEM identifier (Cannot exceed 8 bytes!)
bpbBytesPerSector:  	DW 512
bpbSectorsPerCluster: 	DB 1
bpbReservedSectors: 	DW 1
bpbNumberOfFATs: 	DB 2
bpbRootEntries: 	DW 224
bpbTotalSectors: 	DW 2880
bpbMedia: 		DB 0xf8  ;; 0xF1
bpbSectorsPerFAT: 	DW 9
bpbSectorsPerTrack: 	DW 18
bpbHeadsPerCylinder: 	DW 2
bpbHiddenSectors: 	DD 0
bpbTotalSectorsBig:     DD 0
bsDriveNumber: 	        DB 0
bsUnused: 		DB 0
bsExtBootSignature: 	DB 0x29
bsSerialNumber:	        DD 0xa0a1a2a3
bsVolumeLabel: 	        DB "MOS FLOPPY "
bsFileSystem: 	        DB "FAT12   "

;************************************************;
;	Imprime una hilera. En SI esta el puntero a 
;    la misma
;	DS=>SI: 0 fin de hilera
;************************************************;
Print:
	lodsb		  ; carga el byte siguiente de la hilera en SI a AL
	or	al, al	  ; AL=0?
	jz	PrintDone   ; Si, encontro final de hilera
	mov	ah, 0eh     ; No, imprima el caracter
	int	10h
	jmp	Print       ; Repeticion para imprimir siguiente caracter
PrintDone:
	ret		       ; vamonos

;************************************************;
; Lee una serie de sectores
; CX=>Numero de sectores a leer
; AX=>Sector inicial
; ES:BX=>Buffer de donde leer
;************************************************;

ReadSectors:
     .MAIN
          mov     di, 0x0005                          ; five retries for error
     .SECTORLOOP
          push    ax
          push    bx
          push    cx
          call    LBACHS                              ; convert starting sector to CHS
          mov     ah, 0x02                            ; BIOS read sector
          mov     al, 0x01                            ; read one sector
          mov     ch, BYTE [absoluteTrack]            ; track
          mov     cl, BYTE [absoluteSector]           ; sector
          mov     dh, BYTE [absoluteHead]             ; head
          mov     dl, BYTE [bsDriveNumber]            ; drive
          int     0x13                                ; invoke BIOS
          jnc     .SUCCESS                            ; test for read error
          xor     ax, ax                              ; BIOS reset disk
          int     0x13                                ; invoke BIOS
          dec     di                                  ; decrement error counter
          pop     cx
          pop     bx
          pop     ax
          jnz     .SECTORLOOP                         ; attempt to read again
          int     0x18
     .SUCCESS
          mov     si, msgProgress
          call    Print
          pop     cx
          pop     bx
          pop     ax
          add     bx, WORD [bpbBytesPerSector]        ; queue next buffer
          inc     ax                                  ; queue next sector
          loop    .MAIN                               ; read next sector
          ret

;************************************************;
; Convierte CHS a LBA
; LBA = (cluster - 2) * sectores por cluster
;************************************************;

ClusterLBA:
          sub     ax, 0x0002                          ; zero base cluster number
          xor     cx, cx
          mov     cl, BYTE [bpbSectorsPerCluster]     ; convert byte to word
          mul     cx
          add     ax, WORD [datasector]               ; base data sector
          ret
     
;************************************************;
; Convierte LBA a CHS
; AX=>Direccion LBA a convertir
;
; absolute sector = (logical sector / sectors per track) + 1
; absolute head   = (logical sector / sectors per track) MOD number of heads
; absolute track  = logical sector / (sectors per track * number of heads)
;
;************************************************;

LBACHS:
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [bpbSectorsPerTrack]           ; calculate
          inc     dl                                  ; adjust for sector 0
          mov     BYTE [absoluteSector], dl
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [bpbHeadsPerCylinder]          ; calculate
          mov     BYTE [absoluteHead], dl
          mov     BYTE [absoluteTrack], al
          ret

;*********************************************
;	Punto de inicio del bootloader
;*********************************************

main:

     ;----------------------------------------------------
     ; el codigo esta localizado en 0000:7C00, 
     ; ajustar registros de segmento
     ;----------------------------------------------------
     
          cli				; deshabilitar interrupciones
          mov     ax, 0x07C0  ; configurar registros para que apunten a nuestro segmento
          mov     ds, ax
          mov     es, ax
          mov     fs, ax
          mov     gs, ax

     ;----------------------------------------------------
     ; crear la pila
     ;----------------------------------------------------
     
          mov     ax, 0x0000	; configurar la pila
          mov     ss, ax
          mov     sp, 0xFFFF
          sti				; restaurar interrupciones

     ;----------------------------------------------------
     ; Mostrar mensaje de carga
     ;----------------------------------------------------
     
          mov     si, msgLoading
          call    Print
          
     ;----------------------------------------------------
     ; Cargar tabla del directorio raiz
     ;----------------------------------------------------

     LOAD_ROOT:
     
     ; calcular el tamano del directorio raiz y almacenelo
     ; en el registro cx
     
          xor     cx, cx
          xor     dx, dx
          mov     ax, 0x0020                           ; 32 byte directory entry
          mul     WORD [bpbRootEntries]                ; total size of directory
          div     WORD [bpbBytesPerSector]             ; sectors used by directory
          xchg    ax, cx
          
     ; calcular localizacion del directorio raiz y almacenelo
     ; en el registro ax
     
          mov     al, BYTE [bpbNumberOfFATs]            ; number of FATs
          mul     WORD [bpbSectorsPerFAT]               ; sectors used by FATs
          add     ax, WORD [bpbReservedSectors]         ; adjust for bootsector
          mov     WORD [datasector], ax                 ; base of root directory
          add     WORD [datasector], cx
          
     ; leer directorio raiz en la memoria (7C00:0200)
     
          mov     bx, 0x0200                            ; copiar dir raiz sobre el bootloader
          call    ReadSectors

     ;----------------------------------------------------
     ; Encontrar etapa 2
     ;----------------------------------------------------

     ; navegar directorio raiz para la imagen binaria
          mov     cx, WORD [bpbRootEntries]             ; cargar el loop counter
          mov     di, 0x0200                            ; encontrar primer entrada dir raiz
     .LOOP:
          push    cx
          mov     cx, 0x000B                            ; nombre con 11 caracteres
          mov     si, ImageName                         ; nombre a encontrar
          push    di
          rep  cmpsb                                    ; ver si coinciden
          pop     di
          je      LOAD_FAT
          pop     cx
          add     di, 0x0020                            ; enliste siguiente entrada
          loop    .LOOP
          jmp     FAILURE

     ;----------------------------------------------------
     ; Cargue FAT
     ;----------------------------------------------------

     LOAD_FAT:
     
     ; save starting cluster of boot image
     
          mov     si, msgCRLF
          call    Print
          mov     dx, WORD [di + 0x001A]
          mov     WORD [cluster], dx                  ; files first cluster
          
     ; compute size of FAT and store in "cx"
     
          xor     ax, ax
          mov     al, BYTE [bpbNumberOfFATs]          ; number of FATs
          mul     WORD [bpbSectorsPerFAT]             ; sectors used by FATs
          mov     cx, ax

     ; compute location of FAT and store in "ax"

          mov     ax, WORD [bpbReservedSectors]       ; adjust for bootsector
          
     ; read FAT into memory (7C00:0200)

          mov     bx, 0x0200                          ; copy FAT above bootcode
          call    ReadSectors

     ; read image file into memory (0050:0000)
     
          mov     si, msgCRLF
          call    Print
          mov     ax, 0x0050
          mov     es, ax                              ; destination for image
          mov     bx, 0x0000                          ; destination for image
          push    bx

     ;----------------------------------------------------
     ; Cargue etapa 2
     ;----------------------------------------------------

     LOAD_IMAGE:
     
          mov     ax, WORD [cluster]                  ; cluster to read
          pop     bx                                  ; buffer to read into
          call    ClusterLBA                          ; convert cluster to LBA
          xor     cx, cx
          mov     cl, BYTE [bpbSectorsPerCluster]     ; sectors to read
          call    ReadSectors
          push    bx
          
     ; compute next cluster
     
          mov     ax, WORD [cluster]                  ; identify current cluster
          mov     cx, ax                              ; copy current cluster
          mov     dx, ax                              ; copy current cluster
          shr     dx, 0x0001                          ; divide by two
          add     cx, dx                              ; sum for (3/2)
          mov     bx, 0x0200                          ; location of FAT in memory
          add     bx, cx                              ; index into FAT
          mov     dx, WORD [bx]                       ; read two bytes from FAT
          test    ax, 0x0001
          jnz     .ODD_CLUSTER
          
     .EVEN_CLUSTER:
     
          and     dx, 0000111111111111b               ; take low twelve bits
         jmp     .DONE
         
     .ODD_CLUSTER:
     
          shr     dx, 0x0004                          ; take high twelve bits
          
     .DONE:
     
          mov     WORD [cluster], dx                  ; store new cluster
          cmp     dx, 0x0FF0                          ; test for end of file
          jb      LOAD_IMAGE
          
     DONE:
     
          mov     si, msgCRLF
          call    Print
          push    WORD 0x0050
          push    WORD 0x0000
          retf
          
     FAILURE:
     
          mov     si, msgFailure
          call    Print
          mov     ah, 0x00
          int     0x16                                ; await keypress
          int     0x19                                ; warm boot computer
     
     absoluteSector db 0x00
     absoluteHead   db 0x00
     absoluteTrack  db 0x00
     
     datasector  dw 0x0000
     cluster     dw 0x0000
     ImageName   db "KRNLDR  SYS"
     msgLoading  db 0x0D, 0x0A, "Loading Boot Image ", 0x0D, 0x0A, 0x00
     msgCRLF     db 0x0D, 0x0A, 0x00
     msgProgress db ".", 0x00
     msgFailure  db 0x0D, 0x0A, "ERROR : Press Any Key to Reboot", 0x0A, 0x00
     
          TIMES 510-($-$$) DB 0
          DW 0xAA55
