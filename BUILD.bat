nasm -f bin Stage1\Boot1.asm -o Stage1\Boot1.bin
dd bs=512 count=1 if=Stage1\Boot1.bin of=\\.\a:
pause

nasm -f bin Stage2\Stage2.asm -o Stage2\KRNLDR.SYS
copy Stage2\KRNLDR.SYS  A:\KRNLDR.SYS
pause

nasm -f bin Kernel\Stage3.asm -o Kernel\KRNL.SYS
copy Kernel\KRNL.SYS  A:\KRNL.SYS
pause
