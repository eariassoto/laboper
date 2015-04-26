nasm -f bin Boot/boot.s -o Boot/boot1.bin
nasm -f bin Etapa2/etapa2.s -o Etapa2/KRNLDR.SYS
copy Etapa2\KRNLDR.SYS  A:\KRNLDR.SYS
dd bs=512 count=1 if=Boot/boot1.bin of=\\.\a: