nasm -f bin Boot1.asm -o Boot1.bin
dd bs=512 count=1 if=Boot1.bin of=\\.\a:
pause
