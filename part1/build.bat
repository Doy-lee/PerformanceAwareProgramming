@echo off

REM Setup
REM ===========================================================================
set script_dir_backslash=%~dp0
set script_dir=%script_dir_backslash:~0,-1%
set build_dir=%script_dir%\build
if not exist %build_dir% mkdir %build_dir%

copy /Y %script_dir%\listing_0037_single_register_mov %build_dir% 1>NUL
copy /Y %script_dir%\listing_0038_many_register_mov %build_dir% 1>NUL
copy /Y %script_dir%\listing_0039_more_movs %build_dir% 1>NUL
copy /Y %script_dir%\listing_0040_challenge_movs %build_dir% 1>NUL

REM Build
REM ===========================================================================
pushd %build_dir%
cl %script_dir%\sim8086.c /W4 /WX /Z7 /nologo || exit /b 1
popd

REM Tests
REM ===========================================================================
set listing_0037=%build_dir%\listing_0037_single_register_mov
%build_dir%\sim8086.exe %listing_0037% > %listing_0037%_disassembled.asm
nasm %listing_0037%_disassembled.asm
fc %listing_0037% %listing_0037%_disassembled || exit /b 1

set listing_0038=%build_dir%\listing_0038_many_register_mov
%build_dir%\sim8086.exe %listing_0038% > %listing_0038%_disassembled.asm
nasm %listing_0038%_disassembled.asm
fc %listing_0038% %listing_0038%_disassembled || exit /b 1

set listing_0039=%build_dir%\listing_0039_more_movs
%build_dir%\sim8086.exe %listing_0039% > %listing_0039%_disassembled.asm
nasm %listing_0039%_disassembled.asm
fc %listing_0039% %listing_0039%_disassembled || exit /b 1

set listing_0040=%build_dir%\listing_0040_challenge_movs
%build_dir%\sim8086.exe %listing_0040% > %listing_0040%_disassembled.asm
nasm %listing_0040%_disassembled.asm
fc %listing_0040% %listing_0040%_disassembled || exit /b 1
