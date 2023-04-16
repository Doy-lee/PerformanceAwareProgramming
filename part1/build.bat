@echo off

REM Setup
REM ===========================================================================
set script_dir_backslash=%~dp0
set script_dir=%script_dir_backslash:~0,-1%
set build_dir=%script_dir%\build
if not exist %build_dir% mkdir %build_dir%

REM Build
REM ===========================================================================
pushd %build_dir%
cl %script_dir%\sim8086.c /W4 /WX /Z7 /nologo || exit /b 1
popd

REM Tests
REM ===========================================================================
set listing_0037=listing_0037_single_register_mov
copy /Y %script_dir%\%listing_0037% %build_dir% 1>NUL
set build_dir_listing_0037=%build_dir%\%listing_0037%
%build_dir%\sim8086.exe %build_dir_listing_0037% > %build_dir_listing_0037%_disassembled.asm
nasm %build_dir_listing_0037%_disassembled.asm
fc /B %build_dir_listing_0037% %build_dir_listing_0037%_disassembled || exit /b 1

set listing_0038=listing_0038_many_register_mov
copy /Y %script_dir%\%listing_0038% %build_dir% 1>NUL
set build_dir_listing_0038=%build_dir%\%listing_0038%
%build_dir%\sim8086.exe %build_dir_listing_0038% > %build_dir_listing_0038%_disassembled.asm
nasm %build_dir_listing_0038%_disassembled.asm
fc /B %build_dir_listing_0038% %build_dir_listing_0038%_disassembled || exit /b 1

set listing_0039=listing_0039_more_movs
copy /Y %script_dir%\%listing_0039% %build_dir% 1>NUL
set build_dir_listing_0039=%build_dir%\%listing_0039%
%build_dir%\sim8086.exe %build_dir_listing_0039% > %build_dir_listing_0039%_disassembled.asm
nasm %build_dir_listing_0039%_disassembled.asm
fc /B %build_dir_listing_0039% %build_dir_listing_0039%_disassembled || exit /b 1

set listing_0040=listing_0040_challenge_movs
copy /Y %script_dir%\%listing_0040% %build_dir% 1>NUL
set build_dir_listing_0040=%build_dir%\%listing_0040%
%build_dir%\sim8086.exe %build_dir_listing_0040% > %build_dir_listing_0040%_disassembled.asm
nasm %build_dir_listing_0040%_disassembled.asm
fc /B %build_dir_listing_0040% %build_dir_listing_0040%_disassembled || exit /b 1

set listing_0041=listing_0041_add_sub_cmp_jnz
copy /Y %script_dir%\%listing_0041% %build_dir% 1>NUL
set build_dir_listing_0041=%build_dir%\%listing_0041%
%build_dir%\sim8086.exe %build_dir_listing_0041% > %build_dir_listing_0041%_disassembled.asm
nasm %build_dir_listing_0041%_disassembled.asm
fc /B %build_dir_listing_0041% %build_dir_listing_0041%_disassembled || exit /b 1

set listing_0042=listing_0042_completionist_decode
copy /Y %script_dir%\%listing_0042% %build_dir% 1>NUL
set build_dir_listing_0042=%build_dir%\%listing_0042%
%build_dir%\sim8086.exe %build_dir_listing_0042% > %build_dir_listing_0042%_disassembled.asm
nasm %build_dir_listing_0042%_disassembled.asm
fc /B %build_dir_listing_0042% %build_dir_listing_0042%_disassembled || exit /b 1

set listing_0043=listing_0043_immediate_movs
copy /Y %script_dir%\%listing_0043% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0043%.txt %build_dir% 1>NUL
set build_dir_listing_0043=%build_dir%\%listing_0043%
%build_dir%\sim8086.exe --exec %build_dir_listing_0043% > %build_dir_listing_0043%_disassembled.txt
fc /N %build_dir_listing_0043%.txt %build_dir_listing_0043%_disassembled.txt || exit /b 1

set listing_0044=listing_0044_register_movs
copy /Y %script_dir%\%listing_0044% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0044%.txt %build_dir% 1>NUL
set build_dir_listing_0044=%build_dir%\%listing_0044%
%build_dir%\sim8086.exe --exec %build_dir_listing_0044% > %build_dir_listing_0044%_disassembled.txt
fc /N %build_dir_listing_0044%.txt %build_dir_listing_0044%_disassembled.txt || exit /b 1

set listing_0045=listing_0045_challenge_register_movs
copy /Y %script_dir%\%listing_0045% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0045%.txt %build_dir% 1>NUL
set build_dir_listing_0045=%build_dir%\%listing_0045%
%build_dir%\sim8086.exe --exec %build_dir_listing_0045% > %build_dir_listing_0045%_disassembled.txt
fc /N %build_dir_listing_0045%.txt %build_dir_listing_0045%_disassembled.txt || exit /b 1
set listing_0045=listing_0045_challenge_register_movs

set listing_0046=listing_0046_add_sub_cmp
copy /Y %script_dir%\%listing_0046% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0046%.txt %build_dir% 1>NUL
set build_dir_listing_0046=%build_dir%\%listing_0046%
%build_dir%\sim8086.exe --exec %build_dir_listing_0046% > %build_dir_listing_0046%_disassembled.txt
fc /N %build_dir_listing_0046%.txt %build_dir_listing_0046%_disassembled.txt || exit /b 1
