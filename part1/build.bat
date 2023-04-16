@echo off

REM Setup ==========================================================================================
set script_dir_backslash=%~dp0
set script_dir=%script_dir_backslash:~0,-1%
set build_dir=%script_dir%\Build
if not exist %build_dir% mkdir %build_dir%

REM Build ==========================================================================================
pushd %build_dir%
cl %script_dir%\sim8086.c /W4 /WX /Z7 /nologo || exit /b 1
popd

REM Tests ==========================================================================================
set listing_0037=listing_0037_single_register_mov
set build_dir_listing_0037=%build_dir%\%listing_0037%

copy /Y %script_dir%\%listing_0037% %build_dir% 1>NUL

%build_dir%\sim8086.exe %build_dir_listing_0037% > %build_dir_listing_0037%_disassembled.asm
nasm %build_dir_listing_0037%_disassembled.asm
fc /B %build_dir_listing_0037% %build_dir_listing_0037%_disassembled || exit /b 1

REM ================================================================================================
set listing_0038=listing_0038_many_register_mov
set build_dir_listing_0038=%build_dir%\%listing_0038%

copy /Y %script_dir%\%listing_0038% %build_dir% 1>NUL

%build_dir%\sim8086.exe %build_dir_listing_0038% > %build_dir_listing_0038%_disassembled.asm
nasm %build_dir_listing_0038%_disassembled.asm
fc /B %build_dir_listing_0038% %build_dir_listing_0038%_disassembled || exit /b 1

REM ================================================================================================
set listing_0039=listing_0039_more_movs
set build_dir_listing_0039=%build_dir%\%listing_0039%

copy /Y %script_dir%\%listing_0039% %build_dir% 1>NUL

%build_dir%\sim8086.exe %build_dir_listing_0039% > %build_dir_listing_0039%_disassembled.asm
nasm %build_dir_listing_0039%_disassembled.asm
fc /B %build_dir_listing_0039% %build_dir_listing_0039%_disassembled || exit /b 1

REM ================================================================================================
set listing_0040=listing_0040_challenge_movs
set build_dir_listing_0040=%build_dir%\%listing_0040%

copy /Y %script_dir%\%listing_0040% %build_dir% 1>NUL

%build_dir%\sim8086.exe %build_dir_listing_0040% > %build_dir_listing_0040%_disassembled.asm
nasm %build_dir_listing_0040%_disassembled.asm
fc /B %build_dir_listing_0040% %build_dir_listing_0040%_disassembled || exit /b 1

REM ================================================================================================
set listing_0041=listing_0041_add_sub_cmp_jnz
set build_dir_listing_0041=%build_dir%\%listing_0041%

copy /Y %script_dir%\%listing_0041% %build_dir% 1>NUL

%build_dir%\sim8086.exe %build_dir_listing_0041% > %build_dir_listing_0041%_disassembled.asm
nasm %build_dir_listing_0041%_disassembled.asm
fc /B %build_dir_listing_0041% %build_dir_listing_0041%_disassembled || exit /b 1

REM ================================================================================================
set listing_0042=listing_0042_completionist_decode
set build_dir_listing_0042=%build_dir%\%listing_0042%

copy /Y %script_dir%\%listing_0042% %build_dir% 1>NUL

%build_dir%\sim8086.exe %build_dir_listing_0042% > %build_dir_listing_0042%_disassembled.asm
nasm %build_dir_listing_0042%_disassembled.asm
fc /B %build_dir_listing_0042% %build_dir_listing_0042%_disassembled || exit /b 1

REM ================================================================================================
set listing_0043=listing_0043_immediate_movs
set build_dir_listing_0043=%build_dir%\%listing_0043%

copy /Y %script_dir%\%listing_0043% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0043%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe        %build_dir_listing_0043% > %build_dir_listing_0043%_disassembled.asm
%build_dir%\sim8086.exe --exec %build_dir_listing_0043% > %build_dir_listing_0043%_disassembled.txt

nasm %build_dir_listing_0043%_disassembled.asm

fc /B %build_dir_listing_0043%     %build_dir_listing_0043%_disassembled     || exit /b 1
fc /N %build_dir_listing_0043%.txt %build_dir_listing_0043%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0044=listing_0044_register_movs
set build_dir_listing_0044=%build_dir%\%listing_0044%

copy /Y %script_dir%\%listing_0044% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0044%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe --exec %build_dir_listing_0044% > %build_dir_listing_0044%_disassembled.txt
%build_dir%\sim8086.exe        %build_dir_listing_0044% > %build_dir_listing_0044%_disassembled.asm

nasm %build_dir_listing_0044%_disassembled.asm

fc /B %build_dir_listing_0044%     %build_dir_listing_0044%_disassembled     || exit /b 1
fc /N %build_dir_listing_0044%.txt %build_dir_listing_0044%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0045=listing_0045_challenge_register_movs
set build_dir_listing_0045=%build_dir%\%listing_0045%

copy /Y %script_dir%\%listing_0045% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0045%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe --exec %build_dir_listing_0045% > %build_dir_listing_0045%_disassembled.txt
%build_dir%\sim8086.exe        %build_dir_listing_0045% > %build_dir_listing_0045%_disassembled.asm

nasm %build_dir_listing_0045%_disassembled.asm

fc /B %build_dir_listing_0045%     %build_dir_listing_0045%_disassembled     || exit /b 1
fc /N %build_dir_listing_0045%.txt %build_dir_listing_0045%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0046=listing_0046_add_sub_cmp
set build_dir_listing_0046=%build_dir%\%listing_0046%

copy /Y %script_dir%\%listing_0046% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0046%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe --exec %build_dir_listing_0046% > %build_dir_listing_0046%_disassembled.txt
%build_dir%\sim8086.exe        %build_dir_listing_0046% > %build_dir_listing_0046%_disassembled.asm

nasm %build_dir_listing_0046%_disassembled.asm

fc /B %build_dir_listing_0046%     %build_dir_listing_0046%_disassembled     || exit /b 1
fc /N %build_dir_listing_0046%.txt %build_dir_listing_0046%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0047=listing_0047_challenge_flags
set build_dir_listing_0047=%build_dir%\%listing_0047%

copy /Y %script_dir%\%listing_0047% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0047%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe --exec %build_dir_listing_0047% > %build_dir_listing_0047%_disassembled.txt
%build_dir%\sim8086.exe        %build_dir_listing_0047% > %build_dir_listing_0047%_disassembled.asm

nasm %build_dir_listing_0047%_disassembled.asm

fc /B %build_dir_listing_0047%     %build_dir_listing_0047%_disassembled     || exit /b 1
fc /N %build_dir_listing_0047%.txt %build_dir_listing_0047%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0048=listing_0048_ip_register
set build_dir_listing_0048=%build_dir%\%listing_0048%

copy /Y %script_dir%\%listing_0048% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0048%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe --exec %build_dir_listing_0048% > %build_dir_listing_0048%_disassembled.txt
%build_dir%\sim8086.exe        %build_dir_listing_0048% > %build_dir_listing_0048%_disassembled.asm

nasm %build_dir_listing_0048%_disassembled.asm

fc /B %build_dir_listing_0048%     %build_dir_listing_0048%_disassembled     || exit /b 1
fc /N %build_dir_listing_0048%.txt %build_dir_listing_0048%_disassembled.txt || exit /b 1
