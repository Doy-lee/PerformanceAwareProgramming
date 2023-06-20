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

%build_dir%\sim8086.exe --exec --log-instruction-ptr %build_dir_listing_0048% > %build_dir_listing_0048%_disassembled.txt
%build_dir%\sim8086.exe                              %build_dir_listing_0048% > %build_dir_listing_0048%_disassembled.asm

nasm %build_dir_listing_0048%_disassembled.asm

fc /B %build_dir_listing_0048%     %build_dir_listing_0048%_disassembled     || exit /b 1
fc /N %build_dir_listing_0048%.txt %build_dir_listing_0048%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0049=listing_0049_conditional_jumps
set build_dir_listing_0049=%build_dir%\%listing_0049%

copy /Y %script_dir%\%listing_0049% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0049%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe --exec --log-instruction-ptr %build_dir_listing_0049% > %build_dir_listing_0049%_disassembled.txt
%build_dir%\sim8086.exe                              %build_dir_listing_0049% > %build_dir_listing_0049%_disassembled.asm

nasm %build_dir_listing_0049%_disassembled.asm

fc /B %build_dir_listing_0049%     %build_dir_listing_0049%_disassembled     || exit /b 1
fc /N %build_dir_listing_0049%.txt %build_dir_listing_0049%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0050=listing_0050_challenge_jumps
set build_dir_listing_0050=%build_dir%\%listing_0050%

copy /Y %script_dir%\%listing_0050% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0050%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe --exec --log-instruction-ptr %build_dir_listing_0050% > %build_dir_listing_0050%_disassembled.txt
%build_dir%\sim8086.exe                              %build_dir_listing_0050% > %build_dir_listing_0050%_disassembled.asm

nasm %build_dir_listing_0050%_disassembled.asm

fc /B %build_dir_listing_0050%     %build_dir_listing_0050%_disassembled     || exit /b 1
fc /N %build_dir_listing_0050%.txt %build_dir_listing_0050%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0051=listing_0051_memory_mov
set build_dir_listing_0051=%build_dir%\%listing_0051%

copy /Y %script_dir%\%listing_0051% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0051%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe --exec --log-instruction-ptr %build_dir_listing_0051% > %build_dir_listing_0051%_disassembled.txt
%build_dir%\sim8086.exe                              %build_dir_listing_0051% > %build_dir_listing_0051%_disassembled.asm

nasm %build_dir_listing_0051%_disassembled.asm

fc /B %build_dir_listing_0051%     %build_dir_listing_0051%_disassembled     || exit /b 1
fc /N %build_dir_listing_0051%.txt %build_dir_listing_0051%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0052=listing_0052_memory_add_loop
set build_dir_listing_0052=%build_dir%\%listing_0052%

copy /Y %script_dir%\%listing_0052% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0052%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe --exec --log-instruction-ptr %build_dir_listing_0052% > %build_dir_listing_0052%_disassembled.txt
%build_dir%\sim8086.exe                              %build_dir_listing_0052% > %build_dir_listing_0052%_disassembled.asm

nasm %build_dir_listing_0052%_disassembled.asm

fc /B %build_dir_listing_0052%     %build_dir_listing_0052%_disassembled     || exit /b 1
fc /N %build_dir_listing_0052%.txt %build_dir_listing_0052%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0053=listing_0053_add_loop_challenge
set build_dir_listing_0053=%build_dir%\%listing_0053%

copy /Y %script_dir%\%listing_0053% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0053%.txt %build_dir% 1>NUL

%build_dir%\sim8086.exe --exec --log-instruction-ptr %build_dir_listing_0053% > %build_dir_listing_0053%_disassembled.txt
%build_dir%\sim8086.exe                              %build_dir_listing_0053% > %build_dir_listing_0053%_disassembled.asm

nasm %build_dir_listing_0053%_disassembled.asm

fc /B %build_dir_listing_0053%     %build_dir_listing_0053%_disassembled     || exit /b 1
fc /N %build_dir_listing_0053%.txt %build_dir_listing_0053%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0054=listing_0054_draw_rectangle
set build_dir_listing_0054=%build_dir%\%listing_0054%

copy /Y %script_dir%\%listing_0054% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0054%.txt %build_dir% 1>NUL

pushd %build_dir%
%build_dir%\sim8086.exe --exec --log-instruction-ptr --dump %build_dir_listing_0054% > %build_dir_listing_0054%_disassembled.txt
%build_dir%\sim8086.exe                                     %build_dir_listing_0054% > %build_dir_listing_0054%_disassembled.asm
popd

nasm %build_dir_listing_0054%_disassembled.asm

fc /B %build_dir_listing_0054%     %build_dir_listing_0054%_disassembled     || exit /b 1
fc /N %build_dir_listing_0054%.txt %build_dir_listing_0054%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0055=listing_0055_challenge_rectangle
set build_dir_listing_0055=%build_dir%\%listing_0055%

copy /Y %script_dir%\%listing_0055% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0055%.txt %build_dir% 1>NUL

pushd %build_dir%
%build_dir%\sim8086.exe --exec --log-instruction-ptr --dump %build_dir_listing_0055% > %build_dir_listing_0055%_disassembled.txt
%build_dir%\sim8086.exe                                     %build_dir_listing_0055% > %build_dir_listing_0055%_disassembled.asm
popd

nasm %build_dir_listing_0055%_disassembled.asm

fc /B %build_dir_listing_0055%     %build_dir_listing_0055%_disassembled     || exit /b 1
fc /N %build_dir_listing_0055%.txt %build_dir_listing_0055%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0056=listing_0056_estimating_cycles
set build_dir_listing_0056=%build_dir%\%listing_0056%

copy /Y %script_dir%\%listing_0056% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0056%.txt %build_dir% 1>NUL

pushd %build_dir%
%build_dir%\sim8086.exe --exec --log-instruction-ptr --log-cycle-counts 8086 %build_dir_listing_0056%  > %build_dir_listing_0056%_disassembled.txt
%build_dir%\sim8086.exe                                                      %build_dir_listing_0056%  > %build_dir_listing_0056%_disassembled.asm
%build_dir%\sim8086.exe --exec --log-instruction-ptr --log-cycle-counts 8088 %build_dir_listing_0056% >> %build_dir_listing_0056%_disassembled.txt
popd

nasm %build_dir_listing_0056%_disassembled.asm

fc /B %build_dir_listing_0056%     %build_dir_listing_0056%_disassembled     || exit /b 1
fc /N %build_dir_listing_0056%.txt %build_dir_listing_0056%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0057=listing_0057_challenge_cycles
set build_dir_listing_0057=%build_dir%\%listing_0057%

copy /Y %script_dir%\%listing_0057% %build_dir% 1>NUL
copy /Y %script_dir%\%listing_0057%.txt %build_dir% 1>NUL

pushd %build_dir%
%build_dir%\sim8086.exe --exec --log-instruction-ptr --log-cycle-counts 8086 %build_dir_listing_0057%  > %build_dir_listing_0057%_disassembled.txt
%build_dir%\sim8086.exe                                                      %build_dir_listing_0057%  > %build_dir_listing_0057%_disassembled.asm
%build_dir%\sim8086.exe --exec --log-instruction-ptr --log-cycle-counts 8088 %build_dir_listing_0057% >> %build_dir_listing_0057%_disassembled.txt
popd

nasm %build_dir_listing_0057%_disassembled.asm

fc /B %build_dir_listing_0057%     %build_dir_listing_0057%_disassembled     || exit /b 1
fc /N %build_dir_listing_0057%.txt %build_dir_listing_0057%_disassembled.txt || exit /b 1
