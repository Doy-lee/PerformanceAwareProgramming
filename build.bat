@echo off

REM Setup ==========================================================================================
set script_dir_backslash=%~dp0
set script_dir=%script_dir_backslash:~0,-1%
set build_dir=%script_dir%\Build

set part1_dir=%script_dir%\part1
set part1_build_dir=%build_dir%\part1
if not exist %part1_build_dir% mkdir %part1_build_dir%

set part2_dir=%script_dir%\part2
set part2_build_dir=%build_dir%\part2
if not exist %part2_build_dir% mkdir %part2_build_dir%
goto :part2

REM Part 1 =========================================================================================
REM Build ==========================================================================================
pushd %part1_build_dir%
cl %part1_dir%\sim8086.c /W4 /WX /Z7 /nologo || exit /b 1
popd

REM Tests ==========================================================================================
set listing_0037=listing_0037_single_register_mov
set part1_build_dir_listing_0037=%part1_build_dir%\%listing_0037%

copy /Y %part1_dir%\%listing_0037% %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe %part1_build_dir_listing_0037% > %part1_build_dir_listing_0037%_disassembled.asm
nasm %part1_build_dir_listing_0037%_disassembled.asm
fc /B %part1_build_dir_listing_0037% %part1_build_dir_listing_0037%_disassembled || exit /b 1

REM ================================================================================================
set listing_0038=listing_0038_many_register_mov
set part1_build_dir_listing_0038=%part1_build_dir%\%listing_0038%

copy /Y %part1_dir%\%listing_0038% %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe %part1_build_dir_listing_0038% > %part1_build_dir_listing_0038%_disassembled.asm
nasm %part1_build_dir_listing_0038%_disassembled.asm
fc /B %part1_build_dir_listing_0038% %part1_build_dir_listing_0038%_disassembled || exit /b 1

REM ================================================================================================
set listing_0039=listing_0039_more_movs
set part1_build_dir_listing_0039=%part1_build_dir%\%listing_0039%

copy /Y %part1_dir%\%listing_0039% %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe %part1_build_dir_listing_0039% > %part1_build_dir_listing_0039%_disassembled.asm
nasm %part1_build_dir_listing_0039%_disassembled.asm
fc /B %part1_build_dir_listing_0039% %part1_build_dir_listing_0039%_disassembled || exit /b 1

REM ================================================================================================
set listing_0040=listing_0040_challenge_movs
set part1_build_dir_listing_0040=%part1_build_dir%\%listing_0040%

copy /Y %part1_dir%\%listing_0040% %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe %part1_build_dir_listing_0040% > %part1_build_dir_listing_0040%_disassembled.asm
nasm %part1_build_dir_listing_0040%_disassembled.asm
fc /B %part1_build_dir_listing_0040% %part1_build_dir_listing_0040%_disassembled || exit /b 1

REM ================================================================================================
set listing_0041=listing_0041_add_sub_cmp_jnz
set part1_build_dir_listing_0041=%part1_build_dir%\%listing_0041%

copy /Y %part1_dir%\%listing_0041% %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe %part1_build_dir_listing_0041% > %part1_build_dir_listing_0041%_disassembled.asm
nasm %part1_build_dir_listing_0041%_disassembled.asm
fc /B %part1_build_dir_listing_0041% %part1_build_dir_listing_0041%_disassembled || exit /b 1

REM ================================================================================================
set listing_0042=listing_0042_completionist_decode
set part1_build_dir_listing_0042=%part1_build_dir%\%listing_0042%

copy /Y %part1_dir%\%listing_0042% %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe %part1_build_dir_listing_0042% > %part1_build_dir_listing_0042%_disassembled.asm
nasm %part1_build_dir_listing_0042%_disassembled.asm
fc /B %part1_build_dir_listing_0042% %part1_build_dir_listing_0042%_disassembled || exit /b 1

REM ================================================================================================
set listing_0043=listing_0043_immediate_movs
set part1_build_dir_listing_0043=%part1_build_dir%\%listing_0043%

copy /Y %part1_dir%\%listing_0043% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0043%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe        %part1_build_dir_listing_0043% > %part1_build_dir_listing_0043%_disassembled.asm
%part1_build_dir%\sim8086.exe --exec %part1_build_dir_listing_0043% > %part1_build_dir_listing_0043%_disassembled.txt

nasm %part1_build_dir_listing_0043%_disassembled.asm

fc /B %part1_build_dir_listing_0043%     %part1_build_dir_listing_0043%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0043%.txt %part1_build_dir_listing_0043%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0044=listing_0044_register_movs
set part1_build_dir_listing_0044=%part1_build_dir%\%listing_0044%

copy /Y %part1_dir%\%listing_0044% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0044%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe --exec %part1_build_dir_listing_0044% > %part1_build_dir_listing_0044%_disassembled.txt
%part1_build_dir%\sim8086.exe        %part1_build_dir_listing_0044% > %part1_build_dir_listing_0044%_disassembled.asm

nasm %part1_build_dir_listing_0044%_disassembled.asm

fc /B %part1_build_dir_listing_0044%     %part1_build_dir_listing_0044%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0044%.txt %part1_build_dir_listing_0044%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0045=listing_0045_challenge_register_movs
set part1_build_dir_listing_0045=%part1_build_dir%\%listing_0045%

copy /Y %part1_dir%\%listing_0045% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0045%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe --exec %part1_build_dir_listing_0045% > %part1_build_dir_listing_0045%_disassembled.txt
%part1_build_dir%\sim8086.exe        %part1_build_dir_listing_0045% > %part1_build_dir_listing_0045%_disassembled.asm

nasm %part1_build_dir_listing_0045%_disassembled.asm

fc /B %part1_build_dir_listing_0045%     %part1_build_dir_listing_0045%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0045%.txt %part1_build_dir_listing_0045%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0046=listing_0046_add_sub_cmp
set part1_build_dir_listing_0046=%part1_build_dir%\%listing_0046%

copy /Y %part1_dir%\%listing_0046% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0046%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe --exec %part1_build_dir_listing_0046% > %part1_build_dir_listing_0046%_disassembled.txt
%part1_build_dir%\sim8086.exe        %part1_build_dir_listing_0046% > %part1_build_dir_listing_0046%_disassembled.asm

nasm %part1_build_dir_listing_0046%_disassembled.asm

fc /B %part1_build_dir_listing_0046%     %part1_build_dir_listing_0046%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0046%.txt %part1_build_dir_listing_0046%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0047=listing_0047_challenge_flags
set part1_build_dir_listing_0047=%part1_build_dir%\%listing_0047%

copy /Y %part1_dir%\%listing_0047% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0047%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe --exec %part1_build_dir_listing_0047% > %part1_build_dir_listing_0047%_disassembled.txt
%part1_build_dir%\sim8086.exe        %part1_build_dir_listing_0047% > %part1_build_dir_listing_0047%_disassembled.asm

nasm %part1_build_dir_listing_0047%_disassembled.asm

fc /B %part1_build_dir_listing_0047%     %part1_build_dir_listing_0047%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0047%.txt %part1_build_dir_listing_0047%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0048=listing_0048_ip_register
set part1_build_dir_listing_0048=%part1_build_dir%\%listing_0048%

copy /Y %part1_dir%\%listing_0048% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0048%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr %part1_build_dir_listing_0048% > %part1_build_dir_listing_0048%_disassembled.txt
%part1_build_dir%\sim8086.exe                              %part1_build_dir_listing_0048% > %part1_build_dir_listing_0048%_disassembled.asm

nasm %part1_build_dir_listing_0048%_disassembled.asm

fc /B %part1_build_dir_listing_0048%     %part1_build_dir_listing_0048%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0048%.txt %part1_build_dir_listing_0048%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0049=listing_0049_conditional_jumps
set part1_build_dir_listing_0049=%part1_build_dir%\%listing_0049%

copy /Y %part1_dir%\%listing_0049% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0049%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr %part1_build_dir_listing_0049% > %part1_build_dir_listing_0049%_disassembled.txt
%part1_build_dir%\sim8086.exe                              %part1_build_dir_listing_0049% > %part1_build_dir_listing_0049%_disassembled.asm

nasm %part1_build_dir_listing_0049%_disassembled.asm

fc /B %part1_build_dir_listing_0049%     %part1_build_dir_listing_0049%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0049%.txt %part1_build_dir_listing_0049%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0050=listing_0050_challenge_jumps
set part1_build_dir_listing_0050=%part1_build_dir%\%listing_0050%

copy /Y %part1_dir%\%listing_0050% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0050%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr %part1_build_dir_listing_0050% > %part1_build_dir_listing_0050%_disassembled.txt
%part1_build_dir%\sim8086.exe                              %part1_build_dir_listing_0050% > %part1_build_dir_listing_0050%_disassembled.asm

nasm %part1_build_dir_listing_0050%_disassembled.asm

fc /B %part1_build_dir_listing_0050%     %part1_build_dir_listing_0050%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0050%.txt %part1_build_dir_listing_0050%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0051=listing_0051_memory_mov
set part1_build_dir_listing_0051=%part1_build_dir%\%listing_0051%

copy /Y %part1_dir%\%listing_0051% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0051%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr %part1_build_dir_listing_0051% > %part1_build_dir_listing_0051%_disassembled.txt
%part1_build_dir%\sim8086.exe                              %part1_build_dir_listing_0051% > %part1_build_dir_listing_0051%_disassembled.asm

nasm %part1_build_dir_listing_0051%_disassembled.asm

fc /B %part1_build_dir_listing_0051%     %part1_build_dir_listing_0051%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0051%.txt %part1_build_dir_listing_0051%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0052=listing_0052_memory_add_loop
set part1_build_dir_listing_0052=%part1_build_dir%\%listing_0052%

copy /Y %part1_dir%\%listing_0052% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0052%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr %part1_build_dir_listing_0052% > %part1_build_dir_listing_0052%_disassembled.txt
%part1_build_dir%\sim8086.exe                              %part1_build_dir_listing_0052% > %part1_build_dir_listing_0052%_disassembled.asm

nasm %part1_build_dir_listing_0052%_disassembled.asm

fc /B %part1_build_dir_listing_0052%     %part1_build_dir_listing_0052%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0052%.txt %part1_build_dir_listing_0052%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0053=listing_0053_add_loop_challenge
set part1_build_dir_listing_0053=%part1_build_dir%\%listing_0053%

copy /Y %part1_dir%\%listing_0053% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0053%.txt %part1_build_dir% 1>NUL

%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr %part1_build_dir_listing_0053% > %part1_build_dir_listing_0053%_disassembled.txt
%part1_build_dir%\sim8086.exe                              %part1_build_dir_listing_0053% > %part1_build_dir_listing_0053%_disassembled.asm

nasm %part1_build_dir_listing_0053%_disassembled.asm

fc /B %part1_build_dir_listing_0053%     %part1_build_dir_listing_0053%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0053%.txt %part1_build_dir_listing_0053%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0054=listing_0054_draw_rectangle
set part1_build_dir_listing_0054=%part1_build_dir%\%listing_0054%

copy /Y %part1_dir%\%listing_0054% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0054%.txt %part1_build_dir% 1>NUL

pushd %part1_build_dir%
%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr --dump %part1_build_dir_listing_0054% > %part1_build_dir_listing_0054%_disassembled.txt
%part1_build_dir%\sim8086.exe                                     %part1_build_dir_listing_0054% > %part1_build_dir_listing_0054%_disassembled.asm
popd

nasm %part1_build_dir_listing_0054%_disassembled.asm

fc /B %part1_build_dir_listing_0054%     %part1_build_dir_listing_0054%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0054%.txt %part1_build_dir_listing_0054%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0055=listing_0055_challenge_rectangle
set part1_build_dir_listing_0055=%part1_build_dir%\%listing_0055%

copy /Y %part1_dir%\%listing_0055% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0055%.txt %part1_build_dir% 1>NUL

pushd %part1_build_dir%
%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr --dump %part1_build_dir_listing_0055% > %part1_build_dir_listing_0055%_disassembled.txt
%part1_build_dir%\sim8086.exe                                     %part1_build_dir_listing_0055% > %part1_build_dir_listing_0055%_disassembled.asm
popd

nasm %part1_build_dir_listing_0055%_disassembled.asm

fc /B %part1_build_dir_listing_0055%     %part1_build_dir_listing_0055%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0055%.txt %part1_build_dir_listing_0055%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0056=listing_0056_estimating_cycles
set part1_build_dir_listing_0056=%part1_build_dir%\%listing_0056%

copy /Y %part1_dir%\%listing_0056% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0056%.txt %part1_build_dir% 1>NUL

pushd %part1_build_dir%
%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr --log-cycle-counts 8086 %part1_build_dir_listing_0056%  > %part1_build_dir_listing_0056%_disassembled.txt
%part1_build_dir%\sim8086.exe                                                      %part1_build_dir_listing_0056%  > %part1_build_dir_listing_0056%_disassembled.asm
%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr --log-cycle-counts 8088 %part1_build_dir_listing_0056% >> %part1_build_dir_listing_0056%_disassembled.txt
popd

nasm %part1_build_dir_listing_0056%_disassembled.asm

fc /B %part1_build_dir_listing_0056%     %part1_build_dir_listing_0056%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0056%.txt %part1_build_dir_listing_0056%_disassembled.txt || exit /b 1

REM ================================================================================================
set listing_0057=listing_0057_challenge_cycles
set part1_build_dir_listing_0057=%part1_build_dir%\%listing_0057%

copy /Y %part1_dir%\%listing_0057% %part1_build_dir% 1>NUL
copy /Y %part1_dir%\%listing_0057%.txt %part1_build_dir% 1>NUL

pushd %part1_build_dir%
%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr --log-cycle-counts 8086 %part1_build_dir_listing_0057%  > %part1_build_dir_listing_0057%_disassembled.txt
%part1_build_dir%\sim8086.exe                                                      %part1_build_dir_listing_0057%  > %part1_build_dir_listing_0057%_disassembled.asm
%part1_build_dir%\sim8086.exe --exec --log-instruction-ptr --log-cycle-counts 8088 %part1_build_dir_listing_0057% >> %part1_build_dir_listing_0057%_disassembled.txt
popd

nasm %part1_build_dir_listing_0057%_disassembled.asm

fc /B %part1_build_dir_listing_0057%     %part1_build_dir_listing_0057%_disassembled     || exit /b 1
fc /N %part1_build_dir_listing_0057%.txt %part1_build_dir_listing_0057%_disassembled.txt || exit /b 1

REM Part 2 =========================================================================================
REM Build ==========================================================================================
:part2
pushd %part2_build_dir%
cl %part2_dir%\haversine_generator.c /W4 /WX /Z7 /nologo     /Fe:haversine_generator_debug || exit /b 1
cl %part2_dir%\haversine_generator.c /W4 /WX /Z7 /nologo /O2 /Fe:haversine_generator_release || exit /b 1

cl %part2_dir%\haversine_generator.c /DHAV_PROFILER /W4 /WX /Z7 /nologo     /Fe:haversine_generator_profiled_debug || exit /b 1
cl %part2_dir%\haversine_generator.c /DHAV_PROFILER /W4 /WX /Z7 /nologo /O2 /Fe:haversine_generator_profiled_release || exit /b 1

cl %part2_dir%\haversine.c /W4 /WX /Z7 /nologo     /Fe:haversine_debug || exit /b 1
cl %part2_dir%\haversine.c /W4 /WX /Z7 /nologo /O2 /Fe:haversine_release || exit /b 1

cl %part2_dir%\haversine.c /DHAV_PROFILER /W4 /WX /Z7 /nologo     /Fe:haversine_profiled_debug || exit /b 1
cl %part2_dir%\haversine.c /DHAV_PROFILER /W4 /WX /Z7 /nologo /O2 /Fe:haversine_profiled_release || exit /b 1

cl %part2_dir%\listing_0071_os_timer_main.cpp /W4 /WX /Z7 /O2 /nologo /Fe:listing_0071_os_timer_main_release || exit /b 1
cl %part2_dir%\listing_0072_cpu_timer_main.cpp /W4 /WX /Z7 /O2 /nologo /Fe:listing_0072_cpu_timer_main_release || exit /b 1
cl %part2_dir%\listing_0073_cpu_timer_guessfreq_main.cpp /W4 /WX /Z7 /O2 /nologo /Fe:listing_0073_cpu_timer_guessfreq_release || exit /b 1
popd
