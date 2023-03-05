@echo off
set script_dir_backslash=%~dp0
set script_dir=%script_dir_backslash:~0,-1%
set build_dir=%script_dir%\build
if not exist %build_dir% mkdir %build_dir%

pushd build

copy /Y %script_dir%\listing_0037_single_register_mov %build_dir% 1>NUL
copy /Y %script_dir%\listing_0038_many_register_mov %build_dir% 1>NUL
copy /Y %script_dir%\listing_0039_more_movs %build_dir% 1>NUL
copy /Y %script_dir%\listing_0040_challenge_movs %build_dir% 1>NUL

cl %script_dir%\sim8086.c /W4 /WX /Z7 /nologo

popd
