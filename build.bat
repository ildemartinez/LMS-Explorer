@echo off
if exist "Win32" ( 
	echo Borrando directorio Win32
	rmdir Win32 /s/ q 
)

if exist "Win64" ( 
	echo Borrando directorio Win32
	rmdir Win64 /s/ q
)

echo Estableciendo variables de entorno
call "c:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat"

echo Compilando
msbuild /t:clean /t:build /p:Config=Release;Platform=Win32 LMSExplorer.dproj

if exist .\Win32\Release\lmsexplorer.exe (goto L1)
echo "Error compiling"
pause
exit

:L1
echo --------------------
echo Comprimiendo 
echo --------------------
upx.exe Win32\Release\....exe
cd Win32\Release

echo --------------------
echo Zipping
echo --------------------
"C:\Program Files\7-Zip\7z.exe" a lmsexplorer.zip lmsexplorer.exe ..\..\config.ini_dist

pause

