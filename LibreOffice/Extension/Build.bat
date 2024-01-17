echo off
cls

rem ----------------------------------------------------------------------

set libreOfficeJarFilePath="C:\Program Files\LibreOffice\program\classes\libreoffice.jar"
set sevenZipFilePath="C:\Program Files\7-Zip\7z.exe"

rem ----------------------------------------------------------------------

echo 1. Checking prerequisites...

if not exist %libreOfficeJarFilePath% (
  echo ERROR: Could not find %libreOfficeJarFilePath%
  exit /b
)

if not exist %sevenZipFilePath% (
  echo ERROR: Could not find %sevenZipFilePath%
  exit /b
)

rem ----------------------------------------------------------------------

echo 2. Cleaning up temporary directory...

rmdir /s /q Temp\

rem ----------------------------------------------------------------------

echo 3. Preparing directories...

if not exist "Output" mkdir Output
mkdir Temp
mkdir Temp\javac
mkdir Temp\jar
mkdir Temp\zip
mkdir Temp\zip\META-INF
mkdir Temp\zip\Resources

rem ----------------------------------------------------------------------

echo 4. Compiling Java code...

javac ^
  -cp %libreOfficeJarFilePath% ^
  -d  Temp\javac ^
  Sources\Java\Internal\UObject.java ^
  Sources\Java\Internal\UServiceManager.java ^
  Sources\Java\Internal\UMessageBox.java ^
  Sources\Java\Internal\UPropertyValues.java ^
  Sources\Java\Internal\UPropertySet.java ^
  Sources\Java\Internal\UServiceInfo.java ^
  Sources\Java\Internal\UInputStream.java ^
  Sources\Java\Internal\UOutputStream.java ^
  Sources\Java\Internal\UCellRange.java ^
  Sources\Java\Internal\USpreadsheet.java ^
  Sources\Java\Internal\USpreadsheetDocument.java ^
  Sources\Java\Internal\RsvDecoder.java ^
  Sources\Java\Internal\RsvEncoder.java ^
  Sources\Java\Internal\RsvImporterExporter.java ^
  Sources\Java\RsvImportExportFilter.java ^
  Sources\Java\RsvImportExportFilterProvider.java

if %errorlevel% neq 0 exit /b %errorlevel%

rem ----------------------------------------------------------------------

echo 5. Creating JAR file...

jar cfm Temp\jar\StenwayRSVExtension.uno.jar ^
  Sources\JAR\META-INF\MANIFEST.MF ^
  -C Temp\javac ^
  .

if %errorlevel% neq 0 exit /b %errorlevel%

rem ----------------------------------------------------------------------

echo 6. Preparing files to zip...

copy Sources\OXT\META-INF\manifest.xml Temp\zip\META-INF >NUL
if %errorlevel% neq 0 exit /b %errorlevel%

copy Sources\OXT\description.xml Temp\zip >NUL
if %errorlevel% neq 0 exit /b %errorlevel%

copy Temp\jar\StenwayRSVExtension.uno.jar Temp\zip >NUL
if %errorlevel% neq 0 exit /b %errorlevel%

copy Sources\OXT\StenwayRSVExtension.components Temp\zip >NUL
if %errorlevel% neq 0 exit /b %errorlevel%

copy Sources\OXT\Types.xcu Temp\zip >NUL
if %errorlevel% neq 0 exit /b %errorlevel%

copy Sources\OXT\Filters.xcu Temp\zip >NUL
if %errorlevel% neq 0 exit /b %errorlevel%

copy Resources\Description.txt Temp\zip\Resources >NUL
if %errorlevel% neq 0 exit /b %errorlevel%

copy Resources\Icon128.png Temp\zip\Resources >NUL
if %errorlevel% neq 0 exit /b %errorlevel%

rem ----------------------------------------------------------------------

echo 7. Creating Zip file...

%sevenZipFilePath% a "Temp\StenwayRSVExtension.zip" .\Temp\zip\* >NUL

if %errorlevel% neq 0 exit /b %errorlevel%

rem ----------------------------------------------------------------------

echo 8. Copying and renaming Zip file...

copy "Temp\StenwayRSVExtension.zip" "Output\StenwayRSVExtension.oxt" >NUL

if %errorlevel% neq 0 exit /b %errorlevel%

rem ----------------------------------------------------------------------

echo Done

rem ----------------------------------------------------------------------