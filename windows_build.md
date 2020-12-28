LaGriT CMake build on Windows
=============================

* Open `cmd.exe` and navigate to LaGriT/ dir
* Run:

```
cmake -G"NMake Makefiles" -B"build" -D CMAKE_C_COMPILER=icx -D CMAKE_CXX_COMPILER=icx
```

* Run:

```
cd build/
nmake
```

C:\Users\danie\Documents\GitHub\LaGriT\build>cmake -G"NMake Makefiles" -B"build" -D CMAKE_C_COMPILER=icx -D CMAKE_CXX_COMPILER=icx


file:///C:/Program%20Files%20(x86)/IntelSWTools/documentation_2020/en/ps2020/getstart_clus.htm

0. Open developer `cmd.exe` via `Visual Studio -> Tools -> Command Line -> Developer Command Prompt`

1. `cd` into:

    C:\Program Files (x86)\IntelSWTools\parallel_studio_xe_2020.4.912\bin>

2. Run:

    psxevars.bat intel64

3. `cd` into:

    C:\Users\livingston\Desktop (WIN)\LaGriT\

4. Run

    cmake -G"NMake Makefiles" -B"build" -D CMAKE_C_COMPILER=icx -D CMAKE_CXX_COMPILER=icx

5. Then, `cd build\` and:

    nmake