function(lagrit_config_windows name)
    add_definitions(-Dwin64)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

    if(MSVC_IDE)
        add_compile_options(/MP)
        set_property(GLOBAL PROPERTY USE_FOLDERS ON)
    endif()

    set_target_properties(${name}
        PROPERTIES
            ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
            LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
            RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
    
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS}" "/Qw /Qm64 /QRimplicit-import-")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}" "/Qw /Qm64 /QRimplicit-import-")

    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}" "/Dwin64")

    set(CMAKE_EXE_LINKER_FLAGS
        "${CMAKE_EXE_LINKER_FLAGS}"
        "/NODEFAULTLIB:msvcrtd.lib")

    add_compile_options(
        $<$<CONFIG:>:/MT>
        $<$<CONFIG:Debug>:/MTd>
        $<$<CONFIG:Release>:/MT>)
endfunction()