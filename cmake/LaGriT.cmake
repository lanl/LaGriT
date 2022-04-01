macro(lagrit_config_windows target)
    get_target_property(target_type ${target} TYPE)
    add_definitions(-Dwin64)

    if (target_type STREQUAL "EXECUTABLE")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:msvcrtd.lib")
    else()
        set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
    endif()

    #if(MSVC_IDE)
    #    add_compile_options(/MP)
    #    set_property(GLOBAL PROPERTY USE_FOLDERS ON)
    #endif()

    if(MSVC AND NOT (MSVC_VERSION LESS 1900))
        target_link_libraries(${target} "legacy_stdio_definitions.lib")
    endif()

    #set_target_properties(${target}
    #    PROPERTIES
    #        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    #        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
    #        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
    
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS}" "/Qw /Qm64 /QRimplicit-import-")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}" "/Qw /Qm64 /QRimplicit-import-")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Qsafe-cray-ptr -integer-size=64 -QRimplicit-import- /Dwin64")

    add_compile_options(
        $<$<CONFIG:>:/MT>
        $<$<CONFIG:Debug>:/MTd>
        $<$<CONFIG:Release>:/MT>)
endmacro()
