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
endfunction()