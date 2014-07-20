cmake_minimum_required(VERSION 2.8)

include(CMakeParseArguments)
include(ExternalProject)

project(chicken-eggs C)

find_package(Chicken REQUIRED)

set(CHICKEN_EXTERNAL_EXTENSIONS_SOURCE_DIR
    ${CMAKE_CURRENT_BINARY_DIR} CACHE PATH "")

set(CHICKEN_EXTERNAL_EXTENSIONS_BINARY_DIR
    ${CMAKE_BINARY_DIR} CACHE PATH "")

set(CHICKEN_EXTERNAL_EXTENSIONS_OVERRIDE_DIR
    ${CMAKE_CURRENT_SOURCE_DIR} CACHE PATH "")

function(_add_patch_step name patch)
    set(patch_dir ${CHICKEN_EXTERNAL_EXTENSIONS_OVERRIDE_DIR}/${name}/patches)
    set(patch_file ${patch_dir}/${patch}.patch)

    ExternalProject_Add_Step(${name} apply_${patch}
        COMMAND patch -p1 -i ${patch_file}
        DEPENDEES download DEPENDERS update
        WORKING_DIRECTORY ${CHICKEN_EXTERNAL_EXTENSIONS_SOURCE_DIR}/${name})
endfunction()

function(add_external_chicken_extension name)
    cmake_parse_arguments(extension
        "REQUIRED"
        "VERSION"
        "DEPENDS;PATCH"
        ${ARGN})

    set(args)
    set(cmake_args MODULE_PATH BUILD_TYPE SYSTEM_NAME
        FIND_ROOT_PATH PREFIX_PATH INSTALL_PREFIX
        C_COMPILER C_COMPILER_WORKS)
    foreach(var ${cmake_args})
        list(APPEND args -DCMAKE_${var}=${CMAKE_${var}})
    endforeach()

    set(cache_args)
    set(chicken_cache_vars
        API_VERSION
        EMIT_TYPES EMIT_INLINES
        SYSTEM HOST_SYSTEM TARGET_SYSTEM
        EXTERNAL_EXTENSIONS_SOURCE_DIR
        EXTERNAL_EXTENSIONS_BINARY_DIR
        EXTERNAL_EXTENSIONS_OVERRIDE_DIR
        EXECUTABLE CSI_EXECUTABLE INSTALL_EXECUTABLE
        INCLUDE_DIR LIBRARY STATIC_LIBRARY
        C_FLAGS)
    foreach(var ${chicken_cache_vars})
        get_property(type CACHE CHICKEN_${var} PROPERTY TYPE)
        get_property(value CACHE CHICKEN_${var} PROPERTY VALUE)
        if(type AND value)
            list(APPEND cache_args -DCHICKEN_${var}:${type}=${value})
        elseif(NOT type)
            message(WARNING "No variable: ${var}")
        endif()
    endforeach()

    set(source_dir ${CHICKEN_EXTERNAL_EXTENSIONS_SOURCE_DIR}/${name})

    if(EXISTS ${source_dir}/${name}.scm OR EXISTS ${source_dir}/stamp)
        set(download_command)
    else()
        set(download_command ${CHICKEN_INSTALL_EXECUTABLE} -r ${name})
    endif()

    ExternalProject_Add(${name}
        PREFIX ${CHICKEN_EXTERNAL_EXTENSIONS_BINARY_DIR}/${name}
        DOWNLOAD_COMMAND ${download_command}
        DOWNLOAD_DIR ${CHICKEN_EXTERNAL_EXTENSIONS_SOURCE_DIR}
        UPDATE_COMMAND ${CMAKE_COMMAND} -E touch ${source_dir}/stamp
        SOURCE_DIR ${source_dir}
        CMAKE_ARGS ${args}
        CMAKE_CACHE_ARGS ${cache_args})

    if(NOT EXISTS ${CHICKEN_EXTERNAL_EXTENSIONS_OVERRIDE_DIR})
        message(FATAL_ERROR "Chicken external extensions override dir is not set")
    endif()

    if(NOT EXISTS ${source_dir}/stamp)
        foreach(p ${extension_PATCH})
            _add_patch_step(${name} ${p})
        endforeach()
    endif()

    ExternalProject_Add_Step(${name} copy_cmakelists
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CHICKEN_EXTERNAL_EXTENSIONS_OVERRIDE_DIR}/${name}/CMakeLists.txt
            ${CHICKEN_EXTERNAL_EXTENSIONS_SOURCE_DIR}/${name}
        DEPENDS ${CHICKEN_EXTERNAL_EXTENSIONS_OVERRIDE_DIR}/${name}/CMakeLists.txt
        DEPENDEES download DEPENDERS configure)
    ExternalProject_Add_Step(${name} copy_config
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CHICKEN_EXTERNAL_EXTENSIONS_OVERRIDE_DIR}/ChickenExtensionConfig.cmake.in
            ${CHICKEN_EXTERNAL_EXTENSIONS_SOURCE_DIR}/${name}
        DEPENDS ${CHICKEN_EXTERNAL_EXTENSIONS_OVERRIDE_DIR}/ChickenExtensionConfig.cmake.in
        DEPENDEES download DEPENDERS configure)
    ExternalProject_Add_Step(${name} copy_version
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CHICKEN_EXTERNAL_EXTENSIONS_OVERRIDE_DIR}/ChickenExtensionVersion.cmake.in
            ${CHICKEN_EXTERNAL_EXTENSIONS_SOURCE_DIR}/${name}
        DEPENDS ${CHICKEN_EXTERNAL_EXTENSIONS_OVERRIDE_DIR}/ChickenExtensionVersion.cmake.in
        DEPENDEES download DEPENDERS configure)

    if(extension_DEPENDS)
        add_dependencies(${name} ${extension_DEPENDS})
    endif()
endfunction()

add_external_chicken_extension(bind
    DEPENDS silex matchable coops regex make)

add_external_chicken_extension(bitstring)

add_external_chicken_extension(blob-utils
    DEPENDS setup-helper string-utils)

add_external_chicken_extension(blowfish)

add_external_chicken_extension(check-errors
    DEPENDS setup-helper)

add_external_chicken_extension(concurrent-native-callbacks
    DEPENDS typed-records matchable bind miscmacros shell)

add_external_chicken_extension(condition-utils
    DEPENDS setup-helper check-errors)

add_external_chicken_extension(coops
    DEPENDS matchable record-variants)

add_external_chicken_extension(crc32
    DEPENDS setup-helper)

add_external_chicken_extension(dbus
    PATCH tassq
    DEPENDS easyffi matchable miscmacros foreigners)

add_external_chicken_extension(defstruct)

add_external_chicken_extension(easyffi
    DEPENDS silex matchable regex make)

add_external_chicken_extension(feature-test)

add_external_chicken_extension(filepath
    DEPENDS matchable)

add_external_chicken_extension(fmt
    DEPENDS utf8)

add_external_chicken_extension(foreigners
    DEPENDS matchable)

add_external_chicken_extension(format)

add_external_chicken_extension(iset)

add_external_chicken_extension(kvlists)

add_external_chicken_extension(linenoise)

add_external_chicken_extension(lookup-table
    DEPENDS setup-helper check-errors miscmacros record-variants synch)

add_external_chicken_extension(lru-cache
    DEPENDS record-variants)

add_external_chicken_extension(mailbox
    DEPENDS setup-helper check-errors condition-utils record-variants)

add_external_chicken_extension(mailbox-threads
    DEPENDS mailbox)

add_external_chicken_extension(make)

add_external_chicken_extension(matchable)

add_external_chicken_extension(md5
    DEPENDS message-digest)

add_external_chicken_extension(message-digest
    DEPENDS setup-helper miscmacros check-errors variable-item blob-utils
            string-utils)

add_external_chicken_extension(miscmacros)

add_external_chicken_extension(monad)

# add_external_chicken_extension(parley
#     DEPENDS stty)

add_external_chicken_extension(record-variants)

add_external_chicken_extension(regex)

add_external_chicken_extension(scsh-process)

add_external_chicken_extension(setup-helper)

add_external_chicken_extension(sha1
    DEPENDS message-digest)

add_external_chicken_extension(shell)

add_external_chicken_extension(silex)

add_external_chicken_extension(socket
    DEPENDS foreigners)

add_external_chicken_extension(sql-de-lite
    DEPENDS lru-cache foreigners)

add_external_chicken_extension(string-utils
    DEPENDS setup-helper miscmacros lookup-table check-errors)

# add_external_chicken_extension(stty
#     DEPENDS setup-helper foreigners)

add_external_chicken_extension(synch
    DEPENDS setup-helper check-errors)

add_external_chicken_extension(type-stubs)

add_external_chicken_extension(typed-records
    DEPENDS defstruct type-stubs)

add_external_chicken_extension(utf8
    DEPENDS iset regex make)

add_external_chicken_extension(variable-item
    DEPENDS setup-helper check-errors)
