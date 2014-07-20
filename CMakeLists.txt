cmake_minimum_required(VERSION 2.8)

include(CMakeParseArguments)
include(ExternalProject)

project(chicken-eggs C)

find_package(Chicken REQUIRED)

set(CHICKEN_DOWNLOAD_DIR ${CHICKEN_TMP_DIR}/eggs CACHE PATH "")

function(_add_patch_step name patch)
    set(patch_dir ${PROJECT_SOURCE_DIR}/${name}/patches)
    set(patch_file ${patch_dir}/${patch}.patch)

    ExternalProject_Add_Step(${name} apply_${patch}
        COMMAND patch -p1 -i ${patch_file}
        DEPENDEES download DEPENDERS update
        WORKING_DIRECTORY ${CHICKEN_DOWNLOAD_DIR}/${name})
endfunction()

function(add_chicken_egg name)
    cmake_parse_arguments(extension
        "" "" "DEPENDS;PATCH" ${ARGN})

    set(ARGS "")
    set(CACHE_ARGS "")
    set(EGG_PROJECT_DIR ${PROJECT_SOURCE_DIR}/${name})
    set(SOURCE_DIR ${CHICKEN_DOWNLOAD_DIR}/${name})

    set(cmake_args MODULE_PATH BUILD_TYPE SYSTEM_NAME
        FIND_ROOT_PATH PREFIX_PATH INSTALL_PREFIX
        C_COMPILER C_COMPILER_WORKS)

    foreach(var ${cmake_args})
        list(APPEND ARGS -DCMAKE_${var}=${CMAKE_${var}})
    endforeach()

    if(EXISTS ${SOURCE_DIR}/${name}.scm OR EXISTS ${SOURCE_DIR}/stamp)
        set(DOWNLOAD_COMMAND)
    else()
        set(DOWNLOAD_COMMAND ${CHICKEN_INSTALL_EXECUTABLE} -r ${name})
    endif()

    ExternalProject_Add(${name}
        PREFIX ${name}
        DOWNLOAD_COMMAND ${DOWNLOAD_COMMAND}
        DOWNLOAD_DIR ${CHICKEN_DOWNLOAD_DIR}
        UPDATE_COMMAND ${CMAKE_COMMAND} -E touch ${SOURCE_DIR}/stamp
        SOURCE_DIR ${SOURCE_DIR}
        CMAKE_ARGS ${ARGS})

    if(NOT EXISTS ${SOURCE_DIR}/stamp)
        foreach(p ${extension_PATCH})
            _add_patch_step(${name} ${p})
        endforeach()
    endif()

    ExternalProject_Add_Step(${name} copy_cmakelists
        COMMAND ${CMAKE_COMMAND} -E copy
            ${EGG_PROJECT_DIR}/CMakeLists.txt ${SOURCE_DIR}
        DEPENDS ${EGG_PROJECT_DIR}/CMakeLists.txt
        DEPENDEES download DEPENDERS configure)

    ExternalProject_Add_Step(${name} copy_config
        COMMAND ${CMAKE_COMMAND} -E copy
            ${PROJECT_SOURCE_DIR}/ChickenExtensionConfig.cmake.in
            ${SOURCE_DIR}
        DEPENDS ${PROJECT_SOURCE_DIR}/ChickenExtensionConfig.cmake.in
        DEPENDEES download DEPENDERS configure)

    ExternalProject_Add_Step(${name} copy_version
        COMMAND ${CMAKE_COMMAND} -E copy
            ${PROJECT_SOURCE_DIR}/ChickenExtensionVersion.cmake.in
            ${SOURCE_DIR}
        DEPENDS ${PROJECT_SOURCE_DIR}/ChickenExtensionVersion.cmake.in
        DEPENDEES download DEPENDERS configure)

    if(extension_DEPENDS)
        add_dependencies(${name} ${extension_DEPENDS})
    endif()
endfunction()

add_chicken_egg(bind
    DEPENDS silex matchable coops regex make)

add_chicken_egg(bitstring)

add_chicken_egg(blob-utils
    DEPENDS setup-helper string-utils)

add_chicken_egg(blowfish)

add_chicken_egg(check-errors
    DEPENDS setup-helper)

add_chicken_egg(concurrent-native-callbacks
    DEPENDS typed-records matchable bind miscmacros shell)

add_chicken_egg(condition-utils
    DEPENDS setup-helper check-errors)

add_chicken_egg(coops
    DEPENDS matchable record-variants)

# add_chicken_egg(crc32
#     DEPENDS setup-helper)

add_chicken_egg(dbus
    PATCH tassq
    DEPENDS easyffi matchable miscmacros foreigners)

add_chicken_egg(defstruct)

add_chicken_egg(easyffi
    DEPENDS silex matchable regex make)

add_chicken_egg(feature-test)

add_chicken_egg(filepath
    DEPENDS matchable)

add_chicken_egg(fmt
    DEPENDS utf8)

add_chicken_egg(foreigners
    DEPENDS matchable)

add_chicken_egg(format)

add_chicken_egg(iset)

add_chicken_egg(kvlists)

add_chicken_egg(linenoise)

add_chicken_egg(lookup-table
    DEPENDS setup-helper check-errors miscmacros record-variants synch)

add_chicken_egg(lru-cache
    DEPENDS record-variants)

add_chicken_egg(mailbox
    DEPENDS setup-helper check-errors condition-utils record-variants)

add_chicken_egg(mailbox-threads
    DEPENDS mailbox)

add_chicken_egg(make)

add_chicken_egg(matchable)

add_chicken_egg(md5
    DEPENDS message-digest)

add_chicken_egg(message-digest
    DEPENDS setup-helper miscmacros check-errors variable-item blob-utils
            string-utils)

add_chicken_egg(miscmacros)

add_chicken_egg(monad)

add_chicken_egg(parley
    DEPENDS stty)

add_chicken_egg(record-variants)

add_chicken_egg(regex)

add_chicken_egg(scsh-process)

add_chicken_egg(setup-helper)

add_chicken_egg(sha1
    DEPENDS message-digest)

add_chicken_egg(shell)

add_chicken_egg(silex)

add_chicken_egg(socket
    DEPENDS foreigners)

add_chicken_egg(sql-de-lite
    DEPENDS lru-cache foreigners)

add_chicken_egg(string-utils
    DEPENDS setup-helper miscmacros lookup-table check-errors)

add_chicken_egg(stty
    DEPENDS setup-helper foreigners)

add_chicken_egg(synch
    DEPENDS setup-helper check-errors)

add_chicken_egg(type-stubs)

add_chicken_egg(typed-records
    DEPENDS defstruct type-stubs)

add_chicken_egg(utf8
    DEPENDS iset regex make)

add_chicken_egg(variable-item
    DEPENDS setup-helper check-errors)
