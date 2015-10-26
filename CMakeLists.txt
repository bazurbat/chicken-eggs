cmake_minimum_required(VERSION 2.8)

include(CMakeParseArguments)
include(ExternalProject)

project(chicken-eggs C)

find_package(Chicken REQUIRED)

function(chicken_add_egg name)
    cmake_parse_arguments(extension
        "" "" "DEPENDS" ${ARGN})

    set(args "")
    set(cmake_args MODULE_PATH BUILD_TYPE SYSTEM_NAME
        FIND_ROOT_PATH PREFIX_PATH INSTALL_PREFIX
        C_COMPILER C_COMPILER_WORKS)
    set(chicken_args COMPILER INTERPRETER)

    foreach(var ${cmake_args})
        list(APPEND args -DCMAKE_${var}=${CMAKE_${var}})
    endforeach()
    foreach(var ${chicken_args})
        if(CHICKEN_${var})
            list(APPEND args -DCHICKEN_${var}=${CHICKEN_${var}})
        endif()
    endforeach()

    ExternalProject_Add(${name}
        PREFIX ${name}
        SOURCE_DIR ${PROJECT_SOURCE_DIR}/${name}
        CMAKE_ARGS ${args})

    if(extension_DEPENDS)
        add_dependencies(${name} ${extension_DEPENDS})
    endif()
endfunction()

# chicken_add_egg(bind
#     DEPENDS silex matchable coops regex make)

chicken_add_egg(bitstring)

chicken_add_egg(blob-utils
    DEPENDS setup-helper string-utils)

chicken_add_egg(blowfish)

chicken_add_egg(check-errors
    DEPENDS setup-helper)

# chicken_add_egg(concurrent-native-callbacks
#     DEPENDS typed-records matchable bind miscmacros shell)

chicken_add_egg(condition-utils
    DEPENDS setup-helper check-errors)

chicken_add_egg(coops
    DEPENDS matchable record-variants)

chicken_add_egg(crc32
    DEPENDS setup-helper)

chicken_add_egg(dbus
    DEPENDS easyffi matchable miscmacros foreigners)

chicken_add_egg(defstruct)

chicken_add_egg(easyffi
    DEPENDS silex matchable regex make)

chicken_add_egg(feature-test)

chicken_add_egg(filepath
    DEPENDS matchable)

chicken_add_egg(fmt
    DEPENDS utf8)

chicken_add_egg(foreigners
    DEPENDS matchable)

chicken_add_egg(format)

chicken_add_egg(iset)

chicken_add_egg(kvlists)

chicken_add_egg(linenoise)

chicken_add_egg(lookup-table
    DEPENDS setup-helper check-errors miscmacros record-variants synch)

chicken_add_egg(lru-cache
    DEPENDS record-variants)

chicken_add_egg(mailbox
    DEPENDS setup-helper check-errors condition-utils record-variants)

chicken_add_egg(mailbox-threads
    DEPENDS mailbox)

chicken_add_egg(make)

chicken_add_egg(matchable)

chicken_add_egg(md5
    DEPENDS message-digest)

chicken_add_egg(message-digest
    DEPENDS setup-helper miscmacros check-errors variable-item blob-utils
            string-utils)

chicken_add_egg(miscmacros)

# chicken_add_egg(monad)

# chicken_add_egg(parley
#     DEPENDS stty)

chicken_add_egg(record-variants)

chicken_add_egg(regex)

# chicken_add_egg(scsh-process)

chicken_add_egg(setup-helper)

chicken_add_egg(sha1
    DEPENDS message-digest)

chicken_add_egg(shell)

chicken_add_egg(silex)

chicken_add_egg(socket
    DEPENDS foreigners)

chicken_add_egg(sql-de-lite
    DEPENDS lru-cache foreigners)

chicken_add_egg(string-utils
    DEPENDS setup-helper miscmacros lookup-table check-errors)

# chicken_add_egg(stty
#     DEPENDS setup-helper foreigners)

chicken_add_egg(synch
    DEPENDS setup-helper check-errors)

chicken_add_egg(type-stubs)

# chicken_add_egg(typed-records
#     DEPENDS defstruct type-stubs)

chicken_add_egg(utf8
    DEPENDS iset regex make)

chicken_add_egg(variable-item
    DEPENDS setup-helper check-errors)
