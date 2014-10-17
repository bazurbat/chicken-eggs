cmake_minimum_required(VERSION 2.8)

include(CMakeParseArguments)
include(ExternalProject)

project(chicken-eggs C)

find_package(Chicken REQUIRED)

function(add_chicken_egg name)
    cmake_parse_arguments(extension
        "" "" "DEPENDS" ${ARGN})

    set(args "")
    set(cmake_args MODULE_PATH BUILD_TYPE SYSTEM_NAME
        FIND_ROOT_PATH PREFIX_PATH INSTALL_PREFIX
        C_COMPILER C_COMPILER_WORKS)

    foreach(var ${cmake_args})
        list(APPEND args -DCMAKE_${var}=${CMAKE_${var}})
    endforeach()

    ExternalProject_Add(${name}
        PREFIX ${name}
        SOURCE_DIR ${PROJECT_SOURCE_DIR}/${name}
        CMAKE_ARGS ${args})

    if(extension_DEPENDS)
        add_dependencies(${name} ${extension_DEPENDS})
    endif()
endfunction()

# add_chicken_egg(bind
#     DEPENDS silex matchable coops regex make)

add_chicken_egg(bitstring)

add_chicken_egg(blob-utils
    DEPENDS setup-helper string-utils)

add_chicken_egg(blowfish)

add_chicken_egg(check-errors
    DEPENDS setup-helper)

# add_chicken_egg(concurrent-native-callbacks
#     DEPENDS typed-records matchable bind miscmacros shell)

add_chicken_egg(condition-utils
    DEPENDS setup-helper check-errors)

add_chicken_egg(coops
    DEPENDS matchable record-variants)

add_chicken_egg(crc32
    DEPENDS setup-helper)

add_chicken_egg(dbus
    DEPENDS easyffi matchable miscmacros foreigners)

add_chicken_egg(defstruct)

add_chicken_egg(easyffi
    DEPENDS srfi-13 silex matchable regex make)

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

# add_chicken_egg(monad)

# add_chicken_egg(parley
#     DEPENDS stty)

add_chicken_egg(record-variants)

add_chicken_egg(regex)

# add_chicken_egg(scsh-process)

add_chicken_egg(setup-helper)

add_chicken_egg(sha1
    DEPENDS message-digest)

add_chicken_egg(shell)

add_chicken_egg(silex)

add_chicken_egg(socket
    DEPENDS srfi-13 foreigners)

add_chicken_egg(sql-de-lite
    DEPENDS srfi-18 lru-cache foreigners)

add_chicken_egg(string-utils
    DEPENDS setup-helper miscmacros lookup-table check-errors)

# add_chicken_egg(stty
#     DEPENDS setup-helper foreigners)

add_chicken_egg(synch
    DEPENDS setup-helper check-errors)

add_chicken_egg(type-stubs)

# add_chicken_egg(typed-records
#     DEPENDS defstruct type-stubs)

add_chicken_egg(utf8
    DEPENDS iset regex make)

add_chicken_egg(variable-item
    DEPENDS setup-helper check-errors)

add_chicken_egg(srfi-14)

add_chicken_egg(srfi-13
    DEPENDS srfi-14)

add_chicken_egg(srfi-18)
