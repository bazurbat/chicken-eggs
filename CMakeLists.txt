cmake_minimum_required(VERSION 2.8)

include(CMakeParseArguments)
include(ExternalProject)

project(chicken-eggs C)

find_package(Chicken REQUIRED)

function(add_chicken_egg name)
    cmake_parse_arguments(extension
        "" "" "DEPENDS" ${ARGN})

    # TODO: need a way to pass system name for cross compilation, but it breaks
    # Darwin systems
    set(args "")
    set(cmake_args MODULE_PATH BUILD_TYPE
        FIND_ROOT_PATH PREFIX_PATH INSTALL_PREFIX
        C_COMPILER C_COMPILER_WORKS)

    foreach(var ${cmake_args})
        if(CMAKE_${var})
            list(APPEND args -DCMAKE_${var}=${CMAKE_${var}})
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

add_chicken_egg(bitstring)

add_chicken_egg(blob-utils
    DEPENDS setup-helper string-utils)

add_chicken_egg(blowfish)

add_chicken_egg(check-errors
    DEPENDS setup-helper)

add_chicken_egg(condition-utils
    DEPENDS setup-helper check-errors)

add_chicken_egg(coops
    DEPENDS matchable record-variants)

add_chicken_egg(crc32
    DEPENDS setup-helper)

add_chicken_egg(dbus
    DEPENDS srfi-18 easyffi matchable miscmacros foreigners)

add_chicken_egg(defstruct)

add_chicken_egg(easyffi
    DEPENDS srfi-13 silex matchable regex make)

add_chicken_egg(feature-test)

add_chicken_egg(filepath
    DEPENDS srfi-13 srfi-14 matchable)

add_chicken_egg(fmt
    DEPENDS srfi-13 srfi-69 utf8)

add_chicken_egg(foreigners
    DEPENDS matchable)

add_chicken_egg(format
    DEPENDS srfi-13)

add_chicken_egg(iset
    DEPENDS srfi-14)

add_chicken_egg(kvlists)

add_chicken_egg(linenoise)

add_chicken_egg(lookup-table
    DEPENDS srfi-69 setup-helper check-errors miscmacros record-variants synch)

add_chicken_egg(lru-cache
    DEPENDS srfi-69 record-variants)

add_chicken_egg(mailbox
    DEPENDS srfi-18 setup-helper check-errors condition-utils record-variants)

add_chicken_egg(mailbox-threads
    DEPENDS srfi-18 mailbox)

add_chicken_egg(make)

add_chicken_egg(matchable)

add_chicken_egg(md5
    DEPENDS message-digest)

add_chicken_egg(message-digest
    DEPENDS srfi-13 setup-helper miscmacros check-errors variable-item
    blob-utils string-utils)

add_chicken_egg(miscmacros)

add_chicken_egg(record-variants)

add_chicken_egg(regex)

add_chicken_egg(setup-api)

add_chicken_egg(setup-helper
    DEPENDS setup-api)

add_chicken_egg(sha1
    DEPENDS message-digest)

add_chicken_egg(shell)

add_chicken_egg(silex
    DEPENDS srfi-13)

add_chicken_egg(socket
    DEPENDS srfi-13 srfi-18 foreigners)

add_chicken_egg(sql-de-lite
    DEPENDS srfi-18 object-evict lru-cache foreigners)

add_chicken_egg(string-utils
    DEPENDS srfi-13 setup-helper miscmacros lookup-table check-errors)

add_chicken_egg(synch
    DEPENDS srfi-18 setup-helper check-errors)

add_chicken_egg(type-stubs)

add_chicken_egg(utf8
    DEPENDS srfi-69 iset regex make)

add_chicken_egg(variable-item
    DEPENDS setup-helper check-errors)

add_chicken_egg(srfi-14)

add_chicken_egg(srfi-13
    DEPENDS srfi-14)

add_chicken_egg(srfi-18)

add_chicken_egg(srfi-69)

add_chicken_egg(object-evict
    DEPENDS srfi-69)

add_chicken_egg(tcp)

if(BUILD_OPTIONALS)

add_chicken_egg(bind
    DEPENDS srfi-13 silex matchable coops regex make)

add_chicken_egg(concurrent-native-callbacks
    DEPENDS srfi-69 typed-records matchable bind miscmacros shell)

add_chicken_egg(monad)

add_chicken_egg(parley
    DEPENDS srfi-13 srfi-14 stty)

add_chicken_egg(scsh-process
    DEPENDS srfi-13 srfi-69)

add_chicken_egg(stty
    DEPENDS srfi-69 setup-helper foreigners)

add_chicken_egg(typed-records
    DEPENDS defstruct type-stubs)

endif(BUILD_OPTIONALS)
