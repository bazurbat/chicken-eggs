cmake_minimum_required(VERSION 2.8)

project(message-digest C)

find_package(Chicken REQUIRED)
find_chicken_extension(srfi-13 REQUIRED)
find_chicken_extension(setup-helper  1.5.2 REQUIRED)
find_chicken_extension(miscmacros    2.91  REQUIRED)
find_chicken_extension(check-errors  1.9.0 REQUIRED)
find_chicken_extension(variable-item 1.3.0 REQUIRED)
find_chicken_extension(blob-utils    1.0.0 REQUIRED)
find_chicken_extension(string-utils  1.2.1 REQUIRED)

define_chicken_extension(message-digest
    VERSION 3.0.5
    DESCRIPTION "Message Digest Support"
    CATEGORY "crypt"
    LICENSE "BSD"
    AUTHOR "Kon Lovett")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 1
    -no-procedure-checks-for-toplevel-bindings)

add_chicken_module(message-digest-primitive message-digest-primitive.scm EXTENSION)
add_chicken_module(message-digest-type message-digest-type.scm EXTENSION
    DEPENDS message-digest-primitive)
add_chicken_module(message-digest-parameters message-digest-parameters.scm EXTENSION)
add_chicken_module(message-digest-support message-digest-support.scm EXTENSION
    DEPENDS message-digest-primitive message-digest-type)
add_chicken_module(message-digest-bv message-digest-bv.scm EXTENSION
    DEPENDS message-digest-primitive message-digest-type
            message-digest-support)
add_chicken_module(message-digest-int message-digest-int.scm EXTENSION
    DEPENDS message-digest-type message-digest-support)
add_chicken_module(message-digest-srfi-4 message-digest-srfi-4.scm EXTENSION
    DEPENDS message-digest-primitive message-digest-type
            message-digest-support message-digest-bv)
add_chicken_module(message-digest-update-item message-digest-update-item.scm EXTENSION
    DEPENDS message-digest-primitive message-digest-type
            message-digest-parameters message-digest-support)
add_chicken_module(message-digest-item message-digest-item.scm EXTENSION
    DEPENDS message-digest-type message-digest-update-item)
add_chicken_module(message-digest-port message-digest-port.scm EXTENSION
    DEPENDS message-digest-primitive message-digest-type
            message-digest-bv)
add_chicken_module(message-digest-basic message-digest-basic.scm EXTENSION
    DEPENDS message-digest-primitive message-digest-type
            message-digest-parameters message-digest-bv
            message-digest-int)
add_chicken_module(message-digest message-digest.scm EXTENSION
    DEPENDS message-digest-primitive message-digest-type
            message-digest-parameters message-digest-bv
            message-digest-int message-digest-srfi-4
            message-digest-update-item message-digest-item)

install_chicken_modules(message-digest)
