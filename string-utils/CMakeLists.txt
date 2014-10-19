cmake_minimum_required(VERSION 2.8)

project(string-utils C)

find_package(Chicken REQUIRED)
find_chicken_extension(srfi-13 REQUIRED)
find_chicken_extension(setup-helper 1.5.2  REQUIRED)
find_chicken_extension(miscmacros   2.9    REQUIRED)
find_chicken_extension(lookup-table 1.13.1 REQUIRED)
find_chicken_extension(check-errors 1.12.1 REQUIRED)

define_chicken_extension(${PROJECT_NAME}
    VERSION 1.2.4
    DESCRIPTION "String Utilities"
    CATEGORY "data"
    LICENSE "BSD"
    AUTHOR "Kon Lovett")

set(CHICKEN_OPTIONS -scrutinize -fixnum-arithmetic -inline -local
    -optimize-level 3 -debug-level 0 -no-procedure-checks)

add_chicken_module(unicode-utils unicode-utils.scm EXTENSION)
add_chicken_module(memoized-string memoized-string.scm EXTENSION
    OPTIONS -disable-interrupts
    DEPENDS unicode-utils)
add_chicken_module(to-hex to-hex.scm EXTENSION)
add_chicken_module(string-hexadecimal string-hexadecimal.scm EXTENSION
    DEPENDS to-hex)
add_chicken_module(string-utils string-utils.scm EXTENSION
    DEPENDS memoized-string unicode-utils string-hexadecimal)

install_chicken_modules(string-utils)
