cmake_minimum_required(VERSION 2.8)

project(dbus C)

find_package(PkgConfig)
pkg_check_modules(DBUS REQUIRED dbus-1)

find_package(Chicken REQUIRED)
chicken_find_extension(easyffi REQUIRED)
chicken_find_extension(matchable REQUIRED)
chicken_find_extension(miscmacros REQUIRED)
chicken_find_extension(foreigners REQUIRED)

chicken_define_extension(dbus
    VERSION 0.93
    CATEGORY "os"
    DESCRIPTION "A binding for libdbus, the IPC mechanism"
    LICENSE "MIT"
    AUTHOR "Shawn Rutledge")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

include_directories(${DBUS_INCLUDE_DIRS})

chicken_add_module(dbus dbus.scm EXTENSION)
target_link_libraries(dbus ${DBUS_LDFLAGS})

chicken_install(EXTENSION dbus MODULES)
