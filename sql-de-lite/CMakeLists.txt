cmake_minimum_required(VERSION 2.8)

project(sql-de-lite C)

find_package(Chicken REQUIRED)
chicken_find_extension(lru-cache 0.5.3 REQUIRED)
chicken_find_extension(foreigners REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 0.6.1
    DESCRIPTION "SQLite 3 interface"
    CATEGORY "db"
    LICENSE "BSD"
    AUTHOR "Jim Ursetto")

find_package(PkgConfig)
pkg_check_modules(SQLITE REQUIRED sqlite3)

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 0 -inline -local)

include_directories(${SQLITE_INCLUDE_DIRS})

chicken_add_module(sql-de-lite sql-de-lite.scm EXTENSION)
target_link_libraries(sql-de-lite ${SQLITE_LDFLAGS})

chicken_install(EXTENSION sql-de-lite MODULES)
