# - Find Mapnik
# Find the native Mapnik includes and library
#
#   MAPNIK_FOUND        - True if MAPNIK found.
#   MAPNIK_INCLUDE_DIRS - where to find mapnik/map.hpp, etc.
#   MAPNIK_LIBRARY_DIRS - List of linker search paths
#   MAPNIK_LIBRARIES    - List of libraries when using MAPNIK
#   MAPNIK_DEFINES      - List of definitions
#   MAPNIK_CXXFLAGS     - cxx compiler flags

find_program(MAPNIK_CONFIG "mapnik-config")

function (mapnik_config varname cmdargs)
  execute_process(
    COMMAND "bash" ${MAPNIK_CONFIG} ${cmdargs}
    OUTPUT_VARIABLE raw_output
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  string(REPLACE " " ";" list_output ${raw_output})
  if (${ARGC} EQUAL 3)
    foreach (flag ${list_output})
      string(REGEX MATCH ${ARGV2} match ${flag})
      set(match ${CMAKE_MATCH_1})
      if (match)
        list(APPEND output ${match})
      endif()
    endforeach()
  else()
    set(output ${list_output})
  endif()
  set(${varname} ${output} PARENT_SCOPE)
endfunction()

mapnik_config(MAPNIK_DEFINES "--defines")

mapnik_config(_mapnik_includes "--includes" "-I(.+)")
mapnik_config(_mapnik_dep_includes "--dep-includes" "-I(.+)")
set(MAPNIK_INCLUDE_DIRS ${_mapnik_includes} ${_mapnik_dep_includes})
list(REMOVE_DUPLICATES MAPNIK_INCLUDE_DIRS)

mapnik_config(_mapnik_libs "--libs" "-l(.+)")
mapnik_config(_mapnik_dep_libs "--dep-libs" "-l(.+)")
set(MAPNIK_LIBRARIES ${_mapnik_libs} ${_mapnik_dep_libs})
list(REMOVE_DUPLICATES MAPNIK_LIBRARIES)

mapnik_config(_mapnik_libs_dirs "--libs" "-L(.+)")
mapnik_config(_mapnik_dep_libs_dirs "--dep-libs" "-L(.+)")
set(MAPNIK_LIBRARY_DIRS ${_mapnik_libs_dirs} ${_mapnik_dep_libs_dirs})
list(REMOVE_DUPLICATES MAPNIK_LIBRARY_DIRS)

mapnik_config(MAPNIK_CXXFLAGS "--cxxflags")
#string(REPLACE ";" " " MAPNIK_CXXFLAGS ${MAPNIK_CXXFLAGS})


# handle the QUIETLY and REQUIRED arguments and set MAPNIK_FOUND to TRUE if
# all listed variables are TRUE
include( FindPackageHandleStandardArgs )
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Mapnik DEFAULT_MSG MAPNIK_LIBRARIES MAPNIK_INCLUDE_DIRS)
