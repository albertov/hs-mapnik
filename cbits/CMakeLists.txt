project(CMapnik)
cmake_minimum_required (VERSION 2.8)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

find_package(Mapnik REQUIRED)

set (cmapnik_srcs
  mapnik_c_api.cpp
  )

link_directories(${MAPNIK_LIBRARY_DIRS})
add_library (mapnik_c ${cmapnik_srcs})
target_include_directories(mapnik_c PRIVATE ${MAPNIK_INCLUDE_DIRS})
target_link_libraries(mapnik_c ${MAPNIK_LIBRARIES})
target_compile_definitions(mapnik_c PRIVATE ${MAPNIK_DEFINES})
set_target_properties(mapnik_c PROPERTIES COMPILE_OPTIONS "${MAPNIK_CXXFLAGS}")

install(TARGETS mapnik_c DESTINATION lib)
install(FILES mapnik_c_api.h DESTINATION include)
