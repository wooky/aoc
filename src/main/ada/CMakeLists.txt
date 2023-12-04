project(aoc_ada)

find_program(gprbuild
  NAMES gprbuild
  REQUIRED
)

add_custom_target(${PROJECT_NAME} ALL
  COMMAND ${gprbuild}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
