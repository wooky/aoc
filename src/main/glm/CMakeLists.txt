find_program(gleam
  NAMES gleam
  REQUIRED
)

set(OBJ_DIR "${CMAKE_BINARY_DIR}/glm-obj")
execute_process(
  COMMAND ${CMAKE_COMMAND} -E make_directory ${OBJ_DIR}
  COMMAND ${CMAKE_COMMAND} -E create_symlink ${OBJ_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/build
)

file(GLOB_RECURSE GLM_SOURCES *.gleam *.toml)
set(GLM_LIB "${CMAKE_BINARY_DIR}/glm-obj/dev/javascript/aoc_glm/aoc_glm.mjs")

add_custom_command(
  OUTPUT ${GLM_LIB}
  COMMAND ${gleam} build
  DEPENDS ${GLM_SOURCES}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_target(aoc_glm ALL
  DEPENDS ${GLM_LIB}
)
