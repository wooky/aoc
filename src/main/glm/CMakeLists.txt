project(aoc_glm)

find_program(gleam
  NAMES gleam
  REQUIRED
)

set(OBJ_DIR "${CMAKE_BINARY_DIR}/glm-obj")
execute_process(
  COMMAND ${CMAKE_COMMAND} -E make_directory ${OBJ_DIR}
  COMMAND ${CMAKE_COMMAND} -E create_symlink ${OBJ_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/build
)

add_custom_target(${PROJECT_NAME} ALL
  COMMAND ${gleam} build
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
