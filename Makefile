ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

BUILD_DIR:="${ROOT_DIR}/_build/"
INSTALL_DIR="${ROOT_DIR}/install/"

CMAKE_ARGS_CONFIGURE:=-S ${ROOT_DIR} -B ${BUILD_DIR}
CMAKE_ARGS_BUILD:=--build ${BUILD_DIR} --parallel
CMAKE_ARGS_TEST:=--test-dir ${BUILD_DIR} --extra-verbose

.PHONY: all configure build test install

all : configure build test install

configure :
	@mkdir -p ${BUILD_DIR}
	@cmake ${CMAKE_ARGS_CONFIGURE}

build :
	@cmake ${CMAKE_ARGS_BUILD}

test :
	@ctest ${CMAKE_ARGS_TEST}

install :
	@cmake --install ${ROOT_DIR}/install/

