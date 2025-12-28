BUILD_DIR ?= build
BINARY := $(BUILD_DIR)/code

.PHONY: build run clean

build:
	cmake -S . -B $(BUILD_DIR)
	cmake --build $(BUILD_DIR) --target code

run: build
	$(BINARY) -

clean:
	rm -rf $(BUILD_DIR)
