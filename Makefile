# ============================================================================
# COMPILER CONFIGURATION
# ============================================================================
FC        = gfortran

FCOPT     = -O3
FCOPT    += -march=native -mtune=native
FCOPT    += -funroll-loops
FCOPT    += -fopenmp-simd
FCOPT    += -flto

FCFLAGS   = $(FCOPT) -g -std=f2018
LDFLAGS   = -flto

BUILD_DIR = _build
# ============================================================================
# TARGETS
# ============================================================================
.PHONY: all clean

all: $(BUILD_DIR)/Testv010

$(BUILD_DIR):
	mkdir -p $@

$(BUILD_DIR)/KdTree.o: KdTree.f90 | $(BUILD_DIR)
	$(FC) $(FCFLAGS) -Itree -Inode -J$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/KdTree.a: $(BUILD_DIR)/KdTree.o
	ar rcs $@ $^

$(BUILD_DIR)/$(BUILD_DIR)/Testv010: tests/v0.1.0/Testv010.f90 $(BUILD_DIR)/KdTree.a | $(BUILD_DIR)
	$(FC) $(FCFLAGS) -Itree -Inode -I$(BUILD_DIR) -J$(BUILD_DIR) $< $(BUILD_DIR)/KdTree.a -o $@ $(LDFLAGS)

clean:
	rm -rf $(BUILD_DIR)