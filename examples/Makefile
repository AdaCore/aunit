ALL_TARGETS = calculator failures liskov simple_test test_caller test_fixture

all: $(ALL_TARGETS)

$(ALL_TARGETS):
	make -C $@

clean:
	$(foreach DIR,$(ALL_TARGETS), make -C $(DIR) clean &&) echo end

.PHONY: $(ALL_TARGETS) clean
