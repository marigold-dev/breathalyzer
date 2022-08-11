.PHONY: test test-list-ext

LIGO_COMPILER_VERSION:=0.48.0
TEZOS_PROTOCOL:=jakarta
LIGO_DOCKER := docker run --rm  -v $(PWD):$(PWD) -w $(PWD) ligolang/ligo:$(LIGO_COMPILER_VERSION)

define test_ligo
  $(LIGO_DOCKER) run test $(1) --protocol $(TEZOS_PROTOCOL)
endef

test: test-list-ext test-ticket-factory
	$(call test_ligo,test/test.mligo)

test-list-ext:
	$(call test_ligo,examples/simple/test_list_ext.mligo)

test-ticket-factory:
	$(call test_ligo,examples/ticket_factory/src/oven_sc.mligo)
	$(call test_ligo,examples/ticket_factory/src/mint_sc.mligo)
