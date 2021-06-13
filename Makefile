ultimatetest:
	@echo Running the Ultimate test
	@echo
	zig build-exe lexer.zig
	zig build-exe parser.zig
	zig build-exe codegen.zig
	zig build-exe vm.zig
	@echo
	@echo BEHOLD
	@echo
	cat examples/input14.txt | ./lexer | ./parser | ./codegen | ./vm

test: testzig testnim

testzig:
	zig test lexer.zig
	zig test parser.zig
	zig test ast_interpreter.zig
	zig test codegen.zig
	zig test vm.zig

testnim:
	nim r test_lexical_analyzer.nim

.PHONY: test testzig testnim ultimatetest
