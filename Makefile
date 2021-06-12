testall: testzig testnim

testzig:
	zig test lexer.zig
	zig test parser.zig
	zig test ast_interpreter.zig

testnim:
	nim r test_lexical_analyzer.nim

.PHONY: testall testzig testnim
