test_all:
	@find . -maxdepth 1 -mindepth 1 -type d \
		| sort -V \
		| xargs -I {} bash -c 'cd {} && printf "{}\n" && go test && echo ""'
