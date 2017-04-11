#include "lex.h"
#include "parse.h"
#include "generate.h"

int main() {

	auto lex_context = Lexer::generate_tokens("demo.spy");	
	auto parse_context = Parser::generate_tree(lex_context);
	Generator::generate_c_file("demo.c", parse_context);

}
