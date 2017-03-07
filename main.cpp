#include "lex.h"
#include "parse.h"

int main() {

	Lexer::Lex_Context* context = Lexer::generate_tokens("demo.spy");	
	Parser::generate_tree(context);

}
