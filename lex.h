#ifndef LEX_H
#define LEX_H

#include <fstream>
#include <vector>
#include <string>
#include <stdint.h>

namespace Lexer {

	enum Token_Type {
		TOKEN_INTEGER,
		TOKEN_FLOAT,
		TOKEN_IDENTIFIER,
		TOKEN_STRING,
		TOKEN_OPERATOR	
	};

	struct Token {
		Token_Type type;
		std::string word;
		int line;
		int col;
		union {
			int64_t i;
			double  f;
		};
	};

	class Lex_Context {
		friend Lex_Context* generate_tokens(const std::string&);

		public:
			Lex_Context(const std::string& fname): handle(fname, std::ifstream::binary) { }
			std::vector<Token>* tokens;
			std::vector<std::string> raw_file; // by line
							
		private:
			int get();
			int peek();
			bool matches_number() const;
			bool matches_identifier() const;
			bool matches_string() const;
			bool matches_operator() const;
			void handle_number();	
			void handle_identifier();
			void handle_string();
			void handle_operator();
			void report_error(const std::string&) const;
			void append_token(Token&);

			int on;
			int next;
			int line;
			int col;
			std::ifstream handle;

	};

	Lex_Context* generate_tokens(const std::string&);

};

#endif
