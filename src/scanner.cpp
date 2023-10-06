#include <iostream>
#include <string>
#include <vector>

#include <scanner.h>
#include <tokens.h>

extern "C" {
extern int yylex(void);
}

extern int yylineno;
extern char *yytext;

// ============================================================================
// Scanner/lexer
// ============================================================================

namespace scanner {

token_t scanner_t::next_token()
{
    if (tokens_.empty()) {
        return {0, ""};
    }
    auto token = tokens_.back();
    tokens_.pop_back();
    return token;
}

token_t scanner_t::lookahead(size_t n)
{
    if (tokens_.size() <= n) {
        return {0, ""};
    }
    return tokens_.at(tokens_.size() - n - 1);
}

void scanner_t::check_and_consume(int type)
{
    if (tokens_.empty()) {
        std::cerr << "Unexpected end of file" << std::endl;
        exit(1);
    }
    if (tokens_.back().type != type) {
        std::cerr << "Expected token " << type << " but got " << tokens_.back().type
                  << std::endl;
        std::cerr << "Unexpected token " << tokens_.back().value << std::endl;
        // Print all tokens
        for (auto token : tokens_) {
            std::cerr << token.value << std::endl;
        }
        exit(1);
    }
    tokens_.pop_back();
}

scanner_t create_scanner(void)
{
    std::vector<token_t> tokens;
    int ntoken;
    while ((ntoken = yylex())) {
        tokens.push_back({ntoken, std::string{yytext}});
    }
    std::reverse(tokens.begin(), tokens.end());
    scanner_t scanner{std::move(tokens)};
    return scanner;
}

} // namespace scanner