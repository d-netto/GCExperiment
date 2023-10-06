#pragma once

#include <string>
#include <vector>

namespace scanner {

struct token_t {
    int type;
    std::string value;
};

class scanner_t {
public:
    scanner_t(std::vector<token_t> &&tokens) : tokens_{tokens} {}
    token_t next_token();
    token_t lookahead(size_t n);
    void check_and_consume(int type);

private:
    std::vector<token_t> tokens_;
};

scanner_t create_scanner(void);

} // namespace scanner