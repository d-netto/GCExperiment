#include <iostream>

#include <parser.h>
#include <tokens.h>

// ============================================================================
// Parser
// ============================================================================

namespace parser {

std::unique_ptr<goal_t> parser_t::parse_goal()
{
    auto main_class = parse_main_class();
    std::vector<std::unique_ptr<class_decl_t>> classes;
    while (scanner_.lookahead(0).type != END_OF_FILE) {
        classes.push_back(parse_class_decl());
    }
    return std::make_unique<goal_t>(std::move(main_class), std::move(classes));
}

std::unique_ptr<main_class_t> parser_t::parse_main_class()
{
    scanner_.check_and_consume(CLASS_KW);
    auto class_name = parse_identifier();
    scanner_.check_and_consume(LBRACE);
    scanner_.check_and_consume(PUBLIC_KW);
    scanner_.check_and_consume(STATIC_KW);
    scanner_.check_and_consume(VOID_KW);
    scanner_.check_and_consume(MAIN_KW);
    scanner_.check_and_consume(LPAREN);
    scanner_.check_and_consume(STRING_KW);
    scanner_.check_and_consume(LBRACKET);
    scanner_.check_and_consume(RBRACKET);
    auto arg_name = parse_identifier();
    scanner_.check_and_consume(RPAREN);
    auto statement = parse_statement();
    scanner_.check_and_consume(RBRACE);
    return std::make_unique<main_class_t>(std::move(class_name), std::move(arg_name),
                                          std::move(statement));
}

std::unique_ptr<class_decl_t> parser_t::parse_class_decl()
{
    scanner_.check_and_consume(CLASS_KW);
    auto class_name = parse_identifier();
    std::unique_ptr<identifier_t> extends_class_name = nullptr;
    if (scanner_.lookahead(0).type == EXTENDS_KW) {
        scanner_.check_and_consume(EXTENDS_KW);
        extends_class_name = parse_identifier();
    }
    scanner_.check_and_consume(LBRACE);
    std::vector<std::unique_ptr<var_decl_t>> field_decls;
    while (scanner_.lookahead(0).type == INT_KW ||
           scanner_.lookahead(0).type == BOOLEAN_KW ||
           scanner_.lookahead(0).type == IDENTIFIER) {
        field_decls.push_back(parse_var_decl());
    }
    std::vector<std::unique_ptr<method_decl_t>> method_decls;
    while (scanner_.lookahead(0).type == PUBLIC_KW) {
        method_decls.push_back(parse_method_decl());
    }
    scanner_.check_and_consume(RBRACE);
    return std::make_unique<class_decl_t>(std::move(class_name),
                                          std::move(extends_class_name),
                                          std::move(field_decls), std::move(method_decls));
}

std::unique_ptr<var_decl_t> parser_t::parse_var_decl()
{
    auto type = parse_type();
    auto name = parse_identifier();
    scanner_.check_and_consume(SEMICOLON);
    return std::make_unique<var_decl_t>(std::move(type), std::move(name));
}

std::unique_ptr<method_decl_t> parser_t::parse_method_decl()
{
    scanner_.check_and_consume(PUBLIC_KW);
    auto return_type = parse_type();
    auto name = parse_identifier();
    scanner_.check_and_consume(LPAREN);
    std::vector<std::unique_ptr<type_t>> arg_types;
    std::vector<std::unique_ptr<identifier_t>> arg_names;
    if (scanner_.lookahead(0).type != RPAREN) {
        arg_types.push_back(parse_type());
        arg_names.push_back(parse_identifier());
        while (scanner_.lookahead(0).type == COMMA) {
            scanner_.check_and_consume(COMMA);
            arg_types.push_back(parse_type());
            arg_names.push_back(parse_identifier());
        }
    }
    scanner_.check_and_consume(RPAREN);
    scanner_.check_and_consume(LBRACE);
    std::vector<std::unique_ptr<var_decl_t>> var_decls;
    while (1) {
        if (scanner_.lookahead(0).type == INT_KW ||
            scanner_.lookahead(0).type == BOOLEAN_KW) {
            var_decls.push_back(parse_var_decl());
        }
        else if (scanner_.lookahead(0).type == IDENTIFIER &&
                 scanner_.lookahead(1).type == IDENTIFIER) {
            var_decls.push_back(parse_var_decl());
        }
        else {
            break;
        }
    }
    std::vector<std::unique_ptr<statement_t>> statements;
    while (scanner_.lookahead(0).type != RETURN_KW) {
        statements.push_back(parse_statement());
    }
    scanner_.check_and_consume(RETURN_KW);
    auto return_expression = parse_expression();
    scanner_.check_and_consume(SEMICOLON);
    scanner_.check_and_consume(RBRACE);
    return std::make_unique<method_decl_t>(std::move(return_type), std::move(name),
                                           std::move(arg_types), std::move(arg_names),
                                           std::move(var_decls), std::move(statements),
                                           std::move(return_expression));
}

std::unique_ptr<type_t> parser_t::parse_type()
{
    if (scanner_.lookahead(0).type == INT_KW) {
        scanner_.check_and_consume(INT_KW);
        if (scanner_.lookahead(0).type == LBRACKET) {
            scanner_.check_and_consume(LBRACKET);
            scanner_.check_and_consume(RBRACKET);
            return std::make_unique<type_t>(std::make_unique<identifier_t>("int[]"));
        }
        else {
            return std::make_unique<type_t>(std::make_unique<identifier_t>("int"));
        }
    }
    else if (scanner_.lookahead(0).type == BOOLEAN_KW) {
        scanner_.check_and_consume(BOOLEAN_KW);
        return std::make_unique<type_t>(std::make_unique<identifier_t>("boolean"));
    }
    else {
        return std::make_unique<type_t>(parse_identifier());
    }
}

std::unique_ptr<statement_t> parser_t::parse_statement()
{
    switch (scanner_.lookahead(0).type) {
    case LBRACE: return parse_block_statement();
    case IF_KW: return parse_if_statement();
    case WHILE_KW: return parse_while_statement();
    case PRINTLN_KW: return parse_print_statement();
    case IDENTIFIER:
        if (scanner_.lookahead(1).type == EQUALS) {
            return parse_assign_statement();
        }
        else {
            return parse_array_assign_statement();
        }
    default: {
        auto t = scanner_.lookahead(0);
        std::cerr << "Unexpected token " << t.value << std::endl;
        exit(1);
    }
    }
}

std::unique_ptr<block_statement_t> parser_t::parse_block_statement()
{
    scanner_.check_and_consume(LBRACE);
    std::vector<std::unique_ptr<statement_t>> statements;
    while (scanner_.lookahead(0).type != RBRACE) {
        statements.push_back(parse_statement());
    }
    scanner_.check_and_consume(RBRACE);
    return std::make_unique<block_statement_t>(std::move(statements));
}

std::unique_ptr<if_statement_t> parser_t::parse_if_statement()
{
    scanner_.check_and_consume(IF_KW);
    scanner_.check_and_consume(LPAREN);
    auto condition = parse_expression();
    scanner_.check_and_consume(RPAREN);
    auto then_statement = parse_statement();
    scanner_.check_and_consume(ELSE_KW);
    auto else_statement = parse_statement();
    return std::make_unique<if_statement_t>(std::move(condition), std::move(then_statement),
                                            std::move(else_statement));
}

std::unique_ptr<while_statement_t> parser_t::parse_while_statement()
{
    scanner_.check_and_consume(WHILE_KW);
    scanner_.check_and_consume(LPAREN);
    auto condition = parse_expression();
    scanner_.check_and_consume(RPAREN);
    auto body = parse_statement();
    return std::make_unique<while_statement_t>(std::move(condition), std::move(body));
}

std::unique_ptr<print_statement_t> parser_t::parse_print_statement()
{
    scanner_.check_and_consume(PRINTLN_KW);
    scanner_.check_and_consume(LPAREN);
    auto expression = parse_expression();
    scanner_.check_and_consume(RPAREN);
    scanner_.check_and_consume(SEMICOLON);
    return std::make_unique<print_statement_t>(std::move(expression));
}

std::unique_ptr<assign_statement_t> parser_t::parse_assign_statement()
{
    auto var_name = parse_identifier();
    scanner_.check_and_consume(EQUALS);
    auto expression = parse_expression();
    scanner_.check_and_consume(SEMICOLON);
    return std::make_unique<assign_statement_t>(std::move(var_name), std::move(expression));
}

std::unique_ptr<array_assign_statement_t> parser_t::parse_array_assign_statement()
{
    auto var_name = parse_identifier();
    scanner_.check_and_consume(LBRACKET);
    auto index = parse_expression();
    scanner_.check_and_consume(RBRACKET);
    scanner_.check_and_consume(EQUALS);
    auto expression = parse_expression();
    scanner_.check_and_consume(SEMICOLON);
    return std::make_unique<array_assign_statement_t>(std::move(var_name), std::move(index),
                                                      std::move(expression));
}

static binary_operator_t binary_operator_from_token_type(int type)
{
    switch (type) {
    case AND: return binary_operator_t::and_;
    case LT: return binary_operator_t::less_;
    case PLUS: return binary_operator_t::plus_;
    case MINUS: return binary_operator_t::minus_;
    case TIMES: return binary_operator_t::times_;
    default: std::cerr << "Unexpected token " << type << std::endl; exit(1);
    }
}

std::unique_ptr<expression_t> parser_t::parse_expression()
{
    auto term = parse_term();
    if (scanner_.lookahead(0).type == AND || scanner_.lookahead(0).type == LT ||
        scanner_.lookahead(0).type == PLUS || scanner_.lookahead(0).type == MINUS ||
        scanner_.lookahead(0).type == TIMES) {
        while (true) {
            binary_operator_t binop;
            switch (scanner_.lookahead(0).type) {
            case AND:
            case LT:
            case PLUS:
            case MINUS:
                binop = binary_operator_from_token_type(scanner_.next_token().type);
                term = std::make_unique<binary_expression_t>(std::move(term), binop,
                                                             parse_term());
                break;
            default: goto done;
            }
        }
    }
done:
    return term;
}

std::unique_ptr<expression_t> parser_t::parse_term()
{
    auto factor = parse_factor();
    if (scanner_.lookahead(0).type == TIMES) {
        do {
            scanner_.check_and_consume(TIMES);
            factor = std::make_unique<binary_expression_t>(
                std::move(factor), binary_operator_t::times_, parse_factor());
        } while (scanner_.lookahead(0).type == TIMES);
    }
    return factor;
}

std::unique_ptr<expression_t> parser_t::parse_factor()
{
    std::unique_ptr<expression_t> expression;
    switch (scanner_.lookahead(0).type) {
    case INTEGER: expression = parse_integer_literal_expression(); break;
    case TRUE_KW: expression = parse_true_literal_expression(); break;
    case FALSE_KW: expression = parse_false_literal_expression(); break;
    case NOT: expression = parse_not_expression(); break;
    case LPAREN:
        scanner_.check_and_consume(LPAREN);
        expression = parse_expression();
        scanner_.check_and_consume(RPAREN);
        goto more;
    case NEW_KW:
        scanner_.check_and_consume(NEW_KW);
        if (scanner_.lookahead(0).type == INT_KW) {
            scanner_.check_and_consume(INT_KW);
            scanner_.check_and_consume(LBRACKET);
            expression = parse_expression();
            scanner_.check_and_consume(RBRACKET);
            expression =
                std::make_unique<new_integer_array_expression_t>(std::move(expression));
            break;
        }
        else {
            auto class_name = parse_identifier();
            scanner_.check_and_consume(LPAREN);
            scanner_.check_and_consume(RPAREN);
            expression = std::make_unique<new_object_expression_t>(std::move(class_name));
            goto more;
        }
    case THIS_KW: expression = parse_this_expression(); goto more;
    case IDENTIFIER: expression = parse_identifier_expression();
    default:
more:
        auto ty = scanner_.lookahead(0).type;
        if (ty == DOT) {
            scanner_.check_and_consume(DOT);
            ty = scanner_.lookahead(0).type;
            if (ty == LENGTH_KW) {
                scanner_.check_and_consume(LENGTH_KW);
                expression =
                    std::make_unique<array_length_expression_t>(std::move(expression));
            }
            else {
                auto identifier = parse_identifier();
                scanner_.check_and_consume(LPAREN);
                std::vector<std::unique_ptr<expression_t>> expressions;
                if (scanner_.lookahead(0).type != RPAREN) {
                    expressions.push_back(parse_expression());
                    while (scanner_.lookahead(0).type == COMMA) {
                        scanner_.check_and_consume(COMMA);
                        expressions.push_back(parse_expression());
                    }
                }
                scanner_.check_and_consume(RPAREN);
                expression = std::make_unique<method_call_expression_t>(
                    std::move(expression), std::move(identifier), std::move(expressions));
            }
        }
        else if (ty == LBRACKET) {
            scanner_.check_and_consume(LBRACKET);
            auto index = parse_expression();
            scanner_.check_and_consume(RBRACKET);
            expression = std::make_unique<array_index_expression_t>(std::move(expression),
                                                                    std::move(index));
        }
    }
    return expression;
}

std::unique_ptr<integer_literal_expression_t> parser_t::parse_integer_literal_expression()
{
    auto token = scanner_.next_token();
    if (token.type != INTEGER) {
        std::cerr << "Expected integer literal" << std::endl;
        std::cerr << "Got " << token.value << std::endl;
        exit(1);
    }
    return std::make_unique<integer_literal_expression_t>(std::stoi(token.value));
}

std::unique_ptr<true_literal_expression_t> parser_t::parse_true_literal_expression()
{
    scanner_.check_and_consume(TRUE_KW);
    return std::make_unique<true_literal_expression_t>();
}

std::unique_ptr<false_literal_expression_t> parser_t::parse_false_literal_expression()
{
    scanner_.check_and_consume(FALSE_KW);
    return std::make_unique<false_literal_expression_t>();
}

std::unique_ptr<identifier_expression_t> parser_t::parse_identifier_expression()
{
    auto identifier = parse_identifier();
    return std::make_unique<identifier_expression_t>(std::move(identifier));
}

std::unique_ptr<this_expression_t> parser_t::parse_this_expression()
{
    scanner_.check_and_consume(THIS_KW);
    return std::make_unique<this_expression_t>();
}

std::unique_ptr<new_integer_array_expression_t>
parser_t::parse_new_integer_array_expression()
{
    scanner_.check_and_consume(NEW_KW);
    scanner_.check_and_consume(INT_KW);
    scanner_.check_and_consume(LBRACKET);
    auto expression = parse_expression();
    scanner_.check_and_consume(RBRACKET);
    return std::make_unique<new_integer_array_expression_t>(std::move(expression));
}

std::unique_ptr<new_object_expression_t> parser_t::parse_new_object_expression()
{
    scanner_.check_and_consume(NEW_KW);
    auto class_name = parse_identifier();
    scanner_.check_and_consume(LPAREN);
    scanner_.check_and_consume(RPAREN);
    return std::make_unique<new_object_expression_t>(std::move(class_name));
}

std::unique_ptr<not_expression_t> parser_t::parse_not_expression()
{
    scanner_.check_and_consume(NOT);
    return std::make_unique<not_expression_t>(parse_expression());
}

std::unique_ptr<parentheses_expression_t> parser_t::parse_parentheses_expression()
{
    scanner_.check_and_consume(LPAREN);
    auto expression = parse_expression();
    scanner_.check_and_consume(RPAREN);
    return std::make_unique<parentheses_expression_t>(std::move(expression));
}

std::unique_ptr<identifier_t> parser_t::parse_identifier()
{
    auto token = scanner_.next_token();
    if (token.type != IDENTIFIER) {
        std::cerr << "Expected identifier" << std::endl;
        std::cerr << "Got " << token.value << std::endl;
        exit(1);
    }
    return std::make_unique<identifier_t>(token.value);
}

void goal_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void main_class_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void class_decl_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void var_decl_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void method_decl_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void type_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void statement_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void block_statement_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void if_statement_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void while_statement_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void print_statement_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void assign_statement_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void array_assign_statement_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void binary_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void array_index_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void array_length_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void method_call_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void integer_literal_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void true_literal_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void false_literal_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void identifier_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void this_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void new_integer_array_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void new_object_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void not_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}
void parentheses_expression_t::accept(visitor::visitor_t *v)
{
    v->visit(this);
}

} // namespace parser