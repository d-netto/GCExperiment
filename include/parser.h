#pragma once

#include <memory>
#include <string>
#include <vector>

#include <scanner.h>

namespace visitor {
// Visitor
class visitor_t;
} // namespace visitor

namespace parser {

struct goal_t;
struct main_class_t;
struct class_decl_t;
struct var_decl_t;
struct method_decl_t;
struct type_t;

// Statements
struct statement_t;
struct block_statement_t;
struct if_statement_t;
struct while_statement_t;
struct print_statement_t;
struct assign_statement_t;
struct array_assign_statement_t;

// Expressions
struct expression_t;
struct binary_expression_t;
struct array_index_expression_t;
struct array_length_expression_t;
struct method_call_expression_t;
struct integer_literal_expression_t;
struct true_literal_expression_t;
struct false_literal_expression_t;
struct identifier_expression_t;
struct this_expression_t;
struct new_integer_array_expression_t;
struct new_object_expression_t;
struct not_expression_t;
struct parentheses_expression_t;

struct identifier_t;

struct goal_t {
    std::unique_ptr<main_class_t> main_class;
    std::vector<std::unique_ptr<class_decl_t>> class_decls;
    goal_t(std::unique_ptr<main_class_t> main_class,
           std::vector<std::unique_ptr<class_decl_t>> class_decls)
      : main_class{std::move(main_class)}, class_decls{std::move(class_decls)}
    {
    }
    void accept(visitor::visitor_t *v);
};

struct main_class_t {
    std::unique_ptr<identifier_t> class_name;
    std::unique_ptr<identifier_t> arg_name;
    std::unique_ptr<statement_t> statement;
    main_class_t(std::unique_ptr<identifier_t> class_name,
                 std::unique_ptr<identifier_t> arg_name,
                 std::unique_ptr<statement_t> statement)
      : class_name{std::move(class_name)},
        arg_name{std::move(arg_name)},
        statement{std::move(statement)}
    {
    }
    void accept(visitor::visitor_t *v);
};

struct class_decl_t {
    std::unique_ptr<identifier_t> class_name;
    std::unique_ptr<identifier_t> parent_class_name;
    std::vector<std::unique_ptr<var_decl_t>> field_decls;
    std::vector<std::unique_ptr<method_decl_t>> method_decls;
    class_decl_t(std::unique_ptr<identifier_t> class_name,
                 std::unique_ptr<identifier_t> parent_class_name,
                 std::vector<std::unique_ptr<var_decl_t>> field_decls,
                 std::vector<std::unique_ptr<method_decl_t>> method_decls)
      : class_name{std::move(class_name)},
        parent_class_name{std::move(parent_class_name)},
        field_decls{std::move(field_decls)},
        method_decls{std::move(method_decls)}
    {
    }
    void accept(visitor::visitor_t *v);
};

struct var_decl_t {
    std::unique_ptr<type_t> type;
    std::unique_ptr<identifier_t> var_name;
    var_decl_t(std::unique_ptr<type_t> type, std::unique_ptr<identifier_t> var_name)
      : type{std::move(type)}, var_name{std::move(var_name)}
    {
    }
    void accept(visitor::visitor_t *v);
};

struct method_decl_t {
    std::unique_ptr<type_t> return_type;
    std::unique_ptr<identifier_t> method_name;
    std::vector<std::unique_ptr<type_t>> arg_types;
    std::vector<std::unique_ptr<identifier_t>> arg_names;
    std::vector<std::unique_ptr<var_decl_t>> var_decls;
    std::vector<std::unique_ptr<statement_t>> statements;
    std::unique_ptr<expression_t> return_expression;
    method_decl_t(std::unique_ptr<type_t> return_type,
                  std::unique_ptr<identifier_t> method_name,
                  std::vector<std::unique_ptr<type_t>> arg_types,
                  std::vector<std::unique_ptr<identifier_t>> arg_names,
                  std::vector<std::unique_ptr<var_decl_t>> var_decls,
                  std::vector<std::unique_ptr<statement_t>> statements,
                  std::unique_ptr<expression_t> return_expression)
      : return_type{std::move(return_type)},
        method_name{std::move(method_name)},
        arg_types{std::move(arg_types)},
        arg_names{std::move(arg_names)},
        var_decls{std::move(var_decls)},
        statements{std::move(statements)},
        return_expression{std::move(return_expression)}
    {
    }
    void accept(visitor::visitor_t *v);
};

struct type_t {
    std::unique_ptr<identifier_t> type_name;
    type_t(std::unique_ptr<identifier_t> type_name) : type_name{std::move(type_name)} {}
    void accept(visitor::visitor_t *v);
};

// Statements

struct statement_t {
    virtual ~statement_t() = default;
    virtual void accept(visitor::visitor_t *v);
};

struct block_statement_t : public statement_t {
    std::vector<std::unique_ptr<statement_t>> statements;
    block_statement_t(std::vector<std::unique_ptr<statement_t>> statements)
      : statements{std::move(statements)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct if_statement_t : public statement_t {
    std::unique_ptr<expression_t> condition;
    std::unique_ptr<statement_t> then_statement;
    std::unique_ptr<statement_t> else_statement;
    if_statement_t(std::unique_ptr<expression_t> condition,
                   std::unique_ptr<statement_t> then_statement,
                   std::unique_ptr<statement_t> else_statement)
      : condition{std::move(condition)},
        then_statement{std::move(then_statement)},
        else_statement{std::move(else_statement)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct while_statement_t : public statement_t {
    std::unique_ptr<expression_t> condition;
    std::unique_ptr<statement_t> statement;
    while_statement_t(std::unique_ptr<expression_t> condition,
                      std::unique_ptr<statement_t> statement)
      : condition{std::move(condition)}, statement{std::move(statement)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct print_statement_t : public statement_t {
    std::unique_ptr<expression_t> expression;
    print_statement_t(std::unique_ptr<expression_t> expression)
      : expression{std::move(expression)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct assign_statement_t : public statement_t {
    std::unique_ptr<identifier_t> var_name;
    std::unique_ptr<expression_t> expression;
    assign_statement_t(std::unique_ptr<identifier_t> var_name,
                       std::unique_ptr<expression_t> expression)
      : var_name{std::move(var_name)}, expression{std::move(expression)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct array_assign_statement_t : public statement_t {
    std::unique_ptr<identifier_t> var_name;
    std::unique_ptr<expression_t> index_expression;
    std::unique_ptr<expression_t> expression;
    array_assign_statement_t(std::unique_ptr<identifier_t> var_name,
                             std::unique_ptr<expression_t> index_expression,
                             std::unique_ptr<expression_t> expression)
      : var_name{std::move(var_name)},
        index_expression{std::move(index_expression)},
        expression{std::move(expression)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

// Expressions
enum class binary_operator_t {
    plus_,
    minus_,
    times_,
    less_,
    and_,
};

struct expression_t {
    virtual ~expression_t() = default;
    virtual void accept(visitor::visitor_t *v);
};

struct binary_expression_t : public expression_t {
    std::unique_ptr<expression_t> left;
    binary_operator_t op;
    std::unique_ptr<expression_t> right;
    binary_expression_t(std::unique_ptr<expression_t> left, binary_operator_t op,
                        std::unique_ptr<expression_t> right)
      : left{std::move(left)}, op{op}, right{std::move(right)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct array_index_expression_t : public expression_t {
    std::unique_ptr<expression_t> array_expression;
    std::unique_ptr<expression_t> index_expression;
    array_index_expression_t(std::unique_ptr<expression_t> array_expression,
                             std::unique_ptr<expression_t> index_expression)
      : array_expression{std::move(array_expression)},
        index_expression{std::move(index_expression)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct array_length_expression_t : public expression_t {
    std::unique_ptr<expression_t> array_expression;
    array_length_expression_t(std::unique_ptr<expression_t> array_expression)
      : array_expression{std::move(array_expression)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct method_call_expression_t : public expression_t {
    std::unique_ptr<expression_t> object_expression;
    std::unique_ptr<identifier_t> method_name;
    std::vector<std::unique_ptr<expression_t>> arg_expressions;
    method_call_expression_t(std::unique_ptr<expression_t> object_expression,
                             std::unique_ptr<identifier_t> method_name,
                             std::vector<std::unique_ptr<expression_t>> arg_expressions)
      : object_expression{std::move(object_expression)},
        method_name{std::move(method_name)},
        arg_expressions{std::move(arg_expressions)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct integer_literal_expression_t : public expression_t {
    int value;
    integer_literal_expression_t(int value) : value{value} {}
    void accept(visitor::visitor_t *v) override;
};

struct true_literal_expression_t : public expression_t {
    void accept(visitor::visitor_t *v) override;
};

struct false_literal_expression_t : public expression_t {
    void accept(visitor::visitor_t *v) override;
};

struct identifier_expression_t : public expression_t {
    std::unique_ptr<identifier_t> identifier;
    identifier_expression_t(std::unique_ptr<identifier_t> identifier)
      : identifier{std::move(identifier)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct this_expression_t : public expression_t {
    void accept(visitor::visitor_t *v) override;
};

struct new_integer_array_expression_t : public expression_t {
    std::unique_ptr<expression_t> size_expression;
    new_integer_array_expression_t(std::unique_ptr<expression_t> size_expression)
      : size_expression{std::move(size_expression)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct new_object_expression_t : public expression_t {
    std::unique_ptr<identifier_t> class_name;
    new_object_expression_t(std::unique_ptr<identifier_t> class_name)
      : class_name{std::move(class_name)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct not_expression_t : public expression_t {
    std::unique_ptr<expression_t> expression;
    not_expression_t(std::unique_ptr<expression_t> expression)
      : expression{std::move(expression)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct parentheses_expression_t : public expression_t {
    std::unique_ptr<expression_t> expression;
    parentheses_expression_t(std::unique_ptr<expression_t> expression)
      : expression{std::move(expression)}
    {
    }
    void accept(visitor::visitor_t *v) override;
};

struct identifier_t {
    std::string name;
    identifier_t(std::string name) : name{name} {}
};

class parser_t {
public:
    parser_t(scanner::scanner_t scanner) : scanner_{std::move(scanner)} {}
    std::unique_ptr<goal_t> parse_goal();

private:
    std::unique_ptr<main_class_t> parse_main_class();
    std::unique_ptr<class_decl_t> parse_class_decl();
    std::unique_ptr<var_decl_t> parse_var_decl();
    std::unique_ptr<method_decl_t> parse_method_decl();
    std::unique_ptr<type_t> parse_type();
    std::unique_ptr<statement_t> parse_statement();
    std::unique_ptr<block_statement_t> parse_block_statement();
    std::unique_ptr<if_statement_t> parse_if_statement();
    std::unique_ptr<while_statement_t> parse_while_statement();
    std::unique_ptr<print_statement_t> parse_print_statement();
    std::unique_ptr<assign_statement_t> parse_assign_statement();
    std::unique_ptr<array_assign_statement_t> parse_array_assign_statement();
    std::unique_ptr<expression_t> parse_expression();
    std::unique_ptr<expression_t> parse_term();
    std::unique_ptr<expression_t> parse_factor();
    std::unique_ptr<array_index_expression_t> parse_array_index_expression();
    std::unique_ptr<array_length_expression_t> parse_array_length_expression();
    std::unique_ptr<method_call_expression_t> parse_method_call_expression();
    std::unique_ptr<integer_literal_expression_t> parse_integer_literal_expression();
    std::unique_ptr<true_literal_expression_t> parse_true_literal_expression();
    std::unique_ptr<false_literal_expression_t> parse_false_literal_expression();
    std::unique_ptr<identifier_expression_t> parse_identifier_expression();
    std::unique_ptr<this_expression_t> parse_this_expression();
    std::unique_ptr<new_integer_array_expression_t> parse_new_integer_array_expression();
    std::unique_ptr<new_object_expression_t> parse_new_object_expression();
    std::unique_ptr<not_expression_t> parse_not_expression();
    std::unique_ptr<parentheses_expression_t> parse_parentheses_expression();
    std::unique_ptr<identifier_t> parse_identifier();
    scanner::scanner_t scanner_;
};

} // namespace parser

namespace visitor {

class visitor_t {
public:
    virtual void visit(parser::goal_t *node) = 0;
    virtual void visit(parser::main_class_t *node) = 0;
    virtual void visit(parser::class_decl_t *node) = 0;
    virtual void visit(parser::var_decl_t *node) = 0;
    virtual void visit(parser::method_decl_t *node) = 0;
    virtual void visit(parser::type_t *node) = 0;
    virtual void visit(parser::statement_t *node) = 0;
    virtual void visit(parser::block_statement_t *node) = 0;
    virtual void visit(parser::if_statement_t *node) = 0;
    virtual void visit(parser::while_statement_t *node) = 0;
    virtual void visit(parser::print_statement_t *node) = 0;
    virtual void visit(parser::assign_statement_t *node) = 0;
    virtual void visit(parser::array_assign_statement_t *node) = 0;
    virtual void visit(parser::expression_t *node) = 0;
    virtual void visit(parser::binary_expression_t *node) = 0;
    virtual void visit(parser::array_index_expression_t *node) = 0;
    virtual void visit(parser::array_length_expression_t *node) = 0;
    virtual void visit(parser::method_call_expression_t *node) = 0;
    virtual void visit(parser::integer_literal_expression_t *node) = 0;
    virtual void visit(parser::true_literal_expression_t *node) = 0;
    virtual void visit(parser::false_literal_expression_t *node) = 0;
    virtual void visit(parser::identifier_expression_t *node) = 0;
    virtual void visit(parser::this_expression_t *node) = 0;
    virtual void visit(parser::new_integer_array_expression_t *node) = 0;
    virtual void visit(parser::new_object_expression_t *node) = 0;
    virtual void visit(parser::not_expression_t *node) = 0;
    virtual void visit(parser::parentheses_expression_t *node) = 0;
};

} // namespace visitor