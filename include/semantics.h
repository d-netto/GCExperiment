#pragma once

#include <map>
#include <string>

#include <parser.h>

namespace semantics {

struct type_t {
    virtual std::string as_str() = 0;
};

struct method_symtbl_t {
    std::string name;
    std::vector<std::pair<std::string, type_t *>> params;
    std::map<std::string, type_t *> local_vars;
    type_t *return_type;
    method_symtbl_t() : return_type(nullptr) {}
    method_symtbl_t(std::string name, std::vector<std::pair<std::string, type_t *>> params,
                    std::map<std::string, type_t *> local_vars, type_t *return_type)
      : name(name), params(params), local_vars(local_vars), return_type(return_type)
    {
    }
};

struct integer_t : public type_t {
    std::string as_str() override { return "integer"; }
};
struct boolean_t : public type_t {
    std::string as_str() override { return "boolean"; }
};
struct array_t : public type_t {
    std::string as_str() override { return "array"; }
};

extern integer_t *integer_type;
extern boolean_t *boolean_type;
extern array_t *array_type;

struct class_symtbl_t : public type_t {
    class_symtbl_t *parent_class;
    std::string name;
    std::map<std::string, type_t *> fields;
    std::vector<method_symtbl_t *> methods;
    std::string as_str() override { return name; }
    bool is_subtype(class_symtbl_t *other);
};

struct program_symtbl_t {
    std::map<std::string, class_symtbl_t *> classes;
    type_t *str_to_type(const std::string &name);
    void print();
};

class semantic_vis_accum_classes_t : public visitor::visitor_t {
public:
    program_symtbl_t *program_symtbl;
    void visit(parser::goal_t *node) override;
    void visit(parser::main_class_t *node) override;
    void visit(parser::class_decl_t *node) override;
    void visit(parser::var_decl_t *node) override{};
    void visit(parser::method_decl_t *node) override{};
    void visit(parser::type_t *node) override{};
    void visit(parser::statement_t *node) override{};
    void visit(parser::block_statement_t *node) override{};
    void visit(parser::if_statement_t *node) override{};
    void visit(parser::while_statement_t *node) override{};
    void visit(parser::print_statement_t *node) override{};
    void visit(parser::assign_statement_t *node) override{};
    void visit(parser::array_assign_statement_t *node) override{};
    void visit(parser::expression_t *node) override{};
    void visit(parser::binary_expression_t *node) override{};
    void visit(parser::array_index_expression_t *node) override{};
    void visit(parser::array_length_expression_t *node) override{};
    void visit(parser::method_call_expression_t *node) override{};
    void visit(parser::integer_literal_expression_t *node) override{};
    void visit(parser::true_literal_expression_t *node) override{};
    void visit(parser::false_literal_expression_t *node) override{};
    void visit(parser::identifier_expression_t *node) override{};
    void visit(parser::this_expression_t *node) override{};
    void visit(parser::new_integer_array_expression_t *node) override{};
    void visit(parser::new_object_expression_t *node) override{};
    void visit(parser::not_expression_t *node) override{};
    void visit(parser::parentheses_expression_t *node) override{};
};

class semantic_vis_accum_parent_classes_t : public visitor::visitor_t {
public:
    program_symtbl_t *program_symtbl;
    semantic_vis_accum_parent_classes_t(program_symtbl_t *program_symtbl)
      : program_symtbl(program_symtbl)
    {
    }
    void visit(parser::goal_t *node) override;
    void visit(parser::main_class_t *node) override{};
    void visit(parser::class_decl_t *node) override;
    void visit(parser::var_decl_t *node) override{};
    void visit(parser::method_decl_t *node) override{};
    void visit(parser::type_t *node) override{};
    void visit(parser::statement_t *node) override{};
    void visit(parser::block_statement_t *node) override{};
    void visit(parser::if_statement_t *node) override{};
    void visit(parser::while_statement_t *node) override{};
    void visit(parser::print_statement_t *node) override{};
    void visit(parser::assign_statement_t *node) override{};
    void visit(parser::array_assign_statement_t *node) override{};
    void visit(parser::expression_t *node) override{};
    void visit(parser::binary_expression_t *node) override{};
    void visit(parser::array_index_expression_t *node) override{};
    void visit(parser::array_length_expression_t *node) override{};
    void visit(parser::method_call_expression_t *node) override{};
    void visit(parser::integer_literal_expression_t *node) override{};
    void visit(parser::true_literal_expression_t *node) override{};
    void visit(parser::false_literal_expression_t *node) override{};
    void visit(parser::identifier_expression_t *node) override{};
    void visit(parser::this_expression_t *node) override{};
    void visit(parser::new_integer_array_expression_t *node) override{};
    void visit(parser::new_object_expression_t *node) override{};
    void visit(parser::not_expression_t *node) override{};
    void visit(parser::parentheses_expression_t *node) override{};
};

class semantic_vis_accum_fields_t : public visitor::visitor_t {
public:
    program_symtbl_t *program_symtbl;
    semantic_vis_accum_fields_t(program_symtbl_t *program_symtbl)
      : program_symtbl(program_symtbl)
    {
    }
    void visit(parser::goal_t *node) override;
    void visit(parser::main_class_t *node) override{};
    void visit(parser::class_decl_t *node) override;
    void visit(parser::var_decl_t *node) override{};
    void visit(parser::method_decl_t *node) override{};
    void visit(parser::type_t *node) override{};
    void visit(parser::statement_t *node) override{};
    void visit(parser::block_statement_t *node) override{};
    void visit(parser::if_statement_t *node) override{};
    void visit(parser::while_statement_t *node) override{};
    void visit(parser::print_statement_t *node) override{};
    void visit(parser::assign_statement_t *node) override{};
    void visit(parser::array_assign_statement_t *node) override{};
    void visit(parser::expression_t *node) override{};
    void visit(parser::binary_expression_t *node) override{};
    void visit(parser::array_index_expression_t *node) override{};
    void visit(parser::array_length_expression_t *node) override{};
    void visit(parser::method_call_expression_t *node) override{};
    void visit(parser::integer_literal_expression_t *node) override{};
    void visit(parser::true_literal_expression_t *node) override{};
    void visit(parser::false_literal_expression_t *node) override{};
    void visit(parser::identifier_expression_t *node) override{};
    void visit(parser::this_expression_t *node) override{};
    void visit(parser::new_integer_array_expression_t *node) override{};
    void visit(parser::new_object_expression_t *node) override{};
    void visit(parser::not_expression_t *node) override{};
    void visit(parser::parentheses_expression_t *node) override{};
};

class semantic_vis_accum_local_vars_t : public visitor::visitor_t {
public:
    program_symtbl_t *program_symtbl;
    semantic_vis_accum_local_vars_t(program_symtbl_t *program_symtbl)
      : program_symtbl(program_symtbl)
    {
    }
    void visit(parser::goal_t *node) override;
    void visit(parser::main_class_t *node) override{};
    void visit(parser::class_decl_t *node) override;
    void visit(parser::var_decl_t *node) override{};
    void visit(parser::method_decl_t *node) override{};
    void visit(parser::type_t *node) override{};
    void visit(parser::statement_t *node) override{};
    void visit(parser::block_statement_t *node) override{};
    void visit(parser::if_statement_t *node) override{};
    void visit(parser::while_statement_t *node) override{};
    void visit(parser::print_statement_t *node) override{};
    void visit(parser::assign_statement_t *node) override{};
    void visit(parser::array_assign_statement_t *node) override{};
    void visit(parser::expression_t *node) override{};
    void visit(parser::binary_expression_t *node) override{};
    void visit(parser::array_index_expression_t *node) override{};
    void visit(parser::array_length_expression_t *node) override{};
    void visit(parser::method_call_expression_t *node) override{};
    void visit(parser::integer_literal_expression_t *node) override{};
    void visit(parser::true_literal_expression_t *node) override{};
    void visit(parser::false_literal_expression_t *node) override{};
    void visit(parser::identifier_expression_t *node) override{};
    void visit(parser::this_expression_t *node) override{};
    void visit(parser::new_integer_array_expression_t *node) override{};
    void visit(parser::new_object_expression_t *node) override{};
    void visit(parser::not_expression_t *node) override{};
    void visit(parser::parentheses_expression_t *node) override{};
};

struct context_t {
    class_symtbl_t *current_class;
    method_symtbl_t *current_method;
    context_t() : current_class(nullptr), current_method(nullptr) {}
    context_t(class_symtbl_t *current_class, method_symtbl_t *current_method)
      : current_class(current_class), current_method(current_method)
    {
    }
};

class semantic_vis_type_check_t : public visitor::visitor_t {
public:
    program_symtbl_t *program_symtbl;
    context_t context;
    type_t *current_type;
    semantic_vis_type_check_t(program_symtbl_t *program_symtbl)
      : program_symtbl(program_symtbl), current_type(nullptr)
    {
    }
    void visit(parser::goal_t *node) override;
    void visit(parser::main_class_t *node) override;
    void visit(parser::class_decl_t *node) override;
    void visit(parser::var_decl_t *node) override;
    void visit(parser::method_decl_t *node) override;
    void visit(parser::type_t *node) override;
    void visit(parser::statement_t *node) override;
    void visit(parser::block_statement_t *node) override;
    void visit(parser::if_statement_t *node) override;
    void visit(parser::while_statement_t *node) override;
    void visit(parser::print_statement_t *node) override;
    void visit(parser::assign_statement_t *node) override;
    void visit(parser::array_assign_statement_t *node) override;
    void visit(parser::expression_t *node) override;
    void visit(parser::binary_expression_t *node) override;
    void visit(parser::array_index_expression_t *node) override;
    void visit(parser::array_length_expression_t *node) override;
    void visit(parser::method_call_expression_t *node) override;
    void visit(parser::integer_literal_expression_t *node) override;
    void visit(parser::true_literal_expression_t *node) override;
    void visit(parser::false_literal_expression_t *node) override;
    void visit(parser::identifier_expression_t *node) override;
    void visit(parser::this_expression_t *node) override;
    void visit(parser::new_integer_array_expression_t *node) override;
    void visit(parser::new_object_expression_t *node) override;
    void visit(parser::not_expression_t *node) override;
    void visit(parser::parentheses_expression_t *node) override;
    type_t *symbol_lookup__(const std::string &name, class_symtbl_t *class_symtbl,
                            method_symtbl_t *method_symtbl);
    type_t *symbol_lookup(const std::string &name);
};

} // namespace semantics