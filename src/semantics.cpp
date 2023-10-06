#include <iostream>

#include <semantics.h>

// ============================================================================
// Semantic checker
// ============================================================================

namespace semantics {

integer_t *integer_type = new integer_t();
boolean_t *boolean_type = new boolean_t();
array_t *array_type = new array_t();

bool class_symtbl_t::is_subtype(class_symtbl_t *other)
{
    if (this == other) {
        return true;
    }
    if (parent_class == nullptr) {
        return false;
    }
    return parent_class->is_subtype(other);
}

type_t *program_symtbl_t::str_to_type(const std::string &name)
{
    type_t *type = nullptr;
    if (name == "int") {
        type = integer_type;
    }
    else if (name == "boolean") {
        type = boolean_type;
    }
    else if (name == "int[]") {
        type = array_type;
    }
    else {
        type = classes.at(name);
    }
    return type;
}

////////////////////////////////////////

void semantic_vis_accum_classes_t::visit(parser::goal_t *node)
{
    program_symtbl = new program_symtbl_t();
    node->main_class->accept(this);
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
}

void semantic_vis_accum_classes_t::visit(parser::main_class_t *node)
{
    auto class_symtbl = new class_symtbl_t();
    class_symtbl->name = node->class_name->name;
    class_symtbl->fields = {};
    class_symtbl->methods = {new method_symtbl_t("main", {}, {}, integer_type)};
    program_symtbl->classes[class_symtbl->name] = class_symtbl;
}

void semantic_vis_accum_classes_t::visit(parser::class_decl_t *node)
{
    auto class_symtbl = new class_symtbl_t();
    class_symtbl->parent_class = nullptr;
    class_symtbl->name = node->class_name->name;
    class_symtbl->fields = {};
    class_symtbl->methods = {};
    program_symtbl->classes[class_symtbl->name] = class_symtbl;
}

////////////////////////////////////////

void semantic_vis_accum_parent_classes_t::visit(parser::goal_t *node)
{
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
}

void semantic_vis_accum_parent_classes_t::visit(parser::class_decl_t *node)
{
    if (node->parent_class_name != nullptr) {
        program_symtbl->classes[node->class_name->name]->parent_class =
            program_symtbl->classes.at(node->parent_class_name->name);
    }
}

////////////////////////////////////////

void semantic_vis_accum_fields_t::visit(parser::goal_t *node)
{
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
}

void semantic_vis_accum_fields_t::visit(parser::class_decl_t *node)
{
    auto class_symtbl = program_symtbl->classes.at(node->class_name->name);
    for (auto &field : node->field_decls) {
        auto &type_name = field->type->type_name->name;
        type_t *type = program_symtbl->str_to_type(type_name);
        class_symtbl->fields[field->var_name->name] = type;
    }
}

////////////////////////////////////////

void semantic_vis_accum_local_vars_t::visit(parser::goal_t *node)
{
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
}

void semantic_vis_accum_local_vars_t::visit(parser::class_decl_t *node)
{
    for (auto &method_decl : node->method_decls) {
        auto ms = new method_symtbl_t();
        ms->name = method_decl->method_name->name;
        // Method arguments
        for (auto i = 0; i < method_decl->arg_names.size(); i++) {
            auto &type_name = method_decl->arg_types.at(i)->type_name->name;
            type_t *type = program_symtbl->str_to_type(type_name);
            ms->params.push_back({method_decl->arg_names.at(i)->name, type});
        }
        // Method local variables
        for (auto &var_decl : method_decl->var_decls) {
            auto &type_name = var_decl->type->type_name->name;
            type_t *type = program_symtbl->str_to_type(type_name);
            ms->local_vars[var_decl->var_name->name] = type;
        }
        // Return type
        auto &type_name = method_decl->return_type->type_name->name;
        ms->return_type = program_symtbl->str_to_type(type_name);
        // Add method to class symbol table
        auto class_symtbl = program_symtbl->classes.at(node->class_name->name);
        class_symtbl->methods.push_back(ms);
    }
}

////////////////////////////////////////

void semantic_vis_type_check_t::visit(parser::goal_t *node)
{
    node->main_class->accept(this);
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
}

void semantic_vis_type_check_t::visit(parser::main_class_t *node)
{
    context.current_class = program_symtbl->classes.at(node->class_name->name);
    node->statement->accept(this);
}

void semantic_vis_type_check_t::visit(parser::class_decl_t *node)
{
    context.current_class = program_symtbl->classes.at(node->class_name->name);
    for (auto &method_decl : node->method_decls) {
        method_decl->accept(this);
    }
}

void semantic_vis_type_check_t::visit(parser::var_decl_t *node)
{
    // Do nothing
}

void semantic_vis_type_check_t::visit(parser::method_decl_t *node)
{
    context.current_method = nullptr;
    for (auto i = 0; i < context.current_class->methods.size(); i++) {
        if (context.current_class->methods.at(i)->name == node->method_name->name) {
            context.current_method = context.current_class->methods.at(i);
            break;
        }
    }
    if (context.current_method == nullptr) {
        std::cerr << "Method not found" << std::endl;
        exit(1);
    }
    for (auto &statement : node->statements) {
        statement->accept(this);
    }
    node->return_expression->accept(this);
    if (current_type != context.current_method->return_type) {
        std::cerr << "Return expression must be of type "
                  << context.current_method->return_type->as_str() << std::endl;
        exit(1);
    }
}

void semantic_vis_type_check_t::visit(parser::type_t *node)
{
    // Do nothing
}

void semantic_vis_type_check_t::visit(parser::statement_t *node)
{
    std::cerr << "Not implemented" << std::endl;
    exit(1);
}

void semantic_vis_type_check_t::visit(parser::block_statement_t *node)
{
    for (auto &statement : node->statements) {
        statement->accept(this);
    }
}

void semantic_vis_type_check_t::visit(parser::if_statement_t *node)
{
    node->condition->accept(this);
    if (current_type != boolean_type) {
        std::cerr << "If statement condition must be of type boolean" << std::endl;
        exit(1);
    }
    node->then_statement->accept(this);
    node->else_statement->accept(this);
}

void semantic_vis_type_check_t::visit(parser::while_statement_t *node)
{
    node->condition->accept(this);
    if (current_type != boolean_type) {
        std::cerr << "While statement condition must be of type boolean" << std::endl;
        exit(1);
    }
    node->statement->accept(this);
}

void semantic_vis_type_check_t::visit(parser::print_statement_t *node)
{
    node->expression->accept(this);
    if (current_type != integer_type) {
        std::cerr << "Print statement expression must be of type integer" << std::endl;
        exit(1);
    }
}

void semantic_vis_type_check_t::visit(parser::assign_statement_t *node)
{
    node->expression->accept(this);
    if (current_type != symbol_lookup(node->var_name->name)) {
        std::cerr << "Assign statement expression must be of type " << node->var_name->name
                  << std::endl;
        exit(1);
    }
}

void semantic_vis_type_check_t::visit(parser::array_assign_statement_t *node)
{
    current_type = symbol_lookup(node->var_name->name);
    if (current_type != array_type) {
        std::cerr << "Array assign statement array must be of type array" << std::endl;
        exit(1);
    }
    node->index_expression->accept(this);
    if (current_type != integer_type) {
        std::cerr << "Array assign statement index expression must be of type integer"
                  << std::endl;
        exit(1);
    }
    node->expression->accept(this);
    if (current_type != integer_type) {
        std::cerr << "Array assign statement expression must be of type integer"
                  << std::endl;
        exit(1);
    }
}

void semantic_vis_type_check_t::visit(parser::expression_t *node)
{
    std::cerr << "Not implemented" << std::endl;
    exit(1);
}

void semantic_vis_type_check_t::visit(parser::binary_expression_t *node)
{
    node->left->accept(this);
    auto left_type = current_type;
    node->right->accept(this);
    auto right_type = current_type;
    if (left_type != right_type) {
        std::cerr << "Binary expression types must match" << std::endl;
        exit(1);
    }
    switch (node->op) {
    case parser::binary_operator_t::plus_:
    case parser::binary_operator_t::minus_:
    case parser::binary_operator_t::times_:
        if (left_type != integer_type) {
            std::cerr << "Binary expression operands must be of type integer" << std::endl;
            exit(1);
        }
        current_type = integer_type;
        break;
    case parser::binary_operator_t::less_:
        if (left_type != integer_type) {
            std::cerr << "Binary expression operands must be of type integer" << std::endl;
            exit(1);
        }
        current_type = boolean_type;
        break;
    case parser::binary_operator_t::and_:
        if (left_type != boolean_type) {
            std::cerr << "Binary expression operands must be of type boolean" << std::endl;
            exit(1);
        }
        current_type = boolean_type;
        break;
    default: std::cerr << "Not implemented" << std::endl; exit(1);
    }
}

void semantic_vis_type_check_t::visit(parser::array_index_expression_t *node)
{
    node->array_expression->accept(this);
    if (current_type != array_type) {
        std::cerr << "Array index expression array must be of type array" << std::endl;
        exit(1);
    }
    node->index_expression->accept(this);
    if (current_type != integer_type) {
        std::cerr << "Array index expression index must be of type integer" << std::endl;
        exit(1);
    }
    current_type = integer_type;
}

void semantic_vis_type_check_t::visit(parser::array_length_expression_t *node)
{
    node->array_expression->accept(this);
    if (current_type != array_type) {
        std::cerr << "Array length expression array must be of type array" << std::endl;
        exit(1);
    }
    current_type = integer_type;
}

void semantic_vis_type_check_t::visit(parser::method_call_expression_t *node)
{
    node->object_expression->accept(this);
    auto object_type = current_type;
    auto object_type_as_class_type = dynamic_cast<class_symtbl_t *>(object_type);
    if (object_type_as_class_type == nullptr) {
        std::cerr << "Method call expression object must be of type class" << std::endl;
        exit(1);
    }
    auto class_symtbl = object_type_as_class_type;
    method_symtbl_t *method_symtbl = nullptr;
    for (auto &method_symtbl_ : class_symtbl->methods) {
        if (method_symtbl_->name == node->method_name->name) {
            method_symtbl = method_symtbl_;
            break;
        }
    }
    if (method_symtbl == nullptr) {
        std::cerr << "Method call expression method not found" << std::endl;
        exit(1);
    }
    if (method_symtbl->params.size() != node->arg_expressions.size()) {
        std::cerr << "Method call expression argument count must match" << std::endl;
        exit(1);
    }
    for (auto i = 0; i < method_symtbl->params.size(); i++) {
        node->arg_expressions.at(i)->accept(this);
        auto current_type_as_class_type = dynamic_cast<class_symtbl_t *>(current_type);
        if (current_type_as_class_type != nullptr) {
            auto parami_as_class_type =
                dynamic_cast<class_symtbl_t *>(method_symtbl->params.at(i).second);
            if (parami_as_class_type == nullptr) {
                std::cerr << "Method call expression argument type must match" << std::endl;
                exit(1);
            }
            if (!current_type_as_class_type->is_subtype(parami_as_class_type)) {
                std::cerr << "Method call expression argument type must match" << std::endl;
                exit(1);
            }
        }
        else {
            if (current_type != method_symtbl->params.at(i).second) {
                std::cerr << "Method call expression argument type must match" << std::endl;
                exit(1);
            }
        }
    }
    current_type = method_symtbl->return_type;
}

void semantic_vis_type_check_t::visit(parser::integer_literal_expression_t *node)
{
    current_type = integer_type;
}

void semantic_vis_type_check_t::visit(parser::true_literal_expression_t *node)
{
    current_type = boolean_type;
}

void semantic_vis_type_check_t::visit(parser::false_literal_expression_t *node)
{
    current_type = boolean_type;
}

void semantic_vis_type_check_t::visit(parser::identifier_expression_t *node)
{
    current_type = symbol_lookup(node->identifier->name);
}

void semantic_vis_type_check_t::visit(parser::this_expression_t *node)
{
    current_type = context.current_class;
}

void semantic_vis_type_check_t::visit(parser::new_integer_array_expression_t *node)
{
    node->size_expression->accept(this);
    if (current_type != integer_type) {
        std::cerr << "New integer array expression size must be of type integer"
                  << std::endl;
        exit(1);
    }
    current_type = array_type;
}

void semantic_vis_type_check_t::visit(parser::new_object_expression_t *node)
{
    current_type = program_symtbl->str_to_type(node->class_name->name);
}

void semantic_vis_type_check_t::visit(parser::not_expression_t *node)
{
    node->expression->accept(this);
    if (current_type != boolean_type) {
        std::cerr << "Not expression must be of type boolean" << std::endl;
        exit(1);
    }
    current_type = boolean_type;
}

void semantic_vis_type_check_t::visit(parser::parentheses_expression_t *node)
{
    node->expression->accept(this);
}

type_t *semantic_vis_type_check_t::symbol_lookup__(const std::string &name,
                                                   class_symtbl_t *class_symtbl,
                                                   method_symtbl_t *method_symtbl)
{
    type_t *type = nullptr;
    // primitive types lookup
    if (name == "int") {
        type = integer_type;
    }
    else if (name == "boolean") {
        type = boolean_type;
    }
    else if (name == "int[]") {
        type = array_type;
    }
    if (type != nullptr) {
        return type;
    }
    for (auto &local_var : method_symtbl->local_vars) {
        if (local_var.first == name) {
            type = local_var.second;
            break;
        }
    }
    if (type == nullptr) {
        for (auto &param : method_symtbl->params) {
            if (param.first == name) {
                type = param.second;
                break;
            }
        }
    }
    if (type == nullptr) {
        for (auto &field : class_symtbl->fields) {
            if (field.first == name) {
                type = field.second;
                break;
            }
        }
    }
    return type;
}

type_t *semantic_vis_type_check_t::symbol_lookup(const std::string &name)
{
    type_t *type = nullptr;
    type = symbol_lookup__(name, context.current_class, context.current_method);
    if (type == nullptr) {
        class_symtbl_t *parent_class = context.current_class->parent_class;
        while (parent_class != nullptr) {
            type = symbol_lookup__(name, parent_class, context.current_method);
            if (type != nullptr) {
                break;
            }
            parent_class = parent_class->parent_class;
        }
    }
    if (type == nullptr) {
        std::cerr << "Symbol " << name << " not found" << std::endl;
        exit(1);
    }
    return type;
}

void program_symtbl_t::print()
{
    for (auto &class_symtbl : classes) {
        std::cout << "class " << class_symtbl.first << std::endl;
        for (auto &field : class_symtbl.second->fields) {
            std::cout << "    field " << field.first << " " << field.second->as_str()
                      << std::endl;
        }
        for (auto &method_symtbl : class_symtbl.second->methods) {
            std::cout << "    method " << method_symtbl->name << std::endl;
            for (auto &param : method_symtbl->params) {
                std::cout << "        param " << param.first << " "
                          << param.second->as_str() << std::endl;
            }
            for (auto &local_var : method_symtbl->local_vars) {
                std::cout << "        local_var " << local_var.first << " "
                          << local_var.second->as_str() << std::endl;
            }
            std::cout << "        return_type " << method_symtbl->return_type->as_str()
                      << std::endl;
        }
    }
}

} // namespace semantics