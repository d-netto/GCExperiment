#include <iostream>

#include <compiler.h>

// ============================================================================
// Bytecode compiler
// ============================================================================

namespace compiler {

size_t basic_block_t::bb_num = 0;
size_t basic_block_t::bb_inst_num = 0;

void basic_block_t::compute_jmp_targets_(size_t n, std::set<basic_block_t *> &visited)
{
    if (visited.find(this) != visited.end()) {
        return;
    }
    visited.insert(this);
    if (bb_id == n) {
        if (bb_state == bb_state_t::jmp_target_computed) {
            return;
        }
        bb_state = bb_state_t::jmp_target_computed;
        bb_inst_start = basic_block_t::bb_inst_num;
        basic_block_t::bb_inst_num += instructions.size();
    }
    else {
        if (then_branch != nullptr) {
            then_branch->compute_jmp_targets_(n, visited);
        }
        if (else_branch != nullptr) {
            else_branch->compute_jmp_targets_(n, visited);
        }
    }
}

void basic_block_t::compute_jmp_targets(size_t n)
{
    std::set<basic_block_t *> visited;
    compute_jmp_targets_(n, visited);
}

void basic_block_t::fix_goto_(size_t n, std::set<basic_block_t *> &visited)
{
    if (visited.find(this) != visited.end()) {
        return;
    }
    visited.insert(this);
    if (this->bb_id == n) {
        if (instructions.empty()) {
            return;
        }
        auto &i = instructions.back();
        if (i.op_code == bytecode::op_code_t::goto_) {
            i.operand = then_branch->bb_inst_start;
        }
        if (i.op_code == bytecode::op_code_t::goto_if_false_) {
            i.operand = else_branch->bb_inst_start;
        }
    }
    else {
        if (then_branch != nullptr) {
            then_branch->fix_goto_(n, visited);
        }
        if (else_branch != nullptr) {
            else_branch->fix_goto_(n, visited);
        }
    }
}

void basic_block_t::fix_goto(size_t n)
{
    std::set<basic_block_t *> visited;
    fix_goto_(n, visited);
}

void basic_block_t::try_linearize_(std::vector<bytecode::instruction_t> &vi, size_t n,
                                   std::set<basic_block_t *> &visited)
{
    if (visited.find(this) != visited.end()) {
        return;
    }
    visited.insert(this);
    if (bb_id == n) {
        if (bb_state == bb_state_t::linearized) {
            return;
        }
        bb_state = bb_state_t::linearized;
        vi.insert(vi.end(), instructions.begin(), instructions.end());
    }
    else {
        if (then_branch != nullptr) {
            then_branch->try_linearize_(vi, n, visited);
        }
        if (else_branch != nullptr) {
            else_branch->try_linearize_(vi, n, visited);
        }
    }
}

void basic_block_t::try_linearize(std::vector<bytecode::instruction_t> &vi, size_t n)
{
    std::set<basic_block_t *> visited;
    try_linearize_(vi, n, visited);
}

void layout_visitor_t::visit(parser::goal_t *node)
{
    node->main_class->accept(this);
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
}

void layout_visitor_t::visit(parser::main_class_t *node)
{
    current_class = node->class_name->name;
    classes.push_back(class_layout_t{std::string{""}, current_class, {}});
    methods.push_back(method_layout_t{std::make_pair(current_class, "main"), {}});
}

void layout_visitor_t::visit(parser::class_decl_t *node)
{
    current_class = node->class_name->name;
    std::string parent_class_name;
    if (node->parent_class_name != nullptr) {
        parent_class_name = node->parent_class_name->name;
    }
    // compute the parent class hierarchy
    std::vector<std::string> parent_class_hierarchy;
    while (parent_class_name != "") {
        parent_class_hierarchy.push_back(parent_class_name);
        auto it = std::find_if(classes.begin(), classes.end(),
                               [&parent_class_name](const class_layout_t &cl) {
                                   return cl.name == parent_class_name;
                               });
        if (it == classes.end()) {
            std::cerr << "error: parent class " << parent_class_name << " not found"
                      << std::endl;
            exit(1);
        }
        parent_class_name = it->parent;
    }
    std::reverse(parent_class_hierarchy.begin(), parent_class_hierarchy.end());
    classes.push_back(class_layout_t{parent_class_name, current_class, {}});
    auto &current_class_layout = classes.back();
    // add the fields of the parent classes
    for (auto &c : parent_class_hierarchy) {
        auto it = std::find_if(classes.begin(), classes.end(),
                               [&c](const class_layout_t &cl) { return cl.name == c; });
        current_class_layout.fields.insert(current_class_layout.fields.end(),
                                           it->fields.begin(), it->fields.end());
    }
    // add the fields of the current class
    for (auto &field_decl : node->field_decls) {
        current_class_layout.fields.push_back(field_decl->var_name->name);
    }
    for (auto &method_decl : node->method_decls) {
        method_decl->accept(this);
    }
}

void layout_visitor_t::visit(parser::method_decl_t *node)
{
    methods.push_back(
        method_layout_t{std::make_pair(current_class, node->method_name->name), {}});
}

void vt_visitor_t::visit(parser::goal_t *node)
{
    node->main_class->accept(this);
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
}

void vt_visitor_t::visit(parser::main_class_t *node)
{
    auto class_layout =
        std::find_if(classes.begin(), classes.end(), [&node](const class_layout_t &cl) {
            return cl.name == node->class_name->name;
        });
    if (class_layout == classes.end()) {
        std::cerr << "error: class " << node->class_name->name << " not found" << std::endl;
        exit(1);
    }
    auto method_layout =
        std::find_if(methods.begin(), methods.end(), [&node](const method_layout_t &ml) {
            return ml.method_name.first == node->class_name->name &&
                   ml.method_name.second == "main";
        });
    if (method_layout == methods.end()) {
        std::cerr << "error: method main not found" << std::endl;
        exit(1);
    }
    class_layout->vtbl.push_back(method_layout->method_name);
}

void vt_visitor_t::visit(parser::class_decl_t *node)
{
    std::vector<std::pair<std::string, std::string>> vtbl;
    std::vector<std::string> parent_class_hierarchy;
    auto name = node->parent_class_name != nullptr ? node->parent_class_name->name : "";
    while (name != "") {
        parent_class_hierarchy.push_back(name);
        auto it =
            std::find_if(classes.begin(), classes.end(),
                         [&name](const class_layout_t &cl) { return cl.name == name; });
        if (it == classes.end()) {
            std::cerr << "error: parent class " << name << " not found" << std::endl;
            exit(1);
        }
        name = it->parent;
    }
    std::reverse(parent_class_hierarchy.begin(), parent_class_hierarchy.end());
    for (auto &m : methods) {
        if (m.method_name.first == node->class_name->name ||
            std::find(parent_class_hierarchy.begin(), parent_class_hierarchy.end(),
                      m.method_name.first) != parent_class_hierarchy.end()) {
            auto invtbl = false;
            for (auto i = 0; i < vtbl.size(); ++i) {
                if (vtbl.at(i).second == m.method_name.second) {
                    invtbl = true;
                    vtbl.at(i) = m.method_name;
                    break;
                }
            }
            if (!invtbl) {
                vtbl.push_back(m.method_name);
            }
        }
    }
    auto class_layout =
        std::find_if(classes.begin(), classes.end(), [&node](const class_layout_t &cl) {
            return cl.name == node->class_name->name;
        });
    if (class_layout == classes.end()) {
        std::cerr << "error: class " << node->class_name->name << " not found" << std::endl;
        exit(1);
    }
    class_layout->vtbl = vtbl;
}

void bc_compiler_visitor_t::visit(parser::goal_t *node)
{
    auto bb = current_basic_block = new basic_block_t;
    node->main_class->accept(this);
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
    std::vector<bytecode::instruction_t> instructions;
    bb->compute_jmp_targets(0);
    // no need to fix the goto instructions for the main method
    bb->try_linearize(instructions, 0);
    methods.at(0).instructions.insert(methods.at(0).instructions.end(),
                                      instructions.begin(), instructions.end());
}

void bc_compiler_visitor_t::visit(parser::main_class_t *node)
{
    node->statement->accept(this);
    // put a return instruction at the end of the main method
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::return_, 0});
    current_method++;
}

void bc_compiler_visitor_t::visit(parser::class_decl_t *node)
{
    for (auto &method_decl : node->method_decls) {
        method_decl->accept(this);
        current_method++;
    }
}

void bc_compiler_visitor_t::visit(parser::var_decl_t *node)
{
    // Nothing to do here
}

void bc_compiler_visitor_t::visit(parser::method_decl_t *node)
{
    auto &current_method_layout = methods.at(current_method);
    auto bb = current_basic_block = new basic_block_t;
    for (auto &arg : node->arg_names) {
        current_method_layout.args.push_back(arg->name);
    }
    for (auto &var_decl : node->var_decls) {
        current_method_layout.locals.push_back(var_decl->var_name->name);
    }
    for (auto &statement : node->statements) {
        statement->accept(this);
    }
    node->return_expression->accept(this);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::return_, 0});
    std::vector<bytecode::instruction_t> instructions;
    for (size_t i = bb->bb_id; i < basic_block_t::bb_num; ++i) {
        bb->compute_jmp_targets(i);
    }
    for (size_t i = bb->bb_id; i < basic_block_t::bb_num; ++i) {
        bb->fix_goto(i);
    }
    for (size_t i = bb->bb_id; i < basic_block_t::bb_num; ++i) {
        bb->try_linearize(instructions, i);
    }
    current_method_layout.instructions.insert(current_method_layout.instructions.end(),
                                              instructions.begin(), instructions.end());
    basic_block_t::bb_inst_num = 0;
}

void bc_compiler_visitor_t::visit(parser::type_t *node)
{
    // Nothing to do here
}

void bc_compiler_visitor_t::visit(parser::statement_t *node)
{
    // Nothing to do here
}

void bc_compiler_visitor_t::visit(parser::block_statement_t *node)
{
    for (auto &statement : node->statements) {
        statement->accept(this);
    }
}

void bc_compiler_visitor_t::visit(parser::if_statement_t *node)
{
    node->condition->accept(this);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::goto_if_false_, 0});
    auto bb_cond = current_basic_block;
    auto bb_then_start = current_basic_block = new basic_block_t;
    node->then_statement->accept(this);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::goto_, 0});
    auto bb_then_end = current_basic_block;
    auto bb_else_start = current_basic_block = new basic_block_t;
    node->else_statement->accept(this);
    auto bb_else_end = current_basic_block;
    bb_cond->then_branch = bb_then_start;
    bb_cond->else_branch = bb_else_start;
    current_basic_block = new basic_block_t;
    bb_then_end->then_branch = current_basic_block;
    bb_else_end->then_branch = current_basic_block;
}

void bc_compiler_visitor_t::visit(parser::while_statement_t *node)
{
    auto bb_start = current_basic_block;
    current_basic_block = new basic_block_t;
    node->condition->accept(this);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::goto_if_false_, 0});
    auto bb_cond = current_basic_block;
    auto bb_statement_start = current_basic_block = new basic_block_t;
    node->statement->accept(this);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::goto_, 0});
    auto bb_statement_end = current_basic_block;
    current_basic_block = new basic_block_t;
    bb_start->then_branch = bb_cond;
    bb_cond->then_branch = bb_statement_start;
    bb_statement_end->then_branch = bb_cond;
    bb_cond->else_branch = current_basic_block;
}

void bc_compiler_visitor_t::visit(parser::print_statement_t *node)
{
    node->expression->accept(this);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::print_, 0});
}

void bc_compiler_visitor_t::visit(parser::assign_statement_t *node)
{
    node->expression->accept(this);
    auto class_name = methods.at(current_method).method_name.first;
    auto current_method_layout = methods.at(current_method);
    // try to find the variable in the arguments
    auto it = std::find_if(
        current_method_layout.args.begin(), current_method_layout.args.end(),
        [&node](const std::string &arg) { return arg == node->var_name->name; });
    if (it != current_method_layout.args.end()) {
        auto idx = std::distance(current_method_layout.args.begin(), it) +
                   1; // +1 because the first argument is the this pointer
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::store_, static_cast<long>(idx)});
        return;
    }
    // try to find the variable in the local variables
    it = std::find_if(
        current_method_layout.locals.begin(), current_method_layout.locals.end(),
        [&node](const std::string &local) { return local == node->var_name->name; });
    if (it != current_method_layout.locals.end()) {
        auto idx = std::distance(current_method_layout.locals.begin(), it) +
                   current_method_layout.args.size() +
                   1; // +1 because the first argument is the this pointer
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::store_, static_cast<long>(idx)});
        return;
    }
    // try to find the variable in the fields
    auto current_class_layout = std::find_if(classes.begin(), classes.end(),
                                             [&class_name](const class_layout_t &cl) {
                                                 return cl.name == class_name;
                                             });
    auto it2 = std::find_if(
        current_class_layout->fields.begin(), current_class_layout->fields.end(),
        [&node](const std::string &field) { return field == node->var_name->name; });
    if (it2 != current_class_layout->fields.end()) {
        auto idx = std::distance(current_class_layout->fields.begin(), it2);
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::load_, 0}); // load this pointer
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::putfield_, idx});
        return;
    }
    std::cerr << "error: variable " << node->var_name->name << " not found" << std::endl;
    exit(1);
}

void bc_compiler_visitor_t::visit(parser::array_assign_statement_t *node)
{
    node->index_expression->accept(this);
    node->expression->accept(this);
    auto class_name = methods.at(current_method).method_name.first;
    auto current_method_layout = methods.at(current_method);
    // try to find the variable in the arguments
    auto it = std::find_if(
        current_method_layout.args.begin(), current_method_layout.args.end(),
        [&node](const std::string &arg) { return arg == node->var_name->name; });
    if (it != current_method_layout.args.end()) {
        auto idx = std::distance(current_method_layout.args.begin(), it) +
                   1; // +1 because the first argument is the this pointer
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::iastore_, static_cast<long>(idx)});
        return;
    }
    // try to find the variable in the local variables
    it = std::find_if(
        current_method_layout.locals.begin(), current_method_layout.locals.end(),
        [&node](const std::string &local) { return local == node->var_name->name; });
    if (it != current_method_layout.locals.end()) {
        auto idx = std::distance(current_method_layout.locals.begin(), it) +
                   current_method_layout.args.size() +
                   1; // +1 because the first argument is the this pointer
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::iastore_, static_cast<long>(idx)});
        return;
    }
    // try to find the variable in the fields
    auto current_class_layout = std::find_if(classes.begin(), classes.end(),
                                             [&class_name](const class_layout_t &cl) {
                                                 return cl.name == class_name;
                                             });
    auto it2 = std::find_if(
        current_class_layout->fields.begin(), current_class_layout->fields.end(),
        [&node](const std::string &field) { return field == node->var_name->name; });
    if (it2 != current_class_layout->fields.end()) {
        auto idx = std::distance(current_class_layout->fields.begin(), it2);
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::load_, 0}); // load this pointer
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::getfield_, idx});
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::iastore_, 0});
        return;
    }
    std::cerr << "error: variable " << node->var_name->name << " not found" << std::endl;
    exit(1);
}

void bc_compiler_visitor_t::visit(parser::expression_t *node)
{
    // Nothing to do here
}

void bc_compiler_visitor_t::visit(parser::binary_expression_t *node)
{
    node->left->accept(this);
    node->right->accept(this);
    switch (node->op) {
    case parser::binary_operator_t::plus_:
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::iadd_, 0});
        break;
    case parser::binary_operator_t::minus_:
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::isub_, 0});
        break;
    case parser::binary_operator_t::times_:
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::imul_, 0});
        break;
    case parser::binary_operator_t::less_:
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::ilt_, 0});
        break;
    case parser::binary_operator_t::and_:
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::band_, 0});
        break;
    }
}

void bc_compiler_visitor_t::visit(parser::array_index_expression_t *node)
{
    node->index_expression->accept(this);
    node->array_expression->accept(this);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::iaload_, 0});
}

void bc_compiler_visitor_t::visit(parser::array_length_expression_t *node)
{
    node->array_expression->accept(this);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::length_, 0});
}

void bc_compiler_visitor_t::visit(parser::method_call_expression_t *node)
{
    node->object_expression->accept(this);
    for (auto &arg : node->arg_expressions) {
        arg->accept(this);
    }
    long nargs = node->arg_expressions.size() +
                 1; // +1 because the first argument is the this pointer
    long m_id = -1;
    auto class_name = methods.at(current_method).method_name.first;
    auto current_method_name = methods.at(current_method).method_name.second;
    auto method_to_call = node->method_name->name;
    // try to find type of the object expression
    auto class_symtbl = type_checker.program_symtbl->classes.at(class_name);
    auto it = std::find_if(class_symtbl->methods.begin(), class_symtbl->methods.end(),
                           [current_method_name](const semantics::method_symtbl_t *ms) {
                               return ms->name == current_method_name;
                           });
    if (it == class_symtbl->methods.end()) {
        std::cerr << "error: method " << current_method_name << " not found" << std::endl;
        exit(1);
    }
    auto method_symtbl = *it;
    {
        auto context = type_checker.context;
        type_checker.context = {class_symtbl, method_symtbl};
        node->object_expression->accept(&type_checker);
        type_checker.context = context;
    }
    auto type_as_str = type_checker.current_type->as_str();
    // try to find the method in the corresponding class
    auto class_layout = std::find_if(classes.begin(), classes.end(),
                                     [&type_as_str](const class_layout_t &cl) {
                                         return cl.name == type_as_str;
                                     });
    if (class_layout != classes.end()) {
        auto it = std::find_if(
            class_layout->vtbl.begin(), class_layout->vtbl.end(),
            [&method_to_call](std::pair<std::string, std::string> &method_name) {
                return method_name.second == method_to_call;
            });
        if (it != class_layout->vtbl.end()) {
            m_id = std::distance(class_layout->vtbl.begin(), it);
        }
    }
    if (m_id == -1) {
        std::cerr << "error: method " << node->method_name->name << " not found"
                  << std::endl;
        exit(1);
    }
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::invoke_, m_id, nargs});
}

void bc_compiler_visitor_t::visit(parser::integer_literal_expression_t *node)
{
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::ldc_, node->value});
}

void bc_compiler_visitor_t::visit(parser::true_literal_expression_t *node)
{
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::ldc_, 1});
}

void bc_compiler_visitor_t::visit(parser::false_literal_expression_t *node)
{
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::ldc_, 0});
}

void bc_compiler_visitor_t::visit(parser::identifier_expression_t *node)
{
    auto class_name = methods.at(current_method).method_name.first;
    auto current_method_layout = methods.at(current_method);
    // try to find the variable in the arguments
    auto it = std::find_if(
        current_method_layout.args.begin(), current_method_layout.args.end(),
        [&node](const std::string &arg) { return arg == node->identifier->name; });
    if (it != current_method_layout.args.end()) {
        auto idx = std::distance(current_method_layout.args.begin(), it) +
                   1; // +1 because the first argument is the this pointer
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::load_, static_cast<long>(idx)});
        return;
    }
    // try to find the variable in the local variables
    it = std::find_if(
        current_method_layout.locals.begin(), current_method_layout.locals.end(),
        [&node](const std::string &local) { return local == node->identifier->name; });
    if (it != current_method_layout.locals.end()) {
        auto idx = std::distance(current_method_layout.locals.begin(), it) +
                   current_method_layout.args.size() +
                   1; // +1 because the first argument is the this pointer
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::load_, static_cast<long>(idx)});
        return;
    }
    // try to find the variable in the fields
    auto current_class_layout = std::find_if(classes.begin(), classes.end(),
                                             [&class_name](const class_layout_t &cl) {
                                                 return cl.name == class_name;
                                             });
    auto it2 = std::find_if(
        current_class_layout->fields.begin(), current_class_layout->fields.end(),
        [&node](const std::string &field) { return field == node->identifier->name; });
    if (it2 != current_class_layout->fields.end()) {
        auto idx = std::distance(current_class_layout->fields.begin(), it2);
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::load_, 0}); // load this pointer
        current_basic_block->instructions.push_back(
            bytecode::instruction_t{bytecode::op_code_t::getfield_, idx});
        return;
    }
}

void bc_compiler_visitor_t::visit(parser::this_expression_t *node)
{
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::load_, 0});
}

void bc_compiler_visitor_t::visit(parser::new_integer_array_expression_t *node)
{
    node->size_expression->accept(this);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::newarray_, 0});
}

void bc_compiler_visitor_t::visit(parser::new_object_expression_t *node)
{
    auto class_name = node->class_name->name;
    auto current_class_layout = std::find_if(classes.begin(), classes.end(),
                                             [&class_name](const class_layout_t &cl) {
                                                 return cl.name == class_name;
                                             });
    long idx = std::distance(classes.begin(), current_class_layout);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::new_, idx});
}

void bc_compiler_visitor_t::visit(parser::not_expression_t *node)
{
    node->expression->accept(this);
    current_basic_block->instructions.push_back(
        bytecode::instruction_t{bytecode::op_code_t::bneg_, 0});
}

void bc_compiler_visitor_t::visit(parser::parentheses_expression_t *node)
{
    node->expression->accept(this);
}

void bc_compiler_visitor_t::print()
{
    for (auto &method : methods) {
        std::cout << "method " << method.method_name.first << "."
                  << method.method_name.second << std::endl;
        for (auto &arg : method.args) {
            std::cout << "  arg  " << arg << std::endl;
        }
        for (auto &local : method.locals) {
            std::cout << "  local " << local << std::endl;
        }
        for (auto &instruction : method.instructions) {
            std::cout << "        " << instruction.as_str() << std::endl;
        }
        std::cout << std::endl;
    }
}

} // namespace compiler