#include <iostream>

#include <compiler.h>

// ============================================================================
// LLVM compiler
// ============================================================================

namespace compiler {

// #define LLVM_COMPILER_LOG_ENABLED

static void llvm_compiler_log(std::string msg)
{
#ifdef LLVM_COMPILER_LOG_ENABLED
    std::cerr << "llvm_compiler: " << msg << std::endl;
#endif
}

void llvm_object_layout_visitor_t::visit(parser::goal_t *node)
{
    llvm_compiler_log("llvm_object_layout_visitor_t::visit(parser::goal_t *node)");
    node->main_class->accept(this);
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
}

void llvm_object_layout_visitor_t::visit(parser::main_class_t *node)
{
    llvm_compiler_log("llvm_object_layout_visitor_t::visit(parser::main_class_t *node)");
    current_class = node->class_name->name;
    llvm::StructType::create(
        *llvm_state.context,
        {llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(*llvm_state.context))},
        current_class);
    llvm::StructType::create(
        *llvm_state.context,
        {llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(*llvm_state.context)),
         llvm::Type::getInt64Ty(*llvm_state.context),
         llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(*llvm_state.context))},
        "array");
    auto main_class_layout = llvm_class_layout_t{node->class_name->name, {}, {}};
    classes.push_back(main_class_layout);
}

void llvm_object_layout_visitor_t::visit(parser::class_decl_t *node)
{
    llvm_compiler_log("llvm_object_layout_visitor_t::visit(parser::class_decl_t *node)");
    current_class = node->class_name->name;
    auto class_layout = llvm_class_layout_t{current_class};
    classes.push_back(class_layout);
    // compute the parent class hierarchy
    std::string parent_class_name;
    if (node->parent_class_name != nullptr) {
        parent_class_name = node->parent_class_name->name;
    }
    auto &fields_str = classes.back().fields;
    // add the fields of the parent class
    if (parent_class_name != "") {
        auto it2 = std::find_if(classes.begin(), classes.end(),
                                [&parent_class_name](const llvm_class_layout_t &cl) {
                                    return cl.name == parent_class_name;
                                });
        if (it2 == classes.end()) {
            std::cerr << "error: class " << parent_class_name << " not found" << std::endl;
            exit(1);
        }
        auto &parent_class = *it2;
        for (auto &field : parent_class.fields) {
            fields_str.push_back(field);
        }
    }
    // add the fields of the current class
    for (auto &field_decl : node->field_decls) {
        fields_str.push_back(
            std::make_pair(field_decl->type->type_name->name, field_decl->var_name->name));
    }
    // convert the fields to types
    std::vector<llvm::Type *> fields;
    auto struct_ty = llvm::StructType::create(*llvm_state.context, node->class_name->name);
    for (auto &f_str : fields_str) {
        auto type = type_checker.program_symtbl->str_to_type(f_str.first);
        if (type == semantics::integer_type) {
            fields.push_back(llvm::Type::getInt64Ty(*llvm_state.context));
        }
        else if (type == semantics::boolean_type) {
            fields.push_back(llvm::Type::getInt1Ty(*llvm_state.context));
        }
        else if (type == semantics::array_type) {
            fields.push_back(
                llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(*llvm_state.context)));
        }
        else {
            auto ty = llvm::StructType::getTypeByName(*llvm_state.context, f_str.first);
            if (ty == nullptr) {
                std::cerr << "error: type " << f_str.first << " not found" << std::endl;
                exit(1);
            }
            auto ty_ptr = llvm::PointerType::getUnqual(ty);
            fields.push_back(ty_ptr);
        }
    }
    struct_ty->setBody(fields);
}

void llvm_vt_visitor_t::visit(parser::goal_t *node)
{
    llvm_compiler_log("llvm_vt_visitor_t::visit(parser::goal_t *node)");
    node->main_class->accept(this);
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
}

void llvm_vt_visitor_t::visit(parser::main_class_t *node)
{
    llvm_compiler_log("llvm_vt_visitor_t::visit(parser::main_class_t *node)");
    auto it =
        std::find_if(classes.begin(), classes.end(), [node](const llvm_class_layout_t &cl) {
            return cl.name == node->class_name->name;
        });
    if (it == classes.end()) {
        std::cerr << "error: class " << node->class_name->name << " not found" << std::endl;
        exit(1);
    }
    auto &class_layout = *it;
    auto method_layout = new llvm_method_layout_t{
        {node->class_name->name, "main"},
        {type_checker.program_symtbl->str_to_type(node->class_name->name)}};
    class_layout.vtable.push_back(method_layout);
}

void llvm_vt_visitor_t::visit(parser::class_decl_t *node)
{
    llvm_compiler_log("llvm_vt_visitor_t::visit(parser::class_decl_t *node)");
    auto it =
        std::find_if(classes.begin(), classes.end(), [node](const llvm_class_layout_t &cl) {
            return cl.name == node->class_name->name;
        });
    if (it == classes.end()) {
        std::cerr << "error: class " << node->class_name->name << " not found" << std::endl;
        exit(1);
    }
    auto &class_layout = *it;
    std::string parent_class_name;
    if (node->parent_class_name != nullptr) {
        parent_class_name = node->parent_class_name->name;
    }
    // add the methods of the parent class
    if (parent_class_name != "") {
        auto it2 = std::find_if(classes.begin(), classes.end(),
                                [&parent_class_name](const llvm_class_layout_t &cl) {
                                    return cl.name == parent_class_name;
                                });
        if (it2 == classes.end()) {
            std::cerr << "error: class " << parent_class_name << " not found" << std::endl;
            exit(1);
        }
        auto &parent_class = *it2;
        for (auto &method : parent_class.vtable) {
            class_layout.vtable.push_back(method);
        }
    }
    // add the methods of the current class
    for (auto &method_decl : node->method_decls) {
        int vtable_idx = -1;
        for (auto &method : class_layout.vtable) {
            if (method->method_name.second == method_decl->method_name->name) {
                vtable_idx = std::distance(class_layout.vtable.begin(),
                                           std::find(class_layout.vtable.begin(),
                                                     class_layout.vtable.end(), method));
                break;
            }
        }
        if (vtable_idx == -1) {
            auto method_layout = new llvm_method_layout_t{};
            method_layout->method_name.first = node->class_name->name;
            method_layout->method_name.second = method_decl->method_name->name;
            for (auto &arg_type : method_decl->arg_types) {
                method_layout->signature.push_back(
                    type_checker.program_symtbl->str_to_type(arg_type->type_name->name));
            }
            class_layout.vtable.push_back(method_layout);
        }
        else {
            auto &method_layout = class_layout.vtable[vtable_idx];
            method_layout->method_name.first = node->class_name->name;
        }
    }
}

void llvm_compiler_visitor_t::visit(parser::goal_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::goal_t *node)");
    // signature for libc functions
    auto printf_type = llvm::FunctionType::get(
        llvm::IntegerType::getInt64Ty(*llvm_state.context),
        {llvm::PointerType::getUnqual(llvm::IntegerType::getInt8Ty(*llvm_state.context))},
        true);
    llvm::Function::Create(printf_type, llvm::Function::ExternalLinkage, "printf",
                           llvm_state.module);
    auto calloc_type = llvm::FunctionType::get(
        llvm::PointerType::getUnqual(llvm::IntegerType::getInt8Ty(*llvm_state.context)),
        {llvm::IntegerType::getInt64Ty(*llvm_state.context),
         llvm::IntegerType::getInt64Ty(*llvm_state.context)},
        false);
    llvm::Function::Create(calloc_type, llvm::Function::ExternalLinkage, "calloc",
                           llvm_state.module);
    auto free_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_state.context),
        {llvm::PointerType::getUnqual(llvm::IntegerType::getInt8Ty(*llvm_state.context))},
        false);
    llvm::Function::Create(free_type, llvm::Function::ExternalLinkage, "free",
                           llvm_state.module);
    // signature for main function
    auto main_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*llvm_state.context), {}, false);
    llvm::Function::Create(main_type, llvm::Function::ExternalLinkage, "main",
                           llvm_state.module);
    // signature for minijava functions
    for (auto &class_decl : node->class_decls) {
        for (auto &method_decl : class_decl->method_decls) {
            auto method_name =
                class_decl->class_name->name + "$" + method_decl->method_name->name;
            std::vector<llvm::Type *> args;
            args.push_back(llvm::PointerType::getUnqual(llvm::StructType::getTypeByName(
                *llvm_state.context, class_decl->class_name->name)));
            for (auto &arg_ty : method_decl->arg_types) {
                auto arg_type = arg_ty->type_name->name;
                auto type = type_checker.program_symtbl->str_to_type(arg_type);
                if (type == semantics::integer_type) {
                    args.push_back(llvm::Type::getInt64Ty(*llvm_state.context));
                }
                else if (type == semantics::boolean_type) {
                    args.push_back(llvm::Type::getInt1Ty(*llvm_state.context));
                }
                else if (type == semantics::array_type) {
                    args.push_back(llvm::PointerType::getUnqual(
                        llvm::StructType::getTypeByName(*llvm_state.context, "array")));
                }
                else {
                    auto ty =
                        llvm::StructType::getTypeByName(*llvm_state.context, arg_type);
                    auto ty_ptr = llvm::PointerType::getUnqual(ty);
                    args.push_back(ty_ptr);
                }
            }
            auto ret_type = type_checker.program_symtbl->str_to_type(
                method_decl->return_type->type_name->name);
            if (ret_type == semantics::integer_type) {
                auto function_type = llvm::FunctionType::get(
                    llvm::Type::getInt64Ty(*llvm_state.context), args, false);
                llvm::Function::Create(function_type, llvm::Function::ExternalLinkage,
                                       method_name, llvm_state.module);
            }
            else if (ret_type == semantics::boolean_type) {
                auto function_type = llvm::FunctionType::get(
                    llvm::Type::getInt1Ty(*llvm_state.context), args, false);
                llvm::Function::Create(function_type, llvm::Function::ExternalLinkage,
                                       method_name, llvm_state.module);
            }
            else if (ret_type == semantics::array_type) {
                auto function_type = llvm::FunctionType::get(
                    llvm::PointerType::getUnqual(
                        llvm::StructType::getTypeByName(*llvm_state.context, "array")),
                    args, false);
                llvm::Function::Create(function_type, llvm::Function::ExternalLinkage,
                                       method_name, llvm_state.module);
            }
            else {
                auto ty = llvm::StructType::getTypeByName(
                    *llvm_state.context, method_decl->return_type->type_name->name);
                auto ty_ptr = llvm::PointerType::getUnqual(ty);
                auto function_type = llvm::FunctionType::get(ty_ptr, args, false);
                llvm::Function::Create(function_type, llvm::Function::ExternalLinkage,
                                       method_name, llvm_state.module);
            }
        }
        // emit the vtable as a LLVM global variable
        auto class_name = class_decl->class_name->name;
        auto vtable_ty = llvm::StructType::create(*llvm_state.context,
                                                  class_decl->class_name->name + "_vtable");
        auto it = std::find_if(classes.begin(), classes.end(),
                               [class_name](const llvm_class_layout_t &cl) {
                                   return cl.name == class_name;
                               });
        if (it == classes.end()) {
            std::cerr << "error: class " << class_name << " not found" << std::endl;
            exit(1);
        }
        auto &class_layout = *it;
        // define a constant struct to hold the vtable
        std::vector<llvm::Type *> vtable_fields_ty;
        std::vector<llvm::Constant *> vtable_fields;
        for (auto &method : class_layout.vtable) {
            // lookup LLVM function named <class_name>$<method_name>
            auto method_name = class_name + "$" + method->method_name.second;
            auto method_function = llvm_state.module->getFunction(method_name);
            if (method_function == nullptr) {
                std::cerr << "error: method " << method_name << " not found" << std::endl;
                exit(1);
            }
            vtable_fields_ty.push_back(method_function->getType());
            vtable_fields.push_back(llvm::ConstantExpr::getBitCast(
                method_function,
                llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(*llvm_state.context))));
        }
        vtable_ty->setBody(vtable_fields_ty);
        auto vtable_struct = llvm::ConstantStruct::get(vtable_ty, vtable_fields);
        auto vtable =
            new llvm::GlobalVariable(*llvm_state.module, vtable_ty, true,
                                     llvm::GlobalValue::ExternalLinkage, vtable_struct,
                                     class_decl->class_name->name + "_vtable");
        (void)vtable;
    }
    // emit the array vtable as a LLVM global variable
    auto vtable_ty = llvm::StructType::create(*llvm_state.context, {}, "array_vtable");
    auto vtable_struct = llvm::ConstantStruct::get(vtable_ty, {});
    auto vtable = new llvm::GlobalVariable(*llvm_state.module, vtable_ty, true,
                                           llvm::GlobalValue::ExternalLinkage,
                                           vtable_struct, "array_vtable");
    (void)vtable;
    node->main_class->accept(this);
    for (auto &class_decl : node->class_decls) {
        class_decl->accept(this);
    }
}

void llvm_compiler_visitor_t::visit(parser::main_class_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::main_class_t *node)");
    current_class = node->class_name->name;
    current_method = "main";
    auto main_class = llvm::StructType::getTypeByName(*llvm_state.context, current_class);
    if (main_class == nullptr) {
        std::cerr << "error: main class not found" << std::endl;
        exit(1);
    }
    auto main_function = llvm_state.module->getFunction("main");
    auto entry = llvm::BasicBlock::Create(*llvm_state.context, "entry", main_function);
    llvm_state.builder->SetInsertPoint(entry);
    node->statement->accept(this);
    llvm_state.builder->CreateRetVoid();
}

void llvm_compiler_visitor_t::visit(parser::class_decl_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::class_decl_t *node)");
    current_class = node->class_name->name;
    for (auto &method_decl : node->method_decls) {
        method_decl->accept(this);
    }
}

void llvm_compiler_visitor_t::visit(parser::var_decl_t *node)
{
    // Nothing to do here
}

void llvm_compiler_visitor_t::visit(parser::method_decl_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::method_decl_t *node)");
    locals.clear();
    current_method = node->method_name->name;
    // argument types
    std::vector<llvm::Type *> args = {llvm::PointerType::getUnqual(
        llvm::StructType::getTypeByName(*llvm_state.context, current_class))};
    for (auto &arg_ty : node->arg_types) {
        auto arg_type = arg_ty->type_name->name;
        auto type = type_checker.program_symtbl->str_to_type(arg_type);
        if (type == semantics::integer_type) {
            args.push_back(llvm::Type::getInt64Ty(*llvm_state.context));
        }
        else if (type == semantics::boolean_type) {
            args.push_back(llvm::Type::getInt1Ty(*llvm_state.context));
        }
        else if (type == semantics::array_type) {
            args.push_back(llvm::PointerType::getUnqual(
                llvm::StructType::getTypeByName(*llvm_state.context, "array")));
        }
        else {
            auto ty = llvm::StructType::getTypeByName(*llvm_state.context, arg_type);
            if (ty == nullptr) {
                std::cerr << "error: type " << arg_type << " not found" << std::endl;
                exit(1);
            }
            auto ty_ptr = llvm::PointerType::getUnqual(ty);
            args.push_back(ty_ptr);
        }
    }
    // get function in LLVM IR
    std::string method_name = current_class + "$" + current_method;
    auto function = llvm_state.module->getFunction(method_name);
    auto entry = llvm::BasicBlock::Create(*llvm_state.context, "entry", function);
    llvm_state.builder->SetInsertPoint(entry);
    // set argument names
    auto &arg = function->arg_begin()[0];
    arg.setName("this");
    assert(node->arg_names.size() == function->arg_size() - 1 && "argument size mismatch");
    for (size_t i = 0; i < node->arg_names.size(); ++i) {
        auto &arg = function->arg_begin()[i + 1];
        arg.setName(node->arg_names[i]->name);
    }
    // allocate space for arguments
    for (size_t i = 0; i < node->arg_names.size(); ++i) {
        auto &arg = function->arg_begin()[i + 1];
        auto alloca = llvm_state.builder->CreateAlloca(arg.getType(), nullptr,
                                                       node->arg_names[i]->name);
        llvm_state.builder->CreateStore(&arg, alloca);
        locals[node->arg_names[i]->name] = alloca;
    }
    // allocate space for local variables
    for (auto &var_decl : node->var_decls) {
        auto type =
            type_checker.program_symtbl->str_to_type(var_decl->type->type_name->name);
        if (type == semantics::integer_type) {
            auto alloca = llvm_state.builder->CreateAlloca(
                llvm::Type::getInt64Ty(*llvm_state.context), nullptr,
                var_decl->var_name->name);
            llvm_state.builder->CreateStore(
                llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 0, true)),
                alloca);
            locals[var_decl->var_name->name] = alloca;
        }
        else if (type == semantics::boolean_type) {
            auto alloca =
                llvm_state.builder->CreateAlloca(llvm::Type::getInt1Ty(*llvm_state.context),
                                                 nullptr, var_decl->var_name->name);
            llvm_state.builder->CreateStore(llvm::ConstantInt::get(*llvm_state.context,
                                                                   llvm::APInt(1, 0, true)),
                                            alloca);
            locals[var_decl->var_name->name] = alloca;
        }
        else if (type == semantics::array_type) {
            auto alloca = llvm_state.builder->CreateAlloca(
                llvm::PointerType::getUnqual(
                    llvm::StructType::getTypeByName(*llvm_state.context, "array")),
                nullptr, var_decl->var_name->name);
            llvm_state.builder->CreateStore(
                llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(
                    llvm::StructType::getTypeByName(*llvm_state.context, "array"))),
                alloca);
            locals[var_decl->var_name->name] = alloca;
        }
        else {
            auto ty = llvm::StructType::getTypeByName(*llvm_state.context,
                                                      var_decl->type->type_name->name);
            if (ty == nullptr) {
                std::cerr << "error: type " << var_decl->type->type_name->name
                          << " not found" << std::endl;
                exit(1);
            }
            auto ty_ptr = llvm::PointerType::getUnqual(ty);
            auto alloca =
                llvm_state.builder->CreateAlloca(ty_ptr, nullptr, var_decl->var_name->name);
            llvm_state.builder->CreateStore(llvm::ConstantPointerNull::get(ty_ptr), alloca);
            locals[var_decl->var_name->name] = alloca;
        }
    }
    for (auto &statement : node->statements) {
        statement->accept(this);
    }
    node->return_expression->accept(this);
    llvm_state.builder->CreateRet(current_value);
}

void llvm_compiler_visitor_t::visit(parser::type_t *node)
{
    // Nothing to do here
}

void llvm_compiler_visitor_t::visit(parser::statement_t *node)
{
    // Nothing to do here
}

void llvm_compiler_visitor_t::visit(parser::block_statement_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::block_statement_t *node)");
    for (auto &statement : node->statements) {
        statement->accept(this);
    }
}

void llvm_compiler_visitor_t::visit(parser::if_statement_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::if_statement_t *node)");
    node->condition->accept(this);
    auto condition = current_value;
    auto function = llvm_state.builder->GetInsertBlock()->getParent();
    auto then_block = llvm::BasicBlock::Create(*llvm_state.context, "then", function);
    auto else_block = llvm::BasicBlock::Create(*llvm_state.context, "else");
    auto merge_block = llvm::BasicBlock::Create(*llvm_state.context, "ifcont");
    llvm_state.builder->CreateCondBr(condition, then_block, else_block);
    llvm_state.builder->SetInsertPoint(then_block);
    node->then_statement->accept(this);
    llvm_state.builder->CreateBr(merge_block);
    then_block = llvm_state.builder->GetInsertBlock();
    function->insert(function->end(), else_block);
    llvm_state.builder->SetInsertPoint(else_block);
    node->else_statement->accept(this);
    llvm_state.builder->CreateBr(merge_block);
    else_block = llvm_state.builder->GetInsertBlock();
    function->insert(function->end(), merge_block);
    llvm_state.builder->SetInsertPoint(merge_block);
}

void llvm_compiler_visitor_t::visit(parser::while_statement_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::while_statement_t *node)");
    auto function = llvm_state.builder->GetInsertBlock()->getParent();
    auto while_block = llvm::BasicBlock::Create(*llvm_state.context, "while", function);
    auto loop_block = llvm::BasicBlock::Create(*llvm_state.context, "loop");
    auto merge_block = llvm::BasicBlock::Create(*llvm_state.context, "merge");
    llvm_state.builder->CreateBr(while_block);
    llvm_state.builder->SetInsertPoint(while_block);
    node->condition->accept(this);
    auto condition = current_value;
    llvm_state.builder->CreateCondBr(condition, loop_block, merge_block);
    function->insert(function->end(), loop_block);
    llvm_state.builder->SetInsertPoint(loop_block);
    node->statement->accept(this);
    llvm_state.builder->CreateBr(while_block);
    loop_block = llvm_state.builder->GetInsertBlock();
    function->insert(function->end(), merge_block);
    llvm_state.builder->SetInsertPoint(merge_block);
}

void llvm_compiler_visitor_t::visit(parser::print_statement_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::print_statement_t *node)");
    node->expression->accept(this);
    auto value = current_value;
    auto format_str = llvm_state.builder->CreateGlobalStringPtr("%d\n");
    auto printf_function = llvm_state.module->getFunction("printf");
    llvm_state.builder->CreateCall(printf_function, {format_str, value});
}

void llvm_compiler_visitor_t::visit(parser::assign_statement_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::assign_statement_t *node)");
    // local variable store
    if (locals.find(node->var_name->name) != locals.end()) {
        node->expression->accept(this);
        auto value = current_value;
        auto alloca = locals[node->var_name->name];
        llvm_state.builder->CreateStore(value, alloca);
    }
    // field store
    else {
        auto class_layout = std::find_if(classes.begin(), classes.end(),
                                         [this](const llvm_class_layout_t &cl) {
                                             return cl.name == current_class;
                                         });
        if (class_layout == classes.end()) {
            std::cerr << "error: class " << current_class << " not found" << std::endl;
            exit(1);
        }
        auto &c = *class_layout;
        auto field_idx = std::distance(
            c.fields.begin(),
            std::find_if(c.fields.begin(), c.fields.end(),
                         [node](const std::pair<std::string, std::string> &field) {
                             return field.second == node->var_name->name;
                         }));
        if (field_idx == c.fields.size()) {
            std::cerr << "error: field " << node->var_name->name << " not found"
                      << std::endl;
            exit(1);
        }
        field_idx += 2; // skip vtable and length
        node->expression->accept(this);
        auto value = current_value;
        auto ptr = llvm_state.builder->CreateGEP(
            llvm::PointerType::getUnqual(
                llvm::StructType::getTypeByName(*llvm_state.context, current_class)),
            llvm_state.builder->GetInsertBlock()->getParent()->arg_begin(),
            llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, field_idx, true)),
            "fieldidx");
        llvm_state.builder->CreateStore(value, ptr);
    }
}

void llvm_compiler_visitor_t::visit(parser::array_assign_statement_t *node)
{
    llvm_compiler_log(
        "llvm_compiler_visitor_t::visit(parser::array_assign_statement_t *node)");
    // local variable store
    if (locals.find(node->var_name->name) != locals.end()) {
        auto array = locals[node->var_name->name];
        auto buffer_ptr = llvm_state.builder->CreateGEP(
            llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(*llvm_state.context)),
            array, llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 2, true)),
            "loadtmp");
        auto buffer = llvm_state.builder->CreateLoad(
            llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(*llvm_state.context)),
            buffer_ptr, "loadtmp");
        node->expression->accept(this);
        auto value = current_value;
        node->index_expression->accept(this);
        auto ptr = llvm_state.builder->CreateGEP(
            llvm::Type::getInt64Ty(*llvm_state.context), buffer, current_value, "arrayidx");
        current_value = llvm_state.builder->CreateStore(value, ptr);
    }
    // field store
    else {
        auto class_layout = std::find_if(classes.begin(), classes.end(),
                                         [this](const llvm_class_layout_t &cl) {
                                             return cl.name == current_class;
                                         });
        if (class_layout == classes.end()) {
            std::cerr << "error: class " << current_class << " not found" << std::endl;
            exit(1);
        }
        auto &c = *class_layout;
        auto field_idx = std::distance(
            c.fields.begin(),
            std::find_if(c.fields.begin(), c.fields.end(),
                         [node](const std::pair<std::string, std::string> &field) {
                             return field.second == node->var_name->name;
                         }));
        if (field_idx == c.fields.size()) {
            std::cerr << "error: field " << node->var_name->name << " not found"
                      << std::endl;
            exit(1);
        }
        field_idx += 2; // skip vtable and length
        auto field_ptr = llvm_state.builder->CreateGEP(
            llvm::PointerType::getUnqual(
                llvm::StructType::getTypeByName(*llvm_state.context, "array")),
            llvm_state.builder->GetInsertBlock()->getParent()->arg_begin(),
            llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, field_idx, true)),
            "fieldidx");
        auto array = llvm_state.builder->CreateLoad(
            llvm::PointerType::getUnqual(
                llvm::StructType::getTypeByName(*llvm_state.context, "array")),
            field_ptr, "loadtmp");
        auto buffer_ptr = llvm_state.builder->CreateGEP(
            llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(*llvm_state.context)),
            array, llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 2, true)),
            "loadtmp");
        auto buffer = llvm_state.builder->CreateLoad(
            llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(*llvm_state.context)),
            buffer_ptr, "loadtmp");
        node->expression->accept(this);
        auto value = current_value;
        node->index_expression->accept(this);
        auto ptr = llvm_state.builder->CreateGEP(
            llvm::Type::getInt64Ty(*llvm_state.context), buffer, current_value, "arrayidx");
        current_value = llvm_state.builder->CreateStore(value, ptr);
    }
}

void llvm_compiler_visitor_t::visit(parser::expression_t *node)
{
    // Nothing to do here
}

void llvm_compiler_visitor_t::visit(parser::binary_expression_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::binary_expression_t *node)");
    node->left->accept(this);
    auto lhs = current_value;
    node->right->accept(this);
    auto rhs = current_value;
    switch (node->op) {
    case parser::binary_operator_t::plus_:
        current_value = llvm_state.builder->CreateAdd(lhs, rhs, "addtmp");
        break;
    case parser::binary_operator_t::minus_:
        current_value = llvm_state.builder->CreateSub(lhs, rhs, "subtmp");
        break;
    case parser::binary_operator_t::times_:
        current_value = llvm_state.builder->CreateMul(lhs, rhs, "multmp");
        break;
    case parser::binary_operator_t::less_:
        current_value = llvm_state.builder->CreateICmpSLT(lhs, rhs, "cmptmp");
        break;
    case parser::binary_operator_t::and_:
        current_value = llvm_state.builder->CreateAnd(lhs, rhs, "andtmp");
        break;
    }
}

void llvm_compiler_visitor_t::visit(parser::array_index_expression_t *node)
{
    llvm_compiler_log(
        "llvm_compiler_visitor_t::visit(parser::array_index_expression_t *node)");
    node->array_expression->accept(this);
    auto array = current_value;
    auto buffer_ptr = llvm_state.builder->CreateGEP(
        llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(*llvm_state.context)), array,
        llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 2, true)), "loadtmp");
    auto buffer = llvm_state.builder->CreateLoad(
        llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(*llvm_state.context)),
        buffer_ptr, "loadtmp");
    node->index_expression->accept(this);
    auto index = current_value;
    auto ptr = llvm_state.builder->CreateGEP(llvm::Type::getInt64Ty(*llvm_state.context),
                                             buffer, index, "arrayidx");
    current_value = llvm_state.builder->CreateLoad(
        llvm::Type::getInt64Ty(*llvm_state.context), ptr, "loadtmp");
}

void llvm_compiler_visitor_t::visit(parser::array_length_expression_t *node)
{
    llvm_compiler_log(
        "llvm_compiler_visitor_t::visit(parser::array_length_expression_t *node)");
    node->array_expression->accept(this);
    auto array = current_value;
    auto ptr = llvm_state.builder->CreateGEP(
        llvm::Type::getInt64Ty(*llvm_state.context), array,
        llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 1, true)), "arraylen");
    current_value = llvm_state.builder->CreateLoad(
        llvm::Type::getInt64Ty(*llvm_state.context), ptr, "loadtmp");
}

void llvm_compiler_visitor_t::visit(parser::method_call_expression_t *node)
{
    llvm_compiler_log(
        "llvm_compiler_visitor_t::visit(parser::method_call_expression_t *node)");
    auto current_method_name = current_method;
    auto method_to_call = node->method_name->name;
    auto class_symtbl = type_checker.program_symtbl->classes.at(current_class);
    auto it =
        std::find_if(class_symtbl->methods.begin(), class_symtbl->methods.end(),
                     [&current_method_name](const semantics::method_symtbl_t *method) {
                         return method->name == current_method_name;
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
    auto it2 = std::find_if(classes.begin(), classes.end(),
                            [type_as_str](const llvm_class_layout_t &cl) {
                                return cl.name == type_as_str;
                            });
    if (it2 == classes.end()) {
        std::cerr << "error: class " << type_as_str << " not found" << std::endl;
        exit(1);
    }
    auto &class_layout = *it2;
    auto method_idx =
        std::distance(class_layout.vtable.begin(),
                      std::find_if(class_layout.vtable.begin(), class_layout.vtable.end(),
                                   [&method_to_call](const llvm_method_layout_t *method) {
                                       return method->method_name.second == method_to_call;
                                   }));
    if (method_idx == class_layout.vtable.size()) {
        std::cerr << "error: method " << method_to_call << " not found" << std::endl;
        exit(1);
    }
    node->object_expression->accept(this);
    auto object = current_value;
    std::vector<llvm::Value *> args;
    args.push_back(object);
    for (auto &arg : node->arg_expressions) {
        arg->accept(this);
        args.push_back(current_value);
    }
    auto idx =
        llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, method_idx, true));
    auto vtable_ptr = llvm_state.builder->CreateGEP(
        llvm::PointerType::getUnqual(llvm::IntegerType::getInt8Ty(*llvm_state.context)),
        object, llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 0, true)),
        "vtableptr");
    auto vtable = llvm_state.builder->CreateLoad(
        llvm::PointerType::getUnqual(llvm::IntegerType::getInt8Ty(*llvm_state.context)),
        vtable_ptr, "loadtmp");
    auto method_ptr = llvm_state.builder->CreateGEP(
        llvm::PointerType::getUnqual(llvm::IntegerType::getInt8Ty(*llvm_state.context)),
        vtable, idx, "methodptr");
    auto method = llvm_state.builder->CreateLoad(
        llvm::PointerType::getUnqual(llvm::IntegerType::getInt8Ty(*llvm_state.context)),
        method_ptr, "loadtmp");
    auto function = llvm_state.module->getFunction(type_as_str + "$" + method_to_call);
    if (function == nullptr) {
        std::cerr << "error: method " << method_to_call << " not found" << std::endl;
        exit(1);
    }
    auto function_ty = function->getFunctionType();
    auto function_ptr =
        llvm_state.builder->CreateIntToPtr(method, function_ty->getPointerTo());
    // create call
    current_value =
        llvm_state.builder->CreateCall(function_ty, function_ptr, args, "calltmp");
}

void llvm_compiler_visitor_t::visit(parser::integer_literal_expression_t *node)
{
    llvm_compiler_log(
        "llvm_compiler_visitor_t::visit(parser::integer_literal_expression_t *node)");
    // create integer constant
    current_value =
        llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, node->value, true));
}

void llvm_compiler_visitor_t::visit(parser::true_literal_expression_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::true_literal_expression_t "
                      "*node)");
    // create bool constant true/1
    current_value = llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(1, 1));
}

void llvm_compiler_visitor_t::visit(parser::false_literal_expression_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::false_literal_expression_t "
                      "*node)");
    // create bool constant false/0
    current_value = llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(1, 0));
}

void llvm_compiler_visitor_t::visit(parser::identifier_expression_t *node)
{
    llvm_compiler_log(
        "llvm_compiler_visitor_t::visit(parser::identifier_expression_t *node)");
    if (locals.find(node->identifier->name) != locals.end()) {
        auto alloca = locals[node->identifier->name];
        current_value = llvm_state.builder->CreateLoad(alloca->getAllocatedType(), alloca,
                                                       node->identifier->name.c_str());
    }
    else {
        auto class_layout = std::find_if(classes.begin(), classes.end(),
                                         [this](const llvm_class_layout_t &cl) {
                                             return cl.name == current_class;
                                         });
        if (class_layout == classes.end()) {
            std::cerr << "error: class " << current_class << " not found" << std::endl;
            exit(1);
        }
        auto &c = *class_layout;
        auto field_idx = std::distance(
            c.fields.begin(),
            std::find_if(c.fields.begin(), c.fields.end(),
                         [node](const std::pair<std::string, std::string> &field) {
                             return field.second == node->identifier->name;
                         }));
        if (field_idx == c.fields.size()) {
            std::cerr << "error: field " << node->identifier->name << " not found"
                      << std::endl;
            exit(1);
        }
        field_idx += 2; // skip vtable and length
        auto field_type_as_str = c.fields[field_idx - 2].first;
        auto type = type_checker.program_symtbl->str_to_type(field_type_as_str);
        if (type == semantics::integer_type) {
            auto ptr = llvm_state.builder->CreateGEP(
                llvm::PointerType::getUnqual(
                    llvm::StructType::getTypeByName(*llvm_state.context, current_class)),
                llvm_state.builder->GetInsertBlock()->getParent()->arg_begin(),
                llvm::ConstantInt::get(*llvm_state.context,
                                       llvm::APInt(64, field_idx, true)),
                "fieldidx");
            current_value = llvm_state.builder->CreateLoad(
                llvm::Type::getInt64Ty(*llvm_state.context), ptr, "loadtmp");
        }
        else if (type == semantics::boolean_type) {
            auto ptr = llvm_state.builder->CreateGEP(
                llvm::PointerType::getUnqual(
                    llvm::StructType::getTypeByName(*llvm_state.context, current_class)),
                llvm_state.builder->GetInsertBlock()->getParent()->arg_begin(),
                llvm::ConstantInt::get(*llvm_state.context,
                                       llvm::APInt(64, field_idx, true)),
                "fieldidx");
            current_value = llvm_state.builder->CreateLoad(
                llvm::Type::getInt1Ty(*llvm_state.context), ptr, "loadtmp");
        }
        else if (type == semantics::array_type) {
            auto ptr = llvm_state.builder->CreateGEP(
                llvm::PointerType::getUnqual(
                    llvm::StructType::getTypeByName(*llvm_state.context, current_class)),
                llvm_state.builder->GetInsertBlock()->getParent()->arg_begin(),
                llvm::ConstantInt::get(*llvm_state.context,
                                       llvm::APInt(64, field_idx, true)),
                "fieldidx");
            current_value = llvm_state.builder->CreateLoad(
                llvm::PointerType::getUnqual(
                    llvm::StructType::getTypeByName(*llvm_state.context, "array")),
                ptr, "loadtmp");
        }
        else {
            auto ptr = llvm_state.builder->CreateGEP(
                llvm::PointerType::getUnqual(
                    llvm::StructType::getTypeByName(*llvm_state.context, current_class)),
                llvm_state.builder->GetInsertBlock()->getParent()->arg_begin(),
                llvm::ConstantInt::get(*llvm_state.context,
                                       llvm::APInt(64, field_idx, true)),
                "fieldidx");
            current_value = llvm_state.builder->CreateLoad(
                llvm::PointerType::getUnqual(
                    llvm::StructType::getTypeByName(*llvm_state.context, current_class)),
                ptr, "loadtmp");
        }
    }
}

void llvm_compiler_visitor_t::visit(parser::this_expression_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::this_expression_t *node)");
    current_value = llvm_state.builder->GetInsertBlock()->getParent()->arg_begin();
}

void llvm_compiler_visitor_t::visit(parser::new_integer_array_expression_t *node)
{
    llvm_compiler_log(
        "llvm_compiler_visitor_t::visit(parser::new_integer_array_expression_t *node)");
    auto calloc_function = llvm_state.module->getFunction("calloc");
    auto array = llvm_state.builder->CreateCall(
        calloc_function,
        {llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 1, true)),
         llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 24, true))});
    auto array_vtable_global = llvm_state.module->getGlobalVariable("array_vtable");
    auto vtable_ptr = llvm_state.builder->CreateGEP(
        llvm::PointerType::getUnqual(llvm::IntegerType::getInt8Ty(*llvm_state.context)),
        array, llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 0, true)),
        "vtableptr");
    llvm_state.builder->CreateStore(array_vtable_global, vtable_ptr);
    node->size_expression->accept(this);
    auto nelem = current_value;
    auto buffer_size_in_bytes = llvm_state.builder->CreateMul(
        nelem, llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 8, true)),
        "buffersize");
    auto buffer_alloc = llvm_state.builder->CreateCall(
        calloc_function,
        {llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 1, true)),
         buffer_size_in_bytes});
    auto buffer_ptr = llvm_state.builder->CreateGEP(
        llvm::PointerType::getUnqual(llvm::IntegerType::getInt64Ty(*llvm_state.context)),
        array, llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 2, true)),
        "bufferptr");
    llvm_state.builder->CreateStore(buffer_alloc, buffer_ptr);
    auto buffer_size_ptr = llvm_state.builder->CreateGEP(
        llvm::PointerType::getUnqual(llvm::IntegerType::getInt64Ty(*llvm_state.context)),
        array, llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 1, true)),
        "buffersizeptr");
    llvm_state.builder->CreateStore(nelem, buffer_size_ptr);
    current_value = array;
}

void llvm_compiler_visitor_t::visit(parser::new_object_expression_t *node)
{
    llvm_compiler_log(
        "llvm_compiler_visitor_t::visit(parser::new_object_expression_t *node)");
    auto calloc_function = llvm_state.module->getFunction("calloc");
    auto it =
        std::find_if(classes.begin(), classes.end(), [node](const llvm_class_layout_t &cl) {
            return cl.name == node->class_name->name;
        });
    if (it == classes.end()) {
        std::cerr << "error: class " << node->class_name->name << " not found" << std::endl;
        exit(1);
    }
    auto &c = *it;
    auto class_size_in_bytes = llvm_state.builder->CreateMul(
        llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 8, true)),
        llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, c.fields.size(), true)),
        "classsize");
    class_size_in_bytes = llvm_state.builder->CreateAdd(
        class_size_in_bytes,
        llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 16, true)),
        "classsize"); // add space for vtable and length
    auto object = llvm_state.builder->CreateCall(
        calloc_function,
        {llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 1, true)),
         class_size_in_bytes});
    auto vtable_ptr = llvm_state.builder->CreateGEP(
        llvm::PointerType::getUnqual(llvm::IntegerType::getInt8Ty(*llvm_state.context)),
        object, llvm::ConstantInt::get(*llvm_state.context, llvm::APInt(64, 0, true)),
        "vtableptr");
    auto vtable_global =
        llvm_state.module->getGlobalVariable(node->class_name->name + "_vtable");
    if (vtable_global == nullptr) {
        std::cerr << "error: vtable for class " << node->class_name->name << " not found"
                  << std::endl;
        exit(1);
    }
    llvm_state.builder->CreateStore(vtable_global, vtable_ptr);
    current_value = object;
}

void llvm_compiler_visitor_t::visit(parser::not_expression_t *node)
{
    llvm_compiler_log("llvm_compiler_visitor_t::visit(parser::not_expression_t *node)");
    node->expression->accept(this);
    current_value = llvm_state.builder->CreateNot(current_value, "nottmp");
}

void llvm_compiler_visitor_t::visit(parser::parentheses_expression_t *node)
{
    llvm_compiler_log(
        "llvm_compiler_visitor_t::visit(parser::parentheses_expression_t *node)");
    node->expression->accept(this);
}

} // namespace compiler