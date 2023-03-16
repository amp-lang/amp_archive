//! The code generation library for Amp.

use std::collections::HashMap;

use cranelift::prelude::{
    settings::{self, Flags},
    Configurable,
};
use cranelift_module::{default_libcall_names, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use crate::typechecker::{func::FuncId, Typechecker};

use self::func::CraneliftFunc;

pub mod func;
pub mod stmnt;
pub mod types;
pub mod value;

pub struct Codegen {
    /// Functions declared in the Cranelift context
    pub funcs: HashMap<FuncId, CraneliftFunc>,
    pub pointer_type: cranelift::prelude::Type,

    /// The module used for code generation.
    pub module: ObjectModule,
    pub func_context: cranelift::prelude::FunctionBuilderContext,
}

impl Codegen {
    /// Initializes a new [Codegen] configuration.
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder()
            .unwrap_or_else(|_| {
                panic!(
                    "Could not create an object module for the current target: {}",
                    Triple::host()
                )
            })
            .finish(Flags::new(flag_builder))
            .unwrap();

        let module = ObjectModule::new(
            ObjectBuilder::new(isa_builder, [1, 2, 3, 4], default_libcall_names()).unwrap(),
        );

        Self {
            funcs: HashMap::new(),
            pointer_type: module.target_config().clone().pointer_type(),
            module,
            func_context: cranelift::prelude::FunctionBuilderContext::new(),
        }
    }

    /// Compiles the given [Typechecker] context.
    pub fn compile(&mut self, checker: Typechecker) {
        // Declare functions
        for (idx, item) in checker.funcs.iter().enumerate() {
            let cranelift_func = func::declare_func(self, &item);
            self.funcs.insert(FuncId(idx), cranelift_func);
        }

        let mut context = self.module.make_context();
        let mut func_context = cranelift::prelude::FunctionBuilderContext::new();

        // Define functions
        for (idx, func) in checker.funcs.iter().enumerate() {
            let id = FuncId(idx);
            if let Some(data) = &func.func_impl {
                func::compile_func(
                    self,
                    &mut context,
                    &mut func_context,
                    id,
                    &func.signature,
                    data,
                );
            }
            self.module.clear_context(&mut context);
        }
    }

    /// Finishes compiling the typechecked code.
    #[inline]
    pub fn finish(self) -> Vec<u8> {
        self.module.finish().emit().unwrap()
    }
}
