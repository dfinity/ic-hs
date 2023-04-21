//! This module contains functions to convert between [`wasmparser`] types and
//! [`wasm_encoder`] types. In most cases the types are almost exactly the same
//! for the two crates, but they occasionally vary slightly in terms of
//! structure or ownership of the contained data.
//!
//! In some cases we'll also have internal types which are distinct from both
//! the [`wasmparser`] and [`wasm_encoder`] types, but in most cases the
//! internal type should use [`wasmparser`].

/// Conversion from [`wasmparser`] to internal types.
pub(super) mod parser_to_internal {
    use crate::wasm_utils::wasm_transform;

    fn data_kind(
        kind: wasmparser::DataKind,
    ) -> Result<wasm_transform::DataSegmentKind, wasm_transform::Error> {
        Ok(match kind {
            wasmparser::DataKind::Passive => wasm_transform::DataSegmentKind::Passive,
            wasmparser::DataKind::Active {
                memory_index,
                offset_expr,
            } => {
                let ops: Vec<_> = offset_expr
                    .get_operators_reader()
                    .into_iter()
                    .collect::<Result<_, _>>()?;
                match ops.as_slice() {
                    [_, wasmparser::Operator::End] => wasm_transform::DataSegmentKind::Active {
                        memory_index,
                        offset_expr: ops[0].clone(),
                    },
                    _ => return Err(wasm_transform::Error::InvalidConstExpr),
                }
            }
        })
    }

    pub(crate) fn data_segment(
        data: wasmparser::Data,
    ) -> Result<wasm_transform::DataSegment, wasm_transform::Error> {
        Ok(wasm_transform::DataSegment {
            kind: data_kind(data.kind)?,
            data: data.data,
        })
    }

    pub(crate) fn element_items(
        items: wasmparser::ElementItems,
    ) -> Result<wasm_transform::ElementItems, wasm_transform::Error> {
        match items {
            wasmparser::ElementItems::Functions(reader) => {
                let functions = reader.into_iter().collect::<Result<Vec<_>, _>>()?;
                Ok(wasm_transform::ElementItems::Functions(functions))
            }
            wasmparser::ElementItems::Expressions(reader) => {
                let exprs = reader
                    .into_iter()
                    .map(|expr| super::internal_to_encoder::const_expr(expr?))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(wasm_transform::ElementItems::ConstExprs(exprs))
            }
        }
    }
}

/// Conversion from internal to [`wasm_encoder`] types.
pub(super) mod internal_to_encoder {
    use crate::wasm_utils::wasm_transform;

    pub(crate) fn block_type(ty: &wasmparser::BlockType) -> wasm_encoder::BlockType {
        match ty {
            wasmparser::BlockType::Empty => wasm_encoder::BlockType::Empty,
            wasmparser::BlockType::Type(ty) => wasm_encoder::BlockType::Result(val_type(ty)),
            wasmparser::BlockType::FuncType(f) => wasm_encoder::BlockType::FunctionType(*f),
        }
    }

    pub(crate) fn val_type(v: &wasmparser::ValType) -> wasm_encoder::ValType {
        match v {
            wasmparser::ValType::I32 => wasm_encoder::ValType::I32,
            wasmparser::ValType::I64 => wasm_encoder::ValType::I64,
            wasmparser::ValType::F32 => wasm_encoder::ValType::F32,
            wasmparser::ValType::F64 => wasm_encoder::ValType::F64,
            wasmparser::ValType::V128 => wasm_encoder::ValType::V128,
            wasmparser::ValType::FuncRef => wasm_encoder::ValType::FuncRef,
            wasmparser::ValType::ExternRef => wasm_encoder::ValType::ExternRef,
        }
    }

    pub(crate) fn table_type(t: wasmparser::TableType) -> wasm_encoder::TableType {
        wasm_encoder::TableType {
            element_type: val_type(&t.element_type),
            minimum: t.initial,
            maximum: t.maximum,
        }
    }

    pub(crate) fn memory_type(m: wasmparser::MemoryType) -> wasm_encoder::MemoryType {
        wasm_encoder::MemoryType {
            memory64: m.memory64,
            shared: m.shared,
            minimum: m.initial,
            maximum: m.maximum,
        }
    }

    pub(crate) fn global_type(g: wasmparser::GlobalType) -> wasm_encoder::GlobalType {
        wasm_encoder::GlobalType {
            val_type: val_type(&g.content_type),
            mutable: g.mutable,
        }
    }

    fn tag_kind(k: wasmparser::TagKind) -> wasm_encoder::TagKind {
        match k {
            wasmparser::TagKind::Exception => wasm_encoder::TagKind::Exception,
        }
    }

    fn tag_type(t: wasmparser::TagType) -> wasm_encoder::TagType {
        wasm_encoder::TagType {
            kind: tag_kind(t.kind),
            func_type_idx: t.func_type_idx,
        }
    }

    pub(crate) fn import_type(ty: wasmparser::TypeRef) -> wasm_encoder::EntityType {
        match ty {
            wasmparser::TypeRef::Func(f) => wasm_encoder::EntityType::Function(f),
            wasmparser::TypeRef::Table(t) => wasm_encoder::EntityType::Table(table_type(t)),
            wasmparser::TypeRef::Memory(m) => wasm_encoder::EntityType::Memory(memory_type(m)),
            wasmparser::TypeRef::Global(g) => wasm_encoder::EntityType::Global(global_type(g)),
            wasmparser::TypeRef::Tag(t) => wasm_encoder::EntityType::Tag(tag_type(t)),
        }
    }

    pub(crate) fn op_to_const_expr(
        operator: &wasmparser::Operator,
    ) -> Result<wasm_encoder::ConstExpr, wasmparser::BinaryReaderError> {
        use wasm_encoder::Encode;
        let mut bytes: Vec<u8> = Vec::new();
        op(operator)?.encode(&mut bytes);
        Ok(wasm_encoder::ConstExpr::raw(bytes))
    }

    pub(crate) fn const_expr(
        expr: wasmparser::ConstExpr,
    ) -> Result<wasm_encoder::ConstExpr, wasmparser::BinaryReaderError> {
        let mut reader = expr.get_binary_reader();
        let size = reader.bytes_remaining();
        // The const expression should end in a `End` instruction, but the encoder
        // doesn't expect that instruction to be part of its input so we drop it.
        let bytes = reader.read_bytes(size - 1)?.to_vec();
        match reader.read_operator()? {
            wasmparser::Operator::End => {}
            _ => {
                panic!("const expr didn't end with `End` instruction");
            }
        }
        Ok(wasm_encoder::ConstExpr::raw(bytes))
    }

    pub(crate) struct DerefBytesIterator<'a> {
        data: &'a [u8],
        current: usize,
    }

    impl<'a> DerefBytesIterator<'a> {
        fn new(data: &'a [u8]) -> Self {
            Self { data, current: 0 }
        }
    }

    impl<'a> Iterator for DerefBytesIterator<'a> {
        type Item = u8;

        fn next(&mut self) -> Option<Self::Item> {
            if self.current < self.data.len() {
                let next = self.data[self.current];
                self.current += 1;
                Some(next)
            } else {
                None
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            let remaining = self.data.len() - self.current;
            (remaining, Some(remaining))
        }
    }

    impl<'a> ExactSizeIterator for DerefBytesIterator<'a> {}

    pub(crate) fn data_segment<'a>(
        segment: wasm_transform::DataSegment<'a>,
        temp_const_expr: &'a mut wasm_encoder::ConstExpr,
    ) -> Result<wasm_encoder::DataSegment<'a, DerefBytesIterator<'a>>, wasmparser::BinaryReaderError>
    {
        let mode = match segment.kind {
            wasm_transform::DataSegmentKind::Passive => wasm_encoder::DataSegmentMode::Passive,
            wasm_transform::DataSegmentKind::Active {
                memory_index,
                offset_expr,
            } => {
                *temp_const_expr = op_to_const_expr(&offset_expr)?;
                wasm_encoder::DataSegmentMode::Active {
                    memory_index,
                    offset: temp_const_expr,
                }
            }
        };

        Ok(wasm_encoder::DataSegment {
            mode,
            data: DerefBytesIterator::new(segment.data),
        })
    }

    pub(crate) fn export_kind(export_kind: wasmparser::ExternalKind) -> wasm_encoder::ExportKind {
        match export_kind {
            wasmparser::ExternalKind::Func => wasm_encoder::ExportKind::Func,
            wasmparser::ExternalKind::Table => wasm_encoder::ExportKind::Table,
            wasmparser::ExternalKind::Memory => wasm_encoder::ExportKind::Memory,
            wasmparser::ExternalKind::Global => wasm_encoder::ExportKind::Global,
            wasmparser::ExternalKind::Tag => wasm_encoder::ExportKind::Tag,
        }
    }

    fn memarg(memarg: &wasmparser::MemArg) -> wasm_encoder::MemArg {
        wasm_encoder::MemArg {
            offset: memarg.offset,
            align: memarg.align as u32,
            memory_index: memarg.memory,
        }
    }

    pub(crate) fn element_items(
        element_items: &wasm_transform::ElementItems,
    ) -> wasm_encoder::Elements<'_> {
        match element_items {
            wasm_transform::ElementItems::Functions(funcs) => {
                wasm_encoder::Elements::Functions(funcs)
            }
            wasm_transform::ElementItems::ConstExprs(exprs) => {
                wasm_encoder::Elements::Expressions(exprs)
            }
        }
    }

    /// Convert [`wasmparser::Operator`] to [`wasm_encoder::Instruction`]. A
    /// simplified example of the conversion done in wasm-mutate
    /// [here](https://github.com/bytecodealliance/wasm-tools/blob/a8c4fddd239b0cb8978c76e6dfd856d5bd29b860/crates/wasm-mutate/src/mutators/translate.rs#L279).
    #[allow(unused_variables)]
    pub(crate) fn op(
        op: &wasmparser::Operator<'_>,
    ) -> Result<wasm_encoder::Instruction<'static>, wasmparser::BinaryReaderError> {
        use wasm_encoder::Instruction as I;

        macro_rules! convert {
            ($( @$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident)*) => {
                match op {
                    $(
                        wasmparser::Operator::$op $({ $($arg),* })? => {
                            $(
                                $(let $arg = convert!(map $arg $arg);)*
                            )?
                            convert!(build $op $($($arg)*)?)
                        }
                    )*
                }
            };

            // Mapping the arguments from wasmparser to wasm-encoder types.

            // Arguments which need to be explicitly converted or ignored.
            (map $arg:ident blockty) => (block_type($arg));
            (map $arg:ident targets) => ((
                $arg
                    .targets()
                    .collect::<Result<Vec<_>, wasmparser::BinaryReaderError>>()?
                    .into(),
                $arg.default(),
            ));
            (map $arg:ident ty) => (val_type($arg));
            (map $arg:ident memarg) => (memarg($arg));
            (map $arg:ident table_byte) => (());
            (map $arg:ident mem_byte) => (());
            (map $arg:ident flags) => (());

            // All other arguments are just dereferenced.
            (map $arg:ident $_:ident) => (*$arg);

            // Construct the wasm-encoder Instruction from the arguments of a
            // wasmparser instruction.  There are a few special cases for where the
            // structure of a wasmparser instruction differs from that of
            // wasm-encoder.

            // Single operators are directly converted.
            (build $op:ident) => (Ok(I::$op));

            // Special cases with a single argument.
            (build BrTable $arg:ident) => (Ok(I::BrTable($arg.0, $arg.1)));
            (build F32Const $arg:ident) => (Ok(I::F32Const(f32::from_bits($arg.bits()))));
            (build F64Const $arg:ident) => (Ok(I::F64Const(f64::from_bits($arg.bits()))));
            (build V128Const $arg:ident) => (Ok(I::V128Const($arg.i128())));

            // Standard case with a single argument.
            (build $op:ident $arg:ident) => (Ok(I::$op($arg)));

            // Special case of multiple arguments.
            (build CallIndirect $ty:ident $table:ident $_:ident) => (Ok(I::CallIndirect {
                ty: $ty,
                table: $table,
            }));
            (build ReturnCallIndirect $ty:ident $table:ident) => (Ok(I::ReturnCallIndirect {
                ty: $ty,
                table: $table,
            }));
            (build MemoryGrow $mem:ident $_:ident) => (Ok(I::MemoryGrow($mem)));
            (build MemorySize $mem:ident $_:ident) => (Ok(I::MemorySize($mem)));

            // Standard case of multiple arguments.
            (build $op:ident $($arg:ident)*) => (Ok(I::$op { $($arg),* }));
        }

        wasmparser::for_each_operator!(convert)
    }
}
