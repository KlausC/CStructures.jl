# CStructures

[![Build Status](https://github.com/KlausC/CStructures.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/KlausC/CStructures.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/KlausC/CStructures.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/KlausC/CStructures.jl)

## Purpose

C data structures are different from Julia structures in that they do not keep type information.
While most primitive types and bitstype structures of Julia have an identical memory layout as the corresponding
C data, complexities arise when pointers and strings are embedded.

This package handles the situation, when pointered C-data generated in C have to be accessed in Julia code.
It is also possible in Julia to construct a byte array, which can be processed by a C program given the
layout of the C-structures.

This package was fundamental to implementing the [FuseApi](https://github.com/KlausC/FuseApi.jl) package.

## Installation

]add CStructures

## Usage

    using CStructures

    struct LayoutType <: Layout
        field::Int
    end

    cs = CStruct{LayoutType}(c_data)
    cs.field = cs.field + 1

    se = Cserialize(LayoutType, (field = 42,))
    ccall((:cf, :libc), Cvoid, (Ptr{LayoutType},), se)

    cg = CStructGuarded(se)
    cg.field = 43 

## Layout Elements

A data layout is described by a Julia bitstype struct, a fixed or variable vector descriptor, a `Cstring`, or a
reference descriptor.

### Bits Types

All primitve types defined in Julia, which have an identical C-representation can be used as layout descriptions.
That includes immutable structs of such types, which have the `isbitstype` attribute.

### String Type

The special type `Cstring` is used to represent a `char*` pointer. It occupies the space of any `Ptr`.

### Reference Types

To describe `C`-pointers to primitive objects or C-structures, the `Ptr{T}` notion is used.
Here `T` is a Julia type name. It needs to be defined in the code before the usage.

To support referencing types, which will be defined later, as is possible in `C`, the
special construct `LForwardReference{:S}` was introduced which uses the symbolized name `:S` of
the referenced type, which can be defined later. This feature will become obsolete, as soon as `Julia`
will support forward references [PR#32658](https://github.com/JuliaLang/julia/pull/32658).

Element type `T` must be a reference type.

### Vector Types

A fixed length vector of `N` elements of type `T` is denoted `LFixedVector{T,N}`. It has the size of
`NTuple{N,T}`, where `T` is any of the supported types. A pointer to a vector is `Ptr{LFixedVector{T,N}}`
or `LForwardReference{LFixedVector{T,N}}`.

A variable length vector is denoted `LVarVector{T,F}` with the same restrictions to `T` as for fixed vectors.
It can be embedded as the last element of a layout structure or the element type of a reference.
The actual length can be calculated by `F(x)` with `x` the current structure, where the vector is referenced.
Typically that uses to be `F = (x) -> x.fieldname`, the vector length stored in an integer field.

### Layout Types

An bitstype struct, which is a subtype of `Layout` is composed of fields, which have an identical memory layout as
a corresponding C-struct.
The fields may be any of the mentioned layout types, but not directly self-referential
(only via `Ptr` or `LForwardReference`).

## Accessor Objects

### Accessing C-Data


### Serializing Julia-Data according to Layout

Complex Layout types don't have instances in general.  

## Example

    struct Commandline <: Layout
        argc::CInt
        argv::Ptr{LVarVector{Cstring, (x)->x.argc + 1}}
    end
    # Note `argv[1:argc]` correspond to argv[0] ... argv[argc-1] in `C` and `argv[argc+1] = CNull`.

Assuming a `C`-function returns a pointer to a `C`-structure with the layout of `Commandline`, that could be
`C`-code like

    struct Commandline {
        size_t argc; char** argv
    }

The in `Julia` that could be accessed:

    p = ccall(:argfunction, Ptr{Commandline}, ())
    cline = CStruct(p)
    cline.argc::CInt
    cline.argv[i]::Union{String,Nothing}
