export Layout, LForwardReference, LFixedVector, LVarVector
export is_layout_fixed, is_layout_variable, simple_size, total_size, Cserialize

"""
Layout

All structs used to describe the memory layout (of a C-data structure) need to be
subtypes of this.
Some controlling objects used in such templates to describe vectors and pointers
have also this type.
A `Layout` structure and a memory pointer are needed to construct an `CAccessor` object.
"""
abstract type Layout end

abstract type LVector{T} <: Layout end

Base.eltype(::Type{<:LVector{T}}) where T = T

# Layout Elements
"""
    LFixedVector{T,N}

Denote a fixed size vector with element type `T` and size `N`.
"""
struct LFixedVector{T,N} <: LVector{T}
    p::NTuple{N,T}
end
get_length(::Type{LFixedVector{T,N}}, ::Any) where {T,N} = N
Base.eltype(::Type{LFixedVector{T,N}}) where {T,N} = T

"""
    LVarVector{T,F}

Denote a variable length vector with element type `T` in a template.
`F` is a function, which calculates the length of the vector, given the
accessor object containing the vector.

Example:
    struct A <: Layout
        len::Int
        vec::NVarVector{Float64, (x) -> x.len}
    end

"""
struct LVarVector{T,F}  <: LVector{T}
    p::NTuple{1,T}
end

function get_length(::Type{LVarVector{T,F}}, x) where {T,F}
    F isa Symbol ? getproperty(x, F) :
    F isa Integer ? getindex(x, F) :
    F isa Function ? F(x) :
    0
end
function set_length!(::Type{LVarVector{T,F}}, t::Tuple{U,Ptr}, v) where {T,F,U}
    S, p = t
    ix = 1
    G = Nothing
    if F isa Symbol
        # setproperty!(x, v, F)
        S <: Layout || return v
        i = findfirst(isequal(F), fieldnames(S))
        i === nothing && return v
        G = fieldtype(S, i)
        p += fieldoffset(S, i)
    elseif F isa Integer
        # setindex!(x, v, F)
        S <: LFixedVector || return v
        G = eltype(S)
        ix = Int(F)
        ix <= 0 && return v 
    end
    G === Nothing && return v
    vv = G(v)
    unsafe_store!(Ptr{G}(p), vv, ix)
    vv
end

Base.eltype(::Type{LVarVector{T,F}}) where {T,F} = T

struct LForwardReference{M,L} <: Layout
    p::Ptr{Nothing}
end
Base.eltype(::Type{LForwardReference{M,L}}) where {M,L} = M.name.module.eval(L)

const TEMPLATE_FIXED = true
const TEMPLATE_VAR = false
"""
    is_template_variable(type)

Has the layout described by `type` a variable size
(for example variable sized vector in last field of a struct)?
"""
is_layout_variable(T::Type, deep::Bool=false) = !is_layout_fixed(T, deep)

"""
    is_template_fixed(type)

Has the layout described by `type` a fixed size.
"""
is_layout_fixed(T::Type, deep::Bool=false) = is_layout_fixed(T, deep, Dict())
function is_layout_fixed(::Type{T}, deep::Bool, dup) where T
    isprimitivetype(T) || throw(ArgumentError("$T is not a supported layout type"))
    TEMPLATE_FIXED
end
function is_layout_fixed(::Type{S}, deep::Bool, dup) where {T,S<:Ptr{T}}
    T <: Ptr && throw(ArgumentError("$S is not a supported layout type"))
    get!(dup, S) do
        dup[S] = TEMPLATE_FIXED
        d = is_layout_fixed(T, deep, dup)
        deep ? d : TEMPLATE_FIXED
    end
end
function is_layout_fixed(::Type{S}, deep::Bool, dup) where {S<:LForwardReference}
    is_layout_fixed(Ptr{eltype(S)}, deep, dup)
end
function is_layout_fixed(::Type{S}, deep::Bool, dup) where {T,N,S<:LFixedVector{T,N}}
    get!(dup, S) do
        dup[S] = TEMPLATE_FIXED
        k = is_layout_fixed(T, deep, dup)
        if N > 1 && k == TEMPLATE_VAR
            throw(ArgumentError("$S with variable length elements"))
        end 
        N == 0 ? TEMPLATE_FIXED : k
    end
end
function is_layout_fixed(::Type{S}, deep::Bool, dup) where {T,S<:LVarVector{T}}
    get!(dup, S) do
        dup[S] = TEMPLATE_VAR
        is_layout_fixed(T, deep, dup)
        TEMPLATE_VAR
    end
end
function is_layout_fixed(::Type{T}, deep::Bool, dup) where {T<:Layout}
    get!(dup, T) do
        k = dup[T] = TEMPLATE_FIXED
        if !isbitstype(T)
            text = isconcretetype(T) ? "bits" : "concrete"
            throw(ArgumentError("$T is not a $text type struct"))
        end
        fields = fieldnames(T)
        n = length(fields)
        for i = 1:n
            f = fields[i]
            F = fieldtype(T, f)
            k = is_layout_fixed(F, deep, dup)
            if i < n && k == TEMPLATE_VAR
                throw(ArgumentError("$F has variable length in '$T.$f' - not last field"))
            end
        end
        k
    end
end

function align(p::Integer, s::Integer=sizeof(Ptr)) # s must be 2^n
    t = s - 1
    (p + t )  & ~t
end

function align(p::Integer, ::Type{T}) where T
    align(p, Base.aligned_sizeof(T))
end

simple_size(T::Type, veclens) = blength(T, veclens, Val(false))
total_size(T::Type, veclens) = blength(T, veclens, Val(true))

function blength(::Type{T}, veclens, v::Val{P}) where {P,F,N,T<:LFixedVector{F,N}}
    s = sizeof(T)
    j = 0
    for _ = 1:N
        j, s = blength_helper(F, veclens, j, s, v, T)
    end
    if j < length(veclens)
        throw(ArgumentError("too many variable length specifiers for $T only $j are needed"))
    end
    s
end

function blength(::Type{T}, veclens, v::Val{P}) where {P,S,T<:LVarVector{S}}
    isempty(veclens) && return 0
    n = first(veclens)
    n == 0 && return 0
    n < 0 && throw(ArgumentError("negative vector length '$n' not allowed"))
    vl(i) = i < length(veclens) ? veclens[i+1] : ()
    sum(blength(S, vl(i), v) for i = 1:n)
end

blength(::Type{Ptr{T}}, veclens, v::Val{P}) where {P,T} = P ? blength(T, veclens, v) : 0

function blength(::Type{T}, veclens, v::Val{P}) where {P,T<:Layout}
    s = sizeof(T)
    j = 0
    for i = 1:fieldcount(T)
        F = fieldtype(T, i)
        j, s = blength_helper(F, veclens, j, s, v, T)
    end
    if j < length(veclens)
        throw(ArgumentError("too many variable length specifiers for $T- only $j are needed"))
    end
    s
end

blength(::Type{T}, veclens, ::Val) where T = sizeof(T)

function blength_helper(::Type{F}, veclens, j, s, v::Val{P}, T) where {P,F}
    if is_layout_variable(F, true)
        j += 1
        if j > length(veclens)
            throw(ArgumentError("not enough variable length specifiers for $T"))
        end
        vl = veclens[j]
    else
        vl = ()
    end
    al = Base.datatype_alignment(F)
    s = align(s, al)
    s += F <: LVarVector ? blength(F, vl, v) :
        P && F <: Union{Ptr,LForwardReference} ? blength(eltype(F), vl, v) : 0

    j, s
end

"""
    Cserialize(::Type{T}, source::Any) 

Convert the julia object `source` into a byte vector to be used in C.
The process is controlled by the layout type recursively.

The resulting vector contains only data described in `T`.
The field, vector element or bit data required by `T` are taken from `source`
if available in a corresponding part. Other data are filled with 0-bytes.

If `T` is a structure, corresponding fields in source are by the same name.
If `T` is a vector, corresponding elements in source are by the same index.
If `T` is a `Ptr{S}`, the space for a pointer is reserved and filled with
the offset integer (of same size), while the `S` object is appended at the end
of the serialization stream.

Finally all offset integers are replaced by actual pointers.
"""
function Cserialize(::Type{T}, src) where T
    buf = UInt8[]
    rea = Int[]
    off = Cserialize!(T, src, buf, 0, rea)
    resize!(buf, off)
    relocate!(buf, rea)
end

function Cserialize!(::Type{T}, src, buf, off, rea::Vector{Int}) where T
    ctx = Tuple{Integer,Type,Any}[]
    noff = _Cserialize!(T, src, buf, off, rea, ctx)
    for (poff, F, src) in ctx
        noff = align(noff)
        ensure!(buf, poff, sizeof(Ptr))
        p = pointer(buf) + poff
        q = noff
        Base.unsafe_store!(Ptr{Ptr{F}}(p), Ptr{F}(q))
        noff = Cserialize!(F, src, buf, noff, rea)
    end
    align(noff)
end

function _Cserialize!(::Type{T}, src::S, buf::Vector{UInt8}, off::Integer, rea, ctx) where {T<:Layout,S}
    ensure!(buf, off, sizeof(T))
    noff = off
    for i = 1:fieldcount(T)
        F = fieldtype(T, i)
        f = fieldname(T, i)
        x = fieldoffset(T, i)
        if hasproperty(src, f)
            noff = _Cserialize!(F, getproperty(src, f), buf, off + x, rea, ctx)
        elseif F <: LVarVector
            throw(ArgumentError("need src vector $f to determine length"))
        else
            noff = off + x + Base.aligned_sizeof(F)
        end
    end
    noff
end
function _Cserialize!(::Type{<:LFixedVector{T,N}}, src::AbstractVector, buf::Vector{UInt8}, off::Integer, rea, ctx) where {T,N}
    as = Base.aligned_sizeof(T)
    ensure!(buf, off, as * N)
    n = length(src)
    for i = 1:min(N, n)
        off = _Cserialize!(T, src[i], buf, off, rea, ctx)
        off = align(off, T)
    end
    if N > n
        off += as * (N - n)
    end
    off
end
function _Cserialize!(::Type{S}, src::AbstractVector, buf::Vector{UInt8}, off::Integer, rea, ctx) where {T,F,S<:LVarVector{T,F}}
    for i = 1:length(src)
        off = _Cserialize!(T, src[i], buf, off, rea, ctx)
        off = align(off, T)
    end
    off
end

function _Cserialize!(::Type{Cstring}, src::Nothing, buf::Vector{UInt8}, off::Integer, rea, ctx)
    alignptr(off)
end
function _Cserialize!(::Type{Cstring}, src, buf::Vector{UInt8}, off::Integer, rea, ctx)
    s = string(src)
    n = length(s) + 1
    v = codeunits(s)
    pushall!(rea, ctx, off, LFixedVector{UInt8,n}, v)
    alignptr(off)
end

function _Cserialize!(::Type{T}, src, buf::Vector{UInt8}, off::Integer, rea, ctx) where T
    if isbitstype(T)
        s = Base.aligned_sizeof(T)
        ensure!(buf, off, s)
        p = pointer(buf) + off
        Base.unsafe_store!(Ptr{T}(p), convert(T, src))
        off + s
    else
        throw(ArgumentError("cannot serialize type $T"))
    end
end

function _Cserialize!(::Type{Ptr{T}}, src::Nothing, buf::Vector{UInt8}, off::Integer, rea, ctx) where T
    alignptr(off)
end
function _Cserialize!(::Type{Ptr{T}}, src, buf::Vector{UInt8}, off::Integer, rea, ctx) where T
    pushall!(rea, ctx, off, T, src)
    alignptr(off)
end

alignptr(off) = align(off + sizeof(Ptr))

"""
    pushall!(relocs::Vector{Int}, ctx::Vector{Tuple}, offset, Type, value}

push! the `offset` to `relocs` and the tuple `(offset, Type, value)` in `ctx`.
The `relocs` are finally used to replace offset values by pointers.
The `ctx` is used push back processing for later serializing. 
"""
function pushall!(rea::Vector{<:Integer}, ctx::Vector{<:Tuple}, off, T, v)
    push!(rea, off)
    push!(ctx, (off, T, v))
end

"""
    ensure!(buf::Vector, off, size)

Ensure that the size of `buf` is at least `off + size` by maybe resizing `buf`.
Added space is filled with zero bytes.
"""
function ensure!(buf::Vector{UInt8}, off::Integer, siz::Integer)
    n = sizeof(buf)
    m = off + siz
    if n < m
        resize!(buf, m)
        for i = n+1:m
            buf[i] = 0
        end
    end
    buf
end

"""
    relocate!(buffer::Vector, offsets)

In vector `buffer`, at the byte offsets stored in `offsets`, offset values (into buffer) are
stored as `Int` values. The are replaced by `Ptr` values into the data area of `buffer`.

It is essential, that the data area is not changed after this process, that means no
`resize!`, `push!`, etc. are allowed after this final fix of pointer values to be used in
C-calls.
"""
function relocate!(buf::AbstractVector, rea::AbstractVector{<:Integer})
    p0 = pointer(buf)
    for off in rea
        p = p0 + off
        q = unsafe_load(Ptr{UInt}(p)) + p0
        unsafe_store!(Ptr{Ptr{UInt8}}(p), q)
    end
    buf
end
