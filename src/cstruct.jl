
export Caccessor, CStruct, CVector, CStructAccess, CStructGuarded

import Base: length, size, pointer, show, unsafe_convert, Fix1
import Base: propertynames, getproperty, setproperty!, getindex, setindex!

abstract type CStructAccess{T} end

"""
CStruct{T}(p::Ptr)

Given a C-type pointer `p` to a C-struct and the equivalent Julia struct
with the same memory layout `T`, provide read and write access to the fields.
`T` must be a bits type.

Example:
struct T <: Layout
    a::Cint
    b::Cdouble
end

a = Vector{UInt8}(undef, 100)
p = pointer(a) # usually the data are coming from C
cs = CStruct{T}(p)

cs.a = 1234
cs.b = 3.5
"""
struct CStruct{T} <: CStructAccess{T}
    pointer::Ptr{Nothing}
    function CStruct{T}(p::Ptr) where T
        isbitstype(T) || throw(ArgumentError("$T is not a bitstype"))
        new{T}(p)
    end
    CStruct{T}(data) where T = CStruct{T}(pointer(data))
    CStruct(data) = CStruct(pointer(data))
    CStruct(p::Ptr{T}) where T = CStruct{T}(p)
end

struct CStructGuarded{T,D} <: CStructAccess{T}
    cs::CStruct{T}
    guard::Vector{D}
    function CStructGuarded{T}(data::Vector{D}) where {T,D<:Union{Integer,Ptr}}
        new{T,D}(CStruct{T}(data), data)
    end
end
CStructGuarded(::Type{T}, src=()) where T = CStructGuarded{T}(Cserialize(T, src))

"""
    CVector

Abstract vector type for julia objects used to access elements of C-vectors,
which are based by plain C memory. Memory layout is described by `Layout` structs.
"""
struct CVector{T} <: AbstractVector{T}
    pointer::Ptr{Nothing}
    length::Int
    function CVector{T}(p::Ptr, length::Integer=-1) where T
        isbitstype(T) || throw(ArgumentError("$T is not a bitstype"))
        new{T}(p, length)
    end
end

# accessing the fields represented by CStruct
# to access the pointer use function `pointer`
propertynames(::CStruct{T}) where T = fieldnames(T)
propertynames(::CStructGuarded{T}) where T = fieldnames(T)

function getproperty(cs::CStruct{T}, field::Symbol) where T
    fp = pointer_for_field(cs, field)
    get_from_pointer(fp, cs)
end
getproperty(sg::CStructGuarded, field::Symbol) = getproperty(getfield(sg, :cs), field)

function setproperty!(cs::CStruct{T}, field::Symbol, v) where T
    fp = pointer_for_field(cs, field)
    set_at_pointer!(fp, v)
end
setproperty!(sg::CStructGuarded, field::Symbol, v) = setproperty!(getfield(sg, :cs), field, v)

function getindex(cv::CVector{T}, i::Integer) where T
    p = pointer_for_index(cv, i)
    get_from_pointer(p, cv)
end

function getindex(cv::CVector{T}, r::OrdinalRange) where T
    [getindex(cv, i) for i in r]
end

function setindex!(cv::CVector{T}, v, i::Integer) where T
    p = pointer_for_index(cv, i)
    set_at_pointer!(p, v)
end

size(cv::CVector) = (length(cv),)

"""
    pointer(::Union{CStruct,CVector})
    length(::CVector)

get the internal fields of accessors
"""
pointer(cs::CStruct) = getfield(cs, :pointer)
pointer(cs::CVector) = getfield(cs, :pointer)
length(cv::CVector) = getfield(cv, :length)
pointer(sg::CStructGuarded) = pointer(getfield(sg, :cs))

function show(io::IO, x::CStructAccess{T}) where T
    show(io, typeof(x))
    print(io, '(')
    nf = length(T.types)
    if !Base.show_circular(io, x)
        recur_io = IOContext(io, Pair{Symbol,Any}(:SHOWN_SET, x),
                                 Pair{Symbol,Any}(:typeinfo, Any))
        for i in 1:nf
            f = fieldname(T, i)
            show(recur_io, getproperty(x, f))
            if i < nf
                print(io, ", ")
            end
        end
    end
    print(io, ')')
end

function show(io::IO, x::CVector{T}) where T
    show(io, typeof(x))
    print(io, '[')
    nf = length(x)
    if nf < 0
        print(io, "#= unknown length =#")
    elseif !Base.show_circular(io, x)
        recur_io = IOContext(io, Pair{Symbol,Any}(:SHOWN_SET, x),
                                Pair{Symbol,Any}(:typeinfo, Any))
        for i in 1:nf
            show(recur_io, getindex(x, i))
            if i < nf
                print(io, ", ")
            end
        end
    end
    print(io, ']')
end

"""
    get_from_pointer(::Ptr{T})

For bits types simply load value, convert to Julia accessor if required.
For struct types, create CStruct accessor.
For vector types, create CVector accessor.
"""
function get_from_pointer(fp::Ptr{FT}, parent) where FT <: Ptr
    v = unsafe_load(fp)
    v == C_NULL ? nothing : get_from_pointer(v, parent)
end

function get_from_pointer(fp::Ptr{FT}, parent) where {T,FT<:LVector{T}}
    CVector{T}(fp, get_length(FT, parent))
end

function get_from_pointer(fp::Ptr{FT}, parent) where FT <: Layout
    CStruct{FT}(fp)
end

function get_from_pointer(fp::Ptr{FT}, parent) where FT <: Cstring
    v = unsafe_load(fp)
    v == Cstring(C_NULL) ? "" : unsafe_string(Ptr{UInt8}(v))
end

function get_from_pointer(fp::Ptr{FT}, parent) where FT
    if FT <: Nothing
        fp
    elseif isbitstype(FT)
        unsafe_load(fp)
    else
        throw(ArgumentError("not supported layout type: $FT"))
    end
end

"""
    set_at_pointer(:Ptr, value)

Convert to C primitive or composed object. Store bytes at memory position.
"""
function set_at_pointer!(fp::Ptr{FT}, v) where FT
    w = unsafe_convert(FT, Base.cconvert(FT, v))
    unsafe_store!(fp, w)
end

"""
    pointer_for_field(cs::CStruct{T}, fieldname) where T

For `cs` return pointer to member field `fieldname`.
The pointer has type `Ptr{fieldtype(T, i)}` with `i` the number of the field
within struct type `T`. 
"""
function pointer_for_field(cs::CStruct{T}, field::Symbol) where T
    i = findfirst(Fix1(isequal, field), fieldnames(T))
    i === nothing && throw(ArgumentError("type $T has no field $field"))
    Ptr{fieldtype(T, i)}(getfield(cs, :pointer) + fieldoffset(T, i))
end

function pointer_for_index(cv::CVector{T}, i::Integer) where T
    Ptr{T}(getfield(cv, :pointer) + sizeof(T) * (i - 1))
end

unsafe_convert(::Type{Ptr{T}}, cs::CStructAccess{T}) where T = Ptr{T}(pointer(cs))
unsafe_convert(::Type{Ptr{Vector{T}}}, cs::CVector{T}) where T = Ptr{Vector{T}}(pointer(cs))

"""
    p = pointer(a::Vector{T})::Ptr{T}

return pointer to `a[1]`. The existence of the resulting Ptr will not protect the object
from garbage collection, so you must ensure that the object remains referenced for the whole
time that the Ptr will be used.
The condition `a[i] === unsafe_load(p, i)` is usually true.
Given `p` it is possible to access arbitrary bits data by byte offset and type `S` using
`unsafe_load(Ptr{S}(p + offset))`.

This function is mainly used to simulate a C memory in the data
area of vector `a`.
"""
pointer_from_vector_obs(a::Vector{T}) where T = unsafe_convert(Ptr{T}, a)



export default_value, default_type, construct

function default_type(::Type{T}) where T
    isconcretetype(T) || throw(ArgumentError("no default type defined for $T"))
    T
end
default_type(::Type{<:AbstractArray{T,N}}) where {T,N} = Array{T,N}
default_type(::Type{T}) where T<:Real = isconcretetype(T) ? T : Bool
default_type(::Type{T}) where T<:AbstractIrrational = isconcretetype(T) ? T : Irrational
default_type(::Type{T}) where T<:AbstractFloat = isconcretetype(T) ? T : Float64
default_type(::Type{T}) where T<:Signed = isconcretetype(T) ? T : Int
default_type(::Type{T}) where T<:Unsigned = isconcretetype(T) ? T : UInt
default_type(::Type{T}) where T<:Rational = isconcretetype(T) ? T : Rational{default_type(Signed)}
default_type(::Type{T}) where {S,T<:Rational{S}} = isconcretetype(T) ? T : Rational{default_type(S)}
default_type(::Type{T}) where T<:Complex = Complex{default_type(Real)}
default_type(::Type{T}) where {S,T<:Complex{S}} = Complex{default_type(S)}
default_type(::Type{T}) where T<:AbstractString = isconcretetype(T) ? T : String

default_value(::Type{T}) where T = _default_value(default_type(T))
default_value(::Type{A}) where {T,N,A<:AbstractArray{T,N}} = default_type(A)(undef,zeros(Int,N)...)
default_value(::Type{T}) where T<:Number = default_type(T)(0)
default_value(::Type{T}) where T<:AbstractIrrational = default_type(T)(ℯ)
default_value(::Type{T}) where {S,T<:Complex{S}} = default_type(T)(default_value(S))
default_value(::Type{T}) where T<:AbstractString = default_type(T)("")

function _default_value(::Type{T}) where T
    @assert isconcretetype(T)
    ft = fieldtypes(T)
    fv = default_value.(ft)
    construct(T, fv...)
end

default_value(::Type{T}, v) where T<:Union{Number,AbstractString} = convert(T, v)
function default_value(::Type{T}, v) where {S,T<:AbstractArray{S}}
    convert(T, [default_value(S, x) for x in v])
end
function default_value(::Type{T}, v) where T
    f = fieldnames(T)
    n = length(f)
    r = Vector{Any}(undef, n)
    for i in 1:n
        fn = fieldname(T, i)
        ft = fieldtype(T, i)
        r[i] = hasproperty(v, fn) ? default_value(ft, getproperty(v, fn)) : default_value(ft)
    end
    construct(T, r...)
end

construct(::Type{T}, args...) where T = construct(Val(!ismutabletype(T)), T, args...)
function construct(::Val{true}, ::Type{T}, args...) where T
    r = Vector{T}(undef, 1)
    p = pointer(r)
    _construct!(T, p, args)
    r[1]
end
function construct(::Val{false}, ::Type{T}, args...) where T
    r = _construct_any(T)
    p = pointer_from_objref(r)
    _construct!(T, p, args)
    r
end

function _construct!(::Type{T}, p::Ptr, args) where T
    n = min(length(args), fieldcount(T))
    for i = 1:n
        off = fieldoffset(T, i)
        ft = fieldtype(T, i)
        v = convert(ft, args[i])
        q = p + off
        if isbitstype(ft)
            unsafe_store!(Ptr{ft}(q), v)
        else
            unsafe_store!(Ptr{Ptr{Nothing}}(q), pointer_from_objref(v))
        end
    end
end

function _construct_any(::Type{T}) where T
    m = first(methods(T))
    at = m.sig.types[2:end]
    T(default_value.(at)...)
end
