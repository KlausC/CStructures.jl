

struct A1 <: Layout
    a::Bool
    b::Cint
    c::Float64
    d::Cstring
end


@testset "field access" begin
    a = fill(UInt64(0), 3)
    p = pointer(a)
    cs = CStruct{A1}(p)
    @test cs.a == 0
    @test cs.b == 0
    @test cs.c == 0
    @test cs.d == ""
    v1 = true
    v2 = 0x12345678
    v3 = 47.11
    v4 = "hallo"
    cs.a = v1
    cs.b = v2
    cs.c = v3
    cs.d = v4
    @test cs.a == v1
    @test cs.b == v2
    @test cs.c == v3
    @test cs.d == v4
end

@testset "index access" begin
    a = fill(UInt64(0), 100)
    p = pointer(a)
    cv = CVector{Int}(p, 3)
    @test length(cv) == 3
    cv[1:3] .= (1, 2, 3)
    @test cv[2] == 2
    @test cv[[1,3]] == [1, 3]
end

struct A2 <: Layout
    a::Ptr{A2}
end

@testset "self-referencing" begin
    a = fill(UInt8(0), 100)
    p = pointer(a)
    cs = CStruct{A2}(p)
    @test cs.a === nothing
    io = IOBuffer()
    show(io, cs)
    @test String(take!(io)) == "CStruct{A2}(nothing)"
    cs.a = cs
    @test cs.a === cs
    show(io, cs)
    @test String(take!(io)) == "CStruct{A2}(CStruct{A2}(#= circular reference @-1 =#))"
end

struct A3 <: Layout
    len::Int
    vec::LVarVector{Float64, (x) -> x.len}
end

@testset "variable vector at end of struct" begin
    a = fill(Int(0), 1024)
    p = pointer(a)
    LEN = 25
    cs = CStruct{A3}(p)
    cs.len = LEN
    @test cs.vec isa CVector{Float64}
    @test length(cs.vec) == cs.len == LEN
end

struct A4 <: Layout
    len::Int
    vec::Ptr{LVarVector{Float64, (x) -> x.len}}
end

@testset "pointer to variable vector" begin
    a = fill(Int(0), 1024)
    p = pointer(a)
    a[2] = p + 32
    LEN = 25
    cs = CStruct{A4}(p)
    cs.len = LEN
    @test cs.vec isa CVector{Float64}
    @test length(cs.vec) == cs.len == LEN
end

struct B <: Layout
    a::Int
    b::LVarVector{Float64, :a}
end

@testset "variable length vector in struct" begin
    @test_throws ArgumentError Cserialize(B, ())
    cs = CStruct{B}(Cserialize(B, (a=0, b=Float64[])))
    @test length(cs.b) == cs.a == 0
    cs = CStruct{B}(Cserialize(B, (a=2, b=[1.0; 2; 3])))
    @test length(cs.b) == cs.a == 2
    cs.a = 3
    @test length(cs.b) == cs.a == 3
end

struct I1
    a::Int8
    b::Int16
    I1() = new(1, 1)
end

mutable struct M1
    a::Int8
    b::Float64
    M1() = new(2, 2)
end

struct I2
    a::Int16
    b::M1
    I2() = new(3, M1())
end

mutable struct M2
    a::Int16
    b::M1
    M2() = new(4, M1())
end

@testset "construct function $T" for (T, a1, a2) in ((I1, 12, 12), (M1, 12, 12), (I2, 12, M1()), (M2, 12, M1()))
    @test construct(T, a1, a2) isa T
end
