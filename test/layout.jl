
struct L0
    a::Int
end

struct L1 <: Layout
    a::Int
end

struct L2 <: Layout
    a::LForwardReference{L2, :L3}
end

struct L3 <: Layout
    a::L2
end

struct L4 <: Layout
    a::LForwardReference{L4, :L_not_defined}
end

struct L5 <: Layout
    a::LVarVector{Int,1}
end

struct L6 <: Layout
    a::LVarVector{Int,1}
    b::Int
end
struct L7 <: Layout
    a::Ptr{Ptr{Int}}
end

@testset "simple layout templates" begin
    @test_throws ArgumentError is_layout_fixed(L0)
    @test_throws ArgumentError is_layout_fixed(Vector{Int})
    @test is_layout_fixed(Float64)
    @test is_layout_fixed(L1)
    @test is_layout_fixed(Ptr{L1})
    @test is_layout_fixed(Ptr{L5})
    @test is_layout_fixed(LFixedVector{Int,10})
    @test_throws ArgumentError is_layout_fixed(LFixedVector{Int})
    @test is_layout_variable(LVarVector{Int,1})
    @test is_layout_fixed(L2)
    @test_throws UndefVarError is_layout_fixed(L4)
    @test !is_layout_fixed(L5)
    @test_throws ArgumentError is_layout_fixed(L6)
    @test_throws ArgumentError is_layout_fixed(L7)
    @test is_layout_fixed(LFixedVector{L1,1})
    @test is_layout_fixed(LFixedVector{L5,0})
    @test !is_layout_fixed(LFixedVector{L5,1})
    @test_throws ArgumentError !is_layout_fixed(LFixedVector{L5,2})
    @test !is_layout_fixed(LVarVector{L1,1})
    @test !is_layout_fixed(LVarVector{L5,0})
end
