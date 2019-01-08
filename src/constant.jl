#############################################################################
# constant.jl
# Defines Constant, which is a subtype of AbstractExpr
#############################################################################
export Constant
export vexity, evaluate, sign, conic_form!

const ComplexValue = Union{Complex,AbstractVecOrMat{<:Complex}}

ispos(x::Real) = x >= 0
ispos(v::AbstractVecOrMat{<:Real}) = all(ispos, v)
isneg(x::Real) = x <= 0
isneg(v::AbstractVecOrMat{<:Real}) = all(isneg, v)

_size(x::Number) = (1, 1)
_size(x::AbstractVector) = (length(x), 1)
_size(x::AbstractMatrix) = size(x)

_sign(x::ComplexValue) = ComplexSign()
_sign(x::Value) = ispos(x) ? Positive() :
                  isneg(x) ? Negative() : NoSign()

struct Constant{T<:Value} <: AbstractExpr
    head::Symbol
    id_hash::UInt64
    value::T
    size::Tuple{Int, Int}
    sign::Sign

    function Constant(x::T, sign::Union{ComplexSign,NoSign}) where T<:ComplexValue
        new{T}(:constant, objectid(x), x, _size(x), sign)
    end

    function Constant(x::T, sign::Union{Positive,Negative,NoSign}) where T<:Value
        new{T}(:constant, objectid(x), x, _size(x), sign)
    end

    Constant(x::Value, check_sign::Bool=true) = Constant(x, check_sign ? _sign(x) : NoSign())
end

#### Constant Definition end     #####

vexity(::Constant) = ConstVexity()

evaluate(x::Constant) = x.value

sign(x::Constant) = x.sign

real_conic_form(x::Constant{<:Number}) = [real(x.value)]
real_conic_form(x::Constant{<:AbstractVecOrMat}) = vec(real(x.value))

imag_conic_form(x::Constant{<:Number}) = [im * imag(x.value)]
imag_conic_form(x::Constant{<:AbstractVecOrMat}) = im * vec(imag(x.value))

# We can more efficiently get the length of a constant by asking for the length of its
# value, which Julia can get via Core.arraylen for arrays and knows is 1 for scalars
length(x::Constant) = length(x.value)

function conic_form!(x::Constant, unique_conic_forms::UniqueConicForms=UniqueConicForms())
    if !has_conic_form(unique_conic_forms, x)
        #real_Value = real_conic_form(x)
        #imag_Value = imag_conic_form(x)
        objective = ConicObj()
        objective[objectid(:constant)] = (real_conic_form(x), imag_conic_form(x))
        cache_conic_form!(unique_conic_forms, x, objective)
    end
    return get_conic_form(unique_conic_forms, x)
end
