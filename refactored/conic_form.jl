export ConicObj, ConicConstr
export +

type ConicObj
  vars_to_coeffs::Dict{Uint64, Value}
end

function -(c::ConicObj)
  for var in keys(c.vars_to_coeffs)
    c.vars_to_coeffs[var] *= -1
  end
end

function +(c::ConicObj, d::ConicObj)
  new_obj = ConicObj(copy(c.vars_to_coeffs))
  for var in keys(d.vars_to_coeffs)
    if !haskey(new_obj.vars_to_coeffs, var)
      new_obj.vars_to_coeffs[var] = d.vars_to_coeffs[var]
    else
      # .+ does not behave properly for sparse matrices
      # need to override behavior
      new_obj.vars_to_coeffs[var] .+= d.vars_to_coeffs[var]
    end
  end
  return new_obj
end


type ConicConstr
  coeffs::ConicObj
  cone::Symbol
end

