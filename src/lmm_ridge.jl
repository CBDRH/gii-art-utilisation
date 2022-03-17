
using LinearAlgebra, SparseArrays
import StatsBase: counts 
import NLopt, Statistics
using Optim


"""
carry mats around
"""
struct Dmat
    ZtZ
    XtZ
    Xty
    Zty
    XtX
    Dmat(X,Z,y) = new(Z'Z,X'Z,X' * y,Z' * y,Symmetric(X'X, :U))
end   


"""
Fit a single level


grps: 1,...,n
"""
function lmm_fit(X,grps,y,λ)

    # dimensions / data
    n = length(y)
    pr = length(unique(grps))
    pf = size(X,2)
    dims = (n,pr,pf)
    
    # make Z
    col_j = zeros(Int,n)
    res = counts(grps)
    mm = 1
    for i in 1:pr
        col_j[mm:(mm+res[i]-1)] .= repeat([i],res[i])
        mm += res[i]
    end
    Z = sparse(1:n,col_j,1.0)

    # matrices
    D = [X Z]
    DtD = D'D
    Dty = D'y

    # optimisation setup
    B = zeros(pr+pf)
    θ0 = ones(2)

    # optimise
    opt = Optim.optimize(var -> deviance!(B,D,DtD,Dty,y,var,dims,λ), log.(θ0))
    θ = exp.(Optim.minimizer(opt))

    # output
    out = LMM(B[1:pf],θ,B[(pr+1):end]),opt
    out
end


"""
deviance!

"""
function deviance!(B,D,DtD,Dty,y,logθ,dims,λ)
    σ_b, σ = exp.(logθ)

    # dimensions
    n,pr,pf = dims

    # estimate β and B
    ipsi = [[0.0];repeat([λ],pf-1);repeat([(1/σ_b^2)],pr)]
    XX = DtD*(1/σ^2) + diagm(ipsi) 
    yy = Dty*(1/σ^2)
    B[:] = XX \ yy

    # likelihood
    ldet = 0.5*logdet(cholesky(DtD[(pf+1):end,(pf+1):end]*(1/σ^2) + (1/σ_b^2)*I))
    l = (sum((y - D*B).^2)*(1/σ^2) + sum((B[(pf+1):end].^2))*(1/σ_b^2) + sum((B[2:pf].^2))*λ + n*log(σ^2) + pr*log(σ_b^2) + 2*ldet + n*log(2*π))/2
    l
end


"""
Look for faster package

grps is sorted
"""
function count_grps(sorted_grps)
    return(counts(sorted_grps))
end


"""
A struct to store the results of our LMM estimation
"""
struct LMM
    β
    θ
    b
end
