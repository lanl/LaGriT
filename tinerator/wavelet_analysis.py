import numpy as np
import tinerator.config as cfg

def get_w(dim):
    '''
    Return Haar coefficient matrix

    # Arguments
    dim (int): dimension of the Haar coefficient matrix (dim x dim)
    '''
    _w = np.zeros((dim, dim))
    _l = int(dim / 2)
    _r = np.array(range(_l))  # indices for the rows, low frequency
    _c1 = 2 * _r  # indices for the columns
    _c2 = 2 * _r + 1
    _w[_r[:], _c1[:]] = 0.5
    _w[_r[:], _c2[:]] = 0.5
    _r = np.array(range(_l, dim))  # indices for rows, high frequency
    # column indices remain unchanged
    _w[_r[:], _c1[:]] = 0.5
    _w[_r[:], _c2[:]] = - 0.5
    return _w * np.sqrt(2.0)

def dht(A, N):
    '''
         | B : V |
    M =  |-------|
         | H : D |
    where B is the blur image, V is the vertical detail, H is the
    horizontal detail, and D is the diagonal detail.

    # Arguments
    A (np.ndarray): input to the wavelet analysis
    N (float): number of total refinement levels
    '''
    
    M = np.zeros_like(A)

    # the dimensions m and n are modified, as we iterate over refinement
    # levels. for each level of coarsening, m and n are halved.
    _m, _n = np.shape(A)
    cfg.log.info('Haar wavelet transform of a {0:d}-by-{1:d} matrix.'.format(_m, _n))

    for step in range(N):
        if _m % 2 != 0 or _n % 2 != 0:
            # if non-divisible, DHT can't be applied since it depends on pair-wise
            # comparisons. in that case, we abort the step, set the maximum level
            # of refinement N to the current refinement level, and exit.
            N = step + 1
            cfg.log.info('wavelet step: {0:5d}, m: {1:5d}, n: {2:5d}'.format(step, _m, _n))
            cfg.log.info('dimensions not divisible by 2. abort.')
            import sys
            sys.exit(1)

        # get the coefficient matrix for left matrix multiplication,
        # i.e. C = Wm A. this matrix needs to have dimension `m x m'. after
        # that, carry out the matrix multiplication to get the
        # column-compressed matrix C.
        Wm = get_w(_m)
        C = np.matmul(Wm, A)

        # carry out the row compression. we need a coefficient matrix with
        # dimension `n x n' to multiply M = C Wn^T. This will compress the
        # image one level.
        Wn = get_w(_n)
        M[:_m, :_n] = np.matmul(C, np.transpose(Wn))

        # now get the new image matrix. we know that it is located at the
        # upper left corner of the compressed image M, and has dimensions
        # m/2 x n/2.
        _m = int(_m/2)
        _n = int(_n/2)
        A = M[:_m, :_n]
        cfg.log.info('wavelet step: {0:5d}, m: {1:5d}, n: {2:5d}'.format(step, _m, _n))

    return M


def hard_threshold(M, level, N, t):
    '''
    Hard thresholding as explained in, e.g. (Caviedes-Voullieme and
    Kesserwani, 2015: Benchmarking a multi-resolution discontinuous
    Galerkin shallow water model, Advances in Water Resources,
    86:14-31).

    # Arguments
    M (np.ndarray): Discrete Haar Transform
    level (int): current refinement level
    N (int): total number of refinement levels
    t (float): threshold for cut off
    '''
    
    _r, _c = np.shape(M)

    #      | B : V |
    # M =  |-------|
    #      | H : D |
    # get these matrices for easier reading
    _B = M[:int(_r/2), :int(_c/2)]
    _V = M[int(_r/2):_r, :int(_c/2)]
    _H = M[:int(_r/2), int(_c/2):_c]
    _D = M[int(_r/2):_r, int(_c/2):_c]

    _err = 2**(level - N) * t  # level corrected threshold
    _max_avg = np.max(np.abs(_B))  # - np.min(np.abs(_B)) + 1.0e-11 #
    # maximum average coefficient w/ a
    # division by zero -> TODO
    _max_delta = np.maximum(np.abs(_V), np.maximum(
        np.abs(_H), np.abs(_D)))  # maximum detail
    # normalize the detail coefficient
    _norm_delta = _max_delta / max(_max_avg, 1.0)

    return _norm_delta < _err


def decompress(M, N, t):
    '''
    Decompress the discrete Haar transform and predict refinement levels.

    # Arguments
    M (np.ndarray): Discrete Haar Transform
    N (int): total number of refinement levels
    t (float): threshold for cut off
    '''
    
    T = np.zeros_like(M, dtype=int) + (N - 1)
    m, n = np.shape(M)
    _n = int(n / 2**N)
    _m = int(m / 2**N)

    # decompressing the DHT, in each level we look at the details and
    # see if they are significant.
    # i = 0 is the coarsest level, and the subsequent finer levels are
    # i = 1, 2, ... , N - 1, respectively.
    for i in range(N):
        
        flag = hard_threshold(M[:2*_m, :2*_n], i, N, t)
        # reconstruct the DHT to move to finer resolution
        Wn = get_w(2*_n)
        Wm = get_w(2*_m)
        M[:2*_m, :2*_n] = np.matmul(np.matmul(Wm.T, M[:2*_m, :2*_n]), Wn)

        # now tag cells that are sufficiently refined
        flag_index = np.where(flag)
        for _ii in range(len(flag_index[0])):
            _x = flag_index[0][_ii]
            _y = flag_index[1][_ii]
            if T[(2**(N-i))*_x, (2**(N-i))*_y] > i:
                T[(2**(N-i))*_x:(2**(N-i))*_x+(2**(N-i)),
                  (2**(N-i))*_y:(2**(N-i))*_y+(2**(N-i))] = i

        # adjust the dimensions, we are now refining so m and n need to be doubled
        _m = 2*_m
        _n = 2*_n

    return T
