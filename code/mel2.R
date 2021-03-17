function (time, y, p) 
{
    with(as.list(c(y, p)), {
        Unm <- psi * alpha_n * Dc^2 * En/(theta * Dn * (k_nm + 
            En))
        Ucm <- 0
        Mc <- psi * Dc + Ucm
        Mn <- psi * Dn + Unm
        Lambda_c <- epsilon_c * theta * Mn/(Mc + theta * Mn)
        Lambda_n <- epsilon_n * Mc/(Mc + theta * Mn)
        q <- q_w * Bc/(k_q + Bc)
        Sc <- (Bc + q * Bn)^zc
        Sn <- (Bc + q * Bn)^zn
        Ucv <- gc * Vc * Sc * Ec/(kc + Ec)
        Unv <- gn * Vn * Sn * En/(kn + En)
        Lcv <- mc * Bc
        Lnv <- mn * Bn
        Lce <- beta_ce * Ec
        Lne <- beta_ne * En
        Lcd <- beta_d * Dc
        Lnd <- beta_d * Dn
        Rcm <- Mc * (1 - Lambda_c)
        Rnm <- Mn * (1 - Lambda_n)
        Rcv <- rc * Bc
        Rnv <- rn * Bn
        dE_N <- Rne + Rnm + Rnv - Lne - Unm - Unv
        dE_C <- 0
        dB_C <- Ucv - Rcv - Lcv
        dB_N <- Unv - Rnv - Lnv
        dD_C <- Lcv + Ucm - Lcd - Rcm
        dD_N <- Lnv + Unm - Lnd - Rnm
        Grc <- dB_C/Bc
        Grn <- dB_N/Bn
        A <- log(Bc/(q * Bn)) + tau * (Grc - Grn)
        Vstar <- (A > 0) * Vc + (A < 0) * Vn
        dV_C <- -1 * a * A * Vstar
        dV_N <- -1 * dV_C
        NPP <- Ucv - Rcv
        Net_N_uptake <- Unv - Rnv
        Ecosystem_N = Bn + En + Dn
        Ecosystem_C = Bc + Ec + Dc
        return(list(c(dE_C, dE_N, dB_C, dB_N, dD_C, dD_N, dV_C, 
            dV_N), NPP = NPP, Net_N_uptake = Net_N_uptake, Ecosystem_C = Ecosystem_C, 
            Lne = Lne))
    })
}
