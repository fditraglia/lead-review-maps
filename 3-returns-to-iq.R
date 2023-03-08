# IQ points lost as a function of BLL (Lanphear, 2005)
iq_loss <- function(bll) {
  # Previously we multiplied this by 20568 to convert to a dollar cost; here
  # the result is in IQ points.
  pmin(bll, 10) * 0.513 + (bll >= 10) * pmin(bll - 10, 20 - 10) * 0.19 +
             (bll >= 20) * (bll - 20) * 0.11
}