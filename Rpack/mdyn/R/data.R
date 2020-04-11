#' @title Estimated risk
#'
#' @description List containing th risk of infection by COVID-19 estimated to each city of each available state of Brazil by
#' Peixoto et. al. (2020).
#' @details Data of each state is named in the list by the two letters in lowercase which represent their name.
#' @return \item{State}{The name of the state.}
#' @return \item{City}{The performance in Calculus II.}
#' @return \item{sXXX}{The rank infection estimated for each intensity of moviment \emph{s}. See Peixoto et. al. (2020) for more details.}
#' @return \item{risk_lesser}{Risk calculated considering only the values of s lesser than one.}
#' @return \item{risk_greater}{Risk calculated considering only the values of s greater or equal to one.}
#' @return \item{risk}{Risk calculated considering all values of s.}
#' @references Peixoto, et. al. Modeling future spread of infections via mobile geolocation data and population dynamics.
#' An application to COVID-19 in Brazil. 2020. Available at <https://www.ime.usp.br/~pedrosp/covid-19/>.
"risk"
