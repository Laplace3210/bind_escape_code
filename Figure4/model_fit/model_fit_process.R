
source("D:/all_project/Italy_project/0_data_process/input/data_uesd.R")

gen_SEIARD <- odin.dust::odin_dust("SEIRV.R")


dt <- 0.25

##loading the real data
cases_data <- Italy_reported_cases[1:744,]
cases_data$day <- 1:nrow(cases_data)
cases_data <- cases_data[,-1]
#cases_data$new_cases <- cumsum(cases_data$new_cases)
names(cases_data)[1] <- 'cases'
#cases_data$cases[which(is.na(cases_data$cases)==T)] <- round(mean(c(cases_data$cases[which(is.na(cases_data$cases)==T) - 1 ], cases_data$cases[which(is.na(cases_data$cases)==T) + 1 ] )))
plot(cases_data$cases)
plot(cumsum(cases_data$cases))
cases_data$cases <- cumsum(cases_data$cases)


mc_data_Italy <- mcstate::particle_filter_data(data = cases_data,initial_time = 0,
                                               time = "day",
                                               rate = 1 / dt)

index <- function(info) {
  list(run = c(cases = info$index$E_inc),
       state = c(S0 = info$index$S0,
                 E0 = info$index$E0,
                 I0 = info$index$I0,
                 R0 = info$index$R0,
                 S1 = info$index$S1,
                 E1 = info$index$E1,
                 I1 = info$index$I1,
                 R1 = info$index$R1,
                 S2 = info$index$S2,
                 E2 = info$index$E2,
                 I2 = info$index$I2,
                 R2 = info$index$R2,
                 S3 = info$index$S3,
                 E3 = info$index$E3,
                 I3 = info$index$I3,
                 R3 = info$index$R3,
                 beta_v = info$index$beta_v,
                 rho_t = info$index$rho_t,
                 cases = info$index$E_inc,
                 eff_imm  = info$index$eff_imm,
                 beta_imm_0 = info$index$beta_imm_0,
                 beta_imm_1 = info$index$beta_imm_1,
                 beta_imm_2 = info$index$beta_imm_2,
                 beta_imm_3 = info$index$beta_imm_3
       )
  )
}

gamma_data <- c(gamma_data[1:744], rep(0.3333333, 6) )

Italy_model <- gen_SEIARD$new(pars = list(mobility= merged_mob_ave[1:744], gamma_t = gamma_data[1:744],
                                          p_imm_1 = adj_protection$aju_1[1:744],
                                          p_imm_2 = adj_protection$aju_2[1:744],
                                          p_imm_3 = adj_protection$aju_3[1:744],
                                          imm_score = escape_data$escape_score[1:744],
                                          V1 = as.double(vaccine_data_Italy_merged$d1[1:744]),
                                          V2 = as.double(vaccine_data_Italy_merged$d2[1:744]),
                                          V3 = as.double(vaccine_data_Italy_merged$db1[1:744])), 
                              step = 0, n_particles = 100L)
index(Italy_model$info())


# log-likelihood of Poisson count
ll_pois <- function(obs, model) {
  exp_noise <- 1e6
  if (is.na(obs)) {
    # Creates vector of zeros in ll with same length, if no data
    ll_obs <- numeric(length(model))
  } else {
    lambda <- model +
      rexp(n = length(model), rate = exp_noise)
    ll_obs <- dpois(x = obs, lambda = lambda, log = TRUE)
  }
  ll_obs
}

# Sum log likelihoods from each datastream
combined_compare <- function(state, observed, pars = NULL) {
  ll_cases <- ll_pois(observed$cases, state["cases", , drop = TRUE])
}

n_particles <- 800
filter <- mcstate::particle_filter$new(data = mc_data_Italy,
                                       model = gen_SEIARD,
                                       n_particles = n_particles,
                                       compare = combined_compare,
                                       index = index,
                                       seed = 1L)


c <- mcstate::pmcmc_parameter("c",  0.5, prior = function(p)
  dnorm(p, mean = 0.1, sd = 6, log = TRUE)  )
#alpha <- mcstate::pmcmc_parameter("alpha", 16,  min = 7, max = 20 )
d <- mcstate::pmcmc_parameter("d", 0.7, min = 0, max = 1 )
# rho <- mcstate::pmcmc_parameter("rho", 0.3, min = 0, prior = function(p)
#   dnorm(p, mean = 0.3, sd = 1, log = TRUE) )
E0_ini <- mcstate::pmcmc_parameter("E0_ini",  10, min = 0, prior = function(p)
  dnorm(p, mean = 10, sd = 10, log = TRUE)  )

sigma <- mcstate::pmcmc_parameter("sigma",  11, min = 0, prior = function(p)
  dnorm(p, mean = 10, sd = 10, log = TRUE)  )

proposal_matrix <- diag(c(0.1,0.1,0.1,0.1))
mcmc_pars <- mcstate::pmcmc_parameters$new(list(c = c,  
                                                d = d,
                                                E0_ini = E0_ini,
                                                sigma = sigma),
                                           proposal_matrix,
                                           make_transform(data1 = merged_mob_ave[1:744],
                                                          data2 = gamma_data[1:744],
                                                          data3 = adj_protection$aju_1[1:744],
                                                          data4 = adj_protection$aju_2[1:744],
                                                          data5 = adj_protection$aju_3[1:744],,
                                                          data6 = escape_data$escape_score[1:744],
                                                          data7 = as.double(vaccine_data_Italy_merged$d1[1:744]),
                                                          data8 = as.double(vaccine_data_Italy_merged$d2[1:744]),
                                                          data9 = as.double(vaccine_data_Italy_merged$db1[1:744]) ))

n_steps <- 3000
n_burnin <- 1000

control <- mcstate::pmcmc_control(n_steps,save_state = TRUE,save_trajectories = TRUE,
                                  n_chains = 10,progress = T)

pmcmc_run <- mcstate::pmcmc(mcmc_pars, filter, control = control)


processed_chains <- mcstate::pmcmc_thin(pmcmc_run, burnin = n_burnin)

cases_fitting <- processed_chains$trajectories$state["cases",,]
beta_v_fitting <- processed_chains$trajectories$state["beta_v",,]


df <- apply(processed_chains$trajectories$state["cases",,], 2, mean)
a <- processed_chains$trajectories$state["cases",,]
df_beta_v <- apply(processed_chains$trajectories$state["beta_v",,-1], 2, mean)
df_beta_imm_0<- apply(processed_chains$trajectories$state["beta_imm_0",,-1], 2, mean)
apply(processed_chains$trajectories$state["beta_v",,-1], 2, quantile,prob = c(0.025,0.5,0.975))
plot(df_beta_imm_0)
a <- processed_chains$trajectories$state["beta_v",,-1]
df_rho_t <- apply(processed_chains$trajectories$state["rho_t",,-1], 2, mean)
#df_eff_imm<- apply(processed_chains$trajectories$state["eff_imm",,-1], 2, mean)
plot(df_beta_v,type = "l")
abline(h=0,lty=2, col="red")
plot(df_rho_t,type = "l")
#df_eff_imm[1:2] <- 0
#plot(df_eff_imm,type = "l")

par(mar = c(7, 6, 3, 5))
date_index <- which(seq(ymd("2020-02-21"), ymd('2022-03-05'), by='1 day')  %in% seq(ymd("2020-03-01"), ymd('2022-03-05'), by='2 month') == T)
plot(c(mc_data_Italy$cases[1],diff(mc_data_Italy$cases)),  cex.axis = 1.2,cex.lab = 1.2,
     lty = 1, col=rgb(0.4,0.4,0.8,0.6), xlab = "", xaxt="n",pch = 19,cex=0.7,yaxt="n", ylim = c(0,250000),
     ylab = "Incidence\n(thousands)", main = "Model fitting for daily confirmed cases(Italy, smoothed)")
lines(diff(df), col = 2, pch = 19, lwd=1)
axis(1, at = date_index,labels = Italy_reported_cases[1:744,]$date[date_index], las = 2, cex.axis = 1.2)
axis(2, at = seq(0,250000, by = 50000),labels = seq(0,250, by = 50), las = 2, cex.axis = 1.2)
abline(v=date_index,lty=2,col="lightgray")
abline(h=seq(0,250000, by = 50000),lty=2,col="lightgray")





