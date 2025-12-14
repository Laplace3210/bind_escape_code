## Definition of the time-step and output as "time"
steps_per_day <- user(4)
dt <- 1 / steps_per_day
initial(time) <- 0
update(time) <- (step + 1) * dt

## Core equations for transitions between compartments:
update(S0) <- S0 + n_S1S0 + n_S2S0 + n_S3S0 + n_R0S0 + n_R1S1 + n_R2S2 + n_R3S3  - n_S0E0 - V1[step+1] + V1[step+1]*(1-p_imm_1[step+1]) + V2[step+1]*(1-p_imm_2[step+1]) + V3[step+1]*(1-p_imm_3[step+1]) 
update(E0) <- E0 + n_S0E0 - n_E0I0
update(I0) <- I0 + n_E0I0 - n_I0R0
update(R0) <- R0 + n_I0R0 - n_R0S0

update(S1) <- V1[step+1]  - n_S1E1 - V2[step+1]
update(E1) <- E1 + n_S1E1 - n_E1I1
update(I1) <- I1 + n_E1I1 - n_I1R1
update(R1) <- R1 + n_I1R1 - n_R1S1

update(S2) <- V2[step+1]  - n_S2E2 - V3[step+1]
update(E2) <- E2 + n_S2E2 - n_E2I2
update(I2) <- I2 + n_E2I2 - n_I2R2
update(R2) <- R2 + n_I2R2 - n_R2S2

update(S3) <- V3[step+1]  - n_S3E3
update(E3) <- E3 + n_S3E3 - n_E3I3
update(I3) <- I3 + n_E3I3 - n_I3R3
update(R3) <- R3 + n_I3R3 - n_R3S3

update(x_v) <- rnorm(0, tao_beta_v^2)
update(beta_v) <- if (beta_v * exp(x_v) > 2.5 ) 2.5 else beta_v * exp(x_v)#beta_v * exp(x_v) #23
update(beta_imm_0) <- beta_v * mobility[step+1] 
update(beta_imm_1) <- beta_v * mobility[step+1] * (1 - VE1 * exp(-c * imm_score[step+1]) )
update(beta_imm_2) <- beta_v * mobility[step+1] * (1 - VE2 * exp(-c * imm_score[step+1]) )
update(beta_imm_3) <- beta_v * mobility[step+1] * (1 - VE3 * exp(-c * imm_score[step+1]) )

update(x_rho) <- rnorm(0, tao_rho^2)
update(rho_t) <- if (rho_t * exp(x_rho) > 1 ) 1 else rho_t * exp(x_rho)
# update(x_gamma) <- rnorm(0, tao_gamma^2)
# update(gamma_t) <- if (gamma_t * exp(x_gamma) < 0 ) 0 else gamma_t * exp(x_gamma)

## Individual probabilities of transition:
p_S0E0 <- beta_imm_0 * (I0 + I1 + I2 + I3) / N # S0 to E0
n_S0E0 <- rpois(p_S0E0 * S0 * dt)

p_S1E1 <- beta_imm_1 * (I0 + I1 + I2 + I3) / N # S1 to E1
n_S1E1 <- rpois(p_S1E1 * S1 * dt)

p_S2E2 <- beta_imm_2 * (I0 + I1 + I2 + I3) / N # S2 to E2
n_S2E2 <- rpois(p_S2E2 * S2 * dt)

p_S3E3 <- beta_imm_3 * (I0 + I1 + I2 + I3) / N # S3 to E3
n_S3E3 <- rpois(p_S3E3 * S3 * dt)

p_EI <- 1/sigma
n_E0I0 <- rpois(p_EI  * E0 * dt)
n_E1I1 <- rpois(p_EI  * E1 * dt)
n_E2I2 <- rpois(p_EI  * E2 * dt)
n_E3I3 <- rpois(p_EI  * E3 * dt)

#n_IR
#p_IR <- 1/gamma_t[step+1]
n_I0R0 <- rpois(I0* gamma_t[step+1] *dt )
n_I1R1 <- rpois(I1* gamma_t[step+1] *dt )
n_I2R2 <- rpois(I2* gamma_t[step+1] *dt )
n_I3R3 <- rpois(I3* gamma_t[step+1] *dt )

#n_R_S and n_S_S
p_RS <- 1/omega
n_R0S0 <- rpois(p_RS*R0*dt)
n_R1S1 <- rpois(p_RS*R1*dt)
n_R2S2 <- rpois(p_RS*R2*dt)
n_R3S3 <- rpois(p_RS*R3*dt)

n_S1S0 <- rpois(p_RS*S1*dt)
n_S2S0 <- rpois(p_RS*S2*dt)
n_S3S0 <- rpois(p_RS*S3*dt)

## Total population size
N <- user(59500579)

## Initial states
initial(S0) <- S0_ini
initial(S1) <- S1_ini
initial(S2) <- S2_ini
initial(S3) <- S3_ini
initial(E0) <- E0_ini
initial(E1) <- E1_ini
initial(E2) <- E2_ini
initial(E3) <- E3_ini
initial(I0) <- I0_ini
initial(I1) <- I1_ini
initial(I2) <- I2_ini
initial(I3) <- I3_ini
# initial(IR0) <- IR0_ini
# initial(IR1) <- IR1_ini
# initial(IR2) <- IR2_ini
# initial(IR3) <- IR3_ini
initial(R0) <- R0_ini
initial(R1) <- R1_ini
initial(R2) <- R2_ini
initial(R3) <- R3_ini

## Convert variables (incidence, effective immunity )
# incidence
initial(E_inc) <- 400
#update(E_inc) <- if (step %% steps_per_day == 0) d * (n_I0IR0 + n_I1IR1 + n_I2IR2 + n_I3IR3) * rho_t else E_inc + d * (n_I0IR0 + n_I1IR1 + n_I2IR2 + n_I3IR3) * rho_t
update(E_inc) <- E_inc + d * (n_E0I0 + n_E1I1 + n_E2I2 + n_E3I3)  * rho_t

## User defined parameters - default in parentheses:
mobility[] <- user()
dim(mobility) <- user()

gamma_t[] <- user()
dim(gamma_t) <- user()

#protection index
p_imm_1[] <- user()
dim(p_imm_1) <- user()
p_imm_2[] <- user()
dim(p_imm_2) <- user()
p_imm_3[] <- user()
dim(p_imm_3) <- user()

#immune escape index
imm_score[] <- user()
dim(imm_score) <- user()

#number of population with different vaccine doses
V1[] <- user()
dim(V1) <- user()
V2[] <- user()
dim(V2) <- user()
V3[] <- user()
dim(V3) <- user()

##parameters setting 
tao_beta_v <- user(0.5) # precision beta
tao_rho <- user(0.5) # precision rho
# tao_gamma <- user(0.5)

initial(x_v) <- 0
initial(beta_v) <- 0.24
initial(x_rho) <- 0
initial(rho_t) <- 0.9
# initial(x_gamma) <- 0
# initial(gamma_t) <- 0.1
initial(beta_imm_0) <- 0.403
initial(beta_imm_1) <- 0.258
initial(beta_imm_2) <- 0.235
initial(beta_imm_3) <- 0.2

S0_ini <- user(59500579-14)
S1_ini <- user(0)
S2_ini <- user(0)
S3_ini <- user(0)
E0_ini <- user(100)
E1_ini <- user(0)
E2_ini <- user(0)
E3_ini <- user(0)
I0_ini <- user(14)
I1_ini <- user(0)
I2_ini <- user(0)
I3_ini <- user(0)
# IR0_ini <- user(0)
# IR1_ini <- user(0)
# IR2_ini <- user(0)
# IR3_ini <- user(0)
R0_ini <- user(0)
R1_ini <- user(0)
R2_ini <- user(0)
R3_ini <- user(0)


VE1 <- user(0.79)
VE2 <- user(0.89)
VE3 <- user(0.95)
##parameters should be evaluated 
c <- user(0.85)
sigma <- user(11)
omega <- user(1123) #i.e., log(0.85)/182.5
d <- user(0.7)