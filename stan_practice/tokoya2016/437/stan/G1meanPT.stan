data {
int<lower=0> n;                          //�f�[�^��
real<lower=0> x[n];                      //�f�[�^
real mL; real mH; real sL; real sH;      //���O���z
}
parameters {
real<lower=mL,upper=mH> mu;              //����(�͈͎w��)
real<lower=sL,upper=sH> sigma;           //�W���΍�(�͈͎w��)
}
transformed parameters {
}
model {
x ~ normal(mu,sigma);                    //���K���z
}
generated quantities{
real xaste;
real log_lik;
xaste= normal_rng(mu,sigma);             //�\�����z
log_lik=normal_lpdf(x|mu,sigma);         //�ΐ��m��
}
