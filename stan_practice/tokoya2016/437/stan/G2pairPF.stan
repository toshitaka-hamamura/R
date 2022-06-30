data { 
  int<lower=0> n;                             //�f�[�^��   
  vector[2] x[n];                             //�f�[�^     
  real EQU;                                   //�r�c�A1 - ���ʁA0 - �قȂ�
}
parameters {
  vector                    [2] mu;          //����(�͈͎w�肹��)
  real<lower=0>           sigma1;            //�W���΍�(�͈͎w�肹��)
  real<lower=0>           dummy;             //�_�~�[�̂r�c(�͈͎w�肹��)
  real<lower=-1,upper=1>  rho;                //����
}
transformed parameters {
  real<lower=0>           sigma2;             //�W���΍�2
  cov_matrix[2]           Sigma;
  sigma2 =EQU>0.5 ? sigma1 : dummy;
  Sigma[1,1]  = pow(sigma1,2);                //�����U�s��
  Sigma[2,2]  = pow(sigma2,2);
  Sigma[1,2]  = sigma1*sigma2*rho;
  Sigma[2,1]  = Sigma[1,2];
}
model {
  for(i in 1:n){x[i]~multi_normal(mu,Sigma);} //2�ϗʐ��K���z
}
generated quantities{
  vector[2] xaste;
  real log_lik;
  xaste  =  multi_normal_rng(mu,Sigma);       //�\�����z
  log_lik  = multi_normal_lpdf(x | mu, Sigma);  //�ΐ��ޓx
}
