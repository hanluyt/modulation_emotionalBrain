load('data_matrix_angry.mat') % obtain data matrix
addpath('/Users/user/ANOVA_14_19/matlab/l0-sparse-NMF-master');
addpath('/Users/user/ANOVA_14_19/matlab/hungarian');
%% Angry > Neutral activations(split half)  
num_subjects = size(data_matrix, 2);
all_X = data_matrix;
all_X(all_X<0)=0;

X=data_matrix(:, 1:fix(num_subjects / 2)); 
X(X<0)=0;

X2=data_matrix(:, fix(num_subjects / 2) + 1:num_subjects);
X2(X2<0)=0;

clear data_matrix

%% NMF Algorithm
K = 2;
eps_last = []; % Reconstruction error
similarity = []; % Correlation
sparse = 0.1:0.1:0.9;

[D,N] = size(X); 

for i=1:9
    prcnt = sparse(i); 
    prcnt = prcnt*100; 
    % NMF parameters
    options.K = K;  % number of columns in dictionary matrix W
    options.L = round(D*prcnt/100); % maximal number of nonzeros in each column of W
    options.numIter = 100; % number of (outer) iterations
    options.updateType = 'ANLS_FC';
    options.numUpdateIter = 10;  % number of update (innner) iterations.
    options.verbosity=0;
    eps = []; % reconstruction error
    corr1=[]; % correlation
   
    fprintf('sparse: %d\n', sparse(i))
    
    % NMF procedure was repeated 80 times to weaken the influence of the random initializations
    for B=1:80
        [W, H, INFO] = NMFL0_W(all_X,options);
        [W1,H1,INFO1] = NMFL0_W(X,options);
        [W2,H2,INFO2] = NMFL0_W(X2,options);
        [C,p]=corr(W1,W2);
        [index1,index2] = linear_sum_assignment(-C); 
        LinearInd=sub2ind(size(C),index1,index2);
        corr1(B)=mean(C(LinearInd));
        eps(B)=INFO.E(end);
        fprintf('NMF repeated : %d times\n', B)
    end
    
    eps_last(i)=mean(eps);
    similarity(i)=mean(corr1);  
    
end

%% save reconstruction error and similarity for K = 2
save('similarity_pos2.mat','similarity')
save('eps_pos2.mat','eps_last')

   






